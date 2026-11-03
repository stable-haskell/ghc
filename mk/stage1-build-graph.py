#!/usr/bin/env python3
"""Stage1 package timeline.

Make times the whole stage1 cabal invocation as one phase. This script
records each package component and the dependency it waited on.

    mk/stage1-build-graph.py stamp LOG
        Copy stdin to stdout. Truncate LOG and append "epoch<TAB>line".

    mk/stage1-build-graph.py render --plan PLAN.json --html OUT.html
        [--log STAMPED] [--json OUT.json] [--markdown OUT.md]

    mk/stage1-build-graph.py self-test

Without a log the bars are a plan: every built component lasts one unit of
dependency depth. That axis is not wall-clock time. With a stamped log the
second chart uses the timestamps, and components the log never mentions stay
off that axis.
"""

from __future__ import annotations

import argparse
import html
import json
import re
import sys
import time
from collections import defaultdict
from dataclasses import dataclass, field


ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
# Cabal progress lines, after the fixed-width phase word:
#   Building     filepath-1.5.4.0 (lib)
#   Completed    filepath-1.5.4.0 (lib) (3.2s)
PROGRESS_RE = re.compile(
    r"^(Starting|Building|Installing|Completed)\s+"
    r"(\S+)"
    r"(?:\s+\(([^)]+)\))?"
    r"(?:\s+\(([^)]+)\))?"
    r"\s*$"
)
# Older cabal wording, still emitted by some code paths.
OLD_BUILD_RE = re.compile(
    r"^Building (library|executable '([^']+)'|Setup\.hs) for (\S+?)\.{2,3}\s*$"
)


@dataclass
class Node:
    key: str
    unit_id: str
    pkg: str
    version: str
    stage: str
    component: str
    built: bool
    raw_deps: list[str] = field(default_factory=list)
    raw_exe: list[str] = field(default_factory=list)
    preds: list[str] = field(default_factory=list)

    @property
    def disp(self) -> str:
        return f"{self.pkg}-{self.version}"

    def short(self) -> str:
        comp = (self.component or "lib").replace(":", " ")
        return f"{self.pkg}-{self.version} {comp} [{self.stage}]"


def norm_comp(component: str | None) -> str:
    if not component:
        return "lib"
    text = component.strip()
    if text in ("library", "lib"):
        return "lib"
    if text.lower() in ("setup", "setup.hs"):
        return "setup"
    if text.startswith("executable "):
        name = text[len("executable ") :].strip().strip("'\"")
        return "exe:" + name
    return text


def load_plan(path: str) -> tuple[dict, list[Node]]:
    """Read cabal's plan.json.

    Cabal 3.17 emits one install-plan entry per component (`component-name`
    plus top-level `depends`). A few packages still use a `components` map
    (lib and setup on one entry). An empty `components` map is a package the
    solver kept but this platform does not build (Win32 on Darwin).
    """
    with open(path, encoding="utf-8") as handle:
        doc = json.load(handle)
    units = doc.get("install-plan") or []
    by_unit: dict[str, dict] = {}
    nodes: list[Node] = []
    lib_of: dict[str, str] = {}

    def add(unit: dict, component: str, deps: list[str], exe: list[str], built: bool) -> None:
        unit_id = unit["id"]
        # One entry per component. Package-level entries use the unit id.
        key = unit_id if component in ("", "lib") and "components" not in unit else f"{unit_id}#{component or 'pkg'}"
        if any(n.key == key for n in nodes):
            key = f"{unit_id}#{component or 'pkg'}#{len(nodes)}"
        node = Node(
            key=key,
            unit_id=unit_id,
            pkg=unit.get("pkg-name") or unit_id,
            version=str(unit.get("pkg-version") or ""),
            stage=unit.get("stage") or unit_id.split(":", 1)[0],
            component=norm_comp(component or unit.get("component-name")),
            built=built,
            raw_deps=list(deps or []),
            raw_exe=list(exe or []),
        )
        nodes.append(node)
        by_unit[unit_id] = unit
        # `depends` names the library of that unit.
        if node.component == "lib" or unit_id not in lib_of:
            lib_of[unit_id] = node.key

    for unit in units:
        kind = unit.get("type")
        comps = unit.get("components")
        if kind == "configured" and isinstance(comps, dict):
            if not comps:
                continue
            for name, info in comps.items():
                if not isinstance(info, dict) or not info:
                    continue
                add(unit, name, info.get("depends") or [], info.get("exe-depends") or [], True)
            continue
        built = kind == "configured"
        component = unit.get("component-name") or ""
        add(unit, component, unit.get("depends") or [], unit.get("exe-depends") or [], built)

    # Resolve dependency ids. Component deps inside a `components` map are
    # often unqualified; prefix the unit's own stage, then the other stage.
    index = {n.unit_id: n for n in nodes}
    # lib_of maps unit id -> node key of the library (or the only component).
    # Rebuild it from the nodes we actually created. The last lib wins; an
    # exe-only unit keeps the first assignment from add().
    resolved_lib: dict[str, str] = {}
    for node in nodes:
        if node.unit_id not in resolved_lib or node.component == "lib":
            resolved_lib[node.unit_id] = node.key

    def resolve(dep: str, stage: str, component: str) -> str | None:
        # Setup is compiled by the build-stage compiler, so its bare
        # dependencies are the build-stage units. Every other component
        # uses its own stage.
        if dep.startswith("host:") or dep.startswith("build:"):
            unit_id = dep
        else:
            prefer = "build" if component == "setup" else stage
            unit_id = f"{prefer}:{dep}"
            if unit_id not in resolved_lib:
                other = "host" if prefer == "build" else "build"
                alt = f"{other}:{dep}"
                unit_id = alt if alt in resolved_lib else unit_id
        return resolved_lib.get(unit_id)

    for node in nodes:
        seen: set[str] = set()
        for dep in node.raw_deps + node.raw_exe:
            key = resolve(dep, node.stage, node.component)
            if key and key != node.key and key not in seen:
                seen.add(key)
                node.preds.append(key)
    # Cabal builds Setup before the library or executable of that package.
    setup_of = {n.unit_id: n.key for n in nodes if n.component == "setup"}
    for node in nodes:
        setup = setup_of.get(node.unit_id)
        if setup and setup != node.key and setup not in node.preds:
            node.preds.append(setup)
    return doc, nodes


def select_nodes(nodes: list[Node], roots: list[str] | None) -> list[Node]:
    """Keep the host executables named by roots and everything they wait on.

    Cabal's plan also configures packages that this build does not compile
    (a host copy of alex, libffi-clib with no dependents). Starting from the
    stage1 executables drops those.
    """
    if not roots:
        return nodes
    wanted = {f"exe:{name}" for name in roots}
    by_key = {n.key: n for n in nodes}
    seeds = [
        n.key
        for n in nodes
        if n.built and n.stage == "host" and norm_comp(n.component) in wanted
    ]
    if not seeds:
        raise SystemExit(
            "none of the --root executables are host components in the plan: "
            + ", ".join(roots)
        )
    seen: set[str] = set()
    stack = list(seeds)
    while stack:
        key = stack.pop()
        if key in seen:
            continue
        seen.add(key)
        stack.extend(by_key[key].preds)
    return [n for n in nodes if n.key in seen]


def schedule(nodes: list[Node], duration) -> tuple[dict[str, float], dict[str, float]]:
    built = {n.key: n for n in nodes if n.built}
    colour = {k: 0 for k in built}
    start: dict[str, float] = {}
    finish: dict[str, float] = {}

    def walk(key: str) -> None:
        if colour[key] == 1:
            raise SystemExit(f"stage1 plan has a dependency cycle at {key}")
        if colour[key] == 2:
            return
        colour[key] = 1
        preds = [p for p in built[key].preds if p in built]
        for pred in preds:
            walk(pred)
        start[key] = max((finish[p] for p in preds), default=0.0)
        finish[key] = start[key] + float(duration(key))
        colour[key] = 2

    for key in built:
        walk(key)
    return start, finish


def critical_path(nodes: list[Node], start: dict[str, float], finish: dict[str, float]) -> list[str]:
    built = [n for n in nodes if n.built and n.key in finish]
    if not built:
        return []
    by_key = {n.key: n for n in nodes}
    tip = max(built, key=lambda n: (finish[n.key], n.disp))
    path = [tip.key]
    seen = {tip.key}
    while True:
        cur = path[-1]
        preds = [p for p in by_key[cur].preds if by_key[p].built and p in finish and p not in seen]
        if not preds or start[cur] == 0:
            break
        waited = max(preds, key=lambda p: (finish[p], p))
        if finish[waited] + 1e-9 < start[cur] and finish[waited] != max(finish[p] for p in preds):
            break
        path.append(waited)
        seen.add(waited)
    path.reverse()
    return path


def waits(node: Node, nodes: dict[str, Node], start: dict[str, float], finish: dict[str, float]) -> tuple[list[str], list[str]]:
    preds = [p for p in node.preds if nodes[p].built and p in finish]
    if not preds:
        return [], []
    latest = max(finish[p] for p in preds)
    blocking = [p for p in preds if abs(finish[p] - latest) < 1e-9]
    others = [p for p in preds if p not in blocking]
    return blocking, others


def strip_ansi(line: str) -> str:
    return ANSI_RE.sub("", line).rstrip("\n")


def classify(line: str) -> tuple[str, str, str | None] | None:
    """Return (phase, pkg-version, component) or None."""
    line = strip_ansi(line).strip()
    match = PROGRESS_RE.match(line)
    if match:
        phase, pkg, comp, _dur = match.groups()
        return phase, pkg, comp
    old = OLD_BUILD_RE.match(line)
    if old:
        kind, exe_name, pkg = old.groups()
        if kind == "library":
            comp = "lib"
        elif kind == "Setup.hs":
            comp = "setup"
        else:
            comp = "exe:" + exe_name
        return "Building", pkg, comp
    return None


def match_log(nodes: list[Node], log_path: str) -> dict[str, tuple[float, float, bool]]:
    """Map node key -> (t0, t1, complete) for components the log actually built.

    Host and build units share a package name. A line is given to a candidate
    whose built dependencies are already completed, and build-stage before
    host-stage when several are ready. Components the log never mentions are
    absent from the result so they are not drawn on the wall-clock axis.
    """
    by_key = {n.key: n for n in nodes}
    built = [n for n in nodes if n.built]
    assigned: set[str] = set()
    completed: set[str] = set()
    open_at: dict[str, float] = {}
    done: dict[str, tuple[float, float, bool]] = {}

    def pick(pkg: str, comp: str | None) -> Node | None:
        cands = [n for n in built if n.key not in assigned and n.disp == pkg]
        if comp is not None:
            want = norm_comp(comp)
            cands = [n for n in cands if norm_comp(n.component) == want]
        else:
            libs = [n for n in cands if norm_comp(n.component) == "lib"]
            if libs:
                cands = libs
        if not cands:
            return None

        def rank(node: Node) -> tuple:
            ready = all((p in completed) or (not by_key[p].built) for p in node.preds)
            stage = 0 if node.stage == "build" else 1
            return (0 if ready else 1, stage, node.component)

        cands.sort(key=rank)
        return cands[0]

    last_epoch: float | None = None
    with open(log_path, encoding="utf-8", errors="replace") as handle:
        for raw in handle:
            epoch: float | None
            if "\t" in raw:
                head, msg = raw.split("\t", 1)
                try:
                    epoch = float(head)
                except ValueError:
                    epoch = None
                    msg = raw
            else:
                epoch = None
                msg = raw
            if epoch is not None:
                last_epoch = epoch
            parsed = classify(msg)
            if parsed is None or epoch is None:
                continue
            phase, pkg, comp = parsed
            if phase in ("Starting", "Building"):
                node = pick(pkg, comp)
                if node is None or node.key in open_at:
                    continue
                assigned.add(node.key)
                open_at[node.key] = epoch
            elif phase == "Completed":
                node = None
                want = norm_comp(comp) if comp else None
                for key, _t0 in open_at.items():
                    if key in done:
                        continue
                    cur = by_key[key]
                    if cur.disp != pkg:
                        continue
                    if want is not None and norm_comp(cur.component) != want:
                        continue
                    node = cur
                    break
                if node is None:
                    continue
                t0 = open_at.pop(node.key)
                done[node.key] = (t0, epoch, True)
                completed.add(node.key)
    end = last_epoch if last_epoch is not None else 0.0
    for key, t0 in open_at.items():
        done[key] = (t0, max(end, t0), False)
        completed.add(key)
    return done


def cmd_stamp(log_path: str) -> int:
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[attr-defined]
    with open(log_path, "w", encoding="utf-8") as log:
        for line in sys.stdin:
            sys.stdout.write(line)
            sys.stdout.flush()
            log.write(f"{time.time():.3f}\t{line}")
            log.flush()
    return 0


def bar_rows(nodes: list[Node], start: dict[str, float], finish: dict[str, float], crit: set[str]) -> list[dict]:
    by_key = {n.key: n for n in nodes}
    rows = []
    for node in nodes:
        if not node.built or node.key not in start:
            continue
        blocking, others = waits(node, by_key, start, finish)
        rows.append(
            {
                "key": node.key,
                "label": node.short(),
                "pkg": node.pkg,
                "version": node.version,
                "stage": node.stage,
                "component": node.component,
                "start": start[node.key],
                "end": finish[node.key],
                "critical": node.key in crit,
                "waits_on": [by_key[p].short() for p in blocking],
                "also_needs": [by_key[p].short() for p in others],
            }
        )
    rows.sort(key=lambda row: (row["start"], row["stage"], row["label"]))
    return rows


def render_html(title: str, note: str, rows: list[dict], preexisting: list[str], absent: list[str], meta: dict) -> str:
    if not rows:
        span = 1.0
        t0 = 0.0
    else:
        t0 = min(row["start"] for row in rows)
        span = max(row["end"] for row in rows) - t0
        if span <= 0:
            span = 1.0
    axis = "dependency depth (one unit per built component, not seconds)" if "not wall-clock" in note else "seconds from the first stamped cabal line"
    body = []
    for row in rows:
        left = (row["start"] - t0) / span * 100
        width = max((row["end"] - row["start"]) / span * 100, 0.6)
        kind = "crit" if row["critical"] else row["stage"]
        wait = ", ".join(row["waits_on"]) if row["waits_on"] else "nothing built (installed packages only)"
        also = ""
        if row["also_needs"]:
            also = " Also needs " + ", ".join(row["also_needs"]) + "."
        tip = f"{row['label']}. Waits on {wait}.{also}"
        body.append(
            "<div class='row'>"
            f"<div class='name' title='{html.escape(tip)}'>{html.escape(row['label'])}</div>"
            "<div class='track'>"
            f"<div class='bar {html.escape(kind)}' style='left:{left:.4f}%;width:{width:.4f}%' "
            f"title='{html.escape(tip)}'></div></div>"
            f"<div class='wait'>{html.escape(wait)}</div></div>"
        )
    pre = ", ".join(preexisting) if preexisting else "(none)"
    missing = ""
    if absent:
        missing = (
            "<h2>In the plan, absent from the log</h2>"
            "<p>These components are configured, but this log never built them. "
            "They are not drawn on the wall-clock chart.</p><p class='pre'>"
            + html.escape(", ".join(absent))
            + "</p>"
        )
    compiler = html.escape(str(meta.get("compiler-id") or ""))
    platform = html.escape(f"{meta.get('arch') or ''}-{meta.get('os') or ''}")
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>{html.escape(title)}</title>
<style>
  body {{ font: 14px/1.4 ui-sans-serif, system-ui, sans-serif; margin: 24px; color: #1c1917; }}
  h1 {{ font-size: 20px; margin: 0 0 8px; }}
  .note {{ color: #44403c; max-width: 70rem; }}
  .legend span {{ display: inline-block; margin-right: 16px; }}
  .swatch {{ display: inline-block; width: 12px; height: 12px; margin-right: 4px; vertical-align: -1px; }}
  .host {{ background: #1d4ed8; }}
  .build {{ background: #0f766e; }}
  .crit {{ background: #b45309; }}
  .row {{ display: grid; grid-template-columns: 22rem 1fr 18rem; gap: 8px; align-items: center; margin: 3px 0; }}
  .name {{ overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-family: ui-monospace, monospace; font-size: 12px; }}
  .track {{ position: relative; height: 16px; background: #f5f5f4; }}
  .bar {{ position: absolute; top: 2px; bottom: 2px; border-radius: 2px; }}
  .wait {{ font-size: 12px; color: #44403c; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }}
  h2 {{ font-size: 16px; margin-top: 28px; }}
  .pre {{ font-family: ui-monospace, monospace; font-size: 12px; }}
  .head {{ display: grid; grid-template-columns: 22rem 1fr 18rem; gap: 8px; font-size: 12px; color: #78716c; margin-top: 16px; }}
</style>
</head>
<body>
<h1>{html.escape(title)}</h1>
<p class="note">{html.escape(note)} Compiler {compiler}, platform {platform}. Axis: {html.escape(axis)}.</p>
<p class="legend">
  <span><i class="swatch host"></i>host</span>
  <span><i class="swatch build"></i>build</span>
  <span><i class="swatch crit"></i>critical path</span>
</p>
<div class="head"><div>package</div><div>when it is built</div><div>waiting on</div></div>
{''.join(body)}
<h2>Already installed, not compiled</h2>
<p class="pre">{html.escape(pre)}</p>
{missing}
</body>
</html>
"""


def render_markdown(title: str, note: str, rows: list[dict], preexisting: list[str], meta: dict) -> str:
    lines = [f"### {title}", "", note, ""]
    compiler = meta.get("compiler-id") or "unknown compiler"
    lines.append(
        f"Plan for {compiler} on {meta.get('arch') or '?'}-{meta.get('os') or '?'}. "
        "Each built component is one unit wide. Packages on the same row run together. "
        "The package in **waiting on** is the predecessor that finishes last."
    )
    lines.append("")
    lines.append("```mermaid")
    lines.append("gantt")
    lines.append(f"    title {title}")
    lines.append("    dateFormat X")
    lines.append("    axisFormat %s")
    grouped: dict[float, list[dict]] = defaultdict(list)
    for row in rows:
        grouped[row["start"]].append(row)
    seq = 0
    for depth in sorted(grouped):
        lines.append(f"    section t={depth:g}")
        for row in grouped[depth]:
            seq += 1
            tag = "crit, " if row["critical"] else ""
            # Mermaid treats ':' as field syntax and '[]' as node syntax.
            label = (
                row["label"]
                .replace(":", " ")
                .replace("[", "(")
                .replace("]", ")")
            )
            lines.append(f"    {label} :{tag}p{seq}, {row['start']:g}, {row['end']:g}")
    lines.append("```")
    lines.append("")
    lines.append("| when | package | waiting on |")
    lines.append("| --- | --- | --- |")
    for depth in sorted(grouped):
        for row in grouped[depth]:
            wait = ", ".join(row["waits_on"]) if row["waits_on"] else "installed packages only"
            mark = " **critical**" if row["critical"] else ""
            lines.append(f"| {depth:g} | {row['label']}{mark} | {wait} |")
    lines.append("")
    crit_rows = [row for row in rows if row["critical"]]
    if crit_rows:
        lines.append("Critical path, in build order:")
        lines.append("")
        lines.append("```mermaid")
        lines.append("flowchart LR")
        for i, row in enumerate(crit_rows):
            label = row["label"].replace('"', "'").replace("[", "(").replace("]", ")")
            lines.append(f'    n{i}["{label}"]')
            if i:
                lines.append(f"    n{i-1} --> n{i}")
        lines.append("```")
        lines.append("")
    lines.append("Already installed, not compiled: " + (", ".join(preexisting) if preexisting else "(none)"))
    lines.append("")
    return "\n".join(lines)


def build_view(doc: dict, nodes: list[Node], log_path: str | None) -> dict:
    by_key = {n.key: n for n in nodes}
    plan_start, plan_finish = schedule(nodes, lambda _key: 1.0)
    plan_crit = critical_path(nodes, plan_start, plan_finish)
    plan_rows = bar_rows(nodes, plan_start, plan_finish, set(plan_crit))
    preexisting = sorted({f"{n.pkg}-{n.version}" for n in nodes if not n.built})
    absent: list[str] = []
    log_rows: list[dict] = []
    log_note = ""
    if log_path:
        spans = match_log(nodes, log_path)
        if not spans:
            log_note = "The log had no stamped Building/Completed lines, so there is no wall-clock chart."
        else:
            logged = {n.key: n for n in nodes if n.key in spans}
            # Times come only from the log. Missing components stay off this axis.
            def dur(key: str) -> float:
                t0, t1, _ok = spans[key]
                return max(t1 - t0, 0.0)

            # Rebase so the chart starts at the first stamp, but keep absolute
            # order. schedule() would invent times for nodes without spans, so
            # build the start/finish maps directly from the log.
            log_start = {key: spans[key][0] for key in spans}
            log_finish = {key: spans[key][1] for key in spans}
            # critical_path and bar_rows ignore nodes that have no finish time,
            # so unlogged components stay off this axis without dropping edges.
            log_crit = set(critical_path(nodes, log_start, log_finish))
            log_rows = bar_rows(nodes, log_start, log_finish, log_crit)
            absent = [by_key[n.key].short() for n in nodes if n.built and n.key not in spans]
            absent.sort()
            log_note = "Wall-clock bars are the stamped cabal log. Components the log does not mention are listed under it, not drawn on the same axis."
    return {
        "meta": {k: doc.get(k) for k in ("compiler-id", "os", "arch", "cabal-version")},
        "plan_rows": plan_rows,
        "plan_note": "This chart is a plan. Each built component takes one unit of dependency depth. It is not wall-clock time.",
        "log_rows": log_rows,
        "log_note": log_note,
        "preexisting": preexisting,
        "absent": absent,
        "configured": sorted({f"{n.stage}:{n.pkg}-{n.version} ({n.component})" for n in nodes if n.built}),
    }


def write_outputs(view: dict, html_path: str | None, json_path: str | None, markdown_path: str | None) -> None:
    meta = view["meta"]
    sections = []
    if html_path:
        parts = [
            render_html(
                "Stage1 plan: when each package is built",
                view["plan_note"],
                view["plan_rows"],
                view["preexisting"],
                [],
                meta,
            )
        ]
        if view["log_rows"] or view["log_note"]:
            parts.append("<hr>")
            parts.append(
                render_html(
                    "Stage1 log: when each package was built",
                    view["log_note"] or view["plan_note"],
                    view["log_rows"],
                    view["preexisting"],
                    view["absent"],
                    meta,
                )
            )
        # Two full documents would nest badly. Concatenate body sections instead.
        text = parts[0]
        if len(parts) > 1:
            text = text.replace("</body>", parts[1] + parts[2].split("<body>", 1)[1])
        with open(html_path, "w", encoding="utf-8") as handle:
            handle.write(text)
    if markdown_path:
        text = render_markdown(
            "Stage1 plan: when each package is built",
            view["plan_note"],
            view["plan_rows"],
            view["preexisting"],
            meta,
        )
        if view["log_rows"]:
            text += "\n" + render_markdown(
                "Stage1 log: when each package was built",
                view["log_note"],
                view["log_rows"],
                view["preexisting"],
                meta,
            )
        with open(markdown_path, "w", encoding="utf-8") as handle:
            handle.write(text)
    if json_path:
        with open(json_path, "w", encoding="utf-8") as handle:
            json.dump(view, handle, indent=2)
            handle.write("\n")
    crit = [row["label"] for row in view["plan_rows"] if row["critical"]]
    print(
        f"stage1 plan: {len(view['plan_rows'])} built components, "
        f"depth {max((row['end'] for row in view['plan_rows']), default=0):g}, "
        f"critical: {' -> '.join(crit) if crit else '(empty)'}"
    )


def cmd_render(args: argparse.Namespace) -> int:
    doc, nodes = load_plan(args.plan)
    nodes = select_nodes(nodes, args.root or None)
    view = build_view(doc, nodes, args.log)
    write_outputs(view, args.html, args.json, args.markdown)
    return 0


def self_test() -> int:
    import tempfile
    from pathlib import Path

    plan = {
        "cabal-version": "3.17.0.0",
        "compiler-id": "ghc-9.8.4",
        "os": "osx",
        "arch": "aarch64",
        "install-plan": [
            {"type": "pre-existing", "id": "host:base-4.19", "stage": "host", "pkg-name": "base", "pkg-version": "4.19", "depends": []},
            {"type": "configured", "id": "host:filepath-1.5", "stage": "host", "pkg-name": "filepath", "pkg-version": "1.5", "component-name": "lib", "depends": ["base-4.19"], "exe-depends": []},
            {"type": "configured", "id": "build:filepath-1.5", "stage": "build", "pkg-name": "filepath", "pkg-version": "1.5", "component-name": "lib", "depends": ["build:base-4.19"], "exe-depends": []},
            {"type": "pre-existing", "id": "build:base-4.19", "stage": "build", "pkg-name": "base", "pkg-version": "4.19", "depends": []},
            {
                "type": "configured",
                "id": "host:directory-1.3",
                "stage": "host",
                "pkg-name": "directory",
                "pkg-version": "1.3",
                "component-name": "lib",
                "depends": ["host:filepath-1.5", "base-4.19"],
                "exe-depends": ["build:filepath-1.5"],
            },
            {
                "type": "configured",
                "id": "host:ghc-9.14",
                "stage": "host",
                "pkg-name": "ghc",
                "pkg-version": "9.14",
                "components": {
                    "lib": {"depends": ["directory-1.3", "filepath-1.5"], "exe-depends": []},
                    "setup": {"depends": ["directory-1.3"], "exe-depends": []},
                },
            },
            {"type": "configured", "id": "host:Win32-2.14", "stage": "host", "pkg-name": "Win32", "pkg-version": "2.14", "components": {}},
        ],
    }
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        plan_path = root / "plan.json"
        plan_path.write_text(json.dumps(plan), encoding="utf-8")
        _doc, nodes = load_plan(str(plan_path))
        built = {n.disp + ":" + n.stage + ":" + n.component for n in nodes if n.built}
        assert "Win32-2.14:host:lib" not in built, built
        assert "base-4.19:host:lib" not in {n.disp + ":" + n.stage for n in nodes if n.built}
        ghc_lib = next(n for n in nodes if n.pkg == "ghc" and n.component == "lib")
        ghc_setup = next(n for n in nodes if n.pkg == "ghc" and n.component == "setup")
        assert any(n.pkg == "directory" and n.key in ghc_lib.preds for n in nodes)
        # Bare filepath id on the ghc lib resolves to the host stage.
        assert any(n.pkg == "filepath" and n.stage == "host" and n.key in ghc_lib.preds for n in nodes)
        assert any(n.pkg == "directory" and n.key in ghc_setup.preds for n in nodes)
        start, finish = schedule(nodes, lambda _k: 1.0)
        # filepath (both stages) at 0, directory waits on both filepaths so starts at 1,
        # ghc lib waits on directory so starts at 2. setup waits only on directory.
        directory = next(n for n in nodes if n.pkg == "directory")
        assert start[directory.key] == 1, start
        # setup waits on directory. The library waits on setup as well.
        assert start[ghc_setup.key] == 2, start[ghc_setup.key]
        assert start[ghc_lib.key] == 3, (start[ghc_lib.key], ghc_lib.preds)
        path = critical_path(nodes, start, finish)
        assert path[-1] == ghc_lib.key
        # Stamped log: build filepath first, then host filepath.
        log_path = root / "log"
        lines = [
            "1000.000\tBuilding     filepath-1.5 (lib)\n",
            "1001.500\tCompleted    filepath-1.5 (lib) (1.5s)\n",
            "1002.000\tBuilding     filepath-1.5 (lib)\n",
            "1004.000\tCompleted    filepath-1.5 (lib)\n",
        ]
        log_path.write_text("".join(lines), encoding="utf-8")
        spans = match_log(nodes, str(log_path))
        build_fp = next(n for n in nodes if n.pkg == "filepath" and n.stage == "build")
        host_fp = next(n for n in nodes if n.pkg == "filepath" and n.stage == "host")
        assert spans[build_fp.key][0] == 1000.0, spans
        assert spans[host_fp.key][0] == 1002.0, spans
        html_path = root / "out.html"
        md_path = root / "out.md"
        view = build_view(plan, nodes, str(log_path))
        write_outputs(view, str(html_path), None, str(md_path))
        text = html_path.read_text(encoding="utf-8")
        assert "not wall-clock" in text
        assert "directory-1.3" in text
        assert "Win32" not in text.split("Already installed", 1)[0]
        md = md_path.read_text(encoding="utf-8")
        assert "```mermaid" in md
        assert "waiting on" in md
        # Incomplete log leaves the bar open and does not invent the other packages on that axis.
        partial = root / "partial"
        partial.write_text("10.0\tStarting     directory-1.3 (lib)\n", encoding="utf-8")
        partial_spans = match_log(nodes, str(partial))
        assert set(partial_spans) == {directory.key}
        assert partial_spans[directory.key][2] is False
        # libffi is configured but nothing depends on it. The ghc executable's
        # closure keeps the compiler library and drops libffi.
        exe = Node(
            "host:ghc-bin#exe:ghc",
            "host:ghc-bin",
            "ghc-bin",
            "9.14",
            "host",
            "exe:ghc",
            True,
            preds=[ghc_lib.key],
        )
        orphan = Node(
            "host:libffi#lib",
            "host:libffi",
            "libffi-clib",
            "3.5.2",
            "host",
            "lib",
            True,
        )
        nodes.extend([exe, orphan])
        kept = select_nodes(nodes, ["ghc"])
        assert all(n.pkg != "libffi-clib" for n in kept)
        assert any(n.pkg == "ghc" and n.component == "lib" for n in kept)
    print("self-test ok")
    return 0


def main(argv: list[str]) -> int:
    if not argv or argv[0] in ("-h", "--help"):
        print(__doc__.strip())
        return 0 if argv else 2
    cmd = argv[0]
    if cmd == "stamp":
        if len(argv) != 2:
            print("usage: stage1-build-graph.py stamp LOG", file=sys.stderr)
            return 2
        return cmd_stamp(argv[1])
    if cmd == "self-test":
        return self_test()
    if cmd != "render":
        print(f"unknown command {cmd}", file=sys.stderr)
        return 2
    parser = argparse.ArgumentParser(prog="stage1-build-graph.py render")
    parser.add_argument("--plan", required=True)
    parser.add_argument("--log")
    parser.add_argument("--html")
    parser.add_argument("--json")
    parser.add_argument("--markdown")
    parser.add_argument(
        "--root",
        action="append",
        default=[],
        help="host executable to treat as a stage1 target (repeatable)",
    )
    args = parser.parse_args(argv[1:])
    if not (args.html or args.json or args.markdown):
        parser.error("pass --html, --json, or --markdown")
    return cmd_render(args)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
