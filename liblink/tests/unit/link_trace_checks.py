"""Focused parsers for native linker archive-selection diagnostics."""

from __future__ import annotations

import re
from pathlib import Path


def _path_key(path: str) -> str:
    return path.strip().strip('"').replace("\\", "/").casefold().rstrip("/")


def _basename(path: str) -> str:
    return path.replace("\\", "/").rsplit("/", 1)[-1].casefold()


def _member_key(member: str) -> str:
    name = _basename(member)
    if name.endswith(".obj"):
        name = name[:-4]
    elif name.endswith(".o"):
        name = name[:-2]
    if name.endswith(".c"):
        name = name[:-2]
    return name


def _map_member_key(member: str) -> str:
    return _basename(member)


def _lld_read_archives(trace: str) -> list[str]:
    pattern = re.compile(r"\blld:\s+Reading\s+(.+?)\s*$", re.IGNORECASE)
    return [match.group(1).strip().strip('"')
            for line in trace.splitlines()
            if (match := pattern.search(line))]


def _lld_loaded_members(trace: str) -> list[tuple[str, str]]:
    pattern = re.compile(
        r"\blld:\s+Loaded\s+(.+?)\(([^()]*)\)\s+for\s+.+$",
        re.IGNORECASE,
    )
    return [(match.group(1).strip().strip('"'), match.group(2).strip())
            for line in trace.splitlines()
            if (match := pattern.search(line))]


def is_lld_mingw_verbose_trace(trace: str) -> bool:
    return bool(_lld_read_archives(trace) or _lld_loaded_members(trace))


def _map_input_members(map_text: str) -> list[str]:
    # LLD's GNU/COFF map rows look like:
    # 00003000 00000004 4         member.o:(.data)
    pattern = re.compile(
        r"^\s*[0-9a-f]+\s+[0-9a-f]+\s+\d+\s+(.+?\.o(?:bj)?):\(",
        re.IGNORECASE,
    )
    return [match.group(1).strip()
            for line in map_text.splitlines()
            if (match := pattern.match(line))]


def _matches_forbidden(member: str, forbidden_members: tuple[str, ...]) -> bool:
    key = _member_key(member)
    return any(
        key.endswith(forbidden_key)
        for forbidden_key in (_member_key(forbidden) for forbidden in forbidden_members)
    )


def _has_explicit_path(path: str) -> bool:
    return "/" in path or "\\" in path or bool(re.match(r"^[A-Za-z]:", path))


def verify_lld_mingw_archive_map(
    map_text: str,
    archive: Path,
    link_trace: str,
    *,
    included_source: str,
    forbidden_members: tuple[str, ...],
) -> None:
    """Validate LLD's basename-only COFF map against verbose archive selection."""
    expected_path = _path_key(str(archive))
    expected_basename = _basename(str(archive))

    read_archives = _lld_read_archives(link_trace)
    same_name_reads = [p for p in read_archives if _basename(p) == expected_basename]
    if not any(_path_key(p) == expected_path for p in same_name_reads):
        raise RuntimeError(f"link trace did not read expected archive: {archive}")
    if any(_path_key(p) != expected_path for p in same_name_reads):
        raise RuntimeError(f"link trace has an ambiguous archive basename: {archive}")

    loaded_records = _lld_loaded_members(link_trace)
    same_name_loaded = [
        (loaded_archive, member)
        for loaded_archive, member in loaded_records
        if _basename(loaded_archive) == expected_basename
    ]
    if not same_name_loaded:
        raise RuntimeError(f"link trace did not show selected member from {archive}")
    for _, member in loaded_records:
        if _matches_forbidden(member, forbidden_members):
            raise RuntimeError(
                f"link trace selected archive member {member} beside included {included_source}"
            )
    for loaded_archive, _ in same_name_loaded:
        if (_has_explicit_path(loaded_archive)
                and _path_key(loaded_archive) != expected_path):
            raise RuntimeError(
                f"link trace selected same-basename archive at a different path: {loaded_archive}"
            )
    loaded = [member for _, member in same_name_loaded]

    map_members = _map_input_members(map_text)
    if not map_members:
        raise RuntimeError("link map has no recognized input-member records")
    for member in map_members:
        if _matches_forbidden(member, forbidden_members):
            raise RuntimeError(
                f"link map selected archive member {member} beside included {included_source}"
            )

    selected_keys = {_map_member_key(member) for member in loaded}
    map_keys = {_map_member_key(member) for member in map_members}
    if not selected_keys.intersection(map_keys):
        raise RuntimeError(
            "link map has no input record matching a member selected from the expected archive"
        )


def _macho_selected_members(trace: str, archive: Path) -> list[str]:
    pattern = re.compile(
        r"\b(?:caused|forced) load of\s+(.+?\.a)(?:\[\d+\])?\(([^()]*)\)",
        re.IGNORECASE,
    )
    expected = str(archive)
    result = []
    for line in trace.splitlines():
        match = pattern.search(line)
        if match and match.group(1) == expected:
            result.append(match.group(2).strip())
    return result


def verify_macho_archive_selection(
    archive: Path,
    link_trace: str,
    *,
    included_source: str,
    forbidden_members: tuple[str, ...],
) -> None:
    """Check selected archive members in both ld -why_load output dialects."""
    selected = _macho_selected_members(link_trace, archive)
    if not selected:
        raise RuntimeError(f"link trace did not show archive member selection: {archive}")
    for member in selected:
        if _matches_forbidden(member, forbidden_members):
            raise RuntimeError(
                f"link trace selected archive member {member} beside included {included_source}"
            )
