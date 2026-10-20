"""Real tool invocation checks for profiling-target integration tests."""

import json
import os
from pathlib import Path
import shlex
import subprocess
import sys


def command_target_selectors(command):
    """Return machine and LLVM target selectors from settings or a real command."""
    tokens = shlex.split(command)
    machine, llvm = set(), set()
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if token.startswith("--target="):
            machine.add(token.split("=", 1)[1])
        elif token in ("--target", "-target") and index + 1 < len(tokens):
            index += 1
            machine.add(tokens[index])
        elif token == "-mtriple" and index + 1 < len(tokens):
            index += 1
            llvm.add(tokens[index])
        elif token.startswith("-mtriple="):
            llvm.add(token.split("=", 1)[1])
        index += 1
    return machine, llvm


def command_program_name(command):
    try:
        tokens = shlex.split(command)
    except ValueError:
        return ""
    return Path(tokens[0]).name if tokens else ""


def link_command_tokens(command, expected_program, output_fragment):
    """Return argv for a real link row; reject CPP and compile-only commands."""
    try:
        tokens = shlex.split(command)
    except ValueError:
        return []
    if not tokens or Path(tokens[0]).name != expected_program:
        return []
    if any(flag in tokens for flag in ("-E", "-c", "-S", "-fsyntax-only")):
        return []
    outputs = [tokens[index + 1] for index, token in enumerate(tokens[:-1]) if token == "-o"]
    if not any(output_fragment in output for output in outputs):
        return []
    return tokens


def compile_command_tokens(command, expected_program, source_name):
    """Return argv for a compiler invocation that compiles the fixture source."""
    try:
        tokens = shlex.split(command)
    except ValueError:
        return []
    if not tokens or Path(tokens[0]).name != expected_program:
        return []
    if "-c" not in tokens or any(flag in tokens for flag in ("-E", "-S", "-fsyntax-only")):
        return []
    if not any(Path(token).name == source_name for token in tokens):
        return []
    return tokens


def configured_tool_target_selectors(info, keys):
    """Read the target selectors GHC configured for one external tool."""
    machine = set()
    llvm = set()
    for key in keys:
        configured_machine, configured_llvm = command_target_selectors(info.get(key, ""))
        machine.update(configured_machine)
        llvm.update(configured_llvm)
    return machine, llvm


def check_no_logical_profiling_target_in_toolchain(info):
    """The profiling ABI must not leak into a C/CPP/LLVM machine selector."""
    fields = (
        "C compiler command", "C compiler flags", "C++ compiler command",
        "C++ compiler flags", "C compiler link flags", "CPP command", "CPP flags",
        "Haskell CPP command", "Haskell CPP flags", "C-- CPP command", "C-- CPP flags",
        "LLVM target",
    )
    for key in fields:
        machine, llvm = command_target_selectors(info.get(key, ""))
        selected = machine | llvm
        assert all(not target.endswith("-prof") for target in selected), \
            "logical profiling name leaked into " + key + ": " + repr(selected)
    assert not info.get("LLVM target", "").endswith("-prof"), \
        "logical profiling name leaked into LLVM target: " + info["LLVM target"]


def make_hsc2hs_recording_wrapper(real_hsc2hs, work, env, write_file,
                                  python_executable=None):
    """Record real Cabal -> hsc2hs arguments/response files, then exec helper."""
    log = work / "hsc2hs-invocations.jsonl"
    recorder = work / "record-hsc2hs.py"
    wrapper = work / "record-hsc2hs"
    write_file(recorder, r'''
        # Executed by the adjacent /bin/sh launcher with sys.executable.
        import json
        import os
        from pathlib import Path
        import sys

        log = Path(os.environ["T11_HSC2HS_LOG"])
        responses = []
        for arg in sys.argv[1:]:
            if not arg.startswith("@"):
                continue
            name = arg[1:].strip("\"'")
            path = Path(name)
            try:
                content = path.read_text(errors="replace")
            except OSError as error:
                content = None
                error_text = str(error)
            else:
                error_text = None
            responses.append({"path": str(path), "content": content, "error": error_text})
        with log.open("a") as stream:
            stream.write(json.dumps({"argv": sys.argv[1:], "responses": responses}) + "\n")
        os.execv(os.environ["T11_HSC2HS_REAL"],
                 [os.environ["T11_HSC2HS_REAL"], *sys.argv[1:]])
    ''')
    # Keep the interpreter out of a shebang: Nix paths can be long or contain
    # spaces, while /bin/sh is available in the minimal build environment.
    python_executable = python_executable or sys.executable
    write_file(wrapper, '''
        #!/bin/sh
        exec {python} {recorder} "$@"
    '''.format(python=shlex.quote(python_executable), recorder=shlex.quote(str(recorder))))
    wrapper.chmod(0o755)
    env = env.copy()
    env.update(T11_HSC2HS_REAL=str(real_hsc2hs), T11_HSC2HS_LOG=str(log))
    return wrapper, log, env


def check_hsc2hs_wrapper_path_independence(real_hsc2hs, work, env, write_file):
    """Exercise the recorder with PATH lacking Python and paths containing spaces."""
    probe = work / "hsc2hs wrapper path with spaces"
    python_dir = probe / "python runtime path with spaces"
    python_dir.mkdir(parents=True)
    spaced_python = python_dir / "python"
    spaced_python.symlink_to(sys.executable)
    wrapper, log, wrapper_env = make_hsc2hs_recording_wrapper(
        real_hsc2hs, probe, env, write_file, python_executable=str(spaced_python))
    no_python_path = probe / "PATH without python3"
    no_python_path.mkdir(parents=True)
    response = probe / "version arguments.rsp"
    response.write_text("-V\n")
    expected = subprocess.run([real_hsc2hs, "-V"], env=env, text=True,
                              capture_output=True, check=True, timeout=30)
    isolated_env = wrapper_env.copy()
    isolated_env["PATH"] = str(no_python_path)
    actual = subprocess.run([wrapper, "@" + str(response)], env=isolated_env,
                            text=True, capture_output=True, check=True, timeout=30)
    assert actual.stdout == expected.stdout, "recorder changed hsc2hs version output"
    invocations = [json.loads(line) for line in log.read_text().splitlines() if line]
    assert len(invocations) == 1, "recorder did not log exactly one hsc2hs invocation"
    invocation = invocations[0]
    assert invocation["argv"] == ["@" + str(response)], invocation
    assert invocation["responses"] == [{
        "path": str(response), "content": "-V\n", "error": None
    }], invocation


def check_profile_hsc2hs_invocations(log, profile_info):
    """Validate real hsc2hs args, including Cabal's response-file contents."""
    assert log.is_file(), "Cabal did not invoke the recording hsc2hs wrapper"
    invocations = [json.loads(line) for line in log.read_text().splitlines() if line]
    assert invocations, "Cabal did not invoke hsc2hs"
    with_source = []
    for invocation in invocations:
        args = invocation["argv"]
        for response in invocation["responses"]:
            assert response["error"] is None, \
                "could not capture hsc2hs response file " + response["path"] + ": " + str(response["error"])
        response_text = "\n".join(item["content"] for item in invocation["responses"])
        if ".hsc" in " ".join(args) + response_text:
            with_source.append(invocation)
    assert with_source, "recorded hsc2hs traffic omitted the profiling HSC source"
    assert any(invocation["responses"] for invocation in with_source), \
        "Cabal's hsc2hs response file was not captured"
    configured, _ = configured_tool_target_selectors(
        profile_info, ("C compiler command", "C compiler flags"))
    configured_flags = shlex.split(" ".join(
        profile_info.get(key, "") for key in ("C compiler command", "C compiler flags")))
    configured_abi = {flag for flag in configured_flags
                       if flag.startswith(("-mfloat-abi=", "-mabi="))}
    for invocation in with_source:
        parts = list(invocation["argv"])
        for response in invocation["responses"]:
            assert response["error"] is None, \
                "could not capture hsc2hs response file " + response["path"] + ": " + str(response["error"])
            try:
                parts.extend(shlex.split(response["content"]))
            except ValueError as error:
                raise AssertionError("malformed hsc2hs response file: " + str(error))
        # hsc2hs receives C compiler selectors as --cflag arguments. Decode
        # those values separately from hsc2hs's own option syntax.
        cflags = []
        index = 0
        while index < len(parts):
            token = parts[index]
            if token == "--cflag" and index + 1 < len(parts):
                index += 1
                cflags.append(parts[index])
            elif token.startswith("--cflag="):
                cflags.append(token.split("=", 1)[1])
            index += 1
        actual, llvm = command_target_selectors(shlex.join(cflags))
        actual_abi = {flag for flag in cflags
                      if flag.startswith(("-mfloat-abi=", "-mabi="))}
        assert actual == configured, \
            "hsc2hs response target selectors differ from configured C flags: " + repr(parts)
        assert configured_abi <= actual_abi, \
            "hsc2hs response omitted configured C ABI flags: " + repr(configured_abi - actual_abi)
        assert all(not target.endswith("-prof") for target in actual | llvm), \
            "logical profiling name leaked into hsc2hs arguments: " + repr(parts)


def check_profile_cpp_trace(profile, profile_info, work, env, run_command):
    """Compare the real GHC -v Haskell CPP invocation with its tool settings."""
    source = work / "app/lib/AbiLibrary.hs"
    trace = run_command([profile, "-E", "-v", "-DEXPECT_PROFILE", source], cwd=work, env=env)
    output = trace.stdout + trace.stderr
    expected_program = command_program_name(profile_info.get("Haskell CPP command", ""))
    assert expected_program, "profile settings omitted Haskell CPP command"
    lines = []
    for line in output.splitlines():
        command = line.split("*** C pre-processor:", 1)[-1] if "*** C pre-processor:" in line else line
        if command_program_name(command) != expected_program:
            continue
        try:
            tokens = shlex.split(command)
        except ValueError:
            continue
        if "-E" in tokens:
            lines.append(command)
    assert lines, "profile GHC -v trace omitted the Haskell CPP command"
    expected, _ = configured_tool_target_selectors(
        profile_info, ("Haskell CPP command", "Haskell CPP flags"))
    for line in lines:
        actual, llvm = command_target_selectors(line)
        assert actual == expected, \
            "Haskell CPP target selectors differ from settings: " + line
        assert all(not target.endswith("-prof") for target in actual | llvm), \
            "logical profiling name leaked into Haskell CPP command: " + line
