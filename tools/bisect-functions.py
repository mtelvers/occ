#!/usr/bin/env python3
"""Find which function occ miscompiles within one runtime source file.

usage: tools/bisect-functions.py <ocaml-tree> runtime/<file>.c [test-args...]
       tools/bisect-functions.py --native <test.ml> [timeout] <ocaml-tree> runtime/<file>.c

In native mode the hybrid object replaces the file's member of libasmrun.a,
the other members being gcc's from tools/bisect-native.py, and the oracle is
relinking and running <test.ml> within the timeout.

gcc and occ each compile the file to assembly.  Hybrids are made from
gcc's assembly with the bodies of chosen functions replaced by occ's (occ's
local labels are renamed so the two do not clash), assembled, and linked
into an otherwise gcc-built ocamlrun, which runs the test.  Functions are
then bisected.  Requires a prior run of tools/bisect-runtime.py so that
runtime/gcc/*.b.o exist.
"""
import os, re, subprocess, sys, glob, shutil

args = sys.argv[1:]
native = None; limit = 20
if args and args[0] == "--native":
    native = os.path.abspath(args[1]); args = args[2:]
    if args and args[0].isdigit(): limit = int(args[0]); args = args[1:]
tree = os.path.abspath(args[0]); src = args[1]
test_args = args[2:] or ["boot/ocamlc", "-version"]
os.chdir(tree)
OCC = os.environ.get("OCC", os.path.expanduser("~/occ/_build/default/bin/main.exe"))
CFLAGS = "-O -g -mprfchw -pthread -I ./runtime -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME".split()
if native:
    CFLAGS = "-O -g -mprfchw -pthread -I ./runtime -DNATIVE_CODE -DTARGET_amd64 -DMODEL_default -DSYS_linux -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME".split()
name = os.path.basename(src)[:-2]
work = "runtime/fbisect"; os.makedirs(work, exist_ok=True)

subprocess.run(["gcc", "-O0"] + [f for f in CFLAGS[1:] if f != "-g"] + ["-fno-asynchronous-unwind-tables", "-fno-dwarf2-cfi-asm", "-S", src, "-o", f"{work}/gcc.s"], check=True)
env = dict(os.environ, OCC_NATIVE="cc")
# no -g on either side: the hybrid has a single line table and file numbering
subprocess.run([OCC] + [f for f in CFLAGS if f != "-g"] + ["-S", src, "-o", f"{work}/occ.s"], check=True, env=env)

def functions(path, rename=None):
    """Split assembly into (preamble/data, {name: body}) where body runs from
    the '.type name, @function' line to '.size name, .-name'."""
    text = open(path).read()
    if rename:
        text = re.sub(r'\.L([A-Za-z0-9_.]+)', lambda m: '.L' + rename + m.group(1), text)
    funcs = {}
    for m in re.finditer(r'(\t\.type\t(\S+), @function\n.*?\t\.size\t\2, \.-\2\n)', text, re.S):
        funcs[m.group(2)] = m.group(1)
    rest = re.sub(r'\t\.type\t(\S+), @function\n.*?\t\.size\t\1, \.-\1\n', '', text, flags=re.S)
    return rest, funcs

gcc_rest, gcc_funcs = functions(f"{work}/gcc.s")
occ_rest, occ_funcs = functions(f"{work}/occ.s", rename="occ_")
# occ's private data (string literals, float constants) must come along;
# its definitions of the file's globals must not, gcc's are used
# (.L labels and static locals, which occ names "name.id")
occ_data = "\n".join(m.group(1) for m in re.finditer(
    r'((?:\t\.(?:section|data|bss)[^\n]*\n)(?:\t\.align[^\n]*\n)?\t\.type\t((?:\.L\S+)|(?:\w+\.\d+)), @object\n\t\.size[^\n]*\n\2:\n(?:\t\.(?:ascii|zero|quad|long)[^\n]*\n)+)', occ_rest))
names = sorted(set(gcc_funcs) & set(occ_funcs))
print(len(names), "functions in common;", sorted(set(occ_funcs) - set(gcc_funcs)), "only in occ")

objs = sorted(os.path.basename(o)[:-4] for o in glob.glob("runtime/gcc/*.b.o"))
if native:
    objs = sorted(os.path.basename(o)[:-4] for o in glob.glob("runtime/gcc-n/*.n.o"))
    others = sorted(os.listdir("runtime/asm-n"))
    if not os.path.exists("runtime/libasmrun.a.occ"): shutil.copy("runtime/libasmrun.a", "runtime/libasmrun.a.occ")

def works_native():
    if os.path.exists("runtime/libasmrun.a"): os.remove("runtime/libasmrun.a")
    files = [f"runtime/gcc-n/{n}.n.o" if n != name else f"{work}/hybrid.o" for n in objs] + [f"runtime/asm-n/{m}" for m in others]
    subprocess.run(["ar", "cr", "runtime/libasmrun.a"] + files, check=True)
    r = subprocess.run(["./ocamlopt.opt", "-nostdlib", "-I", "stdlib", "-I", "runtime", "-o", f"{work}/test.opt", native],
                       capture_output=True, text=True, env=dict(os.environ, OCC_NATIVE="cc,as"))
    if r.returncode != 0:
        print(r.stderr[-1500:]); raise SystemExit("link of the test failed")
    try:
        return subprocess.run([f"{work}/test.opt"], capture_output=True, timeout=limit).returncode == 0
    except subprocess.TimeoutExpired:
        return False

def works(occ_set):
    # every body is placed in .text explicitly: the remainder of gcc's file
    # may end in another section
    hybrid = gcc_rest + "\n".join("\t.text\n" + (gcc_funcs[n] if n not in occ_set else occ_funcs[n]) for n in gcc_funcs) + "\n" + occ_data + "\n"
    open(f"{work}/hybrid.s", "w").write(hybrid)
    subprocess.run(["gcc", "-c", f"{work}/hybrid.s", "-o", f"{work}/hybrid.o"], check=True)
    if native: return works_native()
    files = [f"runtime/gcc/{n}.b.o" if n != name else f"{work}/hybrid.o" for n in objs]
    if os.path.exists(f"{work}/lib.a"): os.remove(f"{work}/lib.a")
    subprocess.run(["ar", "cr", f"{work}/lib.a"] + files, check=True)
    subprocess.run(["gcc", "-Wl,-E", "-o", f"{work}/ocamlrun", "runtime/prims.o", f"{work}/lib.a", "-lzstd", "-lm"], check=True)
    r = subprocess.run([f"{work}/ocamlrun"] + test_args, capture_output=True, timeout=120)
    return r.returncode == 0

if not works(set()): print("the all-gcc hybrid fails: harness problem"); sys.exit(1)
if works(set(names)): print("all occ functions pass: the bug is elsewhere (data? static locals?)"); sys.exit(0)
cand = list(names)
while len(cand) > 1:
    a, b = cand[: len(cand) // 2], cand[len(cand) // 2 :]
    if not works(set(a)): cand = a
    elif not works(set(b)): cand = b
    else: print("no single function explains it; failing set:", " ".join(cand)); sys.exit(2)
    print("narrowed to", len(cand), flush=True)
print("miscompiled function:", cand[0])
if native: shutil.copy("runtime/libasmrun.a.occ", "runtime/libasmrun.a")
