#!/usr/bin/env python3
"""Find which native-runtime object occ miscompiles, using a hanging or
failing native OCaml program as the oracle.

usage: tools/bisect-native.py <ocaml-tree> <test.ml> [timeout-seconds]

runtime/*.n.o are rebuilt with gcc into runtime/gcc-n/; runtime/libasmrun.a
is rebuilt from mixes (the original is kept as libasmrun.a.occ), the test is
relinked with ./ocamlopt.opt and run; exit 0 within the timeout passes.
"""
import os, subprocess, sys, glob, shutil

tree = os.path.abspath(sys.argv[1]); test = os.path.abspath(sys.argv[2])
limit = int(sys.argv[3]) if len(sys.argv) > 3 else 20
os.chdir(tree)
CFLAGS = ("-O -g -mprfchw -pthread -I ./runtime -DNATIVE_CODE -DTARGET_amd64 -DMODEL_default -DSYS_linux "
          "-DCAMLDLLIMPORT= -DIN_CAML_RUNTIME").split()
objs = sorted(os.path.basename(o)[:-4] for o in glob.glob("runtime/*.n.o"))
os.makedirs("runtime/gcc-n", exist_ok=True); os.makedirs("runtime/occ-n", exist_ok=True)
for n in objs:
    if not os.path.exists(f"runtime/occ-n/{n}.n.o"): shutil.copy(f"runtime/{n}.n.o", f"runtime/occ-n/{n}.n.o")
    if not os.path.exists(f"runtime/gcc-n/{n}.n.o"):
        subprocess.run(["gcc"] + CFLAGS + ["-c", f"runtime/{n}.c", "-o", f"runtime/gcc-n/{n}.n.o"], check=True)
if not os.path.exists("runtime/libasmrun.a.occ"): shutil.copy("runtime/libasmrun.a", "runtime/libasmrun.a.occ")
# members that are not C (amd64.o from amd64.S) come from the original archive
others = [m for m in subprocess.run(["ar", "t", "runtime/libasmrun.a.occ"], capture_output=True, text=True).stdout.split()
          if not m.endswith(".n.o")]
os.makedirs("runtime/asm-n", exist_ok=True)
subprocess.run(["ar", "x", "../libasmrun.a.occ"] + others, cwd="runtime/asm-n", check=True)
env = dict(os.environ, OCC_NATIVE="cc,as")

def works(occ_set):
    os.remove("runtime/libasmrun.a")
    files = [f"runtime/{'occ-n' if n in occ_set else 'gcc-n'}/{n}.n.o" for n in objs] + [f"runtime/asm-n/{m}" for m in others]
    subprocess.run(["ar", "cr", "runtime/libasmrun.a"] + files, check=True)
    subprocess.run(["./ocamlopt.opt", "-nostdlib", "-I", "stdlib", "-I", "runtime", "-o", "runtime/bisect.opt", test], check=True, env=env)
    try:
        r = subprocess.run(["runtime/bisect.opt"], capture_output=True, timeout=limit)
        return r.returncode == 0
    except subprocess.TimeoutExpired:
        return False

try:
    if not works(set()): print("all-gcc runtime fails: harness problem"); sys.exit(1)
    if works(set(objs)): print("all-occ runtime passes"); sys.exit(0)
    cand = list(objs)
    while len(cand) > 1:
        a, b = cand[: len(cand) // 2], cand[len(cand) // 2 :]
        if not works(set(a)): cand = a
        elif not works(set(b)): cand = b
        else: print("no single object explains it; failing set:", " ".join(cand)); sys.exit(2)
        print("narrowed to", len(cand), flush=True)
    print("miscompiled unit: runtime/%s.c" % cand[0])
finally:
    shutil.copy("runtime/libasmrun.a.occ", "runtime/libasmrun.a")
