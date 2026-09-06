#!/usr/bin/env python3
"""Find which runtime object occ miscompiles by swapping objects with gcc's.

usage: tools/bisect-runtime.py <ocaml-tree-configured-with-occ> [test-args...]

Every runtime/*.b.o is rebuilt with gcc into runtime/gcc/, then ocamlrun is
relinked from mixes of the two object sets and run as
   ocamlrun boot/ocamlc -version
(or the given test arguments) until the smallest failing set of occ objects
is found.  Assumes the occ objects are already built by make.
"""
import os, subprocess, sys, glob, shutil

tree = os.path.abspath(sys.argv[1])
test_args = sys.argv[2:] or ["boot/ocamlc", "-version"]
os.chdir(tree)
OCC = os.environ.get("OCC", os.path.expanduser("~/occ/_build/default/bin/main.exe"))
CFLAGS = "-O -g -mprfchw -pthread -I ./runtime -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME".split()

objs = sorted(os.path.basename(o)[:-4] for o in glob.glob("runtime/*.b.o"))
os.makedirs("runtime/gcc", exist_ok=True)
os.makedirs("runtime/occ", exist_ok=True)
for name in objs:
    if not os.path.exists(f"runtime/occ/{name}.b.o"):
        shutil.copy(f"runtime/{name}.b.o", f"runtime/occ/{name}.b.o")
    if not os.path.exists(f"runtime/gcc/{name}.b.o"):
        subprocess.run(["gcc"] + CFLAGS + ["-c", f"runtime/{name}.c", "-o", f"runtime/gcc/{name}.b.o"], check=True)

def works(occ_set):
    if os.path.exists("runtime/bisect.a"): os.remove("runtime/bisect.a")
    files = [f"runtime/{'occ' if n in occ_set else 'gcc'}/{n}.b.o" for n in objs]
    subprocess.run(["ar", "cr", "runtime/bisect.a"] + files, check=True)
    subprocess.run(["gcc", "-Wl,-E", "-o", "runtime/ocamlrun.bisect", "runtime/prims.o", "runtime/bisect.a", "-lzstd", "-lm"], check=True)
    r = subprocess.run(["runtime/ocamlrun.bisect"] + test_args, capture_output=True, timeout=120)
    return r.returncode == 0

if not works(set()):
    print("even the all-gcc runtime fails: the test or prims.o is the problem"); sys.exit(1)
if works(set(objs)):
    print("the all-occ runtime passes this test"); sys.exit(0)

# delta debugging with a single culprit assumed; falls back to reporting a set
cand = list(objs)
while len(cand) > 1:
    half = cand[: len(cand) // 2]
    if not works(set(half)):
        cand = half
    elif not works(set(cand[len(cand) // 2 :])):
        cand = cand[len(cand) // 2 :]
    else:
        print("no single object explains it; failing set:", " ".join(cand)); sys.exit(2)
    print("narrowed to", len(cand), "objects", flush=True)
print("miscompiled unit: runtime/%s.c" % cand[0])
