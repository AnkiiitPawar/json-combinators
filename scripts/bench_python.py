"""Python's json module (with its C accelerator) on the document that the
Haskell benchmark wrote, for comparison.

    cabal run throughput && python3 scripts/bench_python.py
"""

import json
import pathlib
import platform
import time

data = pathlib.Path("bench-doc.json").read_bytes()
mb = len(data) / 1e6


def best(f, k=5):
    times = []
    for _ in range(k):
        t0 = time.perf_counter()
        f()
        times.append(time.perf_counter() - t0)
    return min(times)


doc = json.loads(data)
t_dec = best(lambda: json.loads(data))
t_enc = best(lambda: json.dumps(doc, ensure_ascii=False, separators=(",", ":")).encode())
print(f"Python {platform.python_version()} json, document {mb:.1f} MB")
print(f"decode  {mb / t_dec:7.1f} MB/s  ({t_dec:.3f} s)")
print(f"encode  {mb / t_enc:7.1f} MB/s  ({t_enc:.3f} s)")
