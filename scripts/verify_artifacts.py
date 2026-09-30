"""Check packaged scientific-file hashes. No dependencies or network calls."""
from pathlib import Path
import csv
import hashlib
import sys

root = Path(__file__).resolve().parents[1]
manifest = root / "PROVENANCE_SHA256.csv"
failures = []
with manifest.open(encoding="utf-8", newline="") as handle:
    records = list(csv.DictReader(handle))
if not records:
    failures.append("Manifest is empty")
seen = set()
for record in records:
    rel = record["path"]
    path = (root / rel).resolve()
    if path == root or root not in path.parents or rel in seen:
        failures.append(f"Invalid or repeated manifest path: {rel}")
        continue
    seen.add(rel)
    if not path.is_file():
        failures.append(f"Missing: {rel}")
        continue
    payload = path.read_bytes()
    if len(payload) != int(record["bytes"]) or hashlib.sha256(payload).hexdigest() != record["sha256"]:
        failures.append(f"Content mismatch: {rel}")
if failures:
    print("\n".join(failures), file=sys.stderr)
    raise SystemExit(1)
print(f"PASS: {len(records)} scientific files match the recorded SHA-256 values.")
print("This verifies bundled file identity, not a financial-data rerun or a digital signature.")
