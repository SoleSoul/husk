#!/bin/sh
# Run the husk test suite.
#   tests/NAME.hs + tests/NAME.out   program and its expected stdout
#   tests/NAME.in                    optional stdin
#   tests/errors/NAME.hs + NAME.err  program that must fail; .err holds a
#                                    fragment that must appear in stderr
cd "$(dirname "$0")/.."
HUSK=./bin/husk
pass=0; fail=0
tmp=$(mktemp)
for t in tests/*.hs; do
  base=${t%.hs}
  [ -f "$base.out" ] || continue
  if [ -f "$base.in" ]; then $HUSK "$t" < "$base.in" > "$tmp" 2>&1; else $HUSK "$t" < /dev/null > "$tmp" 2>&1; fi
  if cmp -s "$tmp" "$base.out"; then pass=$((pass+1)); else
    fail=$((fail+1)); echo "FAIL: $t"; diff "$tmp" "$base.out" | head -10; fi
done
for t in tests/errors/*.hs; do
  base=${t%.hs}
  [ -f "$base.err" ] || continue
  $HUSK "$t" < /dev/null > "$tmp" 2>&1
  if grep -qF -- "$(cat "$base.err")" "$tmp"; then pass=$((pass+1)); else
    fail=$((fail+1)); echo "FAIL: $t (expected: $(cat "$base.err"))"; head -5 "$tmp"; fi
done
rm -f "$tmp"
echo "passed: $pass  failed: $fail"
[ $fail -eq 0 ]
