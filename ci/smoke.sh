#!/usr/bin/env bash
# Smoke test a built jolt-lsp binary: the JSON CLI, the MCP server and the
# LSP server, against a copy of the simple fixture outside the checkout.
# Usage: ci/smoke.sh path/to/jolt-lsp
set -euo pipefail
bin="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
here="$(cd "$(dirname "$0")/.." && pwd)"
work="$(mktemp -d)"
cp -R "$here/test/fixtures/simple" "$work/simple"
cd "$work/simple"
export JOLT_LSP_NO_DEPS=1

fail() { echo "smoke: FAILED: $*"; exit 1; }

out=$("$bin" --json hover app.core/greet)
echo "$out" | grep -q '"fqn":"app.core\\/greet"' || fail "hover: $out"
echo "== cli hover ok"

out=$("$bin" --json check --code '(defn f [x] (inc x)')
echo "$out" | grep -q '"status":"repaired"' || fail "check: $out"
echo "== cli check ok"

out=$(printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' \
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"jolt_references","arguments":{"symbol":"trimmed"}}}' \
  | "$bin" mcp)
echo "$out" | grep -q '"id":2' || fail "mcp: $out"
echo "$out" | grep -q 'app.util' || fail "mcp references: $out"
echo "== mcp ok"

body='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"file://'"$PWD"'"}}'
exit_msg='{"jsonrpc":"2.0","method":"exit"}'
out=$(printf 'Content-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%s' \
  "${#body}" "$body" "${#exit_msg}" "$exit_msg" | "$bin" lsp)
echo "$out" | grep -q 'definitionProvider' || fail "lsp: $out"
echo "== lsp ok"

echo "smoke: ok"
