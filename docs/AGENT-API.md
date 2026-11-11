# Agent API

Two transports: MCP over stdio (`jolt-lsp mcp`) and a JSON CLI
(`jolt-lsp <command> --json`). Both call the same engine.

## Positions and results

CLI and MCP accept **1-based line/column** positions. Every location returns:

- `:file` — absolute path
- `:line` `:col` `:end-line` `:end-col` — 1-based, end exclusive
- `:range` — `[row col end-row end-col]`, 1-based
- `:bytes` — `[start end]` UTF-8 byte offsets for patching
- `:lsp` — `{:start {:line L :character C} ...}` 0-based UTF-16
- `:source` — the matching source text (truncated)

Edits preview by default. Pass `:apply true` (MCP) or `--apply` (CLI) to
write. Previews include the edited files and per-edit old/new text.

## MCP tools

| Tool | Arguments |
|---|---|
| `jolt_index` | `root?`, `refresh?` |
| `jolt_search` | `query`, `limit?`, `root?` |
| `jolt_definition` | `symbol` or `file`+`line`+`col`, `root?` |
| `jolt_references` | `symbol`, `root?` |
| `jolt_hover` | `symbol` or `file`+`line`+`col`, `root?` |
| `jolt_symbols` | `file`, `root?` |
| `jolt_diagnostics` | `file?`, `all?`, `kondo?`, `root?` |
| `jolt_call_hierarchy` | `symbol`, `direction` (`callers`/`callees`) |
| `jolt_impact` | `symbol`, `root?` |
| `jolt_deps` | `ns`, `direction` (`dependencies`/`dependents`) |
| `jolt_form` | `file`, `line`/`col` or `mode: "top"`, `root?` |
| `jolt_context` | `ns` or `file`, `root?` |
| `jolt_rename` | `file`, `line`, `col`, `to`, `apply?` |
| `jolt_clean_ns` | `file`, `apply?` |
| `jolt_add_require` | `file`, `symbol`, `alias?`, `apply?` |
| `jolt_replace_form` | `file`, `old_form`, `new_text`, `apply?` |
| `jolt_insert_form` | `file`, `after`, `new_text`, `apply?` |
| `jolt_refactor` | `kind` (`sort-requires`, `cycle-keyword`, `inline-symbol`, `destructure-keys`, `move-form`), `file`, `line?`, `col?`, `target_file?`, `apply?` |
| `jolt_batch` | `ops: [{:tool, :args}]` (read-only ops) |

Every tool returns content blocks whose text is JSON. `jolt_index` builds or
reuses the cache; pass `refresh: true` after changing files behind the
server's back. `jolt_context` is the one-call pack: vars, dependencies,
dependents, and diagnostics for a namespace.

## CLI commands

`index`, `refresh`, `search`, `def`, `refs`, `hover`, `symbols`,
`diagnostics`, `calls`, `impact`, `deps`, `form`, `context`, `rename`,
`clean-ns`, `add-require`, `replace-form`, `insert-form`, `new-file-ns`,
`ignored-forms`, plus `lsp` and `mcp`.

## Recommended agent flow

1. `jolt_index` once at project start.
2. `jolt_context` for the namespace you are about to change.
3. `jolt_impact` before changing a public var.
4. `jolt_definition` / `jolt_references` to navigate.
5. `jolt_rename`, `jolt_clean_ns`, `jolt_add_require`, `jolt_replace_form`,
   `jolt_refactor` with preview, inspect, then apply.
6. `jolt_diagnostics` to check the result.
