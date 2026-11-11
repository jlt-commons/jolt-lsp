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

Symbols may be fqns (`app.core/run`) or bare names; an ambiguous bare name
returns `{:ok false :candidates [...]}`. Paths may be relative to `root`.

| Tool | Arguments |
|---|---|
| `jolt_index` | `root?`, `refresh?` — counts, plus files that do not read |
| `jolt_search` | `query`, `limit?` |
| `jolt_definition` | `symbol` or `file`+`line`+`col` |
| `jolt_references` | `symbol` or `file`+`line`+`col` |
| `jolt_hover` | `symbol` or `file`+`line`+`col` — doc, arglists, full source (`code`) |
| `jolt_symbols` | `file` |
| `jolt_diagnostics` | `file?` (none = whole project), `kondo?` |
| `jolt_call_hierarchy` | `symbol`, `direction` (`callers`/`callees`) |
| `jolt_impact` | `symbol` — transitive callers with distances |
| `jolt_deps` | `ns`, `direction` (`dependencies`/`dependents`/`cycles`) |
| `jolt_form` | `file`, `line`/`col` or `mode: "top"` |
| `jolt_context` | `ns` or `file` |
| `jolt_check` | `file` or `code`, `fix?` |
| `jolt_rename` | `file`, `line`, `col`, `to`, `apply?` |
| `jolt_clean_ns` | `file`, `apply?` |
| `jolt_add_require` | `file`, `symbol`, `alias?`, `apply?` — returns `use` |
| `jolt_replace_form` | `file`, `old_form`, `new_text`, `occurrence?`, `all?`, `apply?` |
| `jolt_insert_form` | `file`, `new_text`, `after?`/`before?`, `occurrence?`, `apply?` |
| `jolt_refactor` | `kind` (`sort-requires`, `cycle-keyword`, `inline-symbol`, `destructure-keys`, `restructure-keys`, `move-form`), `file`, `line?`, `col?`, `target_file?`, `apply?` |
| `jolt_format` | `file`, `apply?` |
| `jolt_structural_diff` | `file`, `old?` / `old_file?` / `rev?` (default `HEAD`), `new?` — forms changed, by where in the form, grouped by calls |
| `jolt_rename_namespace` | `from`, `to`, `apply?` — moves the files too; returns `moves`, `created`, `deleted` |
| `jolt_fix_declares` | `file`, `apply?` |
| `jolt_extract` | `file`, `forms: [name]`, `to_ns`, `apply?` — returns `target` and `callers` still naming the old place |
| `jolt_batch` | `ops: [{:tool, :args}]` — read-only tools only; `root` is passed down |

Every tool returns one text block holding JSON; a result with `ok: false`
is flagged `isError`.

### Freshness

The index follows the disk. Each call re-analyzes project files whose mtime
changed (and picks up added and deleted ones), so edits made with any tool
are visible on the next call. Applied edits re-index what they wrote.
`refresh: true` rebuilds from scratch.

### Edits

Edits preview by default. A result carries `files`, a `diff` per file
(hunks of changed lines), and `edits` with byte and line/col ranges.
`apply: true` writes and re-indexes. An edit that would leave a file that
read before unreadable is refused with the problem's line/col, and nothing
is written.

`jolt_replace_form` matches by what a form reads as, at any depth, so
`old_form` may differ from the file in whitespace and line breaks; strings
and comments never match.

### Syntax repair

`jolt_check` reports unbalanced delimiters (naming both ends of a
mismatch), unterminated strings (at the line the string opened, not where
the reader gave up), and balanced text that still does not read. It
repairs only what cannot re-parent code: a dropped trailing closer, extra
trailing closers, and a closer the indentation places (parinfer indent
mode, insert-only). Everything else is reported for the agent to fix.

Broken files still index: the broken top-level form is masked and the forms
around it are kept, and `jolt_diagnostics` reports a `syntax-error`.

## CLI commands

`index`, `refresh`, `search`, `def`, `refs`, `hover`, `symbols`,
`diagnostics`, `calls`, `impact`, `deps`, `form`, `context`, `check`, `rename`,
`clean-ns`, `add-require`, `replace-form`, `insert-form`, `refactor`,
`format`, `java`, `new-file-ns`, `ignored-forms`, `structural-diff`,
`rename-ns`, `fix-declares`, `extract`, plus `lsp` and `mcp`.

## Recommended agent flow

1. `jolt_index` once at project start; it lists files that do not read.
2. `jolt_context` for the namespace you are about to change.
3. `jolt_impact` before changing a public var.
4. `jolt_definition` / `jolt_references` to navigate.
5. `jolt_rename`, `jolt_clean_ns`, `jolt_add_require`, `jolt_replace_form`,
   `jolt_refactor` with preview, inspect, then apply.
6. `jolt_check` on text you wrote by hand, `jolt_diagnostics` on the file.
7. `jolt_structural_diff` on what you changed before you call it done: it
   says which forms changed behaviour and which only moved, were renamed or
   reformatted.
