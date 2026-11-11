# Changelog

## v0.1.0 - 2026-11-11

First release.

- Language server (`jolt-lsp lsp`), MCP server for coding agents
  (`jolt-lsp mcp`) and JSON CLI over one engine.
- Fast indexing: a purpose-built parser, parallel analysis (dependencies
  shallowly), and an in-memory index of plain maps patched per file. About
  0.7s for 1MB of source; warm queries take milliseconds.
- The index follows the disk: files changed by any tool are re-read on the
  next call; applied edits re-index what they wrote.
- Syntax diagnosis and safe repair (`jolt_check`, `jolt-lsp.syntax`) and
  structural, formatting-insensitive replace (`jolt_replace_form`,
  `jolt-lsp.replace`), moved here from jolt-nrepl-mcp.
- Edits preview as diffs; an edit that would leave a readable file
  unreadable is refused.
- Diagnostics: syntax errors, unresolved symbols/namespaces, unused
  requires/refers/bindings/privates, duplicate requires, clj-kondo when
  installed. Lint config in clj-kondo's shape (`:lint-as`,
  `:unresolved-symbol {:exclude ...}`), read from `.clj-kondo/config.edn`
  and `.jolt-lsp/config.edn`.
- Refactors: rename, clean-ns, add-require, sort-requires, cycle-keyword,
  inline-symbol, destructure-keys, restructure-keys, move-form, cljfmt
  formatting.
