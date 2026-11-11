# Changelog

## v0.2.1 - 2026-11-11

- A def whose value is a map, `(def m {:k f})`, records the references
  inside the map. They were skipped as an attr-map, so `f` could be reported
  unused. `(def m {:private true})` no longer marks `m` private.

## v0.2.0 - 2026-11-11

- Structural diff (`jolt_structural_diff`, `structural-diff`,
  `jolt-lsp.sdiff`): what changed in a file by form and by where in the
  form, against text, another file or a git revision. Renames, extractions,
  moves, wrapping and comment-only edits are told apart from behaviour
  changes, and changed forms are grouped by the calls between them, with
  namespaces and aliases resolved by the analyzer.
- Namespace rename with file moves (`jolt_rename_namespace`, `rename-ns`):
  a leaf namespace's file moves too, not only a prefix's directory.
- Extract forms into a new namespace (`jolt_extract`, `extract`), with the
  index naming the files that still call a moved var.
- Declare cleanup (`jolt_fix_declares`, `fix-declares`).
- The planning library behind them, `jolt-lsp.surgeon` (clj-surgeon's
  extract, move, rename, fix-declares and cljc split/merge), moved here from
  samizdat. It needs no crypto provider: content revisions are a pure SHA-256.
- Grouping no longer throws when a diff removes a form.
- Licence: GPL-2.0-or-later (with the Classpath Exception) is named as an
  EPL-2.0 Secondary License, so GPL projects can depend on jolt-lsp.

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
