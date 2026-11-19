# Changelog

## Unreleased

- `textDocument/publishDiagnostics` now carries `version`, the version of the
  document the diagnostics were computed for (LSP 3.15). The server keeps the
  `version` from `didOpen` and `didChange`, and `didSave`, which has none,
  sends the last one stored. A client can match each push to the text it was
  computed for instead of waiting for a push before sending the next change.
  A document that isn't open gets a push without `version`, and `didClose`
  forgets it.

## v0.3.3 - 2026-11-19

- The `lsp` server answered nothing until more input arrived, because on Jolt
  a character read of `*in*` waits for a whole line or EOF and an LSP body has
  no newline (https://github.com/burinc/jolt/issues/22). Frames are now read
  as bytes from `System/in`: header lines up to the blank line, then exactly
  Content-Length bytes decoded as UTF-8. MCP mode reads whole lines and is
  unchanged.
  A frame whose body is not JSON is skipped with one line on stderr instead of
  ending the server. A missing, bad or negative Content-Length still ends it,
  with a line on stderr, because without a length the reader can't tell where
  the body stops and would read every later body as headers.
- `jolt.ffi/defcfn` defines a var, so calls to a function declared with it
  (`(ffi/defcfn ^:private unsetenv* "unsetenv" [:string] :int)`, under any
  alias or referred) are no longer reported as unresolved, and go-to-definition
  finds it. No project config is needed, and a project's own `:lint-as` entry
  for it still wins.
- `proxy` method bodies see their params and `this`, for single and multiple
  arities. They were reported as unresolved symbols. The class names are
  recorded as references and the constructor args are walked.
- A `#_` discarded form inside a binding vector (`let`, `loop`, `binding`,
  `with-open`, `when-let`, `for`, `doseq` and the rest) no longer shifts the
  name and value pairs, which had reported later bindings as unresolved or
  unused.
- An `as->` used as a step of `->`, `->>`, `some->`, `some->>`, `cond->` or
  `cond->>` binds its name, since the threaded value is its expr.
- In `(proxy-super method args)` the method name is no longer a reference and
  the args are walked.
- A `def` or `defn` inside a `(comment ...)` block resolves for the rest of
  that comment. It is still not a project definition.
- Stacked discards (`#_ #_ a b`) are skipped in every pairing, and map
  destructuring and `case` clauses skip `#_` too. `as->` is also recognised
  as `clojure.core/as->` or through an alias.
- Renaming, hover and go-to-definition on `this` inside a `proxy` method no
  longer land on the method name: `this` has no declaration of its own.

## v0.3.2 - 2026-11-16

- v0.3.1 again, with its tests: the v0.3.1 tag left out a test file the test
  runner requires, so its release build failed and published nothing.

## v0.3.1 - 2026-11-16

- MCP tools called without `root` used the server's directory, which under
  Kimi is the plugin's install directory, so they indexed the plugin. `root`
  now defaults to the project holding an absolute `file`, then the last root
  used, then the server's directory; a plugin's install directory is never
  the default, and a call with nothing else to go on asks for `root`.

## v0.3.0 - 2026-11-12

- Dependency source tools, adapted from clojure-mcp's deps tools:
  `jolt_deps_list` lists the libraries a project resolves to,
  `jolt_deps_grep` searches their files (ripgrep when installed, a regex
  scan otherwise) and `jolt_deps_read` reads one, or a line range of it.
  Only the project's own dependencies are visible, with an alias's extra
  deps on request; a read outside them is refused. CLI: `deps-list`,
  `deps-grep`, `deps-read`.
- An interrupted JAR unpack is no longer taken for a complete one: JARs
  unpack into a temporary directory that is renamed into place.

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
