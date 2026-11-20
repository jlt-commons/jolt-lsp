# Changelog

## v0.6.0 - 2026-11-20

- clj-kondo's `:discouraged-var` linter: uses of the vars listed under
  `{:linters {:discouraged-var {ns/var {:message "..."}}}}` are flagged,
  warning by default, with the linter's or a var's own `:level`. A
  namespace can change or turn off entries with `:config-in-ns`. An opt-in
  `:presets [:security]` bans `read-string`, `read`, `eval`, `load-string`,
  `load-reader`, `load-file` and `clojure.xml/parse`.
- Renaming a local bound by `{:keys [a] :or {a 1}}` renames the `:or` key
  too, and references list it. The default used to stay under the old name,
  so the renamed binding lost it. A binding named only in `:or` is still
  reported unused, as clj-kondo does.

## v0.5.1 - 2026-11-19

- The optional clj-kondo bridge is gone: jolt-lsp no longer shells out to a
  `clj-kondo` binary, since its own diagnostics now cover clj-kondo's checks,
  config and hooks. The `:kondo` setting, the `--kondo` and `--no-kondo`
  flags and the `kondo` argument of `jolt_diagnostics` are removed, so
  results no longer depend on what else is installed.
- Any `jolt.*` namespace counts as built in. The standalone binary decided by
  loading the namespace, and it cannot load every Jolt namespace a project's
  runtime has (`jolt.nrepl`), so `lint` in CI reported them unknown.

## v0.5.0 - 2026-11-19

- `jolt-lsp lint` checks every project file for a CI gate: findings printed
  as clj-kondo prints them, exit 1 at `--fail-level` (`error` by default),
  `--exclude` regexes and clj-kondo's `:output :exclude-files`. The
  repository is a GitHub Action that runs it with the release binary
  (`uses: jlt-commons/jolt-lsp@v0.5.0`).
- More false positives found linting the jlt-commons and jolt-lang libraries
  are fixed: `data_readers.clj` read as code, the keyword `:/` failing to
  parse (so the rest of the file went unindexed), a `#_` form before an
  arity's params dropping that arity, `:use` and `:refer :all` of core
  namespaces referring nothing, Jolt's built-in `jolt.*` and `clojure.*`
  namespaces counted unknown, top-level `alias` calls, scripts with no `ns`
  form (they run in `user`), record constructors from a `:refer :all`
  namespace, `:refer-clojure :exclude` of a special form, and the vars
  `with-redefs` and `binding` name counted as private calls. A macro linted
  as `def` through `:lint-as` that takes more than a docstring and a value
  no longer has its last argument read as the var's value.
- A document opened through a symlink got its diagnostics under the
  canonical path, with the symlinks resolved, because the index keys files
  that way. A client that names documents by the URI it opened them with
  dropped every push for such a project, and on macOS that includes any
  project under `/tmp` or `/var`. A push for an open document now uses the
  URI from its `didOpen`, and a file open under two URIs gets a push for
  each. The same URI names the document in definitions, references, rename
  edits and other locations. A file the client never opened is named under
  the workspace root as the client gave it in `initialize`, so a definition
  or reference into it opens the same buffer the client's own path would,
  not a second copy under the canonical path. (#7)
- URIs are percent-encoded and decoded properly. Only `%20` was decoded
  before and nothing was encoded, so a path with a space, `#`, `+` or a
  non-ASCII character went wrong in one direction or the other.
- Diagnostics now cover what clj-kondo checks: call arity (project vars,
  core fns, local fns, `recur`, threading forms), unresolved vars, private
  and deprecated uses, forward references, redundant `do` and `let`, loops
  without `recur`, `when` with no body, format string argument counts,
  unused values, redefined vars, inline defs, and type mismatches, with
  types inferred through locals, fn returns across namespaces, `def`
  values, type hints and clj-kondo's `:type-mismatch` config. Some clj-kondo
  code is adapted, as `NOTICE` lists.
- clj-kondo hooks (`:analyze-call`, `:macroexpand`) run natively, from
  `.clj-kondo`, its imports and `:config-paths`, and the
  `clj-kondo.exports` directories dependencies ship. `:hooks false` in
  `.jolt-lsp/config.edn` turns them off.
- The lint config honours clj-kondo's `:level` per linter and
  `:invalid-arity :skip-args`.
- A qualified name into a namespace the project has but the file does not
  require is now a warning ("Are you missing a require?") instead of an
  unknown-namespace error.
- Editing a file now re-resolves the files that depend on it; they kept
  stale results until a rebuild. `:deprecated` metadata is indexed and shown
  by hover, and `:refer-clojure :exclude` is respected. A batch of false
  unresolved symbols is fixed: `clojure.core/defn` written qualified,
  `declare` with metadata, `areduce`/`amap`, reader conditionals in params
  and libspecs, `:refer :all` from an unindexed namespace, locals named like
  special forms, special forms inside a syntax-quote, top-level `require`
  calls, and Jolt's `jolt.*` namespaces.

## v0.4.0 - 2026-11-19

- `textDocument/publishDiagnostics` now carries `version`, the version of the
  document the diagnostics were computed for (LSP 3.15). The server keeps the
  `version` from `didOpen` and `didChange`, and `didSave`, which has none,
  sends the last one stored. A client can match each push to the text it was
  computed for instead of waiting for a push before sending the next change.
  A document that isn't open gets a push without `version`, and `didClose`
  forgets it.
- A request sent before `initialize` now gets the error LSP asks for,
  `-32002` (server not initialized), instead of an answer. A notification
  sent that early is dropped, and `exit` still works at any time.
- The server asks to watch `.clj`, `.cljc` and `.edn` files through
  `client/registerCapability` only when the client's `initialize` request
  sets `workspace.didChangeWatchedFiles.dynamicRegistration` to true. Before,
  it asked every client. Without the registration, the index still picks up
  files changed, added or deleted on disk the next time it's used.

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
