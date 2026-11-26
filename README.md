# jolt-lsp

A language server and agent toolset for Jolt projects.

The engine is one pure library over an in-memory index of plain maps. The MCP server, JSON
CLI, and LSP server are thin transports over it. The primary consumer is a
coding agent; the editor is the second consumer.

Jolt only: `.clj`, `.cljc`, `.edn`, `deps.edn`. No ClojureScript, let-go, or
JVM Java/JDK navigation.

## Quick start

```sh
jolt test      # Clojure tests
jolt spec      # writ specs
jolt plan      # print the writ state graphs, signatures and laws
jolt bench     # index benchmark
jolt dist      # standalone binary at target/jolt-lsp
```

During development the `:dev` alias points at a sibling `../writ` checkout,
so the tasks work without fetching.

## CLI

```sh
jolt-lsp --json index --root .
jolt-lsp --json search trimmed --root .
jolt-lsp --json def src/app/core.clj 10 16 --root .
jolt-lsp --json refs app.util/trimmed --root .
jolt-lsp --json hover app.core/greet --root .
jolt-lsp --json diagnostics --all --root .
jolt-lsp --json calls app.core/greet --direction callers --root .
jolt-lsp --json impact app.util/trimmed --root .
jolt-lsp --json context app.core --root .
jolt-lsp --json form src/app/core.clj 10 4 --root .
jolt-lsp --json check src/app/core.clj --fix --root .
jolt-lsp --json replace-form src/app/core.clj --old "(trimmed name)" --new "(trim name)" --root .
jolt-lsp --json rename src/app/core.clj 10 16 trimmed* --root .   # preview
jolt-lsp --json rename src/app/core.clj 10 16 trimmed* --apply --root .
jolt-lsp --json clean-ns src/app/core.clj --apply --root .
jolt-lsp --json add-require src/app/core.clj clojure.set/union --apply --root .
jolt-lsp --json refactor sort-requires src/app/core.clj --apply --root .
jolt-lsp --json refactor inline-symbol src/app/core.clj 12 9 --apply --root .
jolt-lsp --json refactor destructure-keys src/app/core.clj 12 9 --root .
jolt-lsp --json refactor restructure-keys src/app/core.clj 12 9 --root .
jolt-lsp --json format src/app/core.clj --apply --root .
jolt-lsp --json java java.util.Date --root .
jolt-lsp --json structural-diff src/app/core.clj --rev HEAD --root .
jolt-lsp --json rename-ns app.util app.text --apply --root .
jolt-lsp --json fix-declares src/app/core.clj --root .
jolt-lsp --json extract src/app/core.clj greet --to-ns app.greeting --root .
jolt-lsp --json deps-list --root .
jolt-lsp --json deps-grep 'defn reformat-string' --library cljfmt --glob '*.cljc' --root .
jolt-lsp --json deps-read --library cljfmt cljfmt/core.cljc --offset 640 --limit 20 --root .
```

Positions are 1-based line/column. Locations include 1-based ranges, LSP
0-based UTF-16 ranges, and UTF-8 byte offsets. Edits preview by default;
`--apply` writes.

## Lint and CI

`lint` checks every project file and prints findings the way clj-kondo
does, then exits 1 if any is at or above `--fail-level` (`error` by
default):

```sh
jolt-lsp lint --root .
jolt-lsp lint --fail-level warning --exclude '^test/fixtures/' --root .
```

`--exclude` takes a regex matched against each path relative to the root and
can be given more than once; clj-kondo's `{:output {:exclude-files [...]}}`
in `.clj-kondo/config.edn` is read too. Linter levels, `:lint-as`, hooks and
the rest of the lint config come from `.clj-kondo/config.edn` and
`.jolt-lsp/config.edn`.

Vars a project should not use go under clj-kondo's `:discouraged-var`
linter. `:presets [:security]` adds a rule set for the core vars that
evaluate or load code (`eval`, `read-string`, `load-file` and the rest), at
`:error`; exempt a namespace that needs one with `:config-in-ns`:

```clojure
;; .jolt-lsp/config.edn or .clj-kondo/config.edn
{:linters {:discouraged-var {:presets [:security]
                             my.app/legacy-fn {:message "use my.app/new-fn"}}}
 :config-in-ns {my.app.repl {:linters {:discouraged-var {clojure.core/eval {:level :off}}}}}}
```

clj-kondo itself does not know `:presets`, so put that key in
`.jolt-lsp/config.edn` when both tools read the project.

In GitHub Actions, the repository is also an action that downloads the
release binary for the runner and runs `lint`:

```yaml
      - uses: jlt-commons/jolt-lsp@v0.6.1
        with:
          fail-level: error              # the default
          args: --exclude ^vendor/       # optional
```

`args` is split on spaces with no shell quoting, so write each regex
without quotes or spaces.

## MCP

The MCP server speaks newline-delimited JSON-RPC over stdio:

```json
{
  "mcpServers": {
    "jolt-lsp": {
      "command": "/path/to/jolt-lsp",
      "args": ["mcp"]
    }
  }
}
```

Tools cover index, search, definition, references, hover (with source),
symbols, diagnostics, call hierarchy, impact, namespace dependencies and
cycles, forms, context packs, syntax check and repair, rename, clean-ns,
add-require, structural form replace/insert, refactors, namespace rename
with file moves, extraction into a new namespace, declare cleanup,
structural diff, formatting, searching and reading the source of the
project's dependencies, and batch reads. The index follows the disk, edits return diffs, and an edit
that would break a readable file is refused. See
[docs/AGENT-API.md](docs/AGENT-API.md).

It pairs with [jolt-nrepl-mcp](https://github.com/jlt-commons/jolt-nrepl-mcp),
which uses this library's syntax repair and structural replace for its
REPL tools.

## LSP

`jolt-lsp lsp` speaks LSP over stdio. Implemented: definition, references,
hover, completion, signature help, document and workspace symbols, document
highlight, selection range, folding, rename (with prepare), code actions
(clean namespace), diagnostics, semantic tokens, code lens, formatting,
watched-file updates, and indent-on-Enter. Open buffers are what is indexed.

## Features

- deps.edn project discovery and monorepo subprojects
- dependency source indexing from Maven JARs and git/local roots
- namespaces, vars, aliases, refers, requires, imports, keywords, locals
- definition, references, hover, fuzzy search, document/workspace symbols
- diagnostics covering what clj-kondo checks, all built in (no clj-kondo
  install needed): syntax errors; unresolved namespaces, symbols and vars;
  forward references; call arity for project vars, core fns (from the
  running Jolt's `:arglists`), local fns, `recur` and threading forms;
  type mismatches against clj-kondo's core specs, with types inferred
  through locals, fn returns across namespaces, `def` values and type
  hints; private and deprecated uses; unused namespaces, refers, bindings,
  private vars and values; duplicate requires, redundant `do`/`let`, loops
  without `recur`, `when` with no body, format string argument counts,
  redefined vars and inline defs. Interop, syntax-quoted code and macro
  arguments are not flagged
- the clj-kondo config a project already has: `.clj-kondo/config.edn` with
  its `:config-paths`, imports and the `clj-kondo.exports` dependencies
  ship; `:lint-as`, `:linters` levels and excludes, `:invalid-arity
  :skip-args`, `:type-mismatch` specs, and `:hooks` (`:analyze-call` and
  `:macroexpand`), which run natively in the process
- `jolt-lsp lint` and a GitHub Action for a CI gate
- syntax diagnosis and safe repair (`jolt-lsp.syntax`) and structural,
  formatting-insensitive find/replace (`jolt-lsp.replace`), dependency-free
- reader conditionals analyzed for Jolt's features (`:jolt`, `:clj`, `:default`)
- call hierarchy, transitive impact, namespace dependency graph
- formatting-preserving rename, clean-ns, add-require, form replace/insert
- refactors: sort-requires, cycle-keyword, inline-symbol, destructure-keys,
  restructure-keys, move-form
- project restructuring: rename a namespace (files move with it), extract
  forms into a new namespace, drop declares a reordering makes unneeded
  (`jolt-lsp.surgeon`, from clj-surgeon by way of samizdat)
- structural diff (`jolt-lsp.sdiff`, from sdiff): changes by form and by
  where in the form, renames, extractions and moves told apart from
  behaviour changes, changed forms grouped by the calls between them
- whole-file formatting through cljfmt
- dependency source search and read (`jolt_deps_list`, `jolt_deps_grep`,
  `jolt_deps_read`, adapted from clojure-mcp): only the libraries the
  project resolves to, git checkouts and local roots in place, Maven JARs
  unpacked into the cache; ripgrep when installed, a regex scan otherwise
- custom linters from project files, and clj-depend-style layer rules
- semantic tokens, code lens
- tolerant parsing: a broken top-level form is masked, the forms around it index
- in-memory index of plain maps, parallel analysis (dependencies shallowly),
  per-file patching, polling watcher, benchmark

Dependency indexing covers Clojure sources in JARs and source roots. Java is
out of scope for Jolt.

## Project settings

`.jolt-lsp/config.edn`:

```clojure
{:custom-linters [{:file "linters/my.clj" :fn "my.linters/analyze"}]
 :clj-depend {:rules [{:from "app.ui.*" :to "app.db.*"}]}
 :hooks true                 ; false never loads clj-kondo hook code
 ;; and any clj-kondo lint config, which wins over .clj-kondo/config.edn
 :linters {:unused-value {:level :off}}}
```

A custom linter is a Clojure namespace loaded once; its function takes
`{:db :file :ns :var-defs :var-refs :local-defs :local-refs}` and returns
diagnostic maps. The lint config follows clj-kondo's and is read from
`.clj-kondo/config.edn` as well, so a project linted with clj-kondo keeps
its settings.

## Releases

Each release on the
[Releases page](https://github.com/jlt-commons/jolt-lsp/releases) carries
standalone binaries for macOS (arm64) and Linux (x86_64), so nothing else
needs to be installed. CI pins one through the action
(`uses: jlt-commons/jolt-lsp@v0.6.1`, see [Lint and CI](#lint-and-ci)).

Projects that use jolt-lsp as a library pin a tag and its commit, so a merge
to `main` doesn't move them:

```clojure
{:deps {io.github.jlt-commons/jolt-lsp
        {:git/url "https://github.com/jlt-commons/jolt-lsp"
         :git/tag "v0.5.0" :git/sha "dd8522c"}}}
```

CI runs the tests, the specs and jolt-lsp's own lint on every pull request
and every push to `main`, then builds and smoke-tests the macOS and Linux
binaries and keeps them as workflow artifacts. Nothing is published until a
release is cut, either by pushing a `v*` tag or by running the `ci` workflow
on `main` from the Actions tab with a version such as `v0.5.0`, which tags
that commit. The release gets the binaries once the tests and the smoke test
pass. The `JOLT_PIN` repository variable pins the jolt version CI builds
with, and the latest jolt release is used when it's unset. See
[CHANGELOG.md](CHANGELOG.md) for what each release changed.

## License

Eclipse Public License 2.0, as across jlt-commons, with GPL-2.0-or-later
(with the Classpath Exception) named as a Secondary License so GPL projects
can depend on it; see [NOTICE](NOTICE). Adapted MIT code (clojure-lsp,
clj-surgeon, xi, sdiff) is attributed in [NOTICE](NOTICE), in
[docs/third-party](docs/third-party) and in each adapted file. The
dependency source tools (`jolt-lsp.dep-source`, `jolt-lsp.dep-text`) are
adapted from the deps tools of
[clojure-mcp](https://github.com/bhauman/clojure-mcp) (EPL-2.0).
