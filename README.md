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
jolt-lsp --json diagnostics --all --kondo --root .
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
```

Positions are 1-based line/column. Locations include 1-based ranges, LSP
0-based UTF-16 ranges, and UTF-8 byte offsets. Edits preview by default;
`--apply` writes.

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
add-require, structural form replace/insert, refactors, formatting, and
batch reads. The index follows the disk, edits return diffs, and an edit
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
- built-in diagnostics: syntax errors, unresolved namespace/symbol, unused
  namespace/refer/binding, duplicate require, unused private var; interop,
  syntax-quoted and macro arguments are not flagged
- syntax diagnosis and safe repair (`jolt-lsp.syntax`) and structural,
  formatting-insensitive find/replace (`jolt-lsp.replace`), dependency-free
- reader conditionals analyzed for Jolt's features (`:jolt`, `:clj`, `:default`)
- call hierarchy, transitive impact, namespace dependency graph
- formatting-preserving rename, clean-ns, add-require, form replace/insert
- refactors: sort-requires, cycle-keyword, inline-symbol, destructure-keys,
  restructure-keys, move-form
- whole-file formatting through cljfmt
- custom linters from project files, and clj-depend-style layer rules
- semantic tokens, code lens, clj-kondo findings (on by default when installed)
- tolerant parsing: a broken top-level form is masked, the forms around it index
- in-memory index of plain maps, parallel analysis (dependencies shallowly),
  per-file patching, polling watcher, benchmark

Dependency indexing covers Clojure sources in JARs and source roots. Java is
out of scope for Jolt.

## Project settings

`.jolt-lsp/config.edn`:

```clojure
{:kondo true                 ; nil = on when clj-kondo is installed
 :custom-linters [{:file "linters/my.clj" :fn "my.linters/analyze"}]
 :clj-depend {:rules [{:from "app.ui.*" :to "app.db.*"}]}}
```

A custom linter is a Clojure namespace loaded once; its function takes
`{:db :file :ns :var-defs :var-refs :local-defs :local-refs}` and returns
diagnostic maps.

## License

Eclipse Public License 2.0, as across jlt-commons, with GPL-2.0-or-later
(with the Classpath Exception) named as a Secondary License so GPL projects
can depend on it; see [NOTICE](NOTICE). Adapted clojure-lsp code
(MIT) is attributed in [NOTICE](NOTICE) and in each adapted file.
