# jolt-lsp

A language server and agent toolset for Jolt projects.

The engine is one pure library over a relatum database. The MCP server, JSON
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

During development the `:dev` aliases point at sibling checkouts (`../writ`,
`../../relatum`), so the tasks work without fetching.

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
jolt-lsp --json rename src/app/core.clj 10 16 trimmed* --root .   # preview
jolt-lsp --json rename src/app/core.clj 10 16 trimmed* --apply --root .
jolt-lsp --json clean-ns src/app/core.clj --apply --root .
jolt-lsp --json add-require src/app/core.clj clojure.set/union --apply --root .
jolt-lsp --json refactor sort-requires src/app/core.clj --apply --root .
jolt-lsp --json refactor inline-symbol src/app/core.clj 12 9 --apply --root .
jolt-lsp --json refactor destructure-keys src/app/core.clj 12 9 --root .
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

Tools cover index, search, definition, references, hover, symbols,
diagnostics, call hierarchy, impact, namespace dependencies, forms, context
packs, rename, clean-ns, add-require, form replace/insert, and batch reads.
See [docs/AGENT-API.md](docs/AGENT-API.md).

## LSP

`jolt-lsp lsp` speaks LSP over stdio. Implemented: definition, references,
hover, completion, signature help, document and workspace symbols, document
highlight, selection range, folding, rename, code actions (clean namespace),
diagnostics, and indent-on-Enter.

## Features

- deps.edn project discovery and monorepo subprojects
- dependency source indexing from Maven JARs and git/local roots
- namespaces, vars, aliases, refers, requires, imports, keywords, locals
- definition, references, hover, fuzzy search, document/workspace symbols
- built-in diagnostics: unresolved namespace/symbol, unused namespace/binding,
  duplicate require, unused private var
- call hierarchy, transitive impact, namespace dependency graph
- formatting-preserving rename, clean-ns, add-require, form replace/insert
- refactors: sort-requires, cycle-keyword, inline-symbol, destructure-keys, move-form
- semantic tokens, code lens, optional clj-kondo findings
- tolerant parsing: a broken tail keeps the forms before it
- analysis cache keyed by file hash, per-file index patching, polling watcher, benchmark

Remaining gaps: restructure-keys, whole-file formatting, custom linters and
clj-depend are not implemented. Dependency indexing covers Clojure sources in
JARs/source roots; it does not decompile Java classes. clj-kondo is opt-in
(`--kondo`, or `:kondo true` over MCP).

## License

MIT. Adapted clojure-lsp code is attributed in [NOTICE](NOTICE) and in each
adapted file.
