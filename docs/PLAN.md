# jolt-lsp — design and implementation plan

A language server and agent toolset for Jolt projects.

Primary consumer: coding agents (opencode, Claude Code, scripts). Secondary
consumer: editors speaking LSP. The engine is one pure library over a relatum
database; LSP, MCP, and a JSON CLI are thin transports over it.

Provenance: this is a clean-room implementation. Feature behavior was studied
from a permissively licensed reference server, but no code is copied from it and
it is not named in this repository. clojure-lsp (MIT) is borrowed where noted;
see [Borrowing](#borrowing-clojure-lsp).

---

## 1. What we are building

```
                 ┌─────────────────────────────────────┐
   editor  ──────►  LSP transport  (stdio JSON-RPC)    │
                 │                                     │
   agent   ──────►  MCP transport  (stdio JSON-RPC)    │──► engine
                 │                                     │    (pure fns over
   shell   ──────►  CLI  (--json, one-shot or socket)  │     relatum db)
                 └─────────────────────────────────────┘
```

The engine:

- indexes a Jolt project (deps.edn source roots, `.clj`/`.cljc`/`.edn`)
- extracts namespaces, vars, aliases, refers, requires, imports, keywords,
  locals, occurrences, and call edges
- stores everything in a relatum db with typed tables, uniques, indexes, and
  graph edges
- answers resolution, references, hover, symbols, diagnostics, call hierarchy,
  namespace dependencies, impact, and structural form queries
- produces formatting-preserving edits (rename, clean-ns, add-require, form
  replace/insert) as workspace edits with preview/apply modes

Jolt only. No let-go, no ClojureScript runtime, no Java/JDK indexing in v1.
`.cljc` is read as Clojure, exactly the half Jolt executes.

---

## 2. Feature map

Priority is agent value first, editor completeness second.

### P0 — agent core (must ship)

| Feature | Behavior source | Notes |
|---|---|---|
| Project discovery | reference server | deps.edn only; `:paths`, `:local/root`, git/mvn deps via `jolt.deps`; monorepo subprojects |
| Index: namespaces & vars | reference server | kinds, arglists, docstrings, private, `declare`, `:lint-as` |
| Index: occurrences | reference server | qualified/unqualified, quoted data rules, call-position detection |
| Index: keywords & Integrant keys | reference server | qualified keyword nav + `defmethod ig/init-key` |
| Local scopes | reference server | `let`/`fn`/`loop`/`for`/destructuring/`letfn`, shadowing, unused bindings |
| Definition | both | by cursor or by fqn; JDK/dep source where indexed |
| References | both | project + deps, context snippets, grouped counts |
| Workspace symbol search | both | tiered fuzzy match: exact > prefix > substring > subsequence |
| Hover | both | docstring, arglists, source, defined-by, deprecated |
| Diagnostics | both | unresolved ns, unused/duplicate require, unused binding, unused private var, unresolved symbol; optional clj-kondo |
| Document symbols | both | outline tree |
| Call hierarchy | clojure-lsp | callers/callees with call sites; transitive impact |
| Namespace dependency graph | clojure-lsp | dependencies/dependents, cycles, layer violations (later) |
| Form query | new | enclosing form, top-level form, next/prev form, with byte range |
| Rename | both | vars, locals, keywords; preview + apply |
| Clean namespace | both | drop unused/duplicate requires; preserve formatting |
| Add require | both | alias or refer; resolves previously unresolved symbol |
| Form replace / insert | new | structural, formatting-preserving; agent edit primitive |
| Batch query | new | several read-only ops in one round trip |
| Context pack | new | one call returns ns summary: vars, deps, dependents, diagnostics, hot call sites |

### P1 — editor parity

| Feature | Behavior source | Notes |
|---|---|---|
| Completion | both | locals, project, refers, aliases, core; fuzzy; resolve loads docs |
| Keyword completion | reference server | `:`/`::` notation-aware |
| Auto-require on accept | reference server | adds `:require` with the completion |
| Signature help | both | active arglist + parameter |
| Selection range | both | expand one form at a time |
| Document highlight | both | read/write occurrences in buffer |
| Linked editing | clojure-lsp | alias ↔ namespace name |
| New-file ns insertion | reference server | path → `(ns foo.bar-baz)` |
| Indent on Enter | reference server | `textDocument/onTypeFormatting` |
| Ignored-form ranges | reference server | `#_` and `(comment …)` |
| Code actions | both | add require, clean ns, suppress diagnostic |
| Folding | clojure-lsp | simple and cheap |
| JAR/dependency indexing | reference server | index dep `.clj` sources for definition/hover |

### P2 — refactors and polish

Borrowed/ported from clojure-lsp when cheap, after P0/P1 are green:

`inline-symbol`, `destructure-keys`, `restructure-keys`, `cycle-keyword`,
`sort-clauses`, `cycle-namespaced-map`, `move-form`, `drag`/`drag-param`,
`thread-get`, `add-missing-libspec` variants, `resolve-macro-as`, `format`
(cljfmt-compatible if it runs on Jolt), semantic tokens, code lens,
custom linters, `clj-depend` architecture rules.

### Explicitly out of scope

- let-go (`lgx.edn`, `.lg`) and ClojureScript (`.cljs`, `cljs.core`)
- JVM Java/JDK navigation, decompilation, instance methods
- paredit structural editing commands (the form edit API covers agent needs)
- Calva custom experiments (`testTree`, `projectTree`, `cursorInfo`)

---

## 3. Agent API (the point of the project)

### Transport choices

1. **MCP stdio server** — `jolt-lsp mcp`. Primary integration for opencode and
   Claude Code. Reuse the stdio/JSON-RPC loop pattern from
   `jlt-commons/jolt-nrepl-mcp`; do not reuse its nREPL logic.
2. **JSON CLI** — `jolt-lsp <command> … --json`. Works for shell agents and
   humans, no protocol client needed.
3. **LSP stdio** — `jolt-lsp lsp`. Editors; same engine.
4. **Socket daemon (later)** — `jolt-lsp serve --socket PATH` so repeated CLI
   calls share one warm index. CLI falls back to loading the disk cache.

### MCP / CLI tool surface

Read-only:

| Tool | Arguments | Result |
|---|---|---|
| `jolt_index` | `root?`, `refresh?` | project roots, files, ns/var counts, build warnings, freshness |
| `jolt_search` | `query`, `kind?`, `limit?` | ranked symbols: fqn, kind, file, range, arglists, doc first line |
| `jolt_definition` | `symbol` or `file`+`line`+`col` | locations with source snippet and kind |
| `jolt_references` | `symbol` or position, `limit?` | locations with context; counts by namespace |
| `jolt_hover` | `symbol` or position | doc, arglists, source, defined-by, deprecated |
| `jolt_symbols` | `file` | document outline |
| `jolt_diagnostics` | `file?`/`ns?`/`all?`, `kondo?` | findings with severity, code, range, related locations |
| `jolt_call_hierarchy` | `symbol`/position, `direction`, `depth?` | nodes + call-site edges |
| `jolt_impact` | `symbol`, `depth?` | transitive callers, ranked by proximity |
| `jolt_deps` | `ns`/`file`, `direction`, `depth?` | namespace dependency graph |
| `jolt_form` | `file`, `line`, `col`, `mode` | form text, byte range, kind, parent chain |
| `jolt_context` | `ns`/`file` | context pack for one namespace |
| `jolt_batch` | `ops: [...]` | results in order; read-only ops only |

Editing:

| Tool | Arguments | Result |
|---|---|---|
| `jolt_rename` | `symbol`/position, `to`, `preview?`, `apply?` | edits + diff; refuses unsafe targets |
| `jolt_clean_ns` | `file`, `preview?`, `apply?` | edits + diff |
| `jolt_add_require` | `file`, `symbol`, `alias?`, `apply?` | edits + diff |
| `jolt_replace_form` | `file`, `match`/range, `new-text`, `preview?`, `apply?` | structural replace, formatting preserved |
| `jolt_insert_form` | `file`, `after`/`before`, `text`, `apply?` | structural insert |
| `jolt_format` | `file`, `apply?` | cljfmt-compatible (P2) |

Every edit tool defaults to `preview: true` and returns:
- the resulting file text or a unified diff
- `edits` with `{file, start-byte, end-byte, start, end, old-text, new-text}`
- a `safe?` flag and refusal reasons (`library symbol`, `:keys` destructuring,
  ambiguous target, etc.)

### Result format

- Paths absolute by default; `--relative` option.
- Ranges carry three coordinate systems:
  - `:start`/`:end` LSP-style, 0-based UTF-16 columns
  - `:start-line`/`:start-col` 1-based rows and codepoint columns
  - `:bytes` `[start end]` for patch tools
- Every location includes the source line.
- Deterministic ordering: file, line, character.
- Errors are structured: `{:ok false :error {:code … :message … :data …}}`.
- Read-only batch calls are the default; edits are opt-in.

### Why this beats a plain LSP for agents

- One call returns a context pack instead of five LSP round trips.
- Byte ranges and diffs let an agent patch without re-reading files.
- `impact` and `call_hierarchy` answer "what breaks if I change this?"
- No client required: a shell agent can use `--json`.
- The same engine and cache serve editor and agent, so they never disagree.

---

## 4. Architecture and namespaces

```
jolt-lsp/
├── deps.edn
├── AGENTS.md
├── README.md
├── docs/
│   ├── PLAN.md            (this file)
│   ├── AGENT-API.md       (tool reference, generated from schemas)
│   └── FEATURES.md        (user-facing feature list)
├── resources/
│   └── jolt/core-symbols.edn   (generated from the running Jolt)
├── src/jolt_lsp/
│   ├── main.clj           CLI dispatch
│   ├── lsp.clj            LSP server + capabilities
│   ├── lsp/
│   │   ├── rpc.clj        Content-Length framing, request/notification loop
│   │   └── shim.clj       LSP params/results ↔ engine types
│   ├── mcp.clj            MCP tool schemas + server
│   ├── cli.clj            JSON CLI commands
│   ├── paths.clj          uri ↔ path, project root detection, deps.edn
│   ├── read.clj           safe rewrite-clj parse → neutral token tree
│   ├── nodes.clj          (pure) token-tree accessors
│   ├── extract.clj        (pure) token tree + path → facts
│   ├── facts.clj          fact records and normalization
│   ├── schema.clj         relatum DDL
│   ├── db.clj             create/load/cache/incremental update
│   ├── resolve.clj        (pure) symbol resolution, locals, aliases
│   ├── query.clj          read-side queries (definition, refs, symbols, …)
│   ├── diagnostics.clj    built-in lints
│   ├── graph.clj          call graph, ns deps, impact, cycles
│   ├── forms.clj          enclosing form, form replace/insert
│   ├── edit/
│   │   ├── rename.clj
│   │   ├── clean_ns.clj
│   │   ├── add_require.clj
│   │   └── vedit.clj      byte/range edit assembly and diff
│   ├── watch.clj          polling watcher + on-demand refresh
│   ├── cache.clj          fact snapshot cache (EDN/transit)
│   ├── text.clj           UTF-16 columns, offsets, CRLF
│   ├── hover.clj
│   ├── completion.clj
│   ├── signature.clj
│   ├── symbols.clj
│   └── selection.clj
├── test/
│   ├── spec/              writ specs
│   ├── jolt_lsp/          clojure.test unit/property tests
│   └── fixtures/
└── bench/
```

Rules:

- `extract`, `resolve`, `graph`, `diagnostics`, `edit` are pure and take explicit
  inputs. Only `paths`, `db`, `watch`, `cache`, and transports touch the host.
- Transports never contain feature logic.
- Every engine fn is single-arity where a writ `ann` needs it; multi-arity
  public wrappers delegate to a single-arity core.
- No Java interop beyond Jolt's `java.*` shims; prefer `jolt.fs`,
  `babashka.fs`, `babashka.process`, `jolt.process`.

---

## 5. Relatum data model

Use relatum for the index. The db is passed explicitly; the live index is held
in one atom and swapped once per update.

### Tables

```clojure
(project    [[:id :int :primary :key :auto]
             [:root :text :required :unique]
             [:deps-file :text]
             [:paths :text]                ; EDN/JSON-encoded vector
             [:kind :keyword]])

(file       [[:id :int :primary :key :auto]
             [:path :text :required :unique]
             [:uri :text :required]
             [:project-id :int :references :project :on-delete :cascade]
             [:ns :text]
             [:dialect :keyword]           ; :clj :cljc :edn
             [:source-root :text :nullable]
             [:internal :bool :default true]
             [:mtime :int]
             [:hash :text]
             [:size :int]])

(namespace  [[:id :int :primary :key :auto]
             [:name :text :required]
             [:file-id :int :references :file :on-delete :cascade]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]
             [:in-ns :bool :default false]]
            :unique [[:name :file-id]])

(ns-alias   [[:id :int :primary :key :auto]
             [:ns-id :int :references :namespace :on-delete :cascade]
             [:alias :text :required]
             [:target :text :required]]
            :unique [[:ns-id :alias]])

(ns-require [[:id :int :primary :key :auto]
             [:ns-id :int :references :namespace :on-delete :cascade]
             [:target :text :required]
             [:alias :text :nullable]
             [:refer-all :bool :default false]
             [:as-alias :bool :default false]])

(ns-refer   [[:id :int :primary :key :auto]
             [:ns-id :int :references :namespace :on-delete :cascade]
             [:target :text :required]
             [:name :text :required]
             [:rename :text :nullable]])

(ns-import  [[:id :int :primary :key :auto]
             [:ns-id :int :references :namespace :on-delete :cascade]
             [:simple :text :required]
             [:fq :text :required]])

(var-def    [[:id :int :primary :key :auto]
             [:fqn :text :required :unique]
             [:ns :text :required]
             [:name :text :required]
             [:kind :keyword :required]
             [:file-id :int :references :file :on-delete :cascade]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]
             [:name-row :int] [:name-col :int]
             [:name-end-row :int] [:name-end-col :int]
             [:private :bool :default false]
             [:deprecated :bool :default false]
             [:doc :text :nullable]
             [:arglists :text :nullable]     ; EDN string
             [:params :text :nullable]
             [:defined-by :text :nullable]
             [:external :bool :default false]])

(var-ref    [[:id :int :primary :key :auto]
             [:fqn :text :required]          ; resolved or candidate fqn
             [:ns :text]                     ; file's namespace
             [:file-id :int :references :file :on-delete :cascade]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]
             [:name-row :int] [:name-col :int]
             [:name-end-row :int] [:name-end-col :int]
             [:qualified :bool :default false]
             [:alias :text :nullable]
             [:unresolved :bool :default false]
             [:in-call :bool :default false]
             [:quoted :bool :default false]])

(local-def  [[:id :int :primary :key :auto]
             [:file-id :int :references :file :on-delete :cascade]
             [:name :text :required]
             [:kind :keyword]                ; :param :let :loop :for :fn :letfn …
             [:scope-row :int] [:scope-end-row :int]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]
             [:destructured :bool :default false]
             [:keys-destructuring :bool :default false]])

(local-ref  [[:id :int :primary :key :auto]
             [:local-id :int :references :local-def :on-delete :cascade]
             [:file-id :int :references :file :on-delete :cascade]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(keyword-def [[:id :int :primary :key :auto]
              [:fqn :text :required :unique]   ; colon-prefixed
              [:file-id :int :references :file :on-delete :cascade]
              [:kind :keyword :default :keyword] ; :keyword :integrant-key
              [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(keyword-ref [[:id :int :primary :key :auto]
              [:fqn :text :required]
              [:file-id :int :references :file :on-delete :cascade]
              [:row :int] [:col :int] [:end-row :int] [:end-col :int]
              [:notation :keyword]])           ; :plain ::ns ::alias

(call-edge  [[:id :int :primary :key :auto]
             [:caller :text :required]         ; caller fqn
             [:callee :text :required]         ; resolved callee fqn
             [:file-id :int :references :file :on-delete :cascade]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]])

(ns-dep     [[:id :int :primary :key :auto]
             [:from-ns :text :required]
             [:to-ns :text :required]
             [:kind :keyword :default :require]]) ; :require :use :import

(diagnostic [[:id :int :primary :key :auto]
             [:file-id :int :references :file :on-delete :cascade]
             [:row :int] [:col :int] [:end-row :int] [:end-col :int]
             [:severity :keyword :required]
             [:code :keyword :required]
             [:message :text :required]
             [:source :keyword :default :jolt-lsp]])
```

### Indexes

Declare with `:unique` where the skill says it materializes:

- `var-def/by-fqn` (unique)
- `keyword-def/by-fqn` (unique)
- `ns-alias` unique `[ns-id alias]`
- explicit `:hash`/`:btree` indexes:
  - `var-def` `[:ns]`, `[:file-id]`
  - `var-ref` `[:fqn]`, `[:file-id]`, `[:ns]`
  - `local-def` `[:file-id]`, `[:file-id :name]`
  - `local-ref` `[:local-id]`, `[:file-id]`
  - `keyword-ref` `[:fqn]`, `[:file-id]`
  - `call-edge` `[:callee]`, `[:caller]`, `[:file-id]`
  - `ns-dep` `[:from-ns]`, `[:to-ns]`
  - `diagnostic` `[:file-id]`, `[:code]`
  - `file` `[:path]` (unique already), `[:project-id]`

### Views and graphs

- Materialized: `unused-private-var`, `unresolved-qualified-usage`,
  `namespace-dependents` (optional; benchmark first).
- Graph: `relatum.graph/graph-of` over `call-edge` for BFS/impact/cycles, and
  over `ns-dep` for namespace cycles.
- Prefer `rc/index` direct lookups for known paths (definition by fqn, refs by
  fqn, deps by from-ns); use `rc/q` only for ad-hoc joins.

### Update protocol

1. Parse file text → facts (pure).
2. `db'` = delete all rows where `file-id = id` (cascade local rows) then
   `insert-many` new rows.
3. Recompute per-file diagnostics, replace rows.
4. `reset!` one index atom.
5. Optionally update materialized views and cache facts.

Bulk initial load uses `relatum.engine/transact` for speed after a validation
pass; per-file edits use validating `rc/*` calls. Benchmark both.

Status: relatum is in-memory and immutable, which fits the pure engine; the
per-write cost needs a benchmark gate (see §10).

---

## 6. Writ spec plan

Write specs before implementation, run `writ.spec/plan`, confirm, then code.
Specs live in `test/jolt_lsp/spec/` and are checked by the spec runner.

### `spec/text`, `spec/read`, `spec/extract`
State graph: `text → token tree → facts → normalized`.

`read` wraps the external reader and returns a neutral token tree whose nodes
carry `:tag`, `:children`, `:text`, `:row`, `:col`, `:end-row`, `:end-col`.
`extract` and its accessors are pure over that tree; the reader stays
replaceable and the facts are writ-checkable data.

`text` public fns: `line-starts`, `offset->position`, `position->offset`,
`line-utf16-length`, `utf16->codepoint-col`, `codepoint->utf16-col`,
`utf8-offset`.

`read` public fns: `parse`, `valid?`, `error-message`, `top-level-count`.

`extract` public fns: `ns-name`, `var-names`, `private-var-names`,
`alias-map`, `refer-map`, `required-namespaces`, `keyword-fqns`.

Laws:
- text: line starts at 0 and in bounds; offset/position roundtrip; UTF-16 ↔
  codepoint roundtrip per line; UTF-8 offsets monotone; anchored examples for
  empty text, CRLF, and a non-BMP character.
- read: total; empty and valid forms classified ok; unbalanced input classified
  error with a message and no forms.
- extract: empty source yields no ns, vars, aliases, requires, or keywords;
  `defn`/`defn-` found; `comment`, quote and `#_` define nothing; aliases,
  refers and requires read; qualified and plain keywords recorded.
- parser-dependent namespaces stay at `{:require :tested}` because the reader
  is external; pure accessors move to `:proved` as they stabilize.
- location soundness (M1 tests): every range satisfies
  `0 ≤ start ≤ end ≤ (count s)`; a var's range contains its name.
- fqn law (M1 tests): `(= fqn (str ns "/" name))` for every var.
- no-let-go: extraction ignores `.lg`/`lgx.edn` (Jolt only).

Proof targets: range bounds, offset/position roundtrip.

### `spec/resolve`
State graph: `symbol at position → local | var | core | special | unresolved`.

Laws:
- locals shadow vars (given a local in scope, resolution returns it).
- qualified resolution uses the file's alias map, then full ns.
- unqualified resolution order: refers → current ns → core.
- unresolved is returned only when none of the above matches.
- resolution never throws.

### `spec/graph`
- `callers` is the inverse of `callees` over the same edge set.
- `impact` equals transitive closure of `callers`.
- `cycles` finds exactly the SCCs of size > 1.
- BFS distances are shortest-path distances.

### `spec/diagnostics`
Laws, each emitted iff the predicate holds:
- `unresolved-namespace`: a qualified usage whose prefix is neither the current
  ns, a known alias, nor a required ns.
- `unused-namespace`: a require with no qualified usage and no refer used.
- `duplicate-require`: the same target required twice.
- `unused-binding`: a local never read in scope.
- `unused-private-var`: a private var with no reference inside its own file.
- no diagnostic on a clean fixture.

### `spec/edit`
State graph: `db + request → plan → validated edits → new text`.

Laws:
- rename is a no-op when `to = from`.
- rename changes every occurrence of the target and no other occurrence.
- rename of a local touches only that binding's refs.
- clean-ns output: requires = used ∪ referred; no duplicates; other forms
  byte-identical; idempotent.
- add-require output: the previously unresolved symbol resolves; no duplicate
  require; idempotent.
- form replace changes only the target byte range; all other bytes identical.
- refused targets leave the text unchanged.
- preview never writes; apply writes exactly the previewed text.

### `spec/protocol`
- UTF-16 column conversion roundtrips against byte offsets.
- LSP ranges are 0-based; positions are 1-based internally.
- JSON-RPC framing rejects a message without `Content-Length`.
- Every MCP tool result validates against its declared schema.

### Proof namespace
`test/jolt_lsp/spec/proof/` for:
- position/offset roundtrip
- edit range disjointness
- transitive closure equals BFS reachability
- UTF-16 ↔ codepoint column mapping

Run `(writ.spec/plan '<spec>)` and show it before implementing each round.

---

## 7. Borrowing clojure-lsp

clojure-lsp is MIT (Copyright (c) 2022 clojure-lsp maintainers). Keep the
notice in `NOTICE` and a header in each adapted file. No clj-pulse code is used.

Borrow and adapt (strip JVM-only pieces):

| Source | Use |
|---|---|
| `parser.clj` | safe parse of incomplete code via rewrite-clj, token repair |
| `refactor/edit.clj` | zipper helpers, find-at-pos, range utilities |
| `shared.clj` (`->range`, `->scope-range`, URI helpers subset) | LSP range assembly |
| `feature/rename.clj` | rename target classification, alias/keyword rename rules |
| `feature/clean_ns.clj` | ns form cleanup and sorting |
| `feature/add_missing_libspec.clj` | add-require suggestions and edits |
| `feature/document_symbol.clj` | outline tree building |
| `feature/workspace_symbols.clj` | fuzzy search |
| `feature/signature_help.clj` | active signature/parameter |
| `feature/selection_range.clj` | expand selection |
| `feature/linked_editing_range.clj` | alias ↔ ns |
| `feature/hover.clj` | docstring markdown formatting |
| `feature/call_hierarchy.clj` | hierarchy item assembly |
| `feature/folding.clj` | fold ranges |
| `feature/cycle_keyword.clj`, `destructure_keys.clj`, `restructure_keys.clj`, `sort_clauses.clj`, `cycle_namespaced_map.clj`, `thread_get.clj` | P2 refactors |
| `dep_graph.clj` | namespace dependency graph |
| feature tests | behavioral fixtures to port |

Do not borrow: `java_interop.clj`, `kondo.clj` JVM binding, `classpath.clj`
JVM process code, `db.clj` transit/JVM cache, `http.clj`, `stubs.clj`,
`server.clj` (use a Jolt-native JSON-RPC loop), `custom_linters_api.clj`.

Adaptation notes:

- Replace `clojure.java.io`, `java.nio.file`, `java.net.URI` with
  `jolt.fs`/`babashka.fs` and plain path strings; a small URI codec in `paths`.
- Replace `taoensso.timbre` logging with `clojure.tools.logging` on Jolt.
- Replace `medley` only if needed; Jolt's core covers most of it.
- Replace `rewrite-clj` v1.1.49 (verified working on Jolt).

---

## 8. Jolt translation notes

Verified on jolt 0.8.26:

- `rewrite-clj` 1.1.49 loads and parses; zipper needs
  `{:track-position? true}` for `z/position`; nodes carry
  `:row :col :end-row :end-col` metadata, and `z/position-span` gives
  `[[start-row start-col] [end-row end-col]]`.
- `clojure.data.json` works for JSON (MCP/LSP payloads).
- `babashka.fs` and `babashka.process` are in Jolt's stdlib and work.
- `java.net.URI` shim works (`getPath`, `toString`), so `file:` URIs are fine.
- No `java.nio.file.WatchService` assumed: implement polling (`mtime` scan on
  a short interval) plus explicit `jolt_index refresh?`; optional native
  watcher later through `jolt.ffi`/`jolt.process` if needed.
- Jolt strings are codepoint-indexed. LSP characters are UTF-16 code units.
  `text.clj` must convert columns explicitly; add a writ law for non-BMP text.
- Newlines: normalize `\r\n` for row/col, keep original bytes for edits.
- Parallel parsing: Jolt supports futures/fibers; parse files in chunks with
  `future` only after a benchmark shows a win, and keep facts order-stable.
- Core symbol table: generate `resources/jolt/core-symbols.edn` from the
  running Jolt (`ns-publics`, `:arglists`, `:doc`) rather than hardcoding a
  JVM Clojure table. Include `jolt.*` stdlib namespaces. No `cljs.core`.
- `:lint-as` compatibility: read `.clj-kondo/config.edn` and project settings
  for macro → def/binding mapping, as the reference server does.
- Optional clj-kondo: detect a native `clj-kondo` on PATH and shell out via
  `babashka.process`; never required for core features.
- JAR sources: use Jolt's embedded archive support or `unzip -p`; gate behind a
  capability check and keep it P1.

---

## 9. Milestones (TDD, one gate per round)

Each round: write the writ spec → `plan` → failing tests → implement → green →
lint/typecheck. Push feature branch eagerly.

- **M0 scaffold** — `deps.edn`, tasks (`test`, `spec`, `bench`, `build`),
  namespace skeleton, fixtures, CI gate, NOTICE. No features.
- **M1 extraction & index** — `read`, `extract`, `facts`, `schema`, `db`,
  `paths`; extract vars/ns/aliases/keywords/locals; relatum queries.
  Specs: `index`, `resolve`.
- **M2 read features** — definition, references, search, hover, symbols,
  context pack; diagnostics. Specs: `diagnostics`.
  First useful `jolt-lsp` CLI + MCP.
- **M3 edits** — rename, clean-ns, add-require, form query/replace/insert.
  Specs: `edit`, `protocol`.
- **M4 graph** — call hierarchy, ns deps, impact, cycles. Spec: `graph`.
- **M5 LSP** — framing, capabilities, sync, positions (UTF-16), diagnostics
  push. Port borrowed feature code.
- **M6 editor parity** — completion, keyword completion, auto-require,
  signature help, selection range, highlight, new-file ns, indent, ignored
  forms, code actions.
- **M7 performance** — cache, incremental updates, watcher, JAR indexing,
  `jolt dist` binary; benchmark and publish numbers.
- **M8 P2 refactors** — port the cheap clojure-lsp refactors; optional format,
  semantic tokens, code lens, clj-depend.

---

## 10. Performance and correctness gates

Targets on a 1,000-file Jolt project (to be confirmed on this machine):

| Operation | Target |
|---|---|
| cold index | < 5 s |
| warm start from cache | < 1 s |
| incremental file update | < 50 ms |
| definition | < 5 ms |
| references | < 50 ms |
| diagnostics for one file | < 30 ms |

Gates:

- `jolt test` — unit, property, fixture tests.
- `jolt spec` — writ report `:ok` for every spec.
- `jolt bench` — compare against stored baselines; fail on regression.
- correctness fixtures: a set of small projects with expected definition,
  references, rename, clean-ns, diagnostics, and graph outputs.

If relatum's per-row validation is too slow for bulk indexing, fall back to
`relatum.engine/transact` for the initial load (validate once before the
transaction) and keep `rc/*` for incremental writes. Do not remove the schema.

---

## 11. Risks and open questions

1. **Relatum write cost at 1k+ files.** Mitigate with bulk transact, batch
   inserts, and a benchmark gate. Decision point after M1.
2. **rewrite-clj parse fidelity on Jolt.** Verified on small inputs; run the
   full fixture corpus and the project itself through it in M1.
3. **UTF-16 vs codepoint columns.** Must be centralized in `text.clj`; add
   proof/law. Non-BMP identifiers are rare but tests must cover them.
4. **File freshness.** Polling is the safe default. Decide poll interval and
   whether CLI auto-spawns a socket daemon in M7.
5. **Dependency source indexing.** Deps may ship only class files or source in
   nonstandard jars. Gate JAR indexing behind capabilities; ship source-dir
   deps first.
6. **LSP client coverage.** Primary targets for v1: any LSP client for read
   features; Neovim/VS Code for edits. Calva custom APIs are out.
7. **clj-kondo.** Optional external binary only; no JVM dependency.
8. **License.** clojure-lsp is MIT, so attribution in `NOTICE` plus file
   headers is sufficient. Confirm the project's own license (recommend MIT to
   match the ecosystem) before publishing.

---

## 12. Decisions needed before implementation

1. Approve the P0/P1/P2 cut (especially dropping ClojureScript and Java/JDK).
2. Approve the agent API surface (tool names and preview-by-default edits).
3. Approve relatum as the only index store (vs. relatum + a plain-map fast
   path for bulk load).
4. Approve the namespace prefix `jolt-lsp.*` and the project license (MIT).
5. Confirm the MCP transport reuses the `jolt-nrepl-mcp` stdio pattern.

---

## Implementation status

- **M0 scaffold** — done.
- **M1 extraction and index** — done. rewrite-clj reader, tolerant parse,
  namespace/var/keyword/local extraction, relatum schema, resolution,
  call edges, namespace dependencies.
- **M2 read features and diagnostics** — done. definition, references,
  search, hover, document symbols, context packs, built-in diagnostics.
- **M3 edits** — done. rename (vars, locals, keywords, refer sites),
  clean-ns, add-require, form replace/insert, preview/apply.
- **M4 graph** — done. callers/callees, impact, namespace dependencies,
  dependents, cycles, writ-checked pure algorithms.
- **M5 LSP** — done for the core set. stdio framing, UTF-16 conversion,
  definition, references, hover, diagnostics, symbols, rename, selection,
  completion, highlight, folding, signature help, code actions, indent.
- **M6 editor parity** — mostly done. New-file namespace, ignored-form ranges,
  and indent helpers exist; on-type formatting is wired. JS/CLJS and Java/JDK
  navigation remain out of scope.
- **M7 performance** — done. Analysis cache, dependency source indexing from
  JARs and git/local roots, per-file row patching, polling watcher, benchmark,
  standalone binary.
- **M8 P2 refactors** — done for the planned set: sort-requires, cycle-keyword,
  inline-symbol, destructure-keys, restructure-keys, move-form, semantic
  tokens, code lens, whole-file cljfmt formatting, custom project linters,
  clj-depend-style layer rules, and the clj-kondo bridge.

Known limitations: dependency indexing covers Clojure sources in JARs and
source roots; Java is out of scope. clj-kondo is on by default when installed
and can be disabled per project or per call.
