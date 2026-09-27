# spec-backend-quint

[Quint](https://quint.sh) backend for [`spec-protocol`](https://github.com/egao1980/spec-protocol): emits `.qnt` from a `defspec` and drives `quint typecheck / run / test / verify / compile` via `process-protocol`.

```lisp
(asdf:load-system "spec-backend-quint")            ; sets spec-protocol:*spec-backend*
(asdf:load-system "process-backend-uiop") (asdf:load-system "json-backend-jzon")
(spec-protocol:simulate 'bank :invariants '(no-negatives) :seed 1 :n-traces 2)
```

| Env | Purpose |
|-----|---------|
| `QUINT_BIN` | quint executable (default `quint` on PATH); `npm install --prefix /tmp/quint-tool @informalsystems/quint` |
| `JAVA_HOME` | JDK ≥ 17 for `verify` (Apalache, `:checker :apalache`) and `compile :tlaplus`; `--backend tlc` uses Apalache's bundled TLC |

Tested with quint **0.32.0**, Apalache **0.56.1**, OpenJDK 27. Tests skip when quint / Java are absent.

Emitter notes: actions state only what changes, the frame condition `v' = v` is added per branch; annotated parameters get a return type (`(:returns T)` or `bool` for actions); runs live in the `:instance` module, which is the `--main`.

## License

MIT
