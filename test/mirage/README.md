# Build && Run

You need the following
https://github.com/mirage/ocaml-fat/pull/81 from ocaml-fat

```
$ ./split-mnist-data
$ ocamlfind ocamlopt -o runall -linkpkg -package lwt,lwt.unix runall.ml && rm *.o *.cm?
$ (cd ../.. && opam install -w actor_mirage) && mirage clean  ; mirage configure -t hvt --kv_ro=fat && make depend && make && ./runall
```
