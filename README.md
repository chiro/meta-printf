meta-printf
===========

An experimental implementation of precompiling printf and sprintf in MetaOCaml.

## Examples
```ocaml
mprintf "abc\n"
(* => .<print_char 'a'; print_char 'b'; print_char 'c'; print_newline ()>. *)
```

```ocaml
open Mprintf2

mprintf "%d, %s\n"
(* => .<fun i -> fun s ->
  print_int i; print_char ','; print_char ' '; print_string s;
  print_newline ()>.
*)
```
