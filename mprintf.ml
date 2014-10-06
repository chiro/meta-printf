module S = String

(*

mprintf "abc" ===>
  print_char 'a'; print_char 'b'; print_char 'c'

mprintf "a%db" ===>
  fun i -> print_char 'a'; print_int i; print_char 'b';

mprintf "a%d%sb" ==>
  fun i -> fun s -> print_char 'a'; print_int i; print_string s; print_char 'b'

 *)

type formatter =
  | FChar of char
  | FInt

let rec parse_formatter : string -> formatter list = fun s ->
  if S.length s == 0 then []
  else if S.get s 0 == '%'
  then if S.get s 1 == 'd'
       then FInt :: parse_formatter (S.sub s 2 (S.length s - 2))
       else if S.get s 1 == '%'
       then (FChar '%') :: parse_formatter (S.sub s 2 (S.length s - 2))
       else (FChar '%') :: parse_formatter (S.sub s 2 (S.length s - 1))
  else (FChar (S.get s 0)) :: parse_formatter (S.sub s 1 (S.length s - 1))

let print_formatter = function
  | FChar c -> print_char c
  | FInt -> print_string "%d"

let func f x : 'a =
  match f with
  | FChar c -> Obj.magic .<(print_char c; .~x)>.
  | FInt -> Obj.magic .< fun i -> print_int i; .~x >.

let mprintf : ('a, 'b, 'c) format -> 'a code = fun f ->
  let str = string_of_format f in
  let fmt = parse_formatter str in
  Obj.magic (List.fold_right func fmt .<()>.)
