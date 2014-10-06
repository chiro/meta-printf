module S = String

(*

mprintf "abc" ===>
  print_char 'a'; print_char 'b'; print_char 'c'

mprintf "a%db" ===>
  fun i -> print_char 'a'; print_int i; print_char 'b';

mprintf "a%d%sb" ==>
  fun i -> fun s -> print_char 'a'; print_int i; print_string s; print_char 'b'

 *)

type binding =
  | BInt of int code code
  | BStr of string code code

type formatter =
  | FChar of char
  | FInt
  | FStr

let shd s = S.get s 0
let stl s = S.sub s 1 (S.length s - 1)
let stl2 s = S.sub s 2 (S.length s - 2)

let rec parse_formatter : string -> formatter list = fun s ->
  if S.length s == 0 then []
  else if shd s == '%'
  then aux (S.get s 1) :: parse_formatter (stl2 s)
  else (FChar (shd s)) :: parse_formatter (stl s)
and aux = function
  | 'd' -> FInt
  | 's' -> FStr
  | '%' -> FChar '%'
  | _ -> raise (Failure "Invalid formatter")

let print_formatter = function
  | FChar c -> print_char c
  | FInt -> print_string "%d"
  | FStr -> print_string "%s"

let hd vars = match List.hd vars with
  | BInt ic -> Obj.magic ic
  | BStr sc -> Obj.magic sc
let tl = List.tl

let rec func2 fmt vars =
  match fmt with
  | [] -> .<.<()>.>.
  | (FChar c)::f -> if c == '\n'
                    then .<.<(print_newline (); .~.~(func2 f vars))>.>.
                    else .<.<(print_char c; .~.~(func2 f vars))>.>.
  | (FInt)::f -> .<.<(print_int .~.~(hd vars); .~.~(func2 f (tl vars)))>.>.
  | (FStr)::f -> .<.<(print_string .~.~(hd vars); .~.~(func2 f (tl vars)))>.>.

let rec func fmts vars fmts2 =
  match fmts with
  | [] -> Obj.magic func2 fmts2 vars
  | (FChar c)::f -> Obj.magic (func f vars fmts2)
  | (FInt)::f -> Obj.magic .<fun i -> .~(func f (vars @ [BInt .<.<i>.>.]) fmts2)>.
  | (FStr)::f -> Obj.magic .<fun s -> .~(func f (vars @ [BStr .<.<s>.>.]) fmts2)>.

let mprintf : ('a, 'b, 'c) format -> 'a code = fun f ->
  let str = string_of_format f in
  let fmt = parse_formatter str in
  func fmt [] fmt
