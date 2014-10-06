open Runcode

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
  | FStr
  | FFloat

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
  | 'f' -> FFloat
  | '%' -> FChar '%'
  | _ -> raise (Failure "Invalid formatter")

let print_formatter = function
  | FChar c -> print_char c
  | FInt -> print_string "%d"
  | FStr -> print_string "%s"
  | FFloat -> print_string "%f"

(*
  concat_code2 [.<.<ignore(1)>.>.; .<.<ignore(2)>.>.]
  ==> .<.<ignore(1); ignore(2)>.>.
 *)
let rec concat_codes2 : unit code code list -> unit code code = function
  | [] -> .<.<()>.>.
  | h::t ->
     List.fold_right
     (fun code acc -> .<.<(.~.~acc; .~.~code)>.>.) t h

(*
  concat_code2_rev [.<.<ignore(1)>.>.; .<.<ignore(2)>.>.]
  ==> .<.<ignore(2); ignore(1)>.>.
 *)
let rec concat_codes2_rev : unit code code list -> unit code code = function
  | [] -> .<.<()>.>.
  | h::t ->
     List.fold_left
       (fun code acc -> .<.<(.~.~acc ; .~.~code)>.>.) h t

let rec func fmts vars =
  match fmts with
  | [] -> Obj.magic (concat_codes2_rev vars)
  | (FChar c)::f -> .<.~(func f (.<.<print_char c>.>.::vars))>.
  | (FInt)::f ->
     Obj.magic .<fun i ->
                 .~(func f (.<.<print_int i>.>.::vars))>.
  | (FStr)::f ->
     Obj.magic .<fun s ->
                 .~(func f (.<.<print_string s>.>.::vars))>.
  | (FFloat)::f ->
     Obj.magic .<fun fl ->
                 .~(func f (.<.<print_float fl>.>.::vars))>.

let mprintf = fun f ->
  let str = string_of_format f in
  let fmt = parse_formatter str in
  !. (func fmt [])
