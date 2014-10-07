open Runcode

module S = String

type formatter =
  | FConstant of string
  | FInt
  | FStr
  | FFloat

let shd s = S.get s 0
let stl s = S.sub s 1 (S.length s - 1)
let stl2 s = S.sub s 2 (S.length s - 2)

let rec parse_formatter s = parse_formatter_aux s ""
and parse_formatter_aux s acc : formatter list =
  if S.length s == 0 then
    if S.length acc == 0 then [] else [FConstant acc]
  else if shd s == '%' then
    if S.length acc != 0 then
      FConstant acc :: get_formatter (S.get s 1) :: parse_formatter_aux (stl2 s) ""
    else
      get_formatter (S.get s 1) :: parse_formatter_aux (stl2 s) ""
  else
    parse_formatter_aux (stl s) (acc ^ (S.make 1 (shd s)))
and get_formatter = function
  | 'd' -> FInt
  | 's' -> FStr
  | 'f' -> FFloat
  | '%' -> FConstant "%"
  | _ -> raise (Failure "Invalid formatter")

let rec concat_codes2_rev : unit code code list -> unit code code = function
  | [] -> .<.<()>.>.
  | h::t ->
     List.fold_left
       (fun code acc -> .<.<(.~.~acc ; .~.~code)>.>.) h t

let rec func fmts vars =
  match fmts with
  | [] -> Obj.magic (concat_codes2_rev vars)
  | (FConstant s)::f -> .<.~(func f (.<.<print_string s>.>.::vars))>.
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
