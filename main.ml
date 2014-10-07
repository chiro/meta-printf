open Runcode
open Print_code
open Format
open Mprintf

let printc code =
  print_code Format.std_formatter code;
  print_newline ()

let () =
  let c = mprintf "abc\n" in
  let i1 = mprintf "int : %d\n" 42 in
  (* mprintf "int : %d\n" : int -> unit code *)
  let i2 = mprintf "two ints : %d, %d\n" 2 3 in
  (* mprintf "two ints : %d, %d\n" : int -> int -> unit code *)
  let s = mprintf "string : %s\n" "abc" in
  let fl = mprintf "float : %f\n" 1.2345 in
  printc i1;
  printc i2;
  printc s;
  printc fl;
  !. c;
  !. i1;
  !. i2;
  !. s;
  !. fl
