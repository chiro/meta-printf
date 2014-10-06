open Runcode
open Print_code
open Format
open Mprintf

let printc code =
  print_code Format.std_formatter code;
  print_newline ()

let () =
  let c = mprintf "abc" in
  let i1 = mprintf "int : %d" in
  let i2 = mprintf "two ints : %d, %d" in
  !. c;
  (!. i1) 42;
  (!. i2) 2 3;
  printc c;
  printc i1;
  printc i2
