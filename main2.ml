open Runcode
open Print_code
open Format
open Mprintf2

let printc code =
  print_code Format.std_formatter code;
  print_newline ()

let () =
  let c = mprintf "abc\n" in
  let i1 = mprintf "int : %d\n" in
  let i2 = mprintf "two ints : %d, %d\n" in
  let s = mprintf "string : %s\n" in
  printc c;
  printc i1;
  printc i2;
  printc s;
  !. c;
  (!. i1) 42;
  (!. i2) 2 3;
  (!. s) "abc";
