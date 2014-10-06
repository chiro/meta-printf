open Runcode
open Print_code
open Format
open Mprintf3

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
  let i1_ = (!. i1) 42 in
  let i2_ = (!. i2) 2 3 in
  let s_ = (!. s) "abc" in
  printc i1_;
  printc i2_;
  printc s_;
  !. i1_;
  !. i2_;
  !. s_
