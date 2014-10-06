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
  print_string "start"; print_newline ();
  let i2 = mprintf "two ints : %d, %d\n" in
  let s = mprintf "string : %s\n" in
  let fl = mprintf "float : %f\n" in
  printc c;
  printc i1;
  printc i2;
  printc s;
  let i1_ = (!. i1) 42 in
  let i2_ = (!. i2) 2 3 in
  let s_ = (!. s) "abc" in
  let fl_ = (!. fl) 1.345 in
  printc i1_;
  printc i2_;
  printc s_;
  printc fl_;
  !. i1_;
  !. i2_;
  !. s_;
  !. fl_
