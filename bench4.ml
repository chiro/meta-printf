open Runcode
open Print_code
open Mprintf3

let printc code =
  print_code Format.std_formatter code;
  print_newline ()

let () =
  let p = close_code ((!. (mprintf "%d.%d.%d.%d\n")) 1 2 3 4) in
  for i = 1 to 10000 do
    (run_bytecode p)
  done
