open Runcode
open Mprintf2

let () =
  let p = mprintf "%d.%d.%d.%d\n" in
  for i = 1 to 10000 do
    (!. p) 1 2 3 4
  done
