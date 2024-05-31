let _ =
  let argc = Array.length Sys.argv in
  if argc != 2 then (
    Format.printf "Usage: ./mic [filename]\n";
    exit (-1))
  else
    let fname = Sys.argv.(1) in
    Mic.Compile.compile fname
