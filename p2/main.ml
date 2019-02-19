
let print s = print_string s;
              print_newline ()

let usage () =
  print "Uso:";
  print "\tdelta <fichero>";
  ()

let eval term =
  let term' = Lambda.eval term in
    print ("valor = " ^ Pretty.term term');
    ()


let leer_fichero nombre =
  eval (Lector.fichero nombre)

let _ = if (Array.length (Sys.argv) = 2) then
          leer_fichero(Sys.argv.(1))
        else
          usage ()
