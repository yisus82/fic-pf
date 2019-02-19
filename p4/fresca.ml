

(* Generacion de nuevos nombres de variables *)

let contador = ref (-1)

let fresca () = incr contador; 
                "_" ^ string_of_int !contador 


