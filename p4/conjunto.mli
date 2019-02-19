
type elemento = string
type conjunto

val vacio: conjunto
val es_vacio: conjunto -> bool
val unitario: elemento -> conjunto
val pertenece: conjunto -> elemento -> bool
val anhadir: conjunto -> elemento -> conjunto
val union: conjunto -> conjunto -> conjunto
val interseccion: conjunto -> conjunto -> conjunto
val diferencia : conjunto -> conjunto -> conjunto
val contenido: conjunto -> conjunto -> bool
val igual: conjunto -> conjunto -> bool
val borrar: conjunto -> elemento -> conjunto
val elementos: conjunto -> elemento list

