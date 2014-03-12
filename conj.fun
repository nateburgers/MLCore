(* Nathan Burgers 2014 *)

structure Conj : Conj = struct
type ('a, 'b) t = 'a * 'b
fun new a b = (a,b)
fun one (a,b) = a
fun two (a,b) = b
end			      
