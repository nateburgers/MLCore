(* Nathan Burgers 2014 *)

signature Conj = sig
    type ('a, 'b) t = 'a * 'b
    val new : 'a -> 'b -> 'a * 'b
    val one : 'a * 'b -> 'a
    val two : 'a * 'b -> 'b
end

