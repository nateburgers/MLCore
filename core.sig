(* Nathan Burgers 2014 *)

infixr 0 $
infix 6 o
signature Core = sig
    val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
    val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
    val $ : ('a -> 'b) * 'a -> 'b
    val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
    val id : 'a -> 'a
    val const : 'a -> 'b -> 'a
    val left : 'a -> 'b -> 'a
    val right : 'a -> 'b -> 'b
end

