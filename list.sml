(* Nathan Burgers 2014 *)

signature ListM = sig
    include Monoid
    val intersperse : 'a t -> 'a -> 'a t
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
    val concat : ('a -> 'a -> 'a) -> 'a list -> 'a
end

structure ListM : ListM = struct
open Core
structure M = Monoid (struct
		      type 'a t = 'a list
		      fun return x = [x]
		      fun bind x f = List.concat (map f x)
		      val zero = []
		      fun plus xs = curry op@ xs
		    end)
open M
exception Empty
fun intersperse [] _ = []
  | intersperse [x] _ = [x]
  | intersperse (x::xs) a = x::a::(intersperse xs a)
fun fold f i [] = i
  | fold f i (x::xs) = fold f (f i x) xs
fun foldr f i [] = i
  | foldr f i (x::xs) = f x (foldr f i xs)
fun concat f [] = raise Empty
  | concat f (x::xs) = fold f x xs

end
