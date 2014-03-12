(* Nathan Burgers 2014 *)

signature ListM = sig
    include Monoid
    val intersperse : 'a t -> 'a -> 'a t
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
fun intersperse [] _ = []
  | intersperse [x] _ = [x]
  | intersperse (x::xs) a = x::a::(intersperse xs a)
open M
end
