(* Nathan Burgers 2014 *)

signature QUEUE = sig
type 'a t
val queue : int -> 'a -> 'a t
val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a -> unit
end

structure Queue = struct
type 'a t = { storage : 'a Array.array
	    , enqueue : int ref
	    , dequeue : int ref
	    , size : int ref
	    }
fun queue size init = { storage = Array.array (size,init)
		      , enqueue = ref 0
		      , dequeue = ref 0
		      , size = ref 0
		      }
fun enqueue (q:'a t) x
    = let val () = Array.update (#storage q, !(#enqueue q), x)
	  val () = (#enqueue q) := ((#enqueue q) + 1)
	  val () = #size q := #size q + 1
      in () end
end
