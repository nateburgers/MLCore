(* Nathan Burgers 2014 *)

signature Lazy = sig
    val force : (unit -> 'a) -> 'a
    val delay : (unit -> 'a) -> unit -> 'a
end

structure Lazy : Lazy = struct
open Core
datatype 'a t
  = Unevaluated of unit -> 'a
  | Evaluated of 'a
  | Failed of exn
fun force f = f ()
fun delay f = let val thunk = ref $ Unevaluated f
	      in fn () => case !thunk of
			      Unevaluated f => let val a = f ()
							   handle err => (thunk := Failed err; raise err)
						   val () = thunk := Evaluated a
					       in a
					       end
			   | Evaluated a => a
			   | Failed err => raise err
	      end
end

structure Cont = struct
eqtype effect = unit -> unit

end

structure Continuation = struct
type ('a, 'b) t = ('a -> 'b) -> 'b
fun pure x = fn f => f x
fun bind x f = fn c => x (fn a => f a c)
fun callCC f c = f (fn a => (fn _ => c a)) c
end

