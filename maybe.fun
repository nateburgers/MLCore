(* Nathan Burgers 2014 *)

structure Maybe  = struct
datatype 'a t
  = None
  | Just of 'a
structure M = Monad (
    struct
    type 'a t = 'a t
    fun pure x = Just x
    fun bind (None) f = None
      | bind (Just x) f = f x
    end
) open M
end
