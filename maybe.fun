(* Nathan Burgers 2014 *)

structure Maybe : Maybe = struct
datatype 'a t
  = None
  | Just of 'a
end
