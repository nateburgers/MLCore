(* Nathan Burgers 2014 *)

signature Maybe = sig
    datatype 'a t
      = None
      | Just of 'a
end
