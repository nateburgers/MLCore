(* Nathan Burgers 2014 *)

signature JSON = sig
    datatype t
      = Number of real
      | True
      | False
      | Null
      | String of string
      | Array of t list
      | Object of (t * t) list
    val parse : string -> t Maybe.t
    val toString : t -> string
    val format : string -> string Maybe.t
    structure Parser : sig
		  val json : t Parse.t
		  val number : t Parse.t
		  val string : t Parse.t
		  val array : t Parse.t
		  val object : t Parse.t
	      end
end
