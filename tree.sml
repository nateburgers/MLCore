(* Nathan Burgers 2014 *)

signature Tree' = sig
    datatype a
    val compare : a -> a -> bool
end

signature Tree'' = sig
    datatype t
    val insert : t -> a -> t
end

signature Tree = sig
    include Tree'
    include Tree''
end

functor Tree ( T : Tree' ) : Tree = struct
	open T
	datatype t
	  = Leaf
	  | Branch of a * t * t
	fun insert (Leaf a) b = case compare a b of
				    true => Branch (b, Branch a Leaf Leaf, Leaf)
				  | false => Branch (a, Leaf, Branch b Leaf Leaf)
	end
