(* Nathan Burgers 2014 *)

structure JSON : JSON = struct
datatype t
  = Number of real
  | True
  | False
  | Null
  | String of string
  | Array of t list
  | Object of (t * t) list
structure Parser = struct
open Core
open Parse
val quote = literal "\""
val lbrace = literalWithSpace "["
val rbrace = literalWithSpace "]"
val lbracket = literalWithSpace "{"
val rbracket = literalWithSpace "}"
val comma = literalWithSpace ","
val colon = literalWithSpace ":"
fun json x = sum [number, true, false, null, string, array, object] x
and true x = fmap (const True) (literal "true") x
and false x = fmap (const False) (literal "false") x
and null x = fmap (const Null) (literal "null") x
and number x = fmap (Number o Real.fromString_exc o String.implode)
		    (ignoreSpace (some digit)) x
and string x = fmap (String o String.implode)
		    (ignoreSpace (surround (many (notCharacter #"\"")) quote quote)) x
and array x = fmap Array (surround (separatedBy (ignoreSpace json) comma) lbrace rbrace) x
and pair x = both (thenSpace string) (right (thenSpace colon) json) x
and object x = fmap Object (surround (separatedBy pair comma) lbracket rbracket) x
end
val parse = Parse.invoke (Parse.surround Parser.json Parse.whitespaces Parse.whitespaces)
fun toString x = case x of
		     True => "true"
		   | False => "false"
		   | Null => "null"
		   | Number n => Real.toString n
		   | String s => String.concat ["\"", s, "\""]
		   | Array xs => let val csv = String.concat (ListM.intersperse (map toString xs) ",")
				 in String.concat ["[", csv, "]"] end
		   | Object xs => let fun pair (a,b) = String.concat [(toString a), ":", (toString b)]
				      val csv = String.concat (ListM.intersperse (map pair xs) ",")
				  in String.concat ["{", csv ,"}"] end
fun format s = case parse s of
		   Maybe.None => Maybe.None
		 | Maybe.Just json => Maybe.Just (toString json)
end
