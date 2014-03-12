(* Nathan Burgers 2014 *)

infix 4 <!!> <&&>
signature Parse = sig
    include Monoid
    val invoke : 'a t -> string -> 'a Maybe.t
    val predicate : (char -> bool) -> char t
    val character : char -> char t
    val characterClass : string -> char t
    val literal : string -> char list t
    val literalWithSpace : string -> char list t
    val notp : 'a t -> 'b t -> 'a t
    val <!!> : 'a t * 'b t -> 'a t
    val andp : 'a t -> 'b t -> 'a t
    val <&&> : 'a t * 'b t -> 'a t
    val many : 'a t -> 'a list t
    val some : 'a t -> 'a list t
    val surround : 'a t -> 'b t -> 'c t -> 'a t
    val separatedBy : 'a t -> 'b t -> 'a list t
    val whitespace : char t
    val whitespaces : char list t
    val lower : char t
    val upper : char t
    val alpha : char t
    val digit : char t
    val operator : char t
end

structure Parse : Parse = struct
open Core
datatype 'a result
  = Failure
  | Success of 'a * char list
type 'a t = char list -> 'a result
structure M = Monoid (struct
		       type 'a t = 'a t
		       fun bind f g = fn xs => case f xs of
						   Failure => Failure
						 | Success (result, remainder) => g result remainder
		       fun return x xs = Success (x, xs)
		       fun zero _ = Failure
		       fun plus f g = fn xs => case f xs of
						   Failure => g xs
						 | s as Success _ => s
		       end)
open M
fun invoke p xs = case (p o String.explode) xs of
		      Failure => Maybe.None
		    | Success (result, []) => Maybe.Just result
		    | Success _ => Maybe.None
fun predicate p [] = Failure
  | predicate p (x::xs) = if p x then return x xs else Failure
fun character c = predicate (fn x => x = c)
val characterClass =
    let fun characterClass' [] = zero
	  | characterClass' (c::cs) = plus (character c) (characterClass' cs)
    in characterClass' o String.explode
    end
val literal =
    let fun literal' [] = return []
	  | literal' (c::cs) = cons (character c) (literal' cs)
    in literal' o String.explode
    end
fun notp f g xs = case g xs of
		      Failure => f xs
		    | Success _ => Failure
fun f <!!> g = notp f g
fun andp f g xs = case g xs of
		      Failure => Failure
		    | Success _ => f xs
fun f <&&> g = andp f g
fun some f x = (plus (cons f (some f)) (fmap List.return f)) x
fun many f = plus (some f) (return [])
fun surround f g h = right g (left f h)
fun separatedBy f g = cons f (many (right g f))
val whitespace = characterClass " \t\n\r"
val whitespaces = many whitespace
fun literalWithSpace l = left (literal l) whitespaces
val lower = characterClass "abcdefghijklmnopqrstuvwxyz"
val upper = characterClass "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val alpha = plus lower upper
val digit = characterClass "0123456789"
val operator = characterClass "-+=<>*/%"
end

signature JSON = sig
datatype t
  = Number of real
  | String of string
  | Array of t list
  | Object of (t * t) list
val parse : t Parse.t
structure Parser : sig
	      val json : t Parse.t
	      val number : t Parse.t
	      val string : t Parse.t
	      val array : t Parse.t
	      val object : t Parse.t
	  end
end

structure JSON : JSON = struct
datatype t
  = Number of real
  | String of string
  | Array of t list
  | Object of (t * t) list
structure Parser = struct
open Core
open Parse
val quote = literalWithSpace "\""
val lbrace = literalWithSpace "["
val rbrace = literalWithSpace "]"
val lbracket = literalWithSpace "{"
val rbracket = literalWithSpace "}"
val comma = literalWithSpace ","
val colon = literalWithSpace ":"
fun json x = sum [number, string, array, object] x
and number x = fmap (fn cs => Number (case Real.fromString (String.implode cs) of
					  NONE => 0.0
					| SOME n => n))
		    (some digit) x
and string x = fmap (String o String.implode) (surround (many alpha) quote quote) x
and array x = fmap Array (surround (separatedBy json comma) lbrace rbrace) x
and object x = fmap Object (surround (separatedBy (both string (right colon json)) comma) lbracket rbracket) x
end
val parse = Parser.json
end
