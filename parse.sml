(* Nathan Burgers 2014 *)

infix 4 <!!> <&&>
signature Parse = sig
    include Monoid
    val invoke : 'a t -> string -> 'a Maybe.t
    val predicate : (char -> bool) -> char t
    val not : (char -> bool) -> char t
    val character : char -> char t
    val notCharacter : char -> char t
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
    val thenSpace : 'a t -> 'a t 
    val ignoreSpace : 'a t -> 'a t
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
fun not p [] = Failure
  | not p (x::xs) = if p x then Failure else return x xs
fun character c = predicate (fn x => x = c)
fun notCharacter c = not (fn x => x = c)
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
fun some f x = (plus (cons f (some f)) (fmap ListM.return f)) x
fun many f = plus (some f) (return [])
fun surround f g h = right g (left f h)
fun separatedBy f g = cons f (many (right g f))
val whitespace = characterClass " \t\n\r"
val whitespaces = many whitespace
fun thenSpace p = left p whitespaces
fun ignoreSpace p = surround p whitespaces whitespaces
fun literalWithSpace l = left (literal l) whitespaces
val lower = characterClass "abcdefghijklmnopqrstuvwxyz"
val upper = characterClass "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val alpha = plus lower upper
val digit = characterClass "0123456789"
val operator = characterClass "-+=<>*/%"
end

signature REAL = sig
    include REAL
    val fromString_exc : string -> real
end

structure Real : REAL = struct
open Real
exception Parse
fun fromString_exc s = case Real.fromString s of
			   NONE => raise Parse
			 | SOME n => n
end
