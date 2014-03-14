(* Nathan Burgers 2014 *)

(* Dictionary *)
type ('a, 'b) dict = ('a * 'b) list
fun get [] x = Maybe.None
  | get ((k,v)::xs) x = if k = x then Maybe.Just v else get xs x
fun set xs k v = (k,v)::xs

(* Lark *)
structure Lark = struct
open Core

datatype Type
  = TypeVariable of string
  | TypeApplication of Type * Type
  | TypeAbstraction of Type * Type

datatype Expression
  = Variable of string
  | Application of Expression * Expression
  | Abstraction of Expression * Expression
  | Let of Expression * Expression * Expression

type Context = (Expression, Type) dict

fun var a = TypeVariable a
fun abstract a b = TypeAbstraction (a,b)
fun apply a b = TypeApplication (a,b)

val monadT = (abstract (var "a") (apply (var "monad") (var "a")))
val monadInt = apply monadT (var "int")

fun replace (v as TypeVariable _) a b = if v = a then b else a
  | replace (TypeApplication (x,y)) a b = (TypeApplication (replace x a b, replace y a b))
  | replace (TypeAbstraction (x,y)) a b = (TypeAbstraction (replace x a b, replace y a b))

fun specialise (TypeAbstraction (x,y)) v = replace y x v

fun typeEval (t as TypeVariable _) = t
  | typeEval (t as TypeAbstraction _) = t
  | typeEval (TypeApplication (a,b))
    = let val a' = typeEval a
	  val b' = typeEval b
      in case a' of
	     TypeAbstraction (x,y) => replace y x b'
	   | t => t
      end

fun toString (TypeVariable v) = v
  | toString (TypeAbstraction (x,y)) = "(" ^ toString x ^ " . " ^ toString y ^ ")"
  | toString (TypeApplication (x,y)) = toString x ^ " " ^ toString y

structure TypeParser = struct
open Core
open Parse
datatype Ast
  = Var of string
  | Abs of (Ast list) * (Ast list)
  | App of Ast list
val dot = literal "."
val lparen = literal "("
val rparen = literal ")"
fun program x = ignoreSpace typep x
and typep x = sum [application, wrap, abstraction, variable] x
and wrap x = surround typep (ignoreSpace lparen) (right whitespaces rparen) x
and variable x = lift (Var o String.implode) (some alpha) x
and abstraction x = lift Abs (both (separatedBy variable (some whitespace))
				   (right (ignoreSpace dot) (separatedBy typep (some whitespace)))) x
and application x = lift App (separatedBy (sum [wrap, abstraction, variable]) (some whitespace)) x
fun spaced xs = String.concat (ListM.intersperse xs " ")
fun toString (Var x) = x
  | toString (Abs (xs, ys)) = "(" ^ spaced (map toString xs) ^ "." ^ spaced (map toString ys) ^ ")"
  | toString (App xs) = spaced (map toString xs)
fun translate (Var x) = TypeVariable x
  | translate (Abs (xs,ys)) =
    let val xs' = map translate xs
	val ys' = map translate ys
    in ListM.foldr (curry TypeAbstraction) (ListM.concat (curry TypeApplication) ys') xs'
    end
  | translate (App xs) = ListM.concat (curry TypeApplication) (map translate xs)
end

exception Unparsable
val parse = Parse.invoke TypeParser.program
fun parse_exc s = case parse s of
		      Maybe.None => raise Unparsable
		    | Maybe.Just t => t
val rep = toString o typeEval o TypeParser.translate o parse_exc

exception Quitting
fun repl ()
    = let val _ = TextIO.print "> "
	  val programText = TextIO.inputLine TextIO.stdIn
	  val _ = if programText = (SOME "quit\n")
		  then raise Quitting
		  else ()
	  val parsedText = case programText of
			       NONE => Maybe.None
			     | SOME s => parse s
	  val result = let val errorText = case programText of
					       NONE => "no input"
					     | SOME s => s
		       in case parsedText of
			      Maybe.None => "error: Could not parse: " ^ errorText
			    | Maybe.Just t => (toString o typeEval o TypeParser.translate) t
		       end
	  val _ = TextIO.print (result ^ "\n")
      in repl ()
      end handle Quitting => TextIO.print "Quitting.\n"
end

fun repl x =
    let val _ = TextIO.print (
		"== Lark Language (Alpha) Type System Interpreter ==\n" ^
		"== Nathan Burgers 2014.                          ==\n"
	    )
    in Lark.repl x
    end
