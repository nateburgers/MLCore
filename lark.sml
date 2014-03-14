(* Nathan Burgers 2014 *)

(* Dictionary *)
type ('a, 'b) dict = ('a * 'b) list
fun get [] x = Maybe.None
  | get ((k,v)::xs) x = if k = x then Maybe.Just v else get xs x
fun set xs k v = (k,v)::xs

(* GenType *)
signature GenType = sig
    type t
    val unit : t
    val new : t -> t
end

structure GenType = struct
open Core
type t = int
val unit = 0
val new = curry op+ 1
end

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
	   | t as _ => t
      end

fun toString (TypeVariable v) = v
  | toString (TypeAbstraction (x,y)) = "(" ^ toString x ^ " -> " ^ toString y ^ ")"
  | toString (TypeApplication (x,y)) = toString x ^ " " ^ toString y

structure TypeParser = struct
open Parse
datatype Ast
  = Var of string
  | Abs of (Ast list) * (Ast list)
  | App of Ast list
val dot = literal "."
val lparen = literal "("
val rparen = literal ")"
fun typep x = sum [application, wrap, abstraction, variable] x
and wrap x = surround typep (ignoreSpace lparen) (right whitespaces rparen) x
and variable x = lift (Var o String.implode) (some alpha) x
and abstraction x = lift Abs (both (separatedBy variable (some whitespace))
				   (right (ignoreSpace dot) (separatedBy typep (some whitespace)))) x
and application x = lift App (separatedBy (sum [wrap, abstraction, variable]) (some whitespace)) x
fun spaced xs = String.concat (ListM.intersperse xs " ")
fun toString (Var x) = x
  | toString (Abs (xs, ys)) = "(" ^ spaced (map toString xs) ^ "." ^ spaced (map toString ys) ^ ")"
  | toString (App xs) = spaced (map toString xs)
end

exception Unparsable
val parse = Parse.invoke TypeParser.typep
fun parse_exc s = case parse s of
		      Maybe.None => raise Unparsable
		    | Maybe.Just t => t
val rep = TypeParser.toString o parse_exc

(* fun parse_exc s = case parse s of *)
(* 		      Maybe.None => raise Unparsable *)
(* 		    | Maybe.Just t => t *)
(* val ep = toString o typeEval *)
(* val rep = toString o typeEval o parse_exc *)

end

