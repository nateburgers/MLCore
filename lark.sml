(* Nathan Burgers 2014 *)

use "core.sig";
use "core.fun";
use "monadplus.sml";
use "maybe.fun";
use "list.sml";
use "parse.sml";

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

fun replace (v as TypeVariable _) a b = if v = a then b else v
  | replace (TypeApplication (x,y)) a b = (TypeApplication (replace x a b, replace y a b))
  | replace (TypeAbstraction (x,y)) a b = (TypeAbstraction (replace x a b, replace y a b))

fun specialise (TypeAbstraction (x,y)) v = replace y x v

fun typeEval (TypeVariable v) = TypeVariable v
  | typeEval (TypeAbstraction (a,b)) = TypeAbstraction (typeEval a, typeEval b)
  | typeEval (TypeApplication (a,b))
    = let val a' = typeEval a
	  val b' = typeEval b
      in case a' of
	     TypeAbstraction (x,y) => typeEval (replace y x b')
	   | _ => TypeApplication (a',b')
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
and variable x = map (Var o String.implode) (some alpha) x
and abstraction x = map Abs (both (separatedBy variable (some whitespace))
				   (right (ignoreSpace dot) (separatedBy typep (some whitespace)))) x
and application x = map App (separatedBy (sum [wrap, abstraction, variable]) (some whitespace)) x
fun spaced xs = String.concat (ListM.intersperse xs " ")
fun toString (Var x) = x
  | toString (Abs (xs, ys)) = "(" ^ spaced (ListM.map toString xs) ^ "." ^ spaced (ListM.map toString ys) ^ ")"
  | toString (App xs) = spaced (ListM.map toString xs)
fun translate (Var x) = TypeVariable x
  | translate (Abs (xs,ys)) =
    let val xs' = ListM.map translate xs
	val ys' = ListM.map translate ys
    in ListM.foldr (curry TypeAbstraction) (ListM.concat (curry TypeApplication) ys') xs'
    end
  | translate (App xs) = ListM.concat (curry TypeApplication) (ListM.map translate xs)
end

structure Parser = struct
open Core
open Parse
datatype Ast
  = Symbol of string
  | Keyword of string
  | String of string
  | Number of string
  | List of Ast list
val lparen = literal "("
val rparen = literal ")"
val colon = literal ":"
val quote = literal "\""
val dot = literal "."
fun term x = sum [keyword, symbol, string, number, list] x
and identifier x = let val head = (some alpha)
		       val tail = many (sum [alpha, digit, operator])
		   in (append head tail) x
		   end
and symbol x = map (Symbol o String.implode) identifier x
and keyword x = map (Keyword o String.implode) (left identifier colon) x
and string x = map (String o String.implode) (surround (many (not (fn x => x = #"\""))) quote quote) x
and number x = map (Number o String.implode) (append (some digit) (right dot (some digit))) x
and list x = map List (surround (separatedBy term (some whitespace))
					(ignoreSpace lparen)
					(ignoreSpace rparen)) x
val rec toString =
 fn Symbol s => s
  | Keyword k => k ^ ":"
  | String s => "\"" ^ s ^ "\""
  | Number n => n
  | List xs => "(" ^ String.concat (ListM.intersperse (List.map toString xs) " ") ^ ")"
fun format x = case Parse.invoke term x of
		   Maybe.None => "Could not parse: " ^ x
		 | Maybe.Just t => toString t
end

structure Evaluator = struct
datatype Ast
  = Symbol of string
  | Keyword of string
  | Application of Ast * Ast
  | Abstraction of Ast * Ast
  | Definition of Ast * Ast
  | Condition of Ast * Ast * Ast
exception Malformed
val rec translate =
 fn Parser.Symbol s => Symbol s
  | Parser.Keyword k => Keyword k
  | Parser.String s => Keyword s 
  | Parser.Number n => Keyword n
  | Parser.List ((Parser.Symbol "lambda")::(Parser.Symbol x)::args) =>
    Abstraction (Symbol x, translate (Parser.List args))
  | Parser.List ((Parser.Symbol "define")::(Parser.Symbol x)::body::_) =>
    Definition (Symbol x, translate body)
  | Parser.List ((Parser.Symbol "if")::cond::positive::negative::_) =>
    Condition (translate cond, translate positive, translate negative)
  | Parser.List xs => ListM.concat (curry Application) (ListM.map translate xs)
fun parse x = case Parse.invoke Parser.term x of
		  Maybe.None => raise Malformed
		| Maybe.Just term => translate term
fun replace (s as Symbol _) a b = if s = a then b else s
  | replace (Abstraction (x,y)) a b = Abstraction (replace x a b, replace y a b)
  | replace (Application (x,y)) a b = Application (replace x a b, replace y a b)
exception Undefined of string
(* fun eval context = *)
(*     fn Symbol s => Symbol s *)
(*   | Abstraction (a,b) => Abstraction (eval a, eval b) *)
(*   | Application (a,b) => let val a' = eval a *)
(* 			     val b' = eval b *)
(* 			 in case a' of *)
(* 				Abstraction (x,y) => replace y x b' *)
(* 			      | Symbol s => case get context s of *)
(* 						Maybe.None => raise Undefined s *)
(* 					      | Maybe.Just procedure => procedure (context,b) *)
(* 			      | Application _ => Application (a',b') *)
(* 			 end *)
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

fun main ()
    = let val _ = TextIO.print (
		  "== Lark Language (Alpha) Type System Interpreter ==\n" ^
		  "== Nathan Burgers 2014.                          ==\n"
	      )
      in repl ()
      end
end
