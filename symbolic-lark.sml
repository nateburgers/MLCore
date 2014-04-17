(* Nathan Burgers 2014 *)
use "core.sig";
use "core.fun";
use "monadplus.sml";
use "list.sml";
use "parse.sml";

structure Syntax =
struct
open Core
open Parse

type chars = char list
datatype Atom
  = Symbol of chars
  | String of chars
  | Real of chars
  | Int of chars
  | List of Expression list
datatype Expression
  = Atom of Atom
  | Application of Atom list * Expression
  | Operator of chars * Expression

end

structure Parser =
struct
open Core
open Parse
val nilString = "nil"
datatype Expression
  = Integer of int
  | Real of real
  | Character of char
  | String of string
  | Symbol of string
  | Expression of {
      head : Expression,
      body : Expression list
  }

exception Invalid of string
fun construct constructor typeName characterList
    = case constructor $ String.implode characterList of
	  NONE => raise Invalid $ typeName ^ ": " ^ String.implode characterList
	| SOME value => value
fun makeInteger characterList = Integer $ construct Int.fromString "Integer" characterList
fun makeReal characterList = Real $ construct Real.fromString "Real" characterList
fun makeCharacter character = Character character
fun makeString characterList = String $ String.implode characterList
fun makeSymbol characterList = Symbol $ String.implode characterList
fun makeExpression (head,body) = Expression { head = head, body = body }
fun makeList expressions = Expression { head = Symbol "List", body = expressions }
fun makeOperator ((a,operator),b) = Expression { head = Symbol (String.implode operator), body = [a,b] }

val specialSymbol = characterClass "`~!@#$%^&*-+=\\|<>./?"
val operator = some specialSymbol

fun parseInteger x = (makeInteger <$> (some digit)) x
and parseOperator x = (makeOperator <$> (parseTerm <&> operator <&> parseTerm)) x
and parseReal x = ((makeReal o (fn (above,below) => above @ (#"."::below)))
		    <$> (some digit << literal "." <&> some digit)) x
and parseCharacter x = (makeCharacter <$> (literal "'" >> alpha << literal "'")) x
and parseString x = (makeString <$> (literal "\"" >> many (notCharacter #"\"") << literal "\"")) x
and parseSymbol x = (makeSymbol <$> some alpha) x
and parseTermsSurround l r x = (surround (separatedBy parseTerm (literal ",")) l r) x
and parseTermsSurroundSquare x = parseTermsSurround (literal "[") (literal "]") x
and parseList x = (makeList <$> parseTermsSurround (literal "{") (literal "}")) x
and parseExpression x = (makeExpression <$> (parseSymbol <&> parseTermsSurroundSquare)) x
and parseTerm x = (sum [parseSymbol
		      ,parseReal
		      ,parseInteger
		      ,parseCharacter
		      ,parseString
		      ,parseList
		      ] <&> parseTerm' ) x
and parseLine x = (parseTerm <+ literal "\n") x
val rec toString =
    fn Integer int => Int.toString int
  | Real real => Real.toString real
  | Character char => String.implode [#"'", char, #"'"]
  | String string => "\"" ^ string ^ "\""
  | Symbol symbol => symbol
  | Expression {head=head,body=body} => toString head ^ "[" ^ (String.concat $ ListM.intersperse (ListM.map toString body) ",") ^ "]"

fun format s = case invoke parseLine s of
		   Maybe.None => "Could not parse: " ^ s
		 | Maybe.Just term => toString term

fun makeRepl format ()
    = let val _ = TextIO.print "lark> "
	  val programText = TextIO.inputLine TextIO.stdIn
	  val parsedText = case programText of
			       NONE => "No Input"
			    | SOME s => format s
	  val _ = TextIO.print $ parsedText ^ "\n"
      in makeRepl format ()
      end

val repl = makeRepl format

end
