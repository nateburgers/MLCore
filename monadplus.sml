(* Nathan Burgers 2014 *)

structure Tuple = struct
fun new x y = (x,y)
end

infix 4 <$> <$
signature Functor = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    (* -- *)
    val <$> : ('a -> 'b) * 'a t -> 'b t
    val const : 'a -> 'b t -> 'a t
    val <$ : 'a * 'b t -> 'a t
end

functor Functor ( F : sig
		      type 'a t
		      val map : ('a -> 'b) -> 'a t -> 'b t
		  end ) : Functor = struct
	open Core
	open F
	fun f <$> x = map f x
	fun const x = (map o Core.const) x
	fun f <$ x = const f x
	end

infix 4 <*> << >> <::> <&>
signature Applicative = sig
    include Functor
    val pure : 'a -> 'a t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    (* -- *)
    val <*> : ('a -> 'b) t * 'a t -> 'b t
    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
    val left : 'a t -> 'b t -> 'a t
    val << : 'a t * 'b t -> 'a t
    val right : 'a t -> 'b t -> 'b t
    val >> : 'a t * 'b t -> 'b t
    val cons : 'a t -> 'a list t -> 'a list t
    val <::> : 'a t * 'a list t -> 'a list t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val <&> : 'a t * 'b t -> ('a * 'b) t
    val append : 'a list t -> 'a list t -> 'a list t
end

functor Applicative ( A : sig
			  type 'a t
			  val pure : 'a -> 'a t
			  val apply : ('a -> 'b) t -> 'a t -> 'b t
		      end ) : Applicative = struct
	open Core
	open A
	structure F = Functor ( struct
				type 'a t = 'a t
				fun map f x = apply (pure f) x
				end )
	open F
	fun f <*> x = apply f x
	fun map2 f a b = f <$> a <*> b
	fun map3 f a b c = f <$> a <*> b <*> c
	fun left a b = map2 (fn x => (fn _ => x)) a b
	fun right a b = map2 (fn _ => (fn x => x)) a b
	fun a << b = left a b
	fun a >> b = right a b
	fun cons x xs = map2 (curry op::) x xs
	fun x <::> xs = cons x xs
	fun both a b = map2 Tuple.new a b
	fun a <&> b = both a b
	fun append xs ys = map2 (curry op@) xs ys
	end

infix 4 >>= >=>
infixr 4 =<< <=<
signature Monad = sig
    include Applicative
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    (* -- *)
    val >>= : 'a t * ('a -> 'b t) -> 'b t
    val =<< : ('a -> 'b t) * 'a t -> 'b t
    val compose : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
    val >=> : ('a -> 'b t) * ('b -> 'c t) -> ('a -> 'c t)
    val <=< : ('b -> 'c t) * ('a -> 'b t) -> ('a -> 'c t)
end

functor Monad ( M : sig
		    type 'a t
		    val pure : 'a -> 'a t
		    val bind : 'a t -> ('a -> 'b t) -> 'b t
	      end ) : Monad = struct
	open Core
	open M
	structure A = Applicative ( struct
				    type 'a t = 'a t
				    val pure = pure
				    fun apply x y = bind x (fn a => bind y (fn b => pure (a b)))
				    end )
	open A
	fun x >>= f = bind x f
	fun f =<< x = bind x f
	fun compose f g = fn x => f x >>= g
	fun f >=> g = compose f g
	fun g <=< f = compose f g
	end

infix 3 <+> <+ +>
signature MonadPlus = sig
    include Monad
    val zero : 'a t
    val plus : 'a t -> 'a t -> 'a t
    (* -- *)
    val <+> : 'a t * 'a t -> 'a t
    val rightOption : 'a t -> 'b t -> 'a t
    val <+ : 'a t * 'b t -> 'a t
    val leftOption : 'a t -> 'b t -> 'b t
    val +> : 'a t * 'b t -> 'b t
    val sum : 'a t list -> 'a t
    val filter : ('a -> bool) -> 'a t -> 'a t
end

functor MonadPlus ( M : sig
			type 'a t
			val pure : 'a -> 'a t
			val bind : 'a t -> ('a -> 'b t) -> 'b t
			val zero : 'a t
			val plus : 'a t -> 'a t -> 'a t
		  end ) : MonadPlus = struct
	open Core
	open M
	structure M' = Monad ( struct
			       type 'a t = 'a t
			       val pure = pure
			       val bind = bind
			     end )
	open M'
	fun x <+> y = plus x y
	fun rightOption x y = x << y <+> x
	fun x <+ y = rightOption x y
	fun leftOption x y = y <+ x
	fun x +> y = leftOption x y
	fun sum xs = foldr op<+> zero xs
	fun filter p x = x >>= (fn a => case p a of
					    true => pure a
					  | false => zero )
	end
