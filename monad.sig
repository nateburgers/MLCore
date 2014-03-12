(* Nathan Burgers 2014 *)

infix 4 <$> <$
signature Functor' = sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
end

signature Functor = sig
    include Functor'
    val <$> : ('a -> 'b) * 'a t -> 'b t
    val fconst : 'a -> 'b t -> 'a t
    val <$ : 'a * 'b t -> 'a t
end

functor Functor ( F : Functor' ) : Functor = struct
	open F
	open Core
	fun f <$> x = fmap f x
	fun fconst x = (fmap o const) x
	fun x <$ y = fconst x y
	end

infix 4 <*> <* *>
signature Applicative' = sig
    type 'a t
    val pure : 'a -> 'a t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
end

signature Applicative = sig
    include Applicative'
    include Functor
    val <*> : ('a -> 'b) t * 'a t -> 'b t
    val left : 'a t -> 'b t -> 'a t
    val <* : 'a t * 'b t -> 'a t
    val right : 'a t -> 'b t -> 'b t
    val *> : 'a t * 'b t -> 'b t
    val lift : ('a -> 'b) -> 'a t -> 'b t
    val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
end

functor Applicative ( A : Applicative' ) : Applicative = struct
	open A
	open Core
	fun f <*> g = apply f g
	fun lift f a = pure f <*> a
	structure F = Functor (struct
				type 'a t = 'a t
				val fmap = lift
				end)
	open F
	fun lift2 f a b = f <$> a <*> b
	fun lift3 f a b c = f <$> a <*> b <*> c
	fun left a b = (lift2 Core.left) a b
	fun f <* g = left f g
	fun right a b = (lift2 Core.right) a b
	fun f *> g = right f g
	end

infix 6 >>= >> >=>
infix 6 =<< << <=<
signature Monad' = sig
   type 'a t
   val return : 'a -> 'a t
   val bind : 'a t -> ('a -> 'b t) -> 'b t
end

signature Monad = sig
    include Monad'
    include Applicative
    val compose : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    val >=> : ('a -> 'b t) * ('b -> 'c t) -> 'a -> 'c t
    val <=< : ('b -> 'c t) * ('a -> 'b t) -> 'a -> 'c t
    val >>= : 'a t * ('a -> 'b t) -> 'b t
    val =<< : ('a -> 'b t) * 'a t -> 'b t
    val next : 'a t -> 'b t -> 'b t
    val >> : 'a t * 'b t -> 'b t
    val << : 'a t * 'b t -> 'a t
end

functor Monad ( M : Monad' ) : Monad = struct
	open M
	open Core
	fun x >>= f = bind x f
	fun f =<< x = bind x f
	fun next f g = f >>= (fn _ => g)
	fun f >> g = next f g
	fun g << f = next f g
	fun liftM f x = x >>= (fn a => return (f a))
	fun liftM2 f x y = x >>= (fn a => y >>= (fn b => return (f a b)))
	fun compose f g = fn x => f x >>= g
	fun f >=> g = compose f g
	fun g <=< f = compose f g
	structure A = Applicative (struct
				    type 'a t = 'a t
				    val pure = return
				    fun apply a b = liftM2 id a b
				    end)
	open A
	end

structure List = struct
open List
structure M = Monad (struct
		      type 'a t = 'a list
		      fun return x = [x]
		      fun bind x f = List.concat (map f x)
		    end)
open M
end
