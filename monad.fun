(* Nathan Burgers 2014 *)

functor Functor ( F : Functor' ) : Functor = struct
	open F
	open Core
	fun f <$> x = fmap f x
	fun fconst x = (fmap o const) x
	fun x <$ y = fconst x y
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
