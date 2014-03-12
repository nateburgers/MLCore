(* Nathan Burgers 2014 *)

structure Core : Core = struct
fun curry f x y = f (x,y)
fun uncurry f (x,y) = f x y
fun f $ x = f x
fun f o g = fn x => f (g x)
fun id x = x
fun const x y = x
val left = const
fun right x y = y
end
