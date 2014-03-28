(* Nathan Burgers 2014 *)

use "core.sig";
use "core.fun";
use "lazy.fun";

structure Coroutine = struct

end
			    
structure Port = struct
open Core
open Socket
exception InvalidHost of string

val charWithWord = Char.chr o Word8.toInt
val wordWithChar = Word8.fromInt o Char.ord
fun word8VectorWithString s = Word8Vector.fromList $ map wordWithChar $ String.explode s
fun word8VectorSliceWithString s = Word8VectorSlice.slice (word8VectorWithString s, 0, NONE)
					
fun test hostname portNumber queueSize
    = case NetHostDB.fromString hostname of
	  NONE => NONE
	| SOME host => let fun loop () = 
			       let val address = INetSock.toAddr (host, portNumber)
				   val socket = INetSock.TCP.socket ()
				   val () = bind (socket, address)
				   val () = listen (socket, queueSize)
				   val (socket', outputAddress) = accept socket
				   val outputSocket = INetSock.TCP.socket ()
				   val someVector = recvVec (socket', 1024)
				   val _ = sendVec (socket',  word8VectorSliceWithString "fuck you :D")
				   (* val () = close socket' *)
				   val () = shutdown (socket', NO_RECVS_OR_SENDS)
			       in loop () end
		       in loop () end
end
