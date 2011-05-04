(* $Id: bigint.mli,v 1.1 2011-04-26 13:39:18-07 - - $ *)

module Bigint : sig
   type bigint
   val bigint_of_string : string -> bigint
   val string_of_bigint : bigint -> string
   val add : bigint -> bigint -> bigint
   val sub : bigint -> bigint -> bigint
   val mul : bigint -> bigint -> bigint
   val div : bigint -> bigint -> bigint
   val rem : bigint -> bigint -> bigint
   val pow : bigint -> bigint -> bigint
   val zero : bigint
end

