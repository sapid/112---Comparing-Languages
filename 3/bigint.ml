open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 1000
    let  radixlen =    3

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let bigint_of_string str =
        let rec makelist str first len =
            if   first = len
            then [] 
            else let diff = len - first in
                 let len' = len - radixlen in
                 if   diff < radixlen
                 then [int_of_string (strsub str first diff)]
                 else (int_of_string (strsub str len' radixlen))
                      :: (makelist str first len')
        in  let len = strlen str
            in  if   len = 0
                then Bigint (Pos, [])
                else if   str.[0] = '_'
                     then Bigint (Neg, makelist str 1 len)
                     else Bigint (Pos, makelist str 0 len)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (string_of_int (car reversed)) ::
                        (map (sprintf "%03d") (cdr reversed)))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = car1 - car2 - carry
          in diff mod radix :: sub' cdr1 cdr2 (diff / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else Bigint (neg1, sub' value2 value1 0)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, sub' value1 value2 0)
        else Bigint(neg1, add' value1 value2 0)

    let rec mul' val1 val2 =
         if (car val2) = 1
         then val1
         else (add' val1 (mul' val1 (sub' val2 [1] 0)) 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
         if neg1 = neg2
         then Bigint (Pos, mul' value1 value2)
         else Bigint (Neg, mul' value1 value2)

    let div = add

    let rem = add

    let pow = add

end

