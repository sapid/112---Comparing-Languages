include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let registers = Hashtbl.create 1;;

let print_number number = printf "%s\n%!" (string_of_bigint number)

let print_stackempty () = printf "stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
   try match oper with
      | 'l' -> push (Hashtbl.find registers reg) thestack
      | 's' -> Hashtbl.replace registers reg (pop thestack)
      | _   -> printf "%o %o is unimplemented\n%!" (ord oper) reg
   with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()


let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add (* Done for positive *)
        | '-'  -> executebinop thestack Bigint.sub (* Done for positive *)
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack (* Clear the stack *)
        | 'd'  -> push (Stack.top thestack) thestack (* Dup the top of stack *)
        | 'f'  -> Stack.iter print_number thestack (* Print the whole stack *)
        | 'l'  -> () (* Load a number from a character register *)
        | 'p'  -> print_number (Stack.top thestack) (* Print top of stack *)
        | 's'  -> () (* Needs to pop the top of the stack. *)
        | '\n' -> () (* Done. Run the command *)
        | ' '  -> ()
        | _    -> printf "%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "End_of_file\n%!";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let _ = if not !Sys.interactive then readfiles ()

