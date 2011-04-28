
// $Id: main.cc,v 1.4 2011-01-31 18:27:17-08 - - $

//bpross
//esteggal


#include <deque>
#include <exception>
#include <map>
#include <iostream>
#include <utility>

using namespace std;

#include "bigint.h"
#include "iterstack.h"
#include "util.h"
#include "scanner.h"
#include "trace.h"

typedef iterstack<bigint> bigint_stack;

#define DO_BINOP(FN_NAME,TFLAG,OPER) \
   void FN_NAME (bigint_stack &stack) { \
      bigint right = stack.front(); \
      stack.pop_front(); \
      TRACE (TFLAG, "right = " << right); \
      bigint left = stack.front(); \
      stack.pop_front(); \
      TRACE (TFLAG, "left = " << left); \
      bigint result = left OPER (right); \
      TRACE (TFLAG, "result = " << result); \
      stack.push_front (result); \
   }
DO_BINOP(do_add, '+', +   )
DO_BINOP(do_sub, '-', -   )
DO_BINOP(do_mul, '*', *   )
DO_BINOP(do_div, '/', /   )
DO_BINOP(do_rem, '%', %   )
DO_BINOP(do_pow, '^', .pow)

void do_clear (bigint_stack &stack) {
  TRACE ('c', "");
  stack.clear();
}

void do_dup (bigint_stack &stack) {
  bigint top = stack.front();
  TRACE ('d', top);
  stack.push_front (top);
}

void do_printall (bigint_stack &stack) {
  bigint_stack::const_iterator itor = stack.begin();
  bigint_stack::const_iterator end = stack.end();
  for (; itor != end; ++itor) cout << *itor << endl;
}

void do_print (bigint_stack &stack) {
  cout << stack.front() << endl;
}

void do_debug (bigint_stack &stack) {
   (void) stack; // SUPPRESS: warning: unused parameter 'stack'
   cout << "Y not implemented" << endl;
}

class ydc_quit: public exception {};
void do_quit (bigint_stack &stack) {
   (void) stack; // SUPPRESS: warning: unused parameter 'stack'
   throw ydc_quit ();
}

typedef void (*function) (bigint_stack&);
typedef map <string, function> fnmap;
fnmap load_fn () {
   fnmap functions;
   functions["+"] = do_add;
   functions["-"] = do_sub;
   functions["*"] = do_mul;
   functions["/"] = do_div;
   functions["%"] = do_rem;
   functions["^"] = do_pow;
   functions["Y"] = do_debug;
   functions["c"] = do_clear;
   functions["d"] = do_dup;
   functions["f"] = do_printall;
   functions["p"] = do_print;
   functions["q"] = do_quit;;
   return functions;
}

//
// scan_options
//    Options analysis:  The only option is -Dflags. 
//

void scan_options (int argc, char **argv) {
   opterr = 0;
   for (;;) {
      int option = getopt (argc, argv, "@:");
      if (option == EOF) break;
      switch (option) {
         case '@':
            traceflags::setflags (optarg);
            break;
         default:
            complain() << "-" << (char) optopt << ": invalid option"
                       << endl;
            break;
      }
   }
   if (optind < argc) {
      complain() << "operand not permitted" << endl;
   }
}

int main (int argc, char **argv) {
   sys_info::set_execname (argv[0]);
   scan_options (argc, argv);
   fnmap functions = load_fn();
   bigint_stack operand_stack;
   scanner input;
   try {
      for (;;) {
         try {
            token_t token = input.scan();
            if (token.symbol == SCANEOF) break;
            switch (token.symbol) {
               case NUMBER:
               {
                  operand_stack.push_front (token.lexinfo);
                  break;
               }
               case OPERATOR: {
                  function fn = functions[token.lexinfo];
                  if (fn == NULL) {
                     throw ydc_exn (octal (token.lexinfo[0])
                                    + " is unimplemented");
                  }
                  fn (operand_stack);
                  break;
                  }
               default:
                  break;
            }
         }catch (ydc_exn exn) {
            cout << exn.what() << endl;
         }
      }
   }catch (ydc_quit) {
   }
   return sys_info::get_status ();
}

