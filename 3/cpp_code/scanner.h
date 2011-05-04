// $Id: scanner.h,v 1.1 2011-01-18 22:17:09-08 - - $
//bpross
//esteggal

#ifndef __SCANNER_H__
#define __SCANNER_H__

#include <iostream>
#include <utility>

using namespace std;

#include "trace.h"

enum terminal_symbol {NUMBER, OPERATOR, SCANEOF};
struct token_t {
   terminal_symbol symbol;
   string lexinfo;
};

class scanner {
   private:
      bool seen_eof;
      char lookahead;
      void advance();
   public:
      scanner();
      token_t scan ();
};

ostream &operator<< (ostream &, const terminal_symbol &);
ostream &operator<< (ostream &, const token_t &);

#endif

