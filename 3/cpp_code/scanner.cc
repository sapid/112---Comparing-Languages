// $Id: scanner.cc,v 1.4 2011-01-31 18:27:19-08 - - $
//bpross
//esteggal

#include <iostream>
#include <locale>

using namespace std;

#include "scanner.h"
#include "trace.h"

scanner::scanner () {
   seen_eof = false;
   advance();
}

void scanner::advance () {
   if (! seen_eof) {
      cin.get (lookahead);
      if (cin.eof()) seen_eof = true;
   }
}

token_t scanner::scan() {
   token_t result;
   while (!seen_eof && isspace (lookahead)) advance();
   if (seen_eof) {
      result.symbol = SCANEOF;
   }else if (lookahead == '_' || isdigit (lookahead)) {
      result.symbol = NUMBER;
      do {
         result.lexinfo += lookahead;
         advance();
      }while (!seen_eof && isdigit (lookahead));
   }else {
      result.symbol = OPERATOR;
      result.lexinfo += lookahead;
      advance();
   }
   TRACE ('S', result);
   return result;
}

ostream &operator<< (ostream &out, const terminal_symbol &symbol) {
   switch (symbol) {
      #define CASE_SYMBOL(SYMBOL) case SYMBOL: out << #SYMBOL; break;
      CASE_SYMBOL (NUMBER);
      CASE_SYMBOL (OPERATOR);
      CASE_SYMBOL (SCANEOF);
   }
   return out;
}

ostream &operator<< (ostream &out, const token_t &token) {
   out << token.symbol << ": \"" << token.lexinfo << "\"";
   return out;
}

