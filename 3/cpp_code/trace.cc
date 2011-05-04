// $Id: trace.cc,v 1.2 2011-01-31 18:27:19-08 - - $
//bpross
//esteggal

#include <climits>
#include <vector>

using namespace std;

#include "trace.h"

//
// ** BUG IN STL ** BUG IN STL **
// We should use vector<bool> instead of vector<char>,
// but vector<bool> has a bug:
// http://forums.sun.com/thread.jspa?threadID=5277939
// Static linking works, but doubles the size of the executable
// image.
// ** BUG IN STL ** BUG IN STL **
//

typedef vector<char> boolvec;
boolvec traceflags::flags (UCHAR_MAX + 1, false);
const boolvec trueflags (UCHAR_MAX + 1, true);

void traceflags::setflags (const string &optflags) {
   string::const_iterator itor = optflags.begin();
   string::const_iterator end = optflags.end();
   for (; itor != end; ++itor) {
      if (*itor == '@') {
         flags = trueflags;
      }else {
         flags[*itor] = true;
      }
   }
   // Note that TRACE can trace setflags.
   TRACE ('t',  "optflags = " << optflags);
}

//
// getflag -
//    Check to see if a certain flag is on.
//

bool traceflags::getflag (char flag) {
   // WARNING: Don't TRACE this function or the stack will blow up.
   bool result = flags[flag];
   return result;
}

