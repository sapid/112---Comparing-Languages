// $Id: util.h,v 1.1 2011-01-18 22:17:09-08 - - $
//bpross
//esteggal

//
// util -
//    A utility class to provide various services not conveniently
//    included in other modules.
//

#ifndef __UTIL_H__
#define __UTIL_H__

#include <iostream>
#include <vector>

#ifdef __GNUC__
#include <stdexcept>
#endif

using namespace std;

#include "trace.h"

//
// ydc_exn -
//    Indicate a problem where processing should be abandoned and
//    the main function should take control.
//

class ydc_exn: public runtime_error {
   public:
      explicit ydc_exn (const string &what);
};

//
// octal -
//    Convert integer to octal string.
//

const string octal (int decimal);

//
// sys_info -
//    Keep track of execname and exit status.  Must be initialized
//    as the first thing done inside main.  Main should call:
//       sys_info::set_execname (argv[0]);
//    before anything else.
//

class sys_info {
   private:
      static string execname;
      static int exit_status;
   public:
      static void set_execname (const string &argv0);
      static const string &get_execname () {return execname; }
      static void set_status (int status) {exit_status = status; }
      static int get_status () {return exit_status; }
};

//
// complain -
//    Used for starting error messages.  Sets the exit status to
//    EXIT_FAILURE, writes the program name to cerr, and then
//    returns the cerr ostream.  Example:
//       complain() << filename << ": some problem" << endl;
//

ostream &complain();

//
// operator<< (vector) -
//    An overloaded template operator which allows vectors to be
//    printed out as a single operator, each element separated from
//    the next with spaces.  The item_t must have an output operator
//    defined for it.
//

template <typename item_t>
ostream &operator<< (ostream &out, const vector<item_t> &vec);

#endif

