// $Id: util.cc,v 1.2 2011-01-31 18:27:20-08 - - $
//bpross
//esteggal

#include <cstdlib>
#include <sstream>

using namespace std;

#include "util.h"

ydc_exn::ydc_exn (const string &what): runtime_error (what) {
}

const string octal (int decimal) {
   ostringstream ostring;
   ostring.setf (ios::oct);
   ostring << decimal;
   return ostring.str ();
}

int sys_info::exit_status = EXIT_SUCCESS;
string sys_info::execname; // Must be initialized from main().

void sys_info::set_execname (const string &argv0) {
   execname = argv0;
   cout << boolalpha;
   cerr << boolalpha;
   TRACE ('Y', "execname = " << execname);
}

ostream &complain() {
   sys_info::set_status (EXIT_FAILURE);
   cerr << sys_info::get_execname () << ": ";
   return cerr;
}

template <typename item_t>
ostream &operator<< (ostream &out, const vector<item_t> &vec) {
   typename vector<item_t>::const_iterator itor = vec.begin();
   typename vector<item_t>::const_iterator end = vec.end();

   // If the vector is empty, do nothing.
   if (itor != end) {
      // Print out the first element without a space.
      out << *itor++;
      // Print out the rest of the elements each preceded by a space.
      while (itor != end) out << " " << *itor++;
   }
   return out;
}

