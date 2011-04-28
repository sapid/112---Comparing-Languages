// $Id: bigint.h,v 1.1 2011-01-18 22:17:09-08 - - $
//bpross
//esteggal

#ifndef __BIGINT_H__
#define __BIGINT_H__

#include <exception>
#include <iostream>
#include <utility>

using namespace std;

#include "trace.h"

class bigint;
typedef pair <bigint, bigint> bigpair;

class bigint {
      friend ostream &operator<< (ostream &, const bigint &that);
   private:
      int small_value;
      typedef unsigned char digit_t;
      typedef vector <digit_t> bigvalue_t;
      bool negative;
      bigvalue_t *big_value;

      bigpair div_rem (const bigint &that) const;
      int compare (const bigint &that) const;
      int abscompare (const bigint &that) const;
      bigint mul_by_2 ();
      bigint do_bigadd(const bigint &that) const;
      bigint do_bigsub(const bigint &that) const;
   public:
      //
      // Override implicit members.
      //
      bigint ();
      bigint (const bigint &that);
      bigint &operator= (const bigint &that);
      ~bigint ();
      //
      // Extra ctors to make bigints.
      //
      bigint (const int that);
      bigint (const string &that);
      //
      // Basic add/sub operators.
      //
      bigint operator+ (const bigint &that) const;
      bigint operator- (const bigint &that) const;
      bigint operator- () const;
      int smallint () const;
      //
      // Extended operators implemented with add/sub.
      //
      bigint operator* (const bigint &that) const;
      bigint operator/ (const bigint &that) const;
      bigint operator% (const bigint &that) const;
      bigint pow (const bigint &that) const;
      //
      // Comparison operators.
      //
      bool operator== (const bigint &that) const;
      bool operator!= (const bigint &that) const;
      bool operator<  (const bigint &that) const;
      bool operator<= (const bigint &that) const;
      bool operator>  (const bigint &that) const;
      bool operator>= (const bigint &that) const;
};

//
// Operators with a left side of int.
//
bigint operator+ (int left, const bigint &that);
bigint operator- (int left, const bigint &that);
bigint operator* (int left, const bigint &that);
bigint operator/ (int left, const bigint &that);
bigint operator% (int left, const bigint &that);
bool operator== (int left, const bigint &that);
bool operator!= (int left, const bigint &that);
bool operator<  (int left, const bigint &that);
bool operator<= (int left, const bigint &that);
bool operator>  (int left, const bigint &that);
bool operator>= (int left, const bigint &that);

#endif

