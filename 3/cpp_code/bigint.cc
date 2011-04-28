// $Id: bigint.cc,v 1.17 2011-01-31 18:27:18-08 - - $
//bpross
//esteggal

#include <cstdlib>
#include <exception>
#include <limits>
#include <stack>
#include <stdexcept>

using namespace std;

#include "bigint.h"
#include "trace.h"

bigint::bigint (): negative(false),  
                   big_value (new bigvalue_t (1, 0) ) {
}

bigint::bigint (const bigint &that): big_value 
               (new bigvalue_t (*that.big_value) ) {
   *this = that;
}

bigint &bigint::operator= (const bigint &that) {
   if (this == &that) return *this;
   this->big_value = that.big_value;
   this->negative = that.negative;
   return *this;
}

bigint::~bigint() {
   TRACE ('~', cout << *this);
}

bigint::bigint (int that): negative(false), 
                           big_value (new bigvalue_t(1, 0) ) {
  digit_t temp;
  while(that > 10){ 
    temp = that % 10;
    this->big_value->push_back(temp);
    that /= 10;
  }
}
 

bigint::bigint (const string &that): negative(false), 
                big_value (new bigvalue_t() ) {
   TRACE ('b', that);
   string::const_iterator itor = that.begin();
   string::const_iterator end = that.end();
   if (*itor == '_'){
     this->negative = true;
     ++itor;
   }else{
     this->negative = false;
   }
   digit_t temp;
   for (; end >= itor; end--){
     if(*end == '\0') continue;
     temp = *end - '0';
     this->big_value->push_back(temp);
     } 
/*test print statement

   int size;
   int it;
   digit_t test;
   size = this->big_value->size();
   if(this->negative == true) cout << '-';
   for(it = 0; it < size; it++){
     test = this->big_value->at(it);
     test += '0';
     cout << test;
   }
   cout << endl; 
*/

}

bigint bigint::operator+ (const bigint &that) const {
  int comp = 0;
  int abs = 0;
  comp = this->compare(that);
  if(comp == 0){
    this->do_bigadd(that);
    return *this;
  }else if(comp == -1){
    abs = this->abscompare(that);

    if(abs == 0){
      that.do_bigsub(*this);
      return that;
    }else if(abs == -1){
      that.do_bigsub(*this);
      return that;
    }else if(abs == 1){
      this->do_bigsub(that);
      return *this;
    }else{
      cout << "something is broken in +" << endl;
      return *this;
    }

  }else if(comp == 1){
    abs = this->abscompare(that);

    if(abs == 0){
      this->do_bigsub(that);
      return *this;
    }else if(abs == -1){
      this->do_bigsub(that);
      return *this;
    }else if(abs == 1){
      that.do_bigsub(*this);
      return that;
    }else{
      cout << "something is broken in +" << endl;
      return *this;
    }
    
  }else if(comp == 2){
    cout << "something is broken in +" << endl;
    return *this;
  }
  cout << "Shouldn't be here in + " << endl;
  return *this;
  
//   return this->big_value + that.big_value;
}


bigint bigint::do_bigadd(const bigint &that) const{
   int smaller = 0;
   int abs = 0;
   int itor = 0;
   digit_t itordigit = 0;
   digit_t thisdigit = 0;
   digit_t thatdigit = 0;
   int carry = 0;
   abs = this->abscompare(that);
   if(abs == 0 || abs == 1){
     smaller = that.big_value->size() -1;
   }else if(abs == -1){
     smaller = this->big_value->size() -1;
   }else{
     cout << "something's broken in bigadd" << endl;
   }
   for(itor = 0; itor <= smaller; itor++){
      thisdigit = this->big_value->at(itor);
      thatdigit = that.big_value->at(itor);
      itordigit = thisdigit + thatdigit;
      if(carry == 1) itordigit++;
      if(itordigit >= 10){
        carry = 1;
        itordigit -= 10;
      }else{
        carry = 0;
      }
//      itordigit += 48;
      this->big_value->at(itor) = itordigit;
   }
//   cout << "past for loop" << endl;
   while(carry > 0){
     if(abs == 1){
       this->big_value->at(itor) += 1;
     }else{
       that.big_value->at(itor) +=1;
     }
     itor++;
     carry--;
   }
//   cout << "past while loop" << endl;
   if(abs == 0 || abs == 1){
     return *this;
   }else if(abs == -1){
     return that;
   }else{
     cout << "something's broken in bigadd" << endl;
     return that;
   }
   
}


bigint bigint::do_bigsub(const bigint &that) const{
  int greater = 0;
  int smaller = 0;
  int abs = 0;
  int itor = 0;
  int updigit = 0;
  digit_t itordigit = 0;
  digit_t thisdigit = 0;
  digit_t thatdigit = 0;
  abs = this->abscompare(that);
  // This if statment splits the sub into two sections for 
  //lack of a better algo
  // I only commented one because they are the same thing 
  //with this and that flipfloped
  if(abs == 0 || abs == 1){
    greater = this->big_value->size() -1;
    smaller = that.big_value->size() -1;
    for(itor = greater; itor > 0; itor--){
      //This is a check to make sure there is something to 
      //be subtracted from (in "that").
      if(itor > smaller) break;
      // Use thisdigit and thatdigit to avoid using to many function 
      //calls
      thisdigit = this->big_value->at(itor);
      thatdigit = that.big_value->at(itor);
      if(thisdigit >= thatdigit){
      //This is the easy part of the if statment
        itordigit = thisdigit - thatdigit;
      }else{
        updigit = itor;
        // find the next most significant digit that isn't = 0
        while(this->big_value->at(updigit) <= 0){updigit++;}
        // Subtract 1 from it once it's found
        this->big_value->at(updigit)--;
        // Add 10
        thisdigit += 10;
        // use itordigit to hold the value that should go into 
        //this(itor)
        itordigit = thisdigit - thatdigit;
        // replace the existing value at this(itor)
        this->big_value->at(itor) = itordigit;
      }
      return *this;
    }
  }else if(abs == -1){
    greater = that.big_value->size() -1;
    smaller = this->big_value->size() -1;
    for(itor = greater; itor > 0; itor--){
      if(itor > smaller) break;
      thatdigit = that.big_value->at(itor);
      thisdigit = this->big_value->at(itor);
      if(thatdigit >= thisdigit){
        itordigit = thatdigit - thisdigit;
      }else{
        updigit = itor;
        while(that.big_value->at(updigit) <= 0){updigit++;}
        that.big_value->at(updigit)--;
        thatdigit += 10;
        itordigit = thatdigit - thisdigit;
        that.big_value->at(itor) = itordigit;
        return that;
      }
    }
  }else{
    cout << "something broken in bigsub from abs" << endl;
  }
  cout << "do_bigsub(): Shouldn't be here" << endl;
  return *this;

}

bigint bigint::operator- (const bigint &that) const {
  int comp = 0;
  int abs = 0;
  comp = this->compare(that);
  if(comp == 0){
    this->do_bigsub(that);
    return *this;
  }
  else if(comp == -1){
    abs = this->abscompare(that);

    if(abs == 0){
      that.do_bigadd(*this);
      return that;
    }
    else if(abs == -1){
      that.do_bigadd(*this);
      return that;
    }
    else if(abs == 1){
      this->do_bigadd(that);
      return *this;
    }
    else{
      cout << "something is broken in -" << endl;
      return *this;
    }
  }
  else if(comp == 1){
    abs = this->abscompare(that);

    if(abs == 0){
      this->do_bigadd(that);
      return *this;
    }
    else if(abs == -1){
      this->do_bigadd(that);
      return *this;
    }
    else if(abs == 1){
      that.do_bigadd(*this);
      return that;
    }
    else{
      cout << "something is broken in -" << endl;
      return *this;
    
    }
  }
  else if(comp == 2){
    cout << "something is broken in -" << endl;
    return *this;
  }
  cout << "Shouldn't be here in -" << endl;
  return *this;
}

int bigint::compare (const bigint &that) const {
  if(this->negative == true && that.negative == false){ 
    cout << "this neg that pos " << endl;
    return -1;
  }
  if(this->negative == false && that.negative == true){
    cout << "this post that neg" << endl;
    return 1;
  }
  if(this->negative == true && that.negative == true){
    cout << "this neg that neg " << endl;
    return 0;
  }
  if(this->negative == false && that.negative == false){
    cout << "this pos that pos " << endl;
    return 0;
  }
  
  return 2;

//   return this->small_value < that.small_value ? -1
//        : this->small_value > that.small_value ? +1 : 0;
}

int bigint::abscompare (const bigint &that) const {
  if(this->big_value->size() < that.big_value->size() ){
    return -1;
  }
  if(this->big_value->size() > that.big_value->size() ){
    return 1;
  }
  if(this->big_value->size() == that.big_value->size() ){
    return 0;
  }
  return 2;
//   return abs (this->small_value) < abs (that.small_value) ? -1
//        : abs (this->small_value) > abs (that.small_value) ? +1 : 0;
}

int bigint::smallint () const {
   if (*this < numeric_limits<int>::min()
    || *this > numeric_limits<int>::max())
               throw range_error ("smallint: out of range");
   return small_value;
}

bigint bigint::mul_by_2 () {
   return this->small_value *= 2;
}

static bigpair popstack (stack <bigpair> &egyptstack) {
   bigpair result = egyptstack.top ();
   egyptstack.pop();
   return result;
}

//
// Ancient Egyptian multiplication algorithm.
//
bigint bigint::operator* (const bigint &that) const {
   int binsize = 0;
   int thatsize = 0;
   int thissize = 0;
   int itor = 0;
   bigint top = that;
   bigint count = 1;
   TRACE ('*', *this << " * " << that);
   stack <bigpair> egyptstack;
   popstack (egyptstack); // junk to suppress a warning
   bigint result = 0;
   //if ((*this < 0) != (that < 0)) result = - result;
   // Not sure if this works/compiles: pretty much sudo code, 
   //doesn't seem like enough to work but it makes sense to me.
  bigint product;
  bigint bin;
  bin.big_value->push_back(1);
  //push bin up until it is close to the range of that.big_value
  while(that.abscompare(bin) != -1){
    binsize = bin.big_value->size() -1;
    for(itor = 0; itor > binsize; itor++){
      bin.big_value->at(itor) *= 2;
    }
  }
  // This loop runs as long as that isn't = 1
  thatsize = that.big_value->size() -1;
  while(thatsize > 0){                     
    // go into if statement if that is bigger than bin
    if(that.abscompare(bin) == 1){
      that.do_bigsub(bin);
      //Add this.big_value to product
      product.do_bigadd(*this);
    }
    //Multiply this by 2, divide bin by 2
    thissize = this->big_value->size() -1;
    for(itor = 0; itor > thissize; itor++){
      this->big_value->at(itor) *= 2;
    }
    binsize = bin.big_value->size() -1;
    for(itor = 0; itor > thissize; itor++){
      bin.big_value->at(itor) /= 2;
    }
    thatsize = that.big_value->size() -1;
  }        
   return result;
}

//
// Ancient Egyptian division algorithm.
//
bigpair bigint::div_rem (const bigint &that) const {
   if (that == 0) throw range_error ("divide by 0");
   int top = this->abscompare(that);
   bigpair return_pair;
   TRACE ('/', *this << " /% " << that);
   stack <bigpair> egyptstack;
   bigint quotient = 0;
   bigint remainder = 0;
   int comp = 0;
   if (top == -1){
     quotient = *this;
     while(comp != -1){
       quotient.do_bigadd(quotient);
       remainder = that - quotient;
       return_pair.first = quotient;
       return_pair.second = remainder;
       egyptstack.push(return_pair);
       comp = that.abscompare(quotient);
     }
   }
   else if(top == -1){
     quotient = that;
     while (comp != -1){
       quotient.do_bigadd(quotient);
       remainder = *this - quotient;
       return_pair.first = quotient;
       return_pair.second = remainder;
       egyptstack.push(return_pair);
       comp = this->abscompare(quotient);
     }
   }
   egyptstack.pop();
   return_pair = egyptstack.top();
   return return_pair;
}

bigint bigint::operator/ (const bigint &that) const {
  bigpair div_rem = this->div_rem(that); 
  return div_rem.first;
}

bigint bigint::operator% (const bigint &that) const {
  bigpair div_rem = this->div_rem(that); 
  return div_rem.second;
}

#define TRACE_POW \
   TRACE ('^', "result: " << result << ", base: " << base \
            << ", expt: " << expt);
bigint bigint::pow (const bigint &that) const {
   bigint base = *this;
   if (that > 999) throw range_error ("exp too big");
   int expt = that.smallint();
   bigint result = 1;
   TRACE_POW;
   if (expt < 0) {
      base = 1 / base;
      expt = - expt;
   }
   while (expt > 0) {
      TRACE_POW;
      if (expt & 1) { //odd
         result = result * base;
         --expt;
      }else { //even
         base = base * base;
         expt /= 2;
      }
   }
   TRACE_POW;
   return result;
}

//
// Macros can make repetitive code easier.
//

#define COMPARE(OPER) \
   bool bigint::operator OPER (const bigint &that) const { \
      return compare (that) OPER 0; \
   }
COMPARE (==)
COMPARE (!=)
COMPARE (< )
COMPARE (<=)
COMPARE (> )
COMPARE (>=)

#define INT_LEFT(RESULT,OPER) \
   RESULT operator OPER (int left, const bigint &that) { \
      return bigint (left) OPER that; \
   }
INT_LEFT (bigint, +)
INT_LEFT (bigint, -)
INT_LEFT (bigint, *)
INT_LEFT (bigint, /)
INT_LEFT (bigint, %)
INT_LEFT (bool, ==)
INT_LEFT (bool, !=)
INT_LEFT (bool, < )
INT_LEFT (bool, <=)
INT_LEFT (bool, > )
INT_LEFT (bool, >=)

ostream &operator<< (ostream &out, const bigint &that) {
  int itor;
  int size = 0;
  bigint::digit_t temp;
  size = that.big_value->size();
  cout << that.negative << endl;
  if(that.negative == true) cout << '-';
  for(itor = size - 1; itor >= 0; --itor){
    temp = that.big_value->at(itor);
    temp += '0';
    cout << temp;
  }
   return out;
}

