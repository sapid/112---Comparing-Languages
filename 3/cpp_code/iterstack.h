// $Id: iterstack.h,v 1.1 2011-01-18 22:17:09-08 - - $
//bpross
//esteggal

//
// The class std::stack does not provide an iterator, which is needed
// for this class.  So, like std::stack, class iterstack is implemented
// on top of a std::deque.  We don't use a deque directly because we
// want to restrict operations.
//
// All functions are merely forwarded to the deque as inline functions
// for efficiency.  For detailed documentation of the functions see
// std::deque.
//
// No implementation file is needed because all functions are inline.
// Inline functions are only a good idea for trivial forwarding
// functions.
//

#ifndef __ITERSTACK_H__
#define __ITERSTACK_H__

#include <deque>

using namespace std;

template <typename item_t>
class iterstack {
   private:
      deque<item_t> data;
   public:
      typedef typename deque<item_t>::const_reference const_reference;
      typedef typename deque<item_t>::const_iterator const_iterator;
      void push_front (const item_t &item) { data.push_front (item); }
      void pop_front ()                    { data.pop_front (); }
      void clear ()                        { data.clear (); }
      const_reference front () const       { return data.front (); }
      size_t size () const                 { return data.size (); }
      bool empty () const                  { return data.empty (); }
      const_iterator begin () const        { return data.begin (); }
      const_iterator end () const          { return data.end (); }
};

#endif

