#include <iostream>

#include "decimal.hh"
#include "mpdecimal.h"


using std::cout;
using std::cerr;
using std::endl;


int main(int argc, char** argv) {
  if (argc != 2) {
    cerr << "Missing argument." << endl;
    return 1;
  }

  cout << endl;
  cout << "input: '" << argv[1] << "'" << endl;
  cout << endl;
  decimal::Decimal a(argv[1]);

  cout << ".sign()      " << a.sign() << endl;
  cout << ".coeff()     " << a.coeff() << endl;
  cout << ".exponent()  " << a.exponent() << endl;
  cout << ".repr()      " << a.repr() << endl;
  cout << ".to_eng()    " << a.to_eng() << endl;
  cout << ".adjexp()    " << a.adjexp() << endl;
  cout << endl;

  const mpd_t* value = a.getconst();
  cout << ".getconst()->flags   " << value->flags << endl;
  cout << ".getconst()->exp     " << value->exp << endl;
  cout << ".getconst()->digits  " << value->digits << endl;
  cout << ".getconst()->len     " << value->len << endl;
  cout << ".getconst()->alloc   " << value->alloc << endl;
  cout << endl;

  const mpd_uint128_triple_t triple = a.as_uint128_triple();
  cout << ".as_uint128_triple().tag   " << triple.tag << endl;
  cout << ".as_uint128_triple().sign  " << int(triple.sign) << endl;
  cout << ".as_uint128_triple().hi    " << triple.hi << endl;
  cout << ".as_uint128_triple().lo    " << triple.lo << endl;
  cout << ".as_uint128_triple().exp   " << triple.exp << endl;
  cout << endl;

  for (int ii = 0; ii < value->alloc; ++ii) {
    cout << ".getconst()->value->data[" << ii << "]  " << value->data[ii] << endl;
  }
  cout << endl;

  cout << "sizeof(mpd_ssize_t)  " << sizeof(mpd_ssize_t) << endl;
  cout << "sizeof(mpd_uint_t)   " << sizeof(mpd_uint_t) << endl;
}
