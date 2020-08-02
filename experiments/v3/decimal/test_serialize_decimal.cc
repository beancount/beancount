// Tests on exact serialization of Decimal.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <iostream>

#include "gtest/gtest.h"
#include "decimal.hh"
#include "mpdecimal.h"

#include "experiments/v3/decimal/number.pb.h"

namespace {
using std::cout;
using std::endl;

TEST(SerializationTest, PrintComponents) {
  decimal::Decimal a("42e8");

  const mpd_t* value = a.getconst();
  cout << "sizeof(mpd_ssize_t) " << sizeof(mpd_ssize_t) << endl;
  cout << "sizeof(mpd_uint_t) " << sizeof(mpd_uint_t) << endl;

  cout << endl;

  cout << "flags " << value->flags << endl;
  cout << "exp " << value->exp << endl;
  cout << "digits " << value->digits << endl;
  cout << "len " << value->len << endl;
  cout << "alloc " << value->alloc << endl;
  for (int ii = 0; ii < value->alloc; ++ii) {
    cout << "data[" << ii << "] " << value->data[ii] << endl;
  }
}

TEST(SerializationTest, RoundTripText) {
  decimal::Decimal a("42e8");

  // Serialize.
  beancount::Number number;
  number.set_exact(a.to_sci(false));
  cout << "'" << number.exact() << "'" << endl;

  // Deserialize.
  decimal::Decimal b(number.exact());

  EXPECT_EQ(b, a);
}

TEST(SerializationTest, RoundTripMpd) {
  decimal::Decimal a("42e8");
  cout << "A = " << a << endl;

  // Serialize.
  beancount::Number number;
  {
    auto* mpd = number.mutable_mpd();
    const mpd_t* value = a.getconst();
    mpd->set_flags(value->flags & ~MPD_DATAFLAGS);
    mpd->set_exp(value->exp);
    mpd->set_digits(value->digits);
    mpd->set_len(value->len);
    for (int ii = 0; ii < value->alloc; ++ii) {
      mpd->add_data(value->data[ii]);
    }
  }

  // Deserialize.
  decimal::Decimal b;
  {
    mpd_t* value = b.get();
    const auto& mpd = number.mpd();
    value->flags = mpd.flags() & ~MPD_DATAFLAGS;
    value->exp = mpd.exp();
    value->digits = mpd.digits();
    value->len = mpd.len();
    value->alloc = mpd.data().size();
    assert(value->data == nullptr);
    // We have no access
    value->data = static_cast<mpd_uint_t*>(
      mpd_alloc(value->alloc, sizeof(*value->data)));
    for (int ii = 0; ii < value->alloc; ++ii) {
      value->data[ii] = mpd.data(ii);
    }
  }
  cout << "B = " << b << endl;

  EXPECT_EQ(b, a);
}

}  // namespace
