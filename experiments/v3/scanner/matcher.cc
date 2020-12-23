#include "reflex/matcher.h"

int main( int argc, char** argv )
{
  std::vector<std::string> values = {"2000-09-10", "2000-09-10 12:13"};
  for (auto value : values) {
    std::cout << std::endl;
    std::cout << "Value = '" << value << "'" << std::endl;
    reflex::Matcher matcher(
        R"(\d{4}[\-/]\d{2}[\-/]\d{2}(\s+(\d{1,2}:\d{2}(:\d{2})?))?\b)", value);
    for (;;) {
      int x = matcher.find();
      if (x == 0) {
        break;
      }

      std::cout << "Found '" << matcher.text() << "' with " << x << " "
                << matcher.size() << " "
                << matcher[0].second << " "
                << matcher[1].second << " "
                << std::endl;
    }
  }

  return 0;
}
