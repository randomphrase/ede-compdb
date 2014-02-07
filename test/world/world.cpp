#include "world.hpp"
#include <ostream>

void HelloWorld(std::ostream& os)
{
    os << "Hello, world!\n"
       << "This is a " << BUILD_TYPE << " build.\n";
}
