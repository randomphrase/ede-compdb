#include "world.hpp"
#include "utility.hpp"
#include <ostream>

template <typename Stream>
inline void HelloWorld(Stream& os)
{
    os << "Hello, world!" << EOL
       << "This is a " << BUILD_TYPE << " build.\n";
}
