#ifndef WORLD_HPP
#define WORLD_HPP

#include <iosfwd>

#define HELLO_BAZ HelloBaz

extern const char * BUILD_TYPE;

template <typename Stream>
void HelloWorld(Stream&);

#endif
