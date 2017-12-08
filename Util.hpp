#pragma once

#include <sstream>
#include <iomanip>

template< typename T >
std::string IntegerToHexString( T i ) {
    std::stringstream stream;
    stream << std::setfill ('0') << std::setw(sizeof(T)*2) << std::hex << i;
    return stream.str();
}