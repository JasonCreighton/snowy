// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Common.hpp"

template< typename T >
std::string IntegerToHexString( T i ) {
    std::stringstream stream;
    stream << std::setfill ('0') << std::setw(sizeof(T)*2) << std::hex << i;
    return stream.str();
}