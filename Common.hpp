// Meant to be used as a precompiled header

// Use #ifndef guards instead of my usual #pragma once because of how this file
// ends up being copied into the build directory for precompiled header purposes,
// so there are two Common.hpp files running around, and hence #pragma once
// doesn't quite do what we want.
#ifndef _SNOWY_COMMON_HPP
#define _SNOWY_COMMON_HPP

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <mutex>
#include <sstream>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <thread>
#include <condition_variable>
#include <limits>
#include <list>
#include <iomanip>
#include <random>

#include <cstdint>
#include <cctype> // tolower()
#include <cstring> // memset()
#include <cassert> // assert()
#include <cmath>
#include <cstring>
#include <cstddef>

// This is changed to a version number (eg, X.Y) for release commits
#define SNOWY_VERSION "dev_build"

#endif
