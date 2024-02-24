# Earlier versions may work, this is the oldest I have tried.
cmake_minimum_required(VERSION 3.13)

project(snowy)

# Use C++14
set(CMAKE_CXX_STANDARD 14)

# Enable LTO
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)

# Add RelWithAssertions configuration type
list(APPEND CMAKE_CONFIGURATION_TYPES RelWithAssertions)

# The intent is to make RelWithAssertions similar to RelWithDebInfo.
# Hopefully I've extended all the right magic variables, I got the list from here:
# https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#how-can-i-extend-the-build-modes-with-a-custom-made-one-
set(CMAKE_C_FLAGS_RELWITHASSERTIONS "${CMAKE_C_FLAGS_RELWITHDEBINFO} -DREL_WITH_ASSERTIONS=1")
set(CMAKE_CXX_FLAGS_RELWITHASSERTIONS "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -DREL_WITH_ASSERTIONS=1")
set(CMAKE_EXE_LINKER_FLAGS_RELWITHASSERTIONS "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO}")
set(CMAKE_SHARED_LINKER_FLAGS_RELWITHASSERTIONS "${CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO}")

add_executable(snowy
    # Headers
    Common.hpp
    Constants.hpp
    StaticEvaluationParameters.hpp

    # Implementation files w/ headers
    Board.cpp Board.hpp
    IO.cpp IO.hpp
    MovePicker.cpp MovePicker.hpp
    PerftTest.cpp PerftTest.hpp
    Search.cpp Search.hpp
    TranspositionTable.cpp TranspositionTable.hpp
    UCI.cpp UCI.hpp
    Zobrist.cpp Zobrist.hpp

    # Implementation files w/o headers
    Main.cpp
)

find_package(Threads)
target_link_libraries(snowy ${CMAKE_THREAD_LIBS_INIT})

# target_precompile_headers was introduced in cmake 3.16
if(COMMAND target_precompile_headers)
    target_precompile_headers(snowy PRIVATE Common.hpp)
endif()

# Tests
enable_testing()
file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../chess-data/test/perftsuite.epd" PERFTSUITE_EPD)
file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../chess-data/scripts/chess_artist.py" CHESS_ARTIST_PY)
file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/data/regression.epd" REGRESSION_EPD)

add_test(NAME UnitTests COMMAND snowy -t)
add_test(NAME PerftSuite COMMAND snowy -p "${PERFTSUITE_EPD}")
add_test(NAME RegressionEpd COMMAND python "${CHESS_ARTIST_PY}" --infile "${REGRESSION_EPD}" --outfile - --job test --eval search --movetime 5000 --enginefile $<TARGET_FILE:snowy>)