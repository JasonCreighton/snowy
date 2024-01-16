# Earlier versions may work, this is the oldest I have tried.
cmake_minimum_required(VERSION 3.13)

project(snowy)

# Use C++14
set(CMAKE_CXX_STANDARD 14)

# Enable LTO
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)

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