// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "Common.hpp"
#include "PerftTest.hpp"
#include "Board.hpp"
#include "Search.hpp"

bool RunPerftTestSuite(const char *testSuiteFilename) {
    Board board;
    Search search(board);
    std::ifstream testSuite;

    testSuite.open(testSuiteFilename);

    if (testSuite.fail()) {
        // File does not exist, or some other error
        std::cout << "Failed opening file " << testSuiteFilename << std::endl;
        return false;
    }

    std::cout << "Running Perft tests";


    auto startTime = std::chrono::high_resolution_clock::now();
    int64_t totalPerftCount = 0;
    std::string line;
    while(std::getline(testSuite, line)) {
        std::string fenString;
        std::vector<long> perftCounts;

        {
            std::stringstream ss(line);

            // Slurp in FEN string
            std::getline(ss, fenString, ';');
            while(!ss.eof()) {
                std::string garbage;
                long perftCount;

                // eat "D1", "D2", etc.
                ss >> garbage;

                // Read perft count
                if(ss >> perftCount) {
                    perftCounts.push_back(perftCount);
                }
            }
        }

        // Okay, done with ugly parsing

        // Set up board
        board.ParseFen(fenString);

        // Iterate through depth specificiations
        for(std::size_t i = 0; i < perftCounts.size(); ++i) {
            std::cout << '.' << std::flush;

            int depth = static_cast<int>(i) + 1;
            int64_t calculatedPerftCount = search.Perft(depth);
            totalPerftCount += calculatedPerftCount;

            if(calculatedPerftCount != perftCounts[i]) {
                // Failure!
                std::cout << "FAILED!" << std::endl;
                std::cout << "FEN: " << fenString << std::endl;
                std::cout << "Depth: " << depth << std::endl;
                std::cout << "Expected: " << perftCounts[i] << std::endl;
                std::cout << "Calculated: " << calculatedPerftCount << std::endl;

                return false;
            }
        }
    }
    auto elapsed = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - startTime);
    auto elapsed_us = elapsed.count();
    int64_t perftPerSecond = ((totalPerftCount * 1000000) / elapsed_us);

    std::cout << "done. All OK." << std::endl;
    std::cout << "Total perft count: " << totalPerftCount << " perft/sec: " << perftPerSecond << std::endl;

    return true;
}
