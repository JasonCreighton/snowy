// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "PerftTest.hpp"
#include "Board.hpp"
#include "Search.hpp"

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

bool RunPerftTestSuite(const char *testSuiteFilename) {
    Board board;
    Search search(board);
    std::ifstream testSuite;

    std::cout << "Running Perft tests";

    testSuite.open(testSuiteFilename);

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

            int depth = i + 1;
            long calculatedPerftCount = search.Perft(depth);

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

    std::cout << "done. All OK." << std::endl;

    return true;
}
