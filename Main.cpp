// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "UCI.hpp"
#include "PerftTest.hpp"
#include "Zobrist.hpp"
#include "IO.hpp"
#include "Board.hpp"

#include <iostream>
#include <cstring>

namespace {
    void Usage(const char *programName) {
        std::cerr << programName << " [-t perftsuite...] [-c uci_command...] [-l log_file...]" << std::endl;
    }

    void RunTests() {
        #ifdef NDEBUG
            std::cerr << "WARNING! Assertions disabled! Test is a no-op." << std::endl;
        #endif

        Board::Test();
        Search::Test();

        std::cout << "Tests complete." << std::endl;
    }
}

int main(int argc, char **argv) {
    Zobrist::Init();

    UCI uci;
    bool batchMode = false;
    bool success = true;

    {
        int i = 1;
        bool badArgument = false;
        
        while(i < argc) {
            if(strcmp(argv[i], "-p") == 0) {
                if(i + 1 < argc) {
                    success = success && RunPerftTestSuite(argv[i + 1]);
                    batchMode = true;
                    i += 2;
                } else {
                    badArgument = true;
                    break;
                }
            } else if(strcmp(argv[i], "-c") == 0) {
                if(i + 1 < argc) {
                    std::string command(argv[i + 1]);
                    success = success && uci.DoCommand(command);

                    // Command might have been "go", we want to wait for complete if that's the case
                    uci.WaitForSearch();

                    batchMode = true;
                    i += 2;
                } else {
                    badArgument = true;
                    break;
                }
            } else if (strcmp(argv[i], "-t") == 0) {
                RunTests();
                batchMode = true;
                i += 1;
            } else if (strcmp(argv[i], "-l") == 0) {
                if(i + 1 < argc) {
                    IO::OpenLogFile(argv[i + 1]);
                    i += 2;
                } else {
                    badArgument = true;
                }
            } else {
                badArgument = true;
                break;
            }
        }

        if(badArgument) {
            Usage(argv[0]);
            return 1;
        }
    }

    if(!batchMode) {
        uci.Run();
        uci.WaitForSearch();
    }

    return success ? 0 : 1;
}
