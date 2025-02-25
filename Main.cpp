// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "Common.hpp"
#include "UCI.hpp"
#include "PerftTest.hpp"
#include "Zobrist.hpp"
#include "IO.hpp"
#include "Board.hpp"

namespace {
    void Usage(const char *programName) {
        std::cerr << programName << " [-t perftsuite...] [-c uci_command...] [-l log_file...]" << std::endl;
    }

    bool RunTests() {
        #ifdef NDEBUG
            std::cerr << "ERROR! Assertions disabled! Test is a no-op." << std::endl;
            return false;
        #endif

        Board::Test();
        Search::Test();

        std::cout << "Tests complete." << std::endl;
        return true;
    }
}

int main(int argc, char **argv) {
    Zobrist::Init();

    UCI uci;
    bool quiet = false;
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
                success = success && RunTests();
                batchMode = true;
                i += 1;
            } else if (strcmp(argv[i], "-l") == 0) {
                if(i + 1 < argc) {
                    IO::OpenLogFile(argv[i + 1]);
                    i += 2;
                } else {
                    badArgument = true;
                }
            } else if (strcmp(argv[i], "--quiet") == 0) {
                // Skip the welcome message for when static_eval.py runs us
                quiet = true;
                i += 1;
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
        if (!quiet) {
            IO::PutLine("Snowy " SNOWY_VERSION " by Jason Creighton (built on " __DATE__ " " __TIME__ ")");
        }

        uci.Run();
        uci.WaitForSearch();
    }

    return success ? 0 : 1;
}
