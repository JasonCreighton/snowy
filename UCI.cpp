// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "UCI.hpp"
#include "Board.hpp"
#include "Search.hpp"
#include "IO.hpp"
#include "Constants.hpp"
#include "Util.hpp"

#include <iostream>
#include <string>
#include <sstream>
#include <list>
#include <cassert>

// In case someone wants to use a different build environment
#ifndef GIT_VERSION
#define GIT_VERSION "(version unknown)"
#endif

namespace {
    void ChooseMoveTime(int timeLeft_ms, int increment_ms, int movesUntilNextTimeControl, int& out_softMoveTime_ms, int& out_hardMoveTime_ms) {
        const int TIME_MANAGEMENT_MARGIN_MS = 100;

        // No move time specified, so no time limit
        if(timeLeft_ms == -1) {
            out_softMoveTime_ms = -1;
            out_hardMoveTime_ms = -1;
            return;
        }

        int totalTimeAvailableUntilNextTimeControl_ms = timeLeft_ms + (increment_ms * movesUntilNextTimeControl);
        int idealMoveTime_ms = totalTimeAvailableUntilNextTimeControl_ms / movesUntilNextTimeControl;
        int avoidFlagTime_ms = std::max(timeLeft_ms - TIME_MANAGEMENT_MARGIN_MS, 0);

        int upperLimit_ms = std::min(idealMoveTime_ms * 2, avoidFlagTime_ms);
        int lowerLimit_ms = upperLimit_ms / 4;

        out_softMoveTime_ms = lowerLimit_ms;
        out_hardMoveTime_ms = upperLimit_ms;

        IO::PutInfo("Thinking for " + std::to_string(lowerLimit_ms) + " - " + std::to_string(upperLimit_ms) + " ms");
    }
}

UCI::UCI() : m_Search(m_Board) {
    SetHashTableSize(DEFAULT_HASH_TABLE_SIZE_MB);
}

void UCI::Run() {
    std::string line;

    IO::PutLine("Snowy " GIT_VERSION " by Jason Creighton (built on " __DATE__ " " __TIME__ ")");

    while(IO::GetLine(line)) {
        if(!DoCommand(line)) {
            return;
        }
    }
}

bool UCI::DoCommand(const std::string& commandLine) {
    std::stringstream lineStream(commandLine);
    std::string command;

    lineStream >> command;

    if(command == "uci") {
        IO::PutLine("id name Snowy " GIT_VERSION);
        IO::PutLine("id author Jason Creighton");
        IO::PutLine("option name Hash type spin default " + std::to_string(DEFAULT_HASH_TABLE_SIZE_MB) +
                    " min " + std::to_string(MIN_HASH_TABLE_SIZE_MB) +
                    " max " + std::to_string(MAX_HASH_TABLE_SIZE_MB));
        IO::PutLine("uciok");
    } else if(command == "setoption") {
        std::string constantName;

        lineStream >> constantName;

        if(constantName == "name") {
            std::string name;
            std::string constantValue;
            std::string value;

            lineStream >> name;
            lineStream >> constantValue;
            lineStream >> value;

            if(constantValue != "value") {
                IO::PutInfo(R"(Error parsing setoption: Expected "value")");
            }
            
            SetOption(name, value);
        } else {
            IO::PutInfo(R"(Error parsing setoption: Expected "name")");
        }
    } else if(command == "position") {
        m_Search.WaitForSearch();

        std::string fenString;
        std::string token;
        std::list<std::string> tokens;

        while(lineStream >> token) {
            tokens.push_back(token);
        }

        if(tokens.front() == "startpos") {
            fenString = Board::FEN_START_POSITION;
            tokens.pop_front();
        } else if (tokens.front() == "fen") {
            tokens.pop_front();
            // FEN string should consist of up to 6 tokens (last two seemingly optional)            
            for(int i = 0; i < 6; ++i) {
                if(!tokens.empty() && tokens.front() != "moves") {
                    fenString += tokens.front() + " ";
                    tokens.pop_front();             
                }
            }
        }

        // Set up board
        m_Board.ParseFen(fenString);
        
        if(!tokens.empty() && tokens.front() == "moves") {
            tokens.pop_front();
            while(!tokens.empty()) {
                Board::Move m = Board::ParseMove(tokens.front());
                tokens.pop_front();

                if(!m_Board.Make(m)) {
                    IO::PutInfo("WARNING: Unable to make move " + m.ToString());
                }
            }
        }

        IO::PutInfo("Position hash: " + IntegerToHexString(m_Board.Hash()));
    } else if (command == "isready") {
        IO::PutLine("readyok");
    } else if (command == "go") {
        std::string optionName;
        Search::Parameters params;

        params.Depth = -1;
        params.BruteForce = false;
        params.SoftMoveTime_ms = -1;
        params.HardMoveTime_ms = -1;
        params.ShowHistograms = false;
        int wtime_ms = -1;
        int winc_ms = 0;
        int btime_ms = -1;
        int binc_ms = 0;
        int movesUntilNextTimeControl = 40;
        while(lineStream >> optionName) {
                 if(optionName == "depth") { lineStream >> params.Depth; }
            else if(optionName == "bruteforce") { lineStream >> params.BruteForce; }
            else if(optionName == "movetime") { lineStream >> params.HardMoveTime_ms; }
            else if(optionName == "wtime") { lineStream >> wtime_ms; }
            else if(optionName == "winc") { lineStream >> winc_ms; }
            else if(optionName == "btime") { lineStream >> btime_ms; }
            else if(optionName == "binc") { lineStream >> binc_ms; }
            else if(optionName == "movestogo") { lineStream >> movesUntilNextTimeControl; }
            else if(optionName == "showhistograms") { lineStream >> params.ShowHistograms; }
        }

        if(params.HardMoveTime_ms == -1) {
            // No move time, try to calculate one
            if(m_Board.WhiteToMove()) {
                ChooseMoveTime(wtime_ms, winc_ms, movesUntilNextTimeControl, params.SoftMoveTime_ms, params.HardMoveTime_ms);
            } else {
                ChooseMoveTime(btime_ms, binc_ms, movesUntilNextTimeControl, params.SoftMoveTime_ms, params.HardMoveTime_ms);
            }
            // NB: At this point moveTime_ms might still be -1, depending on the options given
        }

        if(params.Depth == -1) {
            // No depth was specified, unlimited search depth
            params.Depth = MAX_PLY;

            // However, if no move time was given, perhaps a reasonable default
            // so we don't search forever
            if(params.HardMoveTime_ms == -1) {
                params.HardMoveTime_ms = 5000;
            }
        }
        
        m_Search.StartSearch(params);
    } else if (command == "stop") {
        m_Search.StopSearch();
    } else if(command == "perft") {
        m_Search.WaitForSearch();

        int depth;
        lineStream >> depth;

        std::vector<Board::Move> moveList;

        m_Board.FindPseudoLegalMoves(moveList);

        for(auto &m : moveList) {
            if(m_Board.Make(m)) {
                long perftCount = m_Search.Perft(depth - 1);
                if(perftCount != 0) {
                    IO::PutLine(m.ToString() + ": " + std::to_string(perftCount));
                }
                m_Board.Unmake();
            }
        }
    } else if(command == "eval") {
        m_Search.WaitForSearch();

        int score = m_Search.Quiesce();
        int staticEval = m_Board.StaticEvaluation();
        IO::PutInfo("Static evaluation: " + std::to_string(staticEval));
        IO::PutInfo("Quiesce score: " + std::to_string(score));
    } else if(command == "d") {
        m_Search.WaitForSearch();

        m_Board.Print();
    } else if(command == "num_features") {
        IO::PutLine(std::to_string(Board::NUM_FEATURES));
    } else if(command == "features") {
        m_Search.WaitForSearch();
        std::string featureString;

        for(int f : m_Board.EvaluationFeatures()) {
            featureString += std::to_string(f) + " ";
        }

        IO::PutLine(featureString);
    } else if (command == "quit") {
        return false;
    }

    return true;
}

void UCI::WaitForSearch() {
    // Man, I hate dumb little wrappers like this. There must be a better way.
    m_Search.WaitForSearch();
}

void UCI::SetOption(const std::string& name, const std::string& value) {
    if(name == "Hash") {
        int megabytes;        
        std::stringstream valueStream(value);
        valueStream >> megabytes;

        if(megabytes > MAX_HASH_TABLE_SIZE_MB) {
            megabytes = MAX_HASH_TABLE_SIZE_MB;
        }

        if(megabytes < MIN_HASH_TABLE_SIZE_MB) {
            megabytes = MIN_HASH_TABLE_SIZE_MB;
        }

        SetHashTableSize(megabytes);
    } else {
        IO::PutInfo("Unknown option \"" + name + "\"");
    }
}

void UCI::SetHashTableSize(int megabytes) {
    assert(megabytes >= 1);

    int megabytesLog2 = -1;
    while(megabytes != 0) {
        megabytes >>= 1;
        ++megabytesLog2;
    }

    int bytesLog2 = megabytesLog2 + 20;
    m_Search.SetHashTableSize(bytesLog2);
}