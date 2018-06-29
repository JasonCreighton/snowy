// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Common.hpp"
#include "Board.hpp"
#include "Search.hpp"

class UCI {
public:
    UCI();

    bool DoCommand(const std::string& commandLine);
    void Run();
    void WaitForSearch();

private:
    static const int MAX_HASH_TABLE_SIZE_MB = 1024;
    static const int MIN_HASH_TABLE_SIZE_MB = 1;
    static const int DEFAULT_HASH_TABLE_SIZE_MB = 64;

    void SetOption(const std::string& name, const std::string& value);
    void SetHashTableSize(int megabytes);

    Board m_Board;
    Search m_Search;
};