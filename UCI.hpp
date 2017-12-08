#pragma once

#include "Board.hpp"
#include "Search.hpp"

#include <string>

class UCI {
public:
    UCI();

    bool DoCommand(const std::string& commandLine);
    void Run();
    void WaitForSearch();

private:
    Board m_Board;
    Search m_Search;
};