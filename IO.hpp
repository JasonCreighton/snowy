// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include <string>

namespace IO {
    void OpenLogFile(const std::string& filename);

    bool GetLine(std::string& out_line);
    void PutLine(const std::string& line);
    void PutInfo(const std::string& line);
}