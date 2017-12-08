#include "IO.hpp"

#include <iostream>
#include <fstream>
#include <mutex>

namespace {
    std::mutex f_Lock;
    std::ofstream f_LogFile;
}

namespace IO {
    void OpenLogFile(const std::string& filename) {
        f_LogFile.open(filename, std::ios::out | std::ios::app);
        f_LogFile << "--- Log opened ---" << std::endl;
    }

    bool GetLine(std::string& out_line) {
        // The actual read is not synchronized, don't call it from multiple thread
        bool result = (bool)std::getline(std::cin, out_line);

        std::lock_guard<std::mutex> guard(f_Lock);

        if(f_LogFile.is_open()) {
            f_LogFile << ">> " << out_line << std::endl;
        }

        return result;
    }

    void PutLine(const std::string& line) {
        std::lock_guard<std::mutex> guard(f_Lock);

        if(f_LogFile.is_open()) {
            f_LogFile << "<< " << line << std::endl;
        }

        std::cout << line << std::endl;
    }

    void PutInfo(const std::string& line) {
        std::lock_guard<std::mutex> guard(f_Lock);
        
        if(f_LogFile.is_open()) {
            f_LogFile << "== " << line << std::endl;
        }

        std::cerr << line << std::endl;
    }
}