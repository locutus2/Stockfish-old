#include <iostream>
#include <string>
#include <vector>
#include <sstream>

#include "genetics.h"
#include "evolution.h"

std::vector<std::string> split (const std::string &s, char delim) {
    std::vector<std::string> result;
    std::stringstream ss (s);
    std::string item;

    while (getline (ss, item, delim)) {
        result.push_back (item);
					    }
    return result;
}

void readData(std::vector<bool>& labels, std::vector<std::vector<bool>>& data, std::istream& in = std::cin)
{
    std::string line;
    while(std::getline(in, line))
    {
        std::vector<std::string> record = split(line, ';');
	labels.push_back(bool(std::stoi(record[0])));

	std::vector<bool> C;
	for(int i = 1; i <(int)record.size(); ++i)
	    C.push_back(bool(std::stoi(record[i])));

	data.push_back(C);
    }

    std::cout << "Read records: " << data.size() << std::endl; 
}

int main()
{
    std::vector<bool> labels;
    std::vector<std::vector<bool>> data;

    readData(labels, data);

    Evolution e;
    e.setData(labels, data);
    e.init(50);
    e.run();

    return 0;
}
