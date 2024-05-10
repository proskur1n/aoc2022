#include <iterator>
#include <string>
#include <iostream>
#include <fstream>
#include <cassert>
#include <algorithm>

static unsigned long snafuToDecimal(std::string const &snafu) {
    unsigned long decimal = 0;
    unsigned long power = 1;
    for (auto it = snafu.rbegin(); it != snafu.rend(); ++it) {
        switch (*it) {
        case '0':
            break;
        case '1':
            decimal += power;
            break;
        case '2':
            decimal += power * 2;
            break;
        case '-':
            decimal -= power;
            break;
        case '=':
            decimal -= 2 * power;
            break;
        default:
            assert(false && "unknown snafu digit");
        }
        power *= 5;
    }
    return decimal;
}

static std::string decimalToSnafu(unsigned long decimal) {
    if (decimal == 0) {
        return "0";
    }
    std::string snafu;
    while (decimal > 0) {
        unsigned int digit = decimal % 5;
        decimal /= 5;
        switch (digit) {
        case 0:
            snafu += "0";
            break;
        case 1:
            snafu += "1";
            break;
        case 2:
            snafu += "2";
            break;
        case 3:
            snafu += "=";
            decimal += 1;
            break;
        case 4:
            snafu += "-";
            decimal += 1;
            break;
        default:
            assert(false && "unreachable");
        }
    }
    std::reverse(snafu.begin(), snafu.end());
    return snafu;
}

int main() {
    unsigned long sum = 0;
    std::ifstream file("input");
    for (std::string line; std::getline(file, line);) {
        sum += snafuToDecimal(line);
    }
    std::cout << "Part One: " << decimalToSnafu(sum) << '\n';
}
