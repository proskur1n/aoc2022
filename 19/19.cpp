#include <string>
#include <fstream>
#include <iostream>
#include <regex>
#include <cassert>
#include <unordered_map>
#include <chrono>
#include <vector>

struct Blueprint {
    int id;
    struct { int ore; } oreRobotCost;
    struct { int ore; } clayRobotCost;
    struct { int ore; int clay; } obsidianRobotCost;
    struct { int ore; int obsidian; } geodeRobotCost;
};

struct Resources {
    int time;
    int ore;
    int clay;
    int obsidian;
    int oreRobots;
    int clayRobots;
    int obsidianRobots;

    bool operator==(Resources const &) const = default;
};

template <>
struct std::hash<Resources> {
    std::size_t operator()(Resources const &r) const {
        auto hasher = std::hash<int>();
        size_t result = 17;
        result = result * 31 + hasher(r.time);
        result = result * 31 + hasher(r.ore);
        result = result * 31 + hasher(r.clay);
        result = result * 31 + hasher(r.obsidian);
        result = result * 31 + hasher(r.oreRobots);
        result = result * 31 + hasher(r.clayRobots);
        result = result * 31 + hasher(r.obsidianRobots);
        return result;
    }
};

static int sumOneToN(int n) {
    return n * (n + 1) / 2;
}

static int maximumGeodes(Blueprint const &bp, int totalTime) {
    int maximum = 0;
    std::unordered_map<Resources, int> memo;

    auto branchAndBound = [&](auto const &self, Resources resources, int geodes) -> int {
        if (resources.time <= 1) {
            return 0;
        }

        int obsidianUpperBound = resources.obsidian + resources.obsidianRobots * (resources.time) + sumOneToN(resources.time - 1);
        int newGeodeRobotsUpperBound = std::min(resources.time, obsidianUpperBound / bp.geodeRobotCost.obsidian);
        int geodesUpperBound = geodes + sumOneToN(resources.time - 1) - sumOneToN(resources.time - newGeodeRobotsUpperBound - 1);
        if (geodesUpperBound <= maximum) {
            return 0;
        }

        if (auto memoized = memo.find(resources); memoized != memo.end()) {
            return memoized->second;
        }

        int localMaximum = 0;
        Resources updated = resources;
        updated.time -= 1;
        updated.ore += resources.oreRobots;
        updated.clay += resources.clayRobots;
        updated.obsidian += resources.obsidianRobots;

        if (resources.ore >= bp.oreRobotCost.ore) {
            Resources temp = updated;
            temp.ore -= bp.oreRobotCost.ore;
            temp.oreRobots += 1;
            localMaximum = std::max(localMaximum, self(self, temp, geodes));
        }
        if (resources.ore >= bp.clayRobotCost.ore) {
            Resources temp = updated;
            temp.ore -= bp.clayRobotCost.ore;
            temp.clayRobots += 1;
            localMaximum = std::max(localMaximum, self(self, temp, geodes));
        }
        if (resources.ore >= bp.obsidianRobotCost.ore && resources.clay >= bp.obsidianRobotCost.clay) {
            Resources temp = updated;
            temp.ore -= bp.obsidianRobotCost.ore;
            temp.clay -= bp.obsidianRobotCost.clay;
            temp.obsidianRobots += 1;
            localMaximum = std::max(localMaximum, self(self, temp, geodes));
        }
        if (resources.ore >= bp.geodeRobotCost.ore && resources.obsidian >= bp.geodeRobotCost.obsidian) {
            Resources temp = updated;
            temp.ore -= bp.geodeRobotCost.ore;
            temp.obsidian -= bp.geodeRobotCost.obsidian;
            localMaximum = std::max(localMaximum, temp.time + self(self, temp, geodes + temp.time));
        }
        /* Do not construct any new robots this minute. */
        localMaximum = std::max(localMaximum, self(self, updated, geodes));

        if (localMaximum > maximum) {
            maximum = localMaximum;
        }
        if (resources.time > 7) { /* Do not memoize results that are fast to compute. */
            memo[resources] = localMaximum;
        }
        return localMaximum;
    };

    return branchAndBound(branchAndBound, {.time = totalTime, .oreRobots = 1}, 0);
}

struct Timer {
    char const *label;
    std::chrono::time_point<std::chrono::steady_clock> begin;

    explicit Timer(char const *label)
        : label(label)
        , begin(std::chrono::steady_clock::now())
    {
    }

    ~Timer() noexcept {
        auto end = std::chrono::steady_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin);
        std::cout << label << " took " << duration.count() << " milliseconds\n";
    }
};

int main() {
    std::vector<Blueprint> blueprints;
    std::regex delimiter("\\D+");
    std::ifstream file("input");
    for (std::string line; std::getline(file, line);) {
        std::sregex_token_iterator it(line.begin(), line.end(), delimiter, -1);
        ++it;
        Blueprint bp = {};
        bp.id = std::stoi(*it++);
        bp.oreRobotCost.ore = std::stoi(*it++);
        bp.clayRobotCost.ore = std::stoi(*it++);
        bp.obsidianRobotCost.ore = std::stoi(*it++);
        bp.obsidianRobotCost.clay = std::stoi(*it++);
        bp.geodeRobotCost.ore = std::stoi(*it++);
        bp.geodeRobotCost.obsidian = std::stoi(*it++);
        assert(it == std::sregex_token_iterator());
        blueprints.push_back(bp);
    }

    {
        Timer timer("Part One");
        int partOne = 0;
        for (Blueprint const &bp : blueprints) {
            partOne += bp.id * maximumGeodes(bp, 24);
        }
        std::cout << "Part One: " << partOne << '\n';
    }

    {
        Timer timer("Part Two");
        int partTwo = maximumGeodes(blueprints[0], 32) * maximumGeodes(blueprints[1], 32) * maximumGeodes(blueprints[2], 32);
        std::cout << "Part Two: " << partTwo << '\n';
    }
}
