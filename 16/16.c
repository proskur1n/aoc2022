#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 255
#define MAX_TUNNELS 10

typedef uint64_t bitmask_t;

typedef struct {
    bitmask_t bitmask;
    int flowRate;
    int numTunnels;
    int tunnels[MAX_TUNNELS];
} valve_t;

typedef struct node {
    int valve;
    int remainingTime;
    bitmask_t opened;
    int pressure;
    struct node *next;
} node_t;

typedef struct {
    int size;
    int numBuckets;
    node_t **buckets;
} memo_t;

static int nameToId(char const *name) {
    assert(isupper(name[0]) && isupper(name[1]));
    return (name[0] - 'A') * 26 + (name[1] - 'A');
}

static unsigned long hash(int valve, int remainingTime, bitmask_t opened) {
    unsigned long h = 17;
    h = h * 31 + valve;
    h = h * 31 + remainingTime;
    h = h * 31 + opened;
    return h;
}

static void memoize(memo_t *memo, int valve, int remainingTime, bitmask_t opened, int pressure) {
    float load = (float) (memo->size + 1) / (float) memo->numBuckets;
    if (load > 0.75f) {
        int resized = memo->numBuckets * 2;
        if (resized < 16) {
            resized = 16;
        }
        node_t **newBuckets = calloc(resized, sizeof(node_t *));

        for (int i = 0; i < memo->numBuckets; ++i) {
            node_t *node = memo->buckets[i];
            while (node != NULL) {
                node_t *next = node->next;
                unsigned long j = hash(node->valve, node->remainingTime, node->opened) % resized;
                node->next = newBuckets[j];
                newBuckets[j] = node;
                node = next;
            }
        }

        free(memo->buckets);
        memo->numBuckets = resized;
        memo->buckets = newBuckets;
    }

    unsigned long bucket = hash(valve, remainingTime, opened) % memo->numBuckets;
    node_t *node = malloc(sizeof(node_t));
    node->valve = valve;
    node->remainingTime = remainingTime;
    node->opened = opened;
    node->pressure = pressure;
    node->next = memo->buckets[bucket];
    memo->buckets[bucket] = node;
    memo->size++;
}

static node_t *memoized(memo_t *memo, int valve, int remainingTime, bitmask_t opened) {
    if (memo->numBuckets == 0) {
        return NULL;
    }
    unsigned long bucket = hash(valve, remainingTime, opened) % memo->numBuckets;
    node_t *node = memo->buckets[bucket];
    while (node != NULL) {
        if (node->valve == valve && node->remainingTime == remainingTime && node->opened == opened) {
            break;
        }
        node = node->next;
    }
    return node;
}

static int depthFirstSearch(memo_t *memo, memo_t *elephantMemo, valve_t const valves[], int valve, int remainingTime, bitmask_t opened) {
    if (remainingTime < 2) {
        if (elephantMemo != NULL) {
            return depthFirstSearch(elephantMemo, NULL, valves, nameToId("AA"), 26, opened);
        }
        return 0;
    }

    node_t *node = memoized(memo, valve, remainingTime, opened);
    if (node != NULL) {
        return node->pressure;
    }

    int maxPressure = 0;

    if (valves[valve].flowRate > 0 && (opened & valves[valve].bitmask) == 0) {
        // Try to open current valve.
        int time = remainingTime - 1;
        int pressure = valves[valve].flowRate * time + depthFirstSearch(memo, elephantMemo, valves, valve, time, opened | valves[valve].bitmask);
        if (pressure > maxPressure) {
            maxPressure = pressure;
        }
    }

    for (int i = 0; i < valves[valve].numTunnels; ++i) {
        int pressure = depthFirstSearch(memo, elephantMemo, valves, valves[valve].tunnels[i], remainingTime - 1, opened);
        if (pressure > maxPressure) {
            maxPressure = pressure;
        }
    }

    memoize(memo, valve, remainingTime, opened, maxPressure);
    return maxPressure;
}

static int partOne(valve_t const valves[]) {
    memo_t memo = {};
    return depthFirstSearch(&memo, NULL, valves, nameToId("AA"), 30, 0);
}

static int partTwo(valve_t const valves[]) {
    // Inspired by https://www.youtube.com/watch?v=rN4tVLnkgJU
    memo_t memo = {};
    memo_t elephantMemo = {};
    return depthFirstSearch(&memo, &elephantMemo, valves, nameToId("AA"), 26, 0);
}

int main() {
    FILE *file = fopen("input", "r");
    if (file == NULL) {
        perror("Failed to open input");
        return EXIT_FAILURE;
    }

    int numValves = 0;
    valve_t valves[25 * 26 + 25 + 1] = {};
    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file) != NULL) {
        char *name = line + strlen("Valve ");
        int id = nameToId(name);
        int flowRate = atoi(line + strlen("Valve AA has flow rate="));
        valve_t *valve = &valves[id];
        valve->bitmask = 1 << (numValves++);
        valve->flowRate = flowRate;
        char *tok = strtok(strchr(line, ';') + 2, ", \n");
        while (tok != NULL) {
            if (isupper(tok[0])) {
                valve->tunnels[valve->numTunnels++] = nameToId(tok);
            }
            tok = strtok(NULL, ", \n");
        }
    }

    printf("Part One: %d\n", partOne(valves));
    printf("Part Two: %d\n", partTwo(valves));
}
