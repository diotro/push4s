import json
import random

random.seed(8123789)

with open("../resources/collatz-numbers-edge.json") as edge_fd:
    edge_cases = json.load(edge_fd)[1:]
    # Throwing away the names of elements, read as pairs

with open("../resources/collatz-numbers-random.json") as random_fd:
    random_cases = json.load(random_fd)[1:]
    random.shuffle(random_cases)

# 200 total cases, all the edge cases + the rest from the random cases
train_cases = edge_cases + random_cases[:200-len(edge_cases)]

random_cases = random_cases[200-len(edge_cases):]
# 2000 random cases
eval_cases = random_cases[:2000]

def fix(case):
    return {"in": [case[0]], "out":  [case[1]]}

out = {
    "name": "collatz",
    "trainingTestCases": [fix(x) for x in train_cases],
    "evaluationTestCases": [fix(x) for x in eval_cases],
    "stacks": ["Boolean", "Integer"]
}

with open("../resources/collatz_benchmark.json", "w") as out_file:
    json.dump(out, out_file)
