import random

def whitespace() -> str:
    return ''.join(random.choice(["\n", " ", "\t"]) for _ in range(random.randrange(0, 3)))

with open("big-example.txt", "w+") as f:
    n = random.randrange(0, 10000)
    f.write(f"{n}")
    depth = 0

    ops = "+-*/"
    for i in range(0, 1000000):
        f.write(whitespace())
        op = random.randrange(0, 4)
        f.write(ops[op])
        f.write(whitespace())
        if random.randrange(0, 10) < 2:
            f.write("(")
            depth += 1
        n = random.randrange(0, 10000)
        f.write(f"{n}")
        f.write(whitespace())
        if random.randrange(0, 9) < 2 and depth > 0:
            f.write(")")
            depth -= 1

    for i in range(depth):
        f.write(")")
