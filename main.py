# bits = [0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0]
bits = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


print("Bits | State | New State")


def evalbits(a, b, c, d):
    a1 = (c & d) | (b & d) | (b & c) | (a & d) | (a & c) | (a & b)
    a2 = a | b | c | d

    print(f"{a}{b} | {c}{d} | {a1}{a2}")

    return (a1, a2)


assert len(bits) == 12


def printbar():
    print("=" * 12)


# returns true if contains duplicate
def tree_reduce():
    (p1, p2) = evalbits(0, bits[0], 0, bits[1])
    (b1, b2) = evalbits(0, bits[2], 0, bits[3])
    (c1, c2) = evalbits(0, bits[4], 0, bits[5])
    (d1, d2) = evalbits(0, bits[6], 0, bits[7])
    (e1, e2) = evalbits(0, bits[8], 0, bits[9])
    (f1, f2) = evalbits(0, bits[10], 0, bits[11])

    # printbar()

    (x1, x2) = evalbits(p1, p2, b1, b2)
    (y1, y2) = evalbits(c1, c2, d1, d2)
    (z1, z2) = evalbits(e1, e2, f1, f2)

    # printbar()

    (p1, p2) = evalbits(x1, x2, y1, y2)

    # printbar()

    (q1, q2) = evalbits(p1, p2, z1, z2)

    print(q1, q2)

    return q1 & q2 == 1


print(tree_reduce())


# for i in range(len(bits) - 1):
#     A1 = bits[i]
#     A2 = bits[i + 1]

#     # cd + bd + bc + ad + ac + ab
#     O1 = (B1 & B2) | (A2 & B2) | (A2 & B1) | (A1 & B2) | (A1 & B1) | (A1 & A1)
#     O2 = A1 | A2 | B1 | B2

#     print(f"{A1}{A2} | {B1}{B2} | {O1}{O2}")

#     B1 = O1
#     B2 = O2


# for x in range(4):
#     A1 = (x >> 1) & 1
#     A2 = x & 1

#     for y in range(4):
#         B1 = (y >> 1) & 1
#         B2 = y & 1

#         # this expression could possibly be simplified further by considering
#         # that some outputs can be anything

#         # cd + bd + bc + ad + ac + ab
#         O1 = (B1 & B2) | (A2 & B2) | (A2 & B1) | (A1 & B2) | (A1 & B1) | (A1 & A1)
#         O2 = A1 | A2 | B1 | B2

#         print(f"{A1}{A2} | {B1}{B2} | {O1}{O2}")
