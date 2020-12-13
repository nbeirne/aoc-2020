
def solve2Naive(array):
    if len(array) == 0:
        print('hit empty array')
        return 0

    first = array[0]

    if len(array) == 1:
        return 1
    if len(array) == 2:
        return 1

    rest = array[1:]
    snd = rest[0]
    diff = snd-first

    if diff == 3:
        # we know first must be in the array
        withSnd = solve2Naive(rest)
        return withSnd
    elif diff < 3:
        # try with and without snd
        withSnd = solve2Naive(rest)
        withoutSnd = solve2Naive([first] + rest[1:])
        return withSnd + withoutSnd
    else:
        # the jump is too much. stop searching here
        return 0

# split array whenever an element jumps by 3. So [1,2,5,6,7,11,12] => [[0,1,2],[5,6,7],[11,12]
# adding 0 makes the rest of it work...
def segment(arr):
    acc = [[0]]
    for elem in arr:
        if elem - acc[0][0] == 3: # split
            acc = [[elem]]+acc
        else:
            acc[0] = [elem]+acc[0]
    return acc

def solve2(arr):
    segments = segment(arr)
    acc = 1 # since its a mult we start with 1
    for seg in segments:
        seg.reverse()
        acc *= solve2Naive(seg)

    return acc


test1 = [16,10,15,5,1,11,7,19,6,12,4,30]
test2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
test1.sort()
test2.sort()
a1n = solve2Naive([0] + test1)
a2n = solve2Naive([0] + test2)
print(a1n == 8)
print(a2n == 19208)


a1 = solve2(test1)
a2 = solve2(test2)
print(a1 == 8)
print(a2 == 19208)

f = open("input", "r")
lines = f.readlines()
arr = map(int, lines)
arr.sort()
print(solve2(arr))

