prefix_n = 5
f = open("input", "r")
lines = map(int, f.readlines())

def preambleAddsToValue(arr, value):
    for i in arr:
        for j in arr:
            if i+j == value:
                return True
    return False

def solve1(array):
    preamble = array[0:prefix_n]
    values = array[prefix_n:]

    for value in values:
        if preambleAddsToValue(preamble, value):
            # rotate preamble
            preamble.pop(0)
            preamble.append(value)
        else:
            return value

def solve2(array):
    match = solve1(array)
    max_range = len(array)
    for i in range(0, max_range):
        for j in range(2, max_range-i+1):
            sub_array = array[i:i+j]
            if sum(sub_array) == match:
                return min(sub_array)+max(sub_array)


def solve2e(array):
    match = solve1(array)
    max_range = len(array)
    for i in range(0, max_range):
        sum_acc = array[i]
        for j in range(2, max_range-i+1):
            sum_acc += array[i+j-1]
            if sum_acc == match:
                sub_array = array[i:i+j]
                return min(sub_array)+max(sub_array)

print(solve22([35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]))
print(solve22([35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]) == 62)

#print(solve2(lines) == 51152360)
#print(solve1(lines))
#print(solve2(lines))
