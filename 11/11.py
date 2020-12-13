
f = open("input", "r")
lines = f.readlines()

lines = map(lambda line: line.strip(), lines)
lines = map(lambda line: ["."] + list(line) + ["."], lines)
empty = ["."] * len(lines[0])
lines = [empty] + lines + [empty]

def p(array):
    for i in range(0,len(array)):
        print(array[i])
    print("--")

def iter1(array):
    new = map(lambda l: l[:], array) # clones array
    for i in range(0,len(array)):
        for j in range(0,len(array[i])):
            if array[i][j] == '.':
                continue

            beside = [array[i][j+1], array[i][j-1], array[i+1][j], array[i-1][j], array[i+1][j+1], array[i+1][j-1], array[i-1][j+1], array[i-1][j-1]]
            occupied = beside.count('#')

            if array[i][j] == 'L' and occupied == 0:
                new[i][j] = '#'
            if array[i][j] == '#' and occupied >= 4:
                new[i][j] = 'L'
    return new

def iter2(array):
    def findArr(x,y,xD,yD):
        x += xD
        y += yD
        while x > 0 and x < len(array) and y > 0 and y < len(array[0]):
            if array[x][y] == '#': 
                return '#'
            if array[x][y] == 'L': 
                return 'L'
            x += xD
            y += yD
        return '.'

    new = map(lambda l: l[:], array) # clones array
    for i in range(0,len(array)):
        for j in range(0,len(array[i])):
            if array[i][j] == '.':
                continue

            beside = [findArr(i,j,0,1), findArr(i,j,0,-1), findArr(i,j,1,0), findArr(i,j,-1,0), findArr(i,j,1,1), findArr(i,j,1,-1), findArr(i,j,-1,1), findArr(i,j,-1,-1)]
            occupied = beside.count('#')

            if array[i][j] == 'L' and occupied == 0:
                new[i][j] = '#'
            if array[i][j] == '#' and occupied >= 5:
                new[i][j] = 'L'
    return new


def solve(array, iterf):
    prev = array
    while True:
        next = iterf(prev)
        if next == prev:
            return sum(map(lambda l: l.count("#"), next))
        prev = next


p(lines)
next = iter2(lines)
p(next)
next = iter2(next)
p(next)

print(solve(lines, iter1) == 37)
print(solve(lines, iter1))
print(solve(lines, iter2) == 26)
print(solve(lines, iter2))

