
f = open("input", "r")
lines = f.readlines()
input = lines[1]
#input = "7,13,x,x,59,x,31,19"

def solve2(input):

    # helper functions for filter and map 
    def flt(i):
        (a,b) = i
        if b == 'x':
            return False
        else:
            return True

    def conv(i):
        (a,b) = i # (index, busid)
        return (a,int(b))


    print("----")
    print(input)
    input = input.strip().split(",")

    # pair values with their indexes, then remove x's. This means that we have a list of (t,id) where t is the difference between our initial busses id ind and the current one
    # the math means that (intial time + t) is when we expect the bus to arrive. So when we have an expected number, when `(initial time + t) % expected` is 0 we have a solution.
    list = map(conv, filter(flt, enumerate(input)))

    (t,add_by) = list.pop(0) # our starting point is the first elem
    current = 0 # start at 0, add by add_by at every iteration. 
    while list:
        (t,n) = list[0]
        # we match when we fufull the solution above
        if (current + t) % n == 0:
            # found a match
            # we can now jump by n*add_by because we know that's the next possible solution which satisfies _all_ previous numbers in the array
            add_by = n*add_by
            # pop to the next elem and continue trying to find a solution
            (t,n) = list.pop(0)
        else:
            # if it does not match, check next possible value
            current += add_by

    return current

print(solve2("17,x,13,19") == 3417)            # 3417       (17*201)                        (13*263      - 2) (19*180      - 3)
print(solve2("67,7,59,61") == 754018)          # 754018     (67*11254)    (7*107717    - 1) (59*12780    - 2) (61*12361 - 3)
print(solve2("67,x,7,59,61") == 779210)        # 779210     (67*11630)                      (7*111316    - 2) (59*13207    - 3) (61*12774 - 4)
print(solve2("67,7,x,59,61") == 1261476)       # 1261476    (67*18828)    (7*180211    - 1)                   (59*21381    - 3) (61*20680 - 4)
print(solve2("1789,37,47,1889") == 1202161486) # 1202161486 (1789*671974) (37*32490851 - 1) (47*25577904 - 2) (1889*636401 - 3)
print(solve2(input)) # 1202161486 (1789*671974) (37*32490851 - 1) (47*25577904 - 2) (1889*636401 - 3)

