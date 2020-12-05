import re

f = open("input", "r")
lines = f.readlines()

def validate1(dict):
    return \
        "byr" in dict and \
        "iyr" in dict and \
        "eyr" in dict and \
        "hgt" in dict and \
        "hcl" in dict and \
        "ecl" in dict and \
        "pid" in dict 


def validate2(dict):
    print("----")
    #if len(dict) != 7 and len(dict) != 8:
    #    print("FAIL dict wrong len")
    #    return False

    r = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
    for key in dict:
        if key not in r and key != "cid":
            print("FAIL "+key+" not in possible entries")
            return False

    for key in r:
        if key not in dict: 
            print("FAIL "+key+" not in dict")
            return False
        v = dict[key]

        if key == "byr":
            value = int(v)
            if len(v) != 4 or value > 2002 or value < 1920:
                print("FAIL "+key + " = "+v)
                return False

        if key == "iyr":
            value = int(v)
            if len(v) != 4 or value > 2020 or value < 2010:
                print("FAIL "+key + " = "+v)
                return False

        if key == "eyr":
            value = int(v)
            if len(v) != 4 or value > 2030 or value < 2020:
                print("FAIL "+key + " = "+v)
                return False

        if key == "hgt":
            if v[-2:] == "cm":
                n = int(v[:-2])
                if len(v) != 5 or n > 193 or n < 150:
                    print("FAIL "+key + " = "+v)
                    return False
            elif v[-2:] == "in":
                n = int(v[:-2])
                if len(v) != 4 or n > 76 or n < 59:
                    print("FAIL "+key + " = "+v)
                    return False
            else:
                return False

        if key == "hcl":
            r = re.compile("^#[0-9a-f]{6}$")
            if r.match(v) == None:
                print("FAIL "+key + " = "+v)
                return False

        if key == "ecl":
            if v not in ["amb","blu","brn","gry","grn","hzl","oth"]:
                print("FAIL "+key + " = "+v)
                return False

        if key == "pid":
            r = re.compile("^[0-9]{9}$")
            if r.match(v) == None:
                print("FAIL "+key + " = "+v)
                return False

        print("PASS "+key + " = "+v)
    print("PASS.")
    return True







valid1 = 0
valid2 = 0
fails = 0
dict = {}
valid_dicts = []
for line in lines:
    #print("got:"+line)
    line = line.rstrip()
    if line == "":
        if validate1(dict):
            valid1 = valid1 + 1

        if validate2(dict):
            valid2 = valid2 + 1
            valid_dicts.append(dict)
        else: 
            fails = fails + 1
        dict = {}
        continue
    strs = line.split()
    for str in strs:
        elem = str.split(":")
        dict[elem[0]] = elem[1]

print(valid1)
print(valid2)

vd = {}
for dict in valid_dicts:
    if not dict["pid"] in vd:
        print("add pid: "+dict["pid"])
        vd[dict["pid"]] = dict
    else:
        vd.pop(dict["pid"])

print(len(vd))
