from hashlib import *
import time
import multiprocessing
import pyinputplus
import re

__author__ = "Thomas Barth"


def getPassword(pword):
    check = re.compile(r"[A-Z]{3}\d{4}")
    checkpw = check.search(pword)
    if checkpw == None:
        raise Exception(
            "wrong format, format neeeded uuudddd, u = capital letter, d = number"
        )
    else:
        hx = sha256(bytes(checkpw.group(), "utf-8")).hexdigest()
        print(hx)
        return hx


##goes from AAA0000 - AAA0001 osv
def Bruteforce(hx):
    usrPw = list("AAA0000")
    print("Algorithm 1 running")
    start = time.time()
    for a in range(ord("A"), ord("Z") + 1):
        usrPw[0] = chr(a)
        for b in range(ord("A"), ord("Z") + 1):
            usrPw[1] = chr(b)
            for c in range(ord("A"), ord("Z") + 1):
                usrPw[2] = chr(c)
                for d in range(ord("0"), ord("9") + 1):
                    usrPw[3] = chr(d)
                    for e in range(ord("0"), ord("9") + 1):
                        usrPw[4] = chr(e)
                        for f in range(ord("0"), ord("9") + 1):
                            usrPw[5] = chr(f)
                            for g in range(ord("0"), ord("9") + 1):
                                usrPw[6] = chr(g)
                                if (
                                    sha256(bytes("".join(usrPw), "utf-8")).hexdigest()
                                    == hx
                                ):
                                    usrPw = "".join(usrPw)
                                    print(
                                        f"1 used {time.time()-start} s to find {usrPw}"
                                    )
                                    return usrPw


##goes from ZZZ9999 - ZZZ9998 osv
def Bruteforce2(hx):
    usrPw = list("ZZZ9999")
    print("Algorithm 2 running")
    start = time.time()
    for a in reversed(range(ord("A"), ord("Z") + 1)):
        usrPw[0] = chr(a)
        for b in reversed(range(ord("A"), ord("Z") + 1)):
            usrPw[1] = chr(b)
            for c in reversed(range(ord("A"), ord("Z") + 1)):
                usrPw[2] = chr(c)
                for d in reversed(range(ord("0"), ord("9") + 1)):
                    usrPw[3] = chr(d)
                    for e in reversed(range(ord("0"), ord("9") + 1)):
                        usrPw[4] = chr(e)
                        for f in reversed(range(ord("0"), ord("9") + 1)):
                            usrPw[5] = chr(f)
                            for g in reversed(range(ord("0"), ord("9") + 1)):
                                usrPw[6] = chr(g)
                                if (
                                    sha256(bytes("".join(usrPw), "utf-8")).hexdigest()
                                    == hx
                                ):
                                    usrPw = "".join(usrPw)
                                    print(
                                        f"2 used {time.time()-start} s to find {usrPw}"
                                    )
                                    return usrPw


##goes from AAA0000 - BAA0000 osv
def Bruteforce3(hx):
    usrPw = list("AAA0000")
    print("Algorithm 3 running")
    start = time.time()
    for a in range(ord("0"), ord("9") + 1):
        usrPw[6] = chr(a)
        for b in range(ord("0"), ord("9") + 1):
            usrPw[5] = chr(b)
            for c in range(ord("0"), ord("9") + 1):
                usrPw[4] = chr(c)
                for d in range(ord("0"), ord("9") + 1):
                    usrPw[3] = chr(d)
                    for e in range(ord("A"), ord("Z") + 1):
                        usrPw[2] = chr(e)
                        for f in range(ord("A"), ord("Z") + 1):
                            usrPw[1] = chr(f)
                            for g in range(ord("A"), ord("Z") + 1):
                                usrPw[0] = chr(g)
                                if (
                                    sha256(bytes("".join(usrPw), "utf-8")).hexdigest()
                                    == hx
                                ):
                                    usrPw = "".join(usrPw)
                                    print(
                                        f"3 used {time.time()-start} s to find {usrPw}"
                                    )
                                    return usrPw


##goes from ZZZ9999 - YZZ9999 osv
def Bruteforce4(hx):
    usrPw = list("ZZZ9999")
    print("Algorithm 4 running")
    start = time.time()
    for a in reversed(range(ord("0"), ord("9") + 1)):
        usrPw[6] = chr(a)
        for b in reversed(range(ord("0"), ord("9") + 1)):
            usrPw[5] = chr(b)
            for c in reversed(range(ord("0"), ord("9") + 1)):
                usrPw[4] = chr(c)
                for d in reversed(range(ord("0"), ord("9") + 1)):
                    usrPw[3] = chr(d)
                    for e in reversed(range(ord("A"), ord("Z") + 1)):
                        usrPw[2] = chr(e)
                        for f in reversed(range(ord("A"), ord("Z") + 1)):
                            usrPw[1] = chr(f)
                            for g in reversed(range(ord("A"), ord("Z") + 1)):
                                usrPw[0] = chr(g)
                                if (
                                    sha256(bytes("".join(usrPw), "utf-8")).hexdigest()
                                    == hx
                                ):
                                    usrPw = "".join(usrPw)
                                    print(
                                        f"4 used {time.time()-start} s to find {usrPw}"
                                    )

                                    return usrPw


if __name__ == "__main__":
    hxusrpw = pyinputplus.inputCustom(
        getPassword, "write a password in the format uuudddd: "
    )
    print("code by thbar6999")
    p1 = multiprocessing.Process(target=Bruteforce, args=[hxusrpw])
    p2 = multiprocessing.Process(target=Bruteforce2, args=[hxusrpw])
    p3 = multiprocessing.Process(target=Bruteforce3, args=[hxusrpw])
    p4 = multiprocessing.Process(target=Bruteforce4, args=[hxusrpw])
    p1.start()
    p2.start()
    p3.start()
    p4.start()
