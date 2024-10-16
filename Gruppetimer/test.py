def filter(s, min):
    newString = ""
    s = s.strip()
    for st in s.split("\n"):
        splited = st.split(";")
        value = splited[2]
        try:
            if float(value) > min:
                newString += st + "\n"
        except:
            pass

    return newString


input_arg = """\
id;location;impact;time
nc72666881;California;1.43;2016-07-27 00:19:43
us20006i0y;Burma;4.9;2016-07-27 00:20:28
nc72666891;California;0.06;2016-07-27 00:31:37
nc72666892;California;not_a_number;2016-08-23 03:21:18
"""

print(filter(input_arg, 1.1))
