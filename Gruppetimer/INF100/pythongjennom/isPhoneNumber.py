def isPhoneNumber(tekst):  ##leter etter amerikanske tlf i format 123-456-7890
    if len(tekst) != 12:
        return False
    i = 0
    while i < 12:
        if i < 3 or (i > 3 and i < 7) or i > 7:
            if not tekst[i].isdecimal():
                return False
                print(1)
        if i == 3 or i == 7:
            if tekst[i] != "-":
                return False
                print(2)
        i += 1
    return True


tekst = "mitt telefon number er 123-456-7890 men mamma sitt er 253-456-7890"

for i in range(len(tekst)):
    bit = tekst[i : i + 12]
    if isPhoneNumber(bit):
        print(bit)
