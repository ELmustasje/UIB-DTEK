def encrypt(tekst, keyword):
    ##removes all spaces and makes it lowercase
    tekst = tekst.replace(" ", "").lower()

    ##creates a key that is the same length as the plaintext
    key = keyword
    for i in range(len(tekst) - len(key)):
        key += key[i % len(key)]

    ##encrytion algorithm
    encryptedtxt = []
    for i in range(len(tekst)):
        ##turn the letter into unicode
        bokstav = ord(tekst[i])
        ##finds witch place in the alfabeth the letter is
        bokstav = bokstav - ord("a")
        ##takes the position of the letter and adds the position of the key letter, since i add 26 and % 26 it will not be over 26
        bokstav = (bokstav + (26 + (ord(key[i]) - ord("a")))) % 26
        ##adds the encrypted letter to the encryptedtxt list
        encryptedtxt.append(chr(bokstav + (ord("a"))))
    ##turns the list into an string with no spaces
    return "".join(encryptedtxt)


def decrypt(tekst, keyword):
    ##same key algorithm
    key = keyword
    for i in range(len(tekst) - len(key)):
        key += key[i % len(key)]

    ##decryption algorithm
    decryptedtxt = []
    for i in range(len(tekst)):
        ##turn the letter into unicode
        bokstav = ord(tekst[i])
        ##finds witch place in the alfabeth the letter is
        bokstav = bokstav - ord("a")
        ##takes the position of the letter and removes the position of the key letter, since i add 26 and % 26 it will not be over 26
        bokstav = (bokstav + 26 - (ord(key[i]) - ord("a"))) % 26
        ##adds the decrypted letter to the encryptedtxt list
        decryptedtxt.append(chr(bokstav + (ord("a"))))
    ##turns the list into an string with no spaces
    return "".join(decryptedtxt)


##main
plaintext = "cybersecurity is an evolving process and is determined by the weakest link"
keyword = "iknowencryptionnow"

encryptxt = encrypt(plaintext, keyword)
decrypttxt = decrypt(encryptxt, keyword)

print(
    f"""
The plaintext is: {plaintext}

Encrtpted this wil turn into: {encryptxt}

When decrypted it wil go to: {decrypttxt}
"""
)
