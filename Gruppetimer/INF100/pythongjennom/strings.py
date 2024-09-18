##\' = '
##\" = "
##\t = tab
##\n = ny linje
##\\ = \

## raw sting ignorerer alt dette:
print(r"jeg heter \n thomas")

##multiline string ''':
print(
    """ jeg heter thomas
      og kommer fra bergen
      hva heter du"""
)
name = "thomas"
alder = 15

print("jeg heter %s og er %s år gammel" % (name, alder))
print(f"jeg heter {name.upper()} og er {alder} år gammel")

"""
isX():
isalpha() = sjekker om det bare er bokstaver
isalnum() = sjekker om det bare er tall og boksaver
isdesimal() = sjekker om det bare inneholder numeriske tall
isspace() = ser om det er bare spaces
istitle() = sjekker om alle ordene starter med en stor boksav og resten er små
 """

test = ["jeg", "heter", "thomas"]
print(",    ".join(test))
