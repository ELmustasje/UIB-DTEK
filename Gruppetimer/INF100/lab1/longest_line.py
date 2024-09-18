navn = input("hva er ditt navn? ")
adresse = input("hva er din adresse? ")
postnr = input("hva er ditt postnummer og poststed? ")

len_str = []
len_str.append(len(navn))
len_str.append(len(adresse))
len_str.append(len(postnr))

print(max(len_str))
