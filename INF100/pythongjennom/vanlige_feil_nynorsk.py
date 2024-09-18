mine_ord = []
ord_ = []
funnet_ord=[]
endinger = ["lig", "het", "else"]
begynnelser = ["an", "be"]

with open('vanligefeil.txt','r') as file:
	for line in file:
		for word in line.split():
			ord_.append(word)
			

with open('tekst_til_retting.txt','r') as file:
	for line in file:

		for word in line.split():

			for i in endinger:
				if word.endswith(i):
					funnet_ord.append(word)

			for i in begynnelser:
				if word.startswith(i):
					funnet_ord.append(word)

			mine_ord.append(word)




for i in ord_:
	if i in mine_ord:
		funnet_ord.append(i)

print (funnet_ord)

	

			

