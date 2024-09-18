rad1 = input("Første raden: ")
rad2 = input("Andre raden: ")
rad3 = input("Tredje raden: ")
lenStr = []
lenStr.append(len(rad1))
lenStr.append(len(rad2))
lenStr.append(len(rad3))

maxLen = max(lenStr)

for i in range(maxLen + 4):
    print("@", end="")
print("")
print("@ " + rad1.rjust(maxLen) + " @")
print("@ " + rad2.rjust(maxLen) + " @")
print("@ " + rad3.rjust(maxLen) + " @")
for i in range(maxLen + 4):
    print("@", end="")
