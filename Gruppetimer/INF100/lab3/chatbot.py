import sys


def chatbot():
    uput = input("Hi! Do you want to talk to me?\n")
    if uput == "no":
        print("All right, bye!")
        sys.exit()

    else:
        print("That's cool!")
        return 0


while chatbot() == 0:
    chatbot()
