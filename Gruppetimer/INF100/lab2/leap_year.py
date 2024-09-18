ar = int(input("Angi år: "))


def er_skuddar(ar):
    if ar % 4 == 0:
        if ar % 100 == 0:
            if ar % 400 == 0:
                print("Dette er et skuddår.")
            else:
                print("Dette er ikke et skuddår.")
        else:
            print("Dette er et skuddår.")
    else:
        print("Dette er ikke et skuddår.")


er_skuddar(ar)
