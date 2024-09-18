import sys, os
import subprocess


subprocess.check_call(
    [sys.executable, "-m", "pip", "install", "range_key_dict"],
    stdout=subprocess.DEVNULL,
)

from range_key_dict import RangeKeyDict


nm = RangeKeyDict(
    {
        (380, 450): "Voilet",
        (451, 485): "Blue",
        (486, 500): "Cyan",
        (501, 565): "Green",
        (566, 590): "Yellow",
        (591, 625): "Orange",
        (626, 750): "Red",
    }
)
THz = RangeKeyDict(
    {
        (670, 790): "Violet",
        (620, 669): "Blue",
        (600, 619): "Cyan",
        (530, 599): "Green",
        (510, 529): "Yellow",
        (480, 509): "Orange",
        (400, 479): "Red",
    }
)


def synlig_lys(valg):
    if valg != "nm" and valg != "THz":
        print(f"Enheten må være i nm eller THz, det kan ikke være {valg}.")
        return None
    if valg == "nm":
        verdi = int(input("Angi verdi i nm: "))
        if verdi >= 400 and verdi <= 790:
            print(nm[verdi])
        else:
            print(f"{verdi} nm er utenfor det synlige spekteret.")

    if valg == "THz":
        verdi = int(input("Angi verdi i THz: "))
        if verdi >= 380 and verdi <= 750:
            print(THz[verdi])
        else:
            print(f"{verdi} THz er utenfor det synlige spekteret.")


valg = input("Angi enhet (nm eller THz): ")
synlig_lys(valg)
