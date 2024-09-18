# -*- coding: utf-8 -*-
"""
Created on Tue Nov 15 18:22:38 2022

@author: thoma
"""
import random

brett = {1 : " ",2:" ",3:" ",
         4 : " ",5: " ",6: " ",
         7:" ",8:" ",9:" "}

def print_brett():
        print(brett[1] + "|" + brett[2]+ "|"  + brett[3])
        print("-+-+-")
        print(brett[4] + "|" + brett[5]+  "|"  +  brett[6])
        print("-+-+-")
        print(brett[7] + "|" + brett[8]+ "|"  + brett[9])
        
    
def spill():
    tur = "O"
    while True:
        print_brett()
        print(tur + " begynner velg rute med å velde taller som står på den ruten")
        valg = int(input())
        if brett[valg] == " ":
            brett[valg] = tur
            
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
        else:
            print("den er allerede tatt, velg en annen")
            continue
    
        if brett[1]==brett[2]==brett[3] != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[3]==brett[4]==brett[5]  != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[6]==brett[7]==brett[8]  != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[1]==brett[4]==brett[7] != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[2]==brett[5]==brett[8]  != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[3]==brett[6]==brett[8]  != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[1]==brett[5]==brett[9]  != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
        if brett[3]==brett[5]==brett[7]  != " ":
            if tur =='O':
                tur = 'X'
            else:
                tur = 'O' 
            print(tur+" vinner")
            print_brett()
            break
spill()
