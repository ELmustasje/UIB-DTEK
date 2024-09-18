# -*- coding: utf-8 -*-
"""
Created on Tue Nov 15 21:48:04 2022

@author: thoma
"""
import random 

thisdict =	[ #lager 4 randome tall og lagrer de i en liste
   random.randint(1,8),
   random.randint(1,8),
   random.randint(1,8),
   random.randint(1,8)
]
runder = 10 ##hvor mange forsøk

def check_siffer(): ##ser om sifferet er på rikkt plass og hvis ikke ser om det er i listen
     global riktig_plass, riktig_siffer
     
     for i in range (4):
         try:
             if gjett_siffer[i] == thisdict[i]:
                 riktig_plass = riktig_plass + 1
                 
             if gjett_siffer[i] != thisdict[i]:
                if gjett_siffer[i] in thisdict:
                     riktig_siffer = riktig_siffer+1
         except:
            pass
            
     return riktig_plass, riktig_siffer
             
while True: ##spillet 
    print(thisdict[0],thisdict[1],thisdict[2], thisdict[3]) ##dev, fjeners når noen skal spille
    
    riktig_plass = 0
    riktig_siffer = 0
    gjett=input("skriv 4 siffer")
    gjett_siffer=[int(d) for d in str(gjett)]
    check_siffer()
    if riktig_plass == 4:
        print ("du har vunnet")
        break
    else:
        runder = runder - 1
        print ("du har ", runder , " igjen")
        print (riktig_plass , "siffer er på riktig plass i koden")
        print (riktig_siffer , "siffer er i koden men ikke på riktig plass")
        if runder == 0:
            print("du tapte")
            break
        
        
    

    
        




