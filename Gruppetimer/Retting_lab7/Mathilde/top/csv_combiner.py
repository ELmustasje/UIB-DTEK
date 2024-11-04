import csv
import io, os

def combine_csv_in_dir(dirpath, result_path):
    result = []
    for root, dirs, files in os.walk(dirpath):
        for file in files:
            if file.endswith(".csv"):
                with open(os.path.join(root, file), "rt", encoding='utf-8') as f:
                    reader = csv.reader(f)
                    for i, row in enumerate(reader):
                        if i == 0:
                            if not result:
                                result.append(row)
                        else:
                            result.append(row)
    with open(result_path, "wt", encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerows(result)




#CSV-filene i dette eksempelet er best lest med csv-biblioteket. 
#Det blir fort komplisert å tolke dem selv, siden det kan være komma-tegn i kommentar-feltet.
#Les om os -modulen, og legg spesielt merke til os.walk -funksjonen.
#Bruk gjerne en hjelpefunksjon merge_table_into(master_table, new_table) som tar som input en 2D-liste master_table som skal muteres, 
# og en 2D-liste new_table som inneholder det nye innholdet som skal legges til. 
# For hver rad i new_table (bortsett fra første rad), kopier raden inn i master_table.

#Opprett først en 2D-liste for resultat-tabellen vår, som initielt inneholder én rad (overskriftene). 
# På slutten skal vi konvertere denne listen til CSV.
#Bruk os.walk eller os.listdir for å gå igjennom alle filene i mappen gitt ved dirpath (os.walk vil også gå inn i undermapper, 
#og du trenger en nøstet løkke inne i os.walk for å gå gjennom listen med filer; ellers fungerer de nokså likt). 
#For hver fil som ender på .csv (bruk f. eks. .endswith -metoden på strenger), åpne filen og les innholdet.
#Husk å bruke os.path.join -funksjonen for å omgjøre filnavn til filstier.
#For hver .csv -fil du finner, omgjør den til en 2D-liste, og legg til radene i resultat-tabellen (bruk hjelpefunksjonen beskrevet over).

print("Tester combine_csv_in_dir... ", end="")
# Mappen samples må ligge i samme mappe som denne filen
dirpath = os.path.join(os.path.dirname(__file__), "samples")
combine_csv_in_dir(dirpath, "combined_table.csv")
with open("combined_table.csv", "rt", encoding='utf-8') as f:
   content = f.read()
   assert("""\
uibid,karakter,kommentar
abc104,C,hei
abc105,D,"med komma, her er jeg"
abc106,E,tittit
abc101,A,Her er min kommentar
abc102,B,"Jeg er glad, men her er komma"
abc103,C,Katching
""" == content or """\
uibid,karakter,kommentar
abc101,A,Her er min kommentar
abc102,B,"Jeg er glad, men her er komma"
abc103,C,Katching
abc104,C,hei
abc105,D,"med komma, her er jeg"
abc106,E,tittit
""" == content)
print("OK")

print("Tester combine_csv_in_dir... ", end="")
