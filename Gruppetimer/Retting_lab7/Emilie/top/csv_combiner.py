import csv
import os 

def merge_table_into(master_table, new_table):
    master_table.extend(new_table[1:])

def combine_csv_in_dir(dirpath, result_path):
    master_table = [['uibid', 'karakter', 'kommentar']]

    for filename in os.listdir(dirpath):
        if filename.endswith('.csv'):
            filepath = os.path.join(dirpath, filename)

            csvfile = open(filepath, 'r', encoding='utf-8')
            reader = csv.reader(csvfile)
            new_table = list(reader)
            csvfile.close()

            merge_table_into(master_table, new_table)
    
    result_file = open(result_path, 'w', encoding = 'utf-8', newline='')
    writer = csv.writer(result_file)
    writer.writerows(master_table)
    result_file.close()

if __name__ == "__main__":   
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
