import csv
import io
from pathlib import Path
import json

def convert_students_to_csv(data, target_filename):
    why = Path(data).read_text(encoding='utf-8')
    d = json.loads(why)
    #use = d['students']
    header = (list(d['students'][0].keys()))
    reader = []
    for dic in d['students']:
      b = (list(dic.values()))
      reader.append(b)

    with open(target_filename, 'w') as csv_file:
        writer = csv.writer(csv_file, delimiter=';')
        writer.writerow(header)
        writer.writerows(reader)

  
if __name__ == "__main__":
    convert_students_to_csv("students.json", "students.csv")
    convert_students_to_csv("students2.json", "students2.csv")






"""     t = "".join(map(str, (h[i*b : b*(i+1)])))
    da.append(t)
    t = "" """
""" print(t)
print(da) """

""" 
    d = json.loads(data)
    
   """
#print(my_string)

