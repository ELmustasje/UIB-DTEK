from pathlib import Path
import os
import json

directory_of_current_file = os.path.dirname(__file__)
os.chdir(directory_of_current_file) # endrer cwd

def convert_students_to_csv(path, destination):
    json_string = Path(path).read_text(encoding="utf-8")
    json_dict = json.loads(json_string)

    rows = []
    first_row = []
    for key in json_dict["students"][0].keys():
        first_row.append(key)
    rows.append(first_row)

    for count in range(len(json_dict["students"])):
        row = []
        for i in json_dict["students"][count]:
            row.append(str(json_dict["students"][count][i]))
        rows.append(row)
    
    for j in range(len(rows)):
        rows[j] = ";".join(rows[j])
    csv_file = "\n".join(rows)

    Path(destination).write_text(csv_file, encoding = "utf-8")

if __name__ == "__main__":
    convert_students_to_csv("students.json", "students.csv")
    convert_students_to_csv("students2.json", "students2.csv")
