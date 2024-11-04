import json
import csv

def convert_students_to_csv(JSON_filepath, CSV_filepath):
    with open(JSON_filepath, 'r', encoding='utf-8') as json_file:
        data = json.load(json_file)

    with open(CSV_filepath, 'w', encoding='utf-8') as csv_file:

        headers = ['id', 'name', 'area', 'year']
        writer = csv.DictWriter(csv_file, fieldnames = headers, delimiter=';')

        writer.writeheader()
        

        for student in data['students']:
            writer.writerow(student)

if __name__ == "__main__":
    convert_students_to_csv("students.json", "students.csv")
    convert_students_to_csv("students2.json", "students2.csv")
