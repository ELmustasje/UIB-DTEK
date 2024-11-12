
import json
import csv


def convert_students_to_csv(json_filepath, csv_filepath):

  with open(json_filepath, 'r', encoding='utf-8') as json_file:
      data = json.load(json_file)
    
    
  with open(csv_filepath, 'w', encoding='utf-8', newline='') as csv_file:
        writer = csv.writer(csv_file, delimiter=';')
        writer.writerow(['id', 'name', 'area', 'year'])
        for student in data['students']:
            writer.writerow([student['id'], student['name'], student['area'], student['year']])



if __name__ == "__main__":
    convert_students_to_csv("students.json", "students.csv")
    convert_students_to_csv("students2.json", "students2.csv")
