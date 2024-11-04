import json

def convert_students_to_csv(json_path, csv_path):
    with open(json_path, 'r') as json_file:
        data = json.load(json_file)

    csv_lines = ["id;name;area;year"]
    
    for student in data['students']:
        csv_line = f"{student['id']};{student['name']};{student['area']};{student['year']}"
        csv_lines.append(csv_line)

    with open(csv_path, 'w') as csv_file:
        csv_file.write("\n".join(csv_lines))

if __name__ == "__main__":
    convert_students_to_csv("students.json", "students.csv")
    convert_students_to_csv("students2.json", "students2.csv")