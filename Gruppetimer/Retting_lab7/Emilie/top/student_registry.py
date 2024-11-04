import json
def convert_students_to_csv (json_file_path, csv_file_path):
    json_file = open(json_file_path, 'r')
    json_string = json_file.read()
    json_file.close()

    data = json.loads(json_string)

    csv_content = 'id;name;area;year\n'

    for student in data ['students']:
        student_data = f'{student['id']};{student['name']};{student['area']};{student['year']}\n'
        csv_content += student_data

    csv_file = open (csv_file_path, 'w')
    csv_file.write(csv_content)
    csv_file.close()

if __name__ == "__main__":
    convert_students_to_csv("students.json", "students.csv")
    convert_students_to_csv("students2.json", "students2.csv")
