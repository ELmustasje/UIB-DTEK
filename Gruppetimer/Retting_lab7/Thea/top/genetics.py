
from pathlib import Path

def count_overlap(path1, path2):
    
    with open(path1, 'r', encoding='utf-8') as file1:
        lines1 =file1.readlines()
    lines1 = [line.strip() for line in lines1 if line.strip()]

    with open(path2, 'r', encoding='utf-8') as file2:
        lines2 = file2.readlines()
    lines2 = [line.strip() for line in lines2 if line.strip()]

    amount = set(lines1)

    common_elements = set(lines2) & amount
    count_common = len(common_elements)

    return count_common



def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')

    # Tester effektivitet (testen tar laaang tid ved feil løsning):
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')

test_count_overlap_sample()
