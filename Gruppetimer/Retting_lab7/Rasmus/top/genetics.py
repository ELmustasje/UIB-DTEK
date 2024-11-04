from pathlib import Path
import os

directory_of_current_file = os.path.dirname(__file__)
os.chdir(directory_of_current_file) # endrer cwd

def count_overlap(path1, path2):
    dna_1_list = Path(path1).read_text(encoding="utf-8").splitlines()
    dna_2_list = Path(path2).read_text(encoding="utf-8").splitlines()

    dna_1 = set()
    dna_2 = set()
    for i in dna_1_list:
        dna_1.add(i)    #   Gjør om lister til mengder
    i = 0
    for i in dna_2_list:
        dna_2.add(i)

    matches = 0
    for seq in dna_1:
        if seq in dna_2:
            matches += 1
    return matches

def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')

    # Tester effektivitet (testen tar laaang tid ved feil løsning):
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')

test_count_overlap_sample()