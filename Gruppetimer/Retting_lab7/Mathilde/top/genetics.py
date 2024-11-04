from pathlib import Path
import csv
import io


def count_overlap(path1, path2):
    file_1 = Path(path1).read_text(encoding='utf-8')
    file_1_split = file_1.splitlines()
    file_2 = Path(path2).read_text(encoding='utf-8')
    file_2_split = file_2.splitlines()

    file_mengde = set(file_1_split)
    

    count = 0

    for sequense in file_2_split:
        if sequense in file_mengde:
            count += 1
    return count



def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')

    # Tester effektivitet (testen tar laaang tid ved feil løsning):
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')

if __name__ == "__main__":
    test_count_overlap_sample()