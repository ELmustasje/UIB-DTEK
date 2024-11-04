def count_overlap (path1, path2):
    file1 = open(path1, 'r')
    sequences1 = set(line.strip() for line in file1 if line.strip())  # Fjern newline og tomme linjer
    file1.close()

    file2 = open(path2, 'r')
    sequences2 = [line.strip() for line in file2 if line.strip()]  # Fjern newline og tomme linjer
    file2.close()

    overlap_count = sum(1 for sequence in sequences2 if sequence in sequences1)

    return overlap_count

def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')

    # Tester effektivitet (testen tar laaang tid ved feil løsning):
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')
