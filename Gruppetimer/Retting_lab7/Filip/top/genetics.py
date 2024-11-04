def count_overlap(path1, path2):
    with open(path1, 'r') as file1:
        sequences1 = {line.strip() for line in file1 if line.strip()}

    with open(path2, 'r') as file2:
        sequences2 = {line.strip() for line in file2 if line.strip()}

    overlap = sequences1.intersection(sequences2)

    return len(overlap)

def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')

    # Tester effektivitet (testen tar laaang tid ved feil løsning):
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')

       