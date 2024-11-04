def count_overlap(path1, path2):
    with open(path1, 'r') as file1:
        dna_sequences1 = [line.strip() for line in file1 if line.strip()]

    with open(path2, 'r') as file2:
        dna_sequences2 = [line.strip() for line in file2 if line.strip()]

    dna_set1 = set(dna_sequences1)
    
    overlap_count = sum(1 for seq in dna_sequences2 if seq in dna_set1)

    return overlap_count


def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')

if __name__ == "__main__":
    test_count_overlap_sample()