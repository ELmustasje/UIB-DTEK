from pathlib import Path
def count_overlap(path1, path2):
    path1 = Path(path1).read_text(encoding="utf-8")
    path2 = Path(path2).read_text(encoding="utf-8")
    return check_overlap(path1, path2)


def check_overlap(path, path11):
    path = path.split("\n")
    path11 = path11.split("\n")
    path = [i for i in path if i]
    path11 = [i for i in path11 if i]
    order = set(path).intersection(path11)
    
    return len(order)





 
""" def test_count_overlap_sample():
    print('Tester count_overlap... ', end='')
    assert 2 == count_overlap('sample1.txt', 'sample2.txt')

    # Tester effektivitet (testen tar laaang tid ved feil løsning):
    assert 100001 == count_overlap('id1.txt', 'id2.txt')
    print('OK')

test_count_overlap_sample() """