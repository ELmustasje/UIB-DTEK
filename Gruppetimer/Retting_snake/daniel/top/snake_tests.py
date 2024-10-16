from snake_view import is_corner
from snake_main import is_legal_move

def run_all():
    ''' Run all tests for the snake program '''
    test_is_legal_move()
    test_is_corner()

def test_is_legal_move():
    print('Tester is_legal_move...', end='')
    board = [
        [0, 3, 4],
        [0, 2, 5],
        [0, 1, 0],
        [-1, 0, 0],
    ]
    assert is_legal_move((2, 2), board) is True
    assert is_legal_move((1, 3), board) is False
    assert is_legal_move((1, 1), board) is False
    assert is_legal_move((0, 2), board) is False
    assert is_legal_move((0, 0), board) is True
    assert is_legal_move((3, 0), board) is True
    assert is_legal_move((3, 2), board) is True
    assert is_legal_move((-1, 0), board) is False
    assert is_legal_move((0, -1), board) is False
    assert is_legal_move((3, -1), board) is False
    assert is_legal_move((3, 3), board) is False
    assert is_legal_move((4, 2), board) is False
    print('OK')

def test_is_corner():
    print('Tester is_corner...', end='')
    board = [
    [0, 1, 0, 0],
    [0, 1, 0, 0],
    [0, 1, 1, 1],
    [0, 0, 0, 0]
]
    assert is_corner(board, 1, 1) is False
    assert is_corner(board, 2, 1) is True
    assert is_corner(board, 2, 2) is False
    print("OK")


if __name__ == '__main__':
    print('Tester...')
    run_all()
    print('Ferdig!')
