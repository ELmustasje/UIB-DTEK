from snake_view import get_color
from snake_main import add_apple_at_random_location
from snake_main import is_legal_move
from snake_main import get_next_head_position



def run_all():
    ''' Run all tests for the snake program '''
    # As you add more test functions to this file, call them here
    test_get_color()
    test_add_apple_at_random_location()  
    test_is_legal_move()  
    test_get_next_head_position()


def test_get_color():
    print('Tester get_color...', end='')
    assert 'cyan' == get_color(-1)
    assert 'lightgray' == get_color(0)
    assert 'orange' == get_color(1)
    assert 'orange' == get_color(42)
    print('OK')

def test_get_next_head_position():
    print('Tester get_next_head_position...', end='')
    assert (3, 9) == get_next_head_position((3, 8), 'east')
    assert (3, 7) == get_next_head_position((3, 8), 'west')
    assert (2, 8) == get_next_head_position((3, 8), 'north')
    assert (4, 8) == get_next_head_position((3, 8), 'south')
    assert (1, 6) == get_next_head_position((1, 5), 'east')
    assert (1, 4) == get_next_head_position((1, 5), 'west')
    assert (0, 5) == get_next_head_position((1, 5), 'north')
    assert (2, 5) == get_next_head_position((1, 5), 'south')
    print('OK')


def test_add_apple_at_random_location():
    print('Tester add_apple_at_random_location...', end='')
    NUMBER_OF_RUNS = 1000
    legal_states = [
        [[2, 3, -1, -1], [1, 0, 0, 0]],
        [[2, 3, -1, 0], [1, -1, 0, 0]],
        [[2, 3, -1, 0], [1, 0, -1, 0]],
        [[2, 3, -1, 0], [1, 0, 0, -1]],
    ]
    counters = [0] * len(legal_states)
    for _ in range(NUMBER_OF_RUNS):
        a = [[2, 3, -1, 0], [1, 0, 0, 0]]
        add_apple_at_random_location(a)
        assert a in legal_states
    print('OK')


def test_is_legal_move():
    print('Tester is_legal_move...', end='')
    board = [
        [0, 3, 4],
        [0, 2, 5],
        [0, 1, 0],
        [-1, 0, 0],
    ]

#NB!!! i dennde testkoden måtte jeg legge til snake_body i testen ettersom at jeg hadde det som ett argument i main koden min
def test_is_legal_move():
    print('Tester is_legal_move...', end='')
    board = [
        [0, 3, 4],
        [0, 2, 5],
        [0, 1, 0],
        [-1, 0, 0],
    ]
    snake_body = [(2, 1), (1, 1), (0, 1), (0, 2)]  # Legg til slangens kropp

    assert is_legal_move((2, 2), board, snake_body) is True
    assert is_legal_move((1, 3), board, snake_body) is False  # Utenfor brettet
    assert is_legal_move((1, 1), board, snake_body) is False  # Krasjer med seg selv
    assert is_legal_move((0, 2), board, snake_body) is False  # Krasjer med seg selv

    assert is_legal_move((0, 0), board, snake_body) is True
    assert is_legal_move((3, 0), board, snake_body) is True  # Eplets posisjon er lovlig
    assert is_legal_move((3, 2), board, snake_body) is True
    assert is_legal_move((-1, 0), board, snake_body) is False  # Utenfor brettet
    assert is_legal_move((0, -1), board, snake_body) is False  # Utenfor brettet
    assert is_legal_move((3, -1), board, snake_body) is False  # Utenfor brettet
    assert is_legal_move((3, 3), board, snake_body) is False  # Utenfor brettet
    assert is_legal_move((4, 2), board, snake_body) is False  # Utenfor brettet

    print('OK')

if __name__ == '__main__':
    print('Starting snake_test.py')
    run_all()
    print('Finished snake_test.py')


