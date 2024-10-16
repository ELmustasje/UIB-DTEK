# snake_tests.py
from snake_view import colour
from snake_main import apple

def run_all():
    ''' Run all tests for the snake program '''
    # As you add more test functions to this file, call them here
    test_get_color()
    test_apple()


def test_get_color():
    print('Tester get_color...', end='')
    assert 'cyan' == colour(-1)
    assert 'lightgray' == colour(0)
    assert 'orange' == colour(1)
    assert 'orange' == colour(42)
    print('OK')




def test_apple():
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
        y,x = a
        apple(y,x)
        assert a in legal_states
    print('OK')

if __name__ == '__main__':
    print('Starting snake_test.py')
    run_all()
    print('Finished snake_test.py')