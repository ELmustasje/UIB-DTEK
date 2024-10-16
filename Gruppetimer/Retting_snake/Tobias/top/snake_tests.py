# snake_tests.py
from snake_view import get_color


def run_all():
    ''' Run all tests for the snake program '''
    # As you add more test functions to this file, call them here
    test_get_color()


def test_get_color():
    print('Tester get_color...', end='')
    assert 'cyan' == get_color(-1)
    assert 'lightgray' == get_color(0)
    assert 'orange' == get_color(1)
    assert 'orange' == get_color(42)
    print('OK')


if __name__ == '__main__':
    print('Starting snake_test.py')
    run_all()
    print('Finished snake_test.py')
