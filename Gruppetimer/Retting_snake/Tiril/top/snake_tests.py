# snake_tests.py
from snake_view import get_color
from snake_view import draw_board
from snake_main import ny_hode_pos
from snake_main import is_legal_move

#tester
if __name__ == '__main__':
    def test_ny_hode_pos():
        print('Tester eple_pos...', end='')
        assert (3, 9) == ny_hode_pos((3, 8), 'east')
        assert (3, 7) == ny_hode_pos((3, 8), 'west')
        assert (2, 8) == ny_hode_pos((3, 8), 'north')
        assert (4, 8) == ny_hode_pos((3, 8), 'south')
        assert (1, 6) == ny_hode_pos((1, 5), 'east')
        assert (1, 4) == ny_hode_pos((1, 5), 'west')
        assert (0, 5) == ny_hode_pos((1, 5), 'north')
        assert (2, 5) == ny_hode_pos((1, 5), 'south')
        print('OK')

    def test_is_legal_move():
        print('Tester is_legal_move...', end='')
        board = [
            [0, 3, 4],
            [0, 2, 5],
            [0, 1, 0],
            [-1, 0, 0],
        ]
        assert is_legal_move((2, 2), board) is True
        assert is_legal_move((1, 3), board) is False # Utenfor brettet
        assert is_legal_move((1, 1), board) is False # Krasjer med seg selv
        assert is_legal_move((0, 2), board) is False # Krasjer med seg selv

        assert is_legal_move((0, 0), board) is True
        assert is_legal_move((3, 0), board) is True # Eplets posisjon er lovlig
        assert is_legal_move((3, 2), board) is True
        assert is_legal_move((-1, 0), board) is False # Utenfor brettet
        assert is_legal_move((0, -1), board) is False # Utenfor brettet
        assert is_legal_move((3, -1), board) is False # Utenfor brettet
        assert is_legal_move((3, 3), board) is False # Utenfor brettet
        assert is_legal_move((4, 2), board) is False # Utenfor brettet
        print('OK')
    
    test_is_legal_move()
    test_ny_hode_pos()

    from uib_inf100_graphics.event_app import run_app
    run_app(width=500, height=400, title='Snake')



    def run_all():
        ''' Run all tests for the snake program '''
        # As you add more test functions to this file, call them here
        test_get_color()


    def test_get_color():
        print('Tester get_color...', end='')
        assert 'yellow' == get_color(-1)
        assert 'white' == get_color(0)
        assert 'pink' == get_color(1)
        assert 'pink' == get_color(42)
        print('OK')



    from uib_inf100_graphics.simple import canvas, display

    print('Starting snake_test.py')
    run_all()
    print('Finished snake_test.py')
    test_board = [
        [1, 2, 3, 0, 5, 4,-1,-1, 1, 2, 3],
        [0, 4, 0, 7, 0, 3,-1, 0, 0, 4, 0],
        [0, 5, 0, 8, 1, 2,-1,-1, 0, 5, 0],
        [0, 6, 0, 9, 0, 0, 0,-1, 0, 6, 0],
        [0, 7, 0,10,11,12,-1,-1, 0, 7, 0],
    ]

    draw_board(canvas, 25, 25, 375, 375, test_board, True)
    display(canvas)

