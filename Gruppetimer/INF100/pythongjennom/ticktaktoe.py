import os

board = {
    "TL": " ",
    "T": " ",
    "TR": " ",
    "ML": " ",
    "M": " ",
    "MR": " ",
    "DL": " ",
    "D": " ",
    "DR": " ",
}


def printBoard(board):
    print(board["TL"] + "|" + board["T"] + "|" + board["TR"])
    print("-+-+-")
    print(board["ML"] + "|" + board["M"] + "|" + board["MR"])
    print("-+-+-")
    print(board["DL"] + "|" + board["D"] + "|" + board["DR"])


def checkFrom(valg):
    val = board.values()
    if valg in board.keys():
        board["TL"].key


turn = 0
while True:
    printBoard(board)
    valg = input("velg en rute, X begynner")
    if valg in board:
        print("1")
        if board[valg] == " ":
            print("2")
            if turn == 0:
                board[valg] = "X"
                turn = 1
            else:
                board[valg] = "O"
                turn = 0
            os.system("CLS")
        else:
            print("allerede tatt, vel en annen")
    check = list(board.keys())
