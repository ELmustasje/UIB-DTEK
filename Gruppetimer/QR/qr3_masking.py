from qr2_matrix_completion import set_fixed_fields, set_meta_fields

def should_flip(row, col, mask_no):
    r = False
    match mask_no:
        case 0:
            r = (row+col)%2 == 0
        case 1:
            r = row % 2 == 0
        case 2:
            r = col % 3 == 0
        case 3:
            r = (row + col) % 3 == 0
        case 4:
            r = (row //2 + col//3) % 2 == 0
        case 5:
            r = (row*col) % 2 +(row*col) % 3 == 0
        case 6:
            r = ((row*col)%2+(row*col)%3)%2==0
        case 7:
            r = ((row+col)%2+(row*col)%3)%2==0
    return(r)

def create_empty_matrix_copy(matrix):
    rm = list()
    for y in range(len(matrix)):
        rm.append(list())
        for x in matrix[y]:
            rm[y].append(0)
    return(rm)

def get_masked_matrix(matrix, mask_no):
    rm = create_empty_matrix_copy(matrix)
    for y in range(len(matrix)):
        for x in range(len(matrix[y])):
            if should_flip(y, x, mask_no):
                rm[y][x] = int(not bool(matrix[y][x]))
            else:
                rm[y][x] = matrix[y][x]
    return(rm)

def score_matrix(matrix):
    sums = [0,0]
    for y in matrix:
        for x in y:
            sums[x] += 1
    return abs(sums[0]-sums[1])

def get_refined_matrix(raw_matrix, error_correction_level, qr_layout):
    l = [None, (2<<32)-1]
    for i in range(8):
        matrix = get_masked_matrix(raw_matrix, i)
        set_fixed_fields(matrix, qr_layout)
        set_meta_fields(matrix, error_correction_level, i, qr_layout)
        s = score_matrix(matrix)
        if s < l[1]:
            l[0] = matrix
            l[1] = s
    return l[0]