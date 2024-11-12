def get_next_pos(row, column, size):
    if column == 0:
        return (row-1, column)
    if column % 2 == 0:
        return (row, column-1)
#   if row == size-1:
#       return (row, column-1)
    if column % 4 == 1:
        if row == size-1:
            return (row, column-1)
        return (row+1,column+1)
#    if row == 0:
#        return (row, column-1)
    if column % 4 == 3:
        if row == 0:
            return (row, column-1)
        return (row-1, column+1)


def bit_list_to_raw_matrix(bit_list, qr_layout):
    illegal_pos = list()
    matrix = list()
    ipn = ["fixed_positions", "meta_positions"]
    ipa = [["zeros", "ones"], ["first", "second"]]
    for i in range(len(ipn)):
        for d in range(len(ipa[i])):
            for e in qr_layout[ipn[i]][ipa[i][d]]:
                illegal_pos.append((e[0], e[1]))
    sl = qr_layout["side_length"]

    for y in range(sl):
        matrix.append(list())
        for x in range(sl):
            matrix[y].append(0)
    i = 0
    ct = (sl-1,sl-1)
    while ct != (-1,0):
        if ct in illegal_pos:
            ct = get_next_pos(ct[0], ct[1], sl)
            continue
        try:
            c = bit_list[i]
        except:
            break
        matrix[ct[0]][ct[1]] = c
        ct = get_next_pos(ct[0], ct[1], sl)
        i += 1
    assert(i >= len(bit_list))
    return matrix
