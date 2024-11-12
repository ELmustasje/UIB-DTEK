from qr6_error_correction import generate_error_correction

def string_to_data(content_string):
    rl = list()
    for c in content_string:
        d = c.encode("ascii")
        d = int(d[0])
        nl = [0] * 8
        for i in range(7,0,-1):
            nl[i] = d & 0x1
            d >>= 1
        rl.extend(nl)
    return rl

def get_core_bit_list(content_string):
    mode = [0,1,0,0]
    length = len(content_string)
    data = string_to_data(content_string)
    la = [0] * 8
    for i in range(7,-1,-1):
        la[i] = length & 0x1
        length >>= 1
    return mode + la + data + [0,0,0,0]

def pad_bit_list(core_bit_list, pad_to_bytes):
    padding = [[1, 1, 1, 0, 1, 1, 0, 0],[0, 0, 0, 1, 0, 0, 0, 1]]
    bits = 8 * pad_to_bytes
    i = 0
    while len(core_bit_list) < bits:
        core_bit_list.extend(padding[i%2])
        i+=1

def string_to_bit_list(content_string, qr_layout):
    max_size = qr_layout["byte_capacity"] * 8
    cbl = get_core_bit_list(content_string)
    ecorr_code = ["L","M","Q","H"]
    rs = max_size - len(cbl)
    assert(rs >= 80)
    ai = (rs - 80)//48
    err_corr = ecorr_code[min(ai,3)]
    rs -= 80 + ai * 48
    if(rs > 0):
        pad_bit_list(cbl, rs//8)
    corr_bits = generate_error_correction(cbl, err_corr)
    
    return(corr_bits, err_corr)