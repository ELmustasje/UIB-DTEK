def compress(raw_binary):
    for i in raw_binary:
        if i == "1" or i == "0":
            None
        else:
            print("stengen kan bare være 0 eller 1")
            raise TypeError

    comp = []
    count = 1

    if raw_binary[0] == "1":
        comp.append(0)

    for i in range(len(raw_binary) - 1):
        if raw_binary[i] == raw_binary[i + 1]:
            count += 1
        else:
            comp.append(count)
            count = 1
    comp.append(count)
    return comp


def decompress(compressed_binary):
    raw = ""
    sym = -1
    for i in compressed_binary:
        if sym == 1:
            raw += "1" * i
            sym *= -1
        else:
            raw += "0" * i
            sym *= -1
    return raw
