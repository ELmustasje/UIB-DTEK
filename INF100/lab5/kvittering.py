from decimal import Decimal, getcontext


def receipt_content(prices_filename, cash_register_filename):
    """Construct contents of a receipt of the cash register events,
    given the store prices."""
    priceDict = {}
    itemDict = {}

    buyContents = []
    returnContents = []

    total = Decimal("0.00")
    mva = Decimal("0.00")
    paid = Decimal("0.00")
    change = Decimal("0.00")

    with open(prices_filename) as f:
        for csvLine in f:
            listLine = csvLine.strip().split(";")
            priceDict[listLine[0]] = Decimal(listLine[1])

    with open(cash_register_filename) as f:
        for csvLine in f:
            listLine = csvLine.strip().split(";")
            if listLine[0] == "buy":
                itemDict[listLine[1]] = itemDict.get(listLine[1], 0) + 1
            elif listLine[0] == "return":
                itemDict[listLine[1]] = itemDict.get(listLine[1], 0) - 1
            elif listLine[0] == "pay":
                paid = Decimal(listLine[1])

    for key, value in itemDict.items():
        total += priceDict[key] * value
        if value > 0:
            buyContents.append((value, key, priceDict[key] * value))
        elif value < 0:
            returnContents.append((value, key, priceDict[key] * value))

    returnContents.sort(key=lambda tup: tup[1])
    buyContents.sort(key=lambda tup: tup[1])
    contents = buyContents + returnContents

    mva = total * Decimal("0.20")
    change = -1 * (paid - total)

    return (
        contents,
        total,
        mva,
        paid,
        change,
    )


def receipt(prices_filename, cash_register_filename):
    """Construct a receipt of the cash register events,
    given the store prices."""

    # get receipt content from receipt_content()
    purchases_returns, total, vat, payment, change = receipt_content(
        prices_filename, cash_register_filename
    )

    # the formatted lines of the receipt
    receipt_lines = [f"{'Nr.':>4}  {'Item':18}  {'Price':>10}"]

    for nr, item, price in purchases_returns:
        receipt_lines.append(f"{nr:4d}  {item:18}  {price:10.2f}")

    receipt_lines.append(f"Total: {total}")
    receipt_lines.append(f"Of which VAT: {vat:.2f}")
    receipt_lines.append(f"Payment: {payment}")
    receipt_lines.append(f"Change {change}")

    # add some dividers
    max_len = max(len(line) for line in receipt_lines)
    divider = "-" * max_len
    receipt_lines.insert(1, divider)
    receipt_lines.insert(-4, divider)
    receipt_lines.insert(-2, divider)

    return "\n".join(receipt_lines)
