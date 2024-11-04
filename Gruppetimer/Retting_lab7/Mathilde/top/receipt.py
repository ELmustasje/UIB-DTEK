from decimal import *
from pathlib import Path

def receipt_content(prices_filename, cash_register_filename):
    """Construct contents of a receipt of the cash register events,
    given the store prices."""
    
    price_dict = {}
    item_dict = {}

    buy_contents = []
    return_contents = []

    total = Decimal("0.00")
    mva = Decimal("0.00")
    paid = Decimal("0.00")
    change = Decimal("0.00")



    with open(prices_filename, 'r', encoding='utf-8') as price_file:
        for csv_line in price_file:
            list_line = csv_line.strip().split(";")
            price_dict[list_line[0]] = Decimal(list_line[1])

    with open(cash_register_filename, 'r', encoding='utf-8') as register_file:
        for csv_line in register_file:
            list_line = csv_line.strip().split(";")
            if list_line[0] == "buy":
                item_dict[list_line[1]] = item_dict.get(list_line[1], 0) + 1
            elif list_line[0] == "return":
                item_dict[list_line[1]] = item_dict.get(list_line[1], 0) - 1
            elif list_line[0] == "pay":
                paid = Decimal(list_line[1])
            
    for key, value in item_dict.items():
        total += price_dict[key] * value
        if value > 0:
            buy_contents.append([value, key, price_dict[key] * value])
        elif value < 0:
            return_contents.append([value, key, price_dict[key] * value])
    
    return_contents.sort(key=lambda tup: tup[1])
    buy_contents.sort(key=lambda tup: tup[1])
    contents = buy_contents + return_contents

    mva = total * Decimal("0.20")
    change = -1 * (paid - total)
    return (
        contents,
        total,
        mva,
        paid,
        change
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

print("Tester receipt... ", end="")
expected_value = """\
 Nr.  Item                     Price
------------------------------------
   2  apple                    10.00
   1  chips                    24.30
   1  dish soap                26.20
   1  frozen pizza             54.40
   1  peanuts                  18.50
   1  toilet paper             34.00
   3  tomato                   30.00
  -1  pocket book            -149.00
  -1  toothpaste              -13.70
------------------------------------
Total: 34.70
Of which VAT: 6.94
------------------------------------
Payment: 100.00
Change -65.30"""

accutal = receipt("prices.txt", "cash_register.txt")
assert(expected_value == accutal)

print("OK")

#Samarbeidet med Mathilde