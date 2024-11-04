from decimal import *
from pathlib import Path
import os
directory_of_current_file = os.path.dirname(__file__)
os.chdir(directory_of_current_file) # endrer cwd

def receipt_content(prices_filename, cash_register_filename):
    """Construct contents of a receipt of the cash register events,
    given the store prices."""

    #   Starter med å lese filene
    prices = Path(prices_filename).read_text(encoding="utf-8")
    cash_register = Path(cash_register_filename).read_text(encoding="utf-8")

    #   Første element i tuplen skal være en liste av tupler med (antall, produkt, total pris)
    prices = prices.splitlines()
    price_2d_list = []
    for price in prices:
        price = price.split(";")
        price_2d_list.append(price)
    
    #   Skal opprette en dict med pris for hver vare

    price_for_wares = dict()
    for ware in price_2d_list:
        price_for_wares[ware[0]] = ware[1]

    event_2d_list = []
    cash_register = cash_register.splitlines()
    for cash in cash_register:
        cash = cash.split(";")
        event_2d_list.append(cash)

    buy = dict()
    returned = dict()
    pay = dict()

    #   Nå lager jeg en dict med varene som er kjøpt/returnert, og hvor mange
    for event in range(len(event_2d_list)):
        if event_2d_list[event][0] == "buy":
            try:
                buy[event_2d_list[event][1]] = buy[event_2d_list[event][1]] + 1
            except:
                buy[event_2d_list[event][1]] = 1
        if event_2d_list[event][0] == "return":
            try:
                returned[event_2d_list[event][1]] = returned[event_2d_list[event][1]] + 1
            except:
                returned[event_2d_list[event][1]] = 1
        #if event_2d_list[event][0] == "pay":
            #pay("pay") = event_2d_list[event][1]
        
    #   Nå skal lista med tupler lages!

    item_events = []
    bought_item_events = []
    returned_item_events = []
    for i in buy:
        tuple_1 = (buy[i], i, Decimal(buy[i] * Decimal(price_for_wares[i])))
        bought_item_events.append(tuple_1)
    bought_item_events = sorted(bought_item_events, key = lambda x:x[1])
    #   Sorteres basert på det andre elementet i tuplen
    i = 0
    for i in returned:
        tuple_1 = (-returned[i], i, -Decimal(returned[i] * Decimal(price_for_wares[i])))
        returned_item_events.append(tuple_1)
    returned_item_events = sorted(returned_item_events, key = lambda x:x[1])
    item_events = bought_item_events + returned_item_events
    
    #   item-events er en liste med tupler: (antall, produkt, pris)
    #   Det andre elementet i hovedtupelen er den totale prisen

    total_price = 0
    for element in item_events:
        total_price += element[2]
    
    #   Nå skal jeg finne ut av andelen mva.
    vat = total_price * Decimal(0.2)

    #   Nå skal jeg finne ut hvor mye som blei betalt

    payment = Decimal(event_2d_list[len(event_2d_list)-1][1])
    
    #   Sist: hvor mye som blei betalt tilbake
    
    change = total_price - payment
    
    #   Nå oppretter jeg tidenes tuple

    return(item_events, total_price, vat, payment, change)




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
assert(expected_value == receipt("prices.txt", "cash_register.txt"))
print("OK")
