from datetime import datetime, timedelta, timezone
import json
import csv
import matplotlib.pyplot as plt
import requests
from pathlib import Path
import io


#del a

def get_earthquakes_csv_string(n, magnitude):

    end_time = datetime.now(timezone.utc)
    start_time = end_time - timedelta(days=n)
    my_datetime_object = start_time
    my_iso8601_string = my_datetime_object.strftime('%Y-%m-%dT%H:%M:%S%z')
    #n er antall dager og magnitude er styrken på jordskjelvet
    #dersom det er flere enn 5000 slike jordskjelv, skal kun de 5000 sterkeste inkluderes
    baseurl = 'https://earthquake.usgs.gov/fdsnws/event/1/query'
    headers = {'User-Agent': 'no.uib.ii.inf100.h24.lab7.makar9281'}
    params = {
    'format': 'csv',
    'starttime': f'{my_iso8601_string}', # ISO 8601 format
    'endtime': f'{end_time}', # ISO 8601 format
    'minmagnitude': magnitude,
    'orderby': 'magnitude',
    'limit': 5000,
    }

    response = requests.get(baseurl, params=params, headers=headers)
    content = response.content.decode('utf-8')
    

    dict_list = csv.DictReader(content.splitlines())
    earthquakes = [row for row in dict_list]
    return content


#del b

def get_earthquake_list(csv_string):
    reader = csv.DictReader(io.StringIO(csv_string), delimiter=',', quotechar='"')
    headers = reader.fieldnames
    earthquake = []
    for row in reader:
        lengdegrad = float(row['longitude'])
        breddegrad = float(row['latitude'])
        try:
            row['mag'] == int
            styrke = int(row['mag'])
        except:
            row['mag'] == float
            styrke = float(row['mag'])
        earthquake.append((lengdegrad, breddegrad, styrke))
    return earthquake


#del c

def plot_earthquakes(data_points):
    x_verdi = []
    y_verdi = []
    styrke = []
    size_list = []
    #size = (3**styrke['mag'])/10
    for value in data_points:
        x_verdi.append(value[0])
        y_verdi.append(value[1])
        styrke.append(value[2])
        size = (3**value[2])/10
        size_list.append(size)
    
    
    plt.scatter(x_verdi, y_verdi, s=size_list, alpha=0.2)
    

#del d
def load_coastlines():
    with open('ne_110m_coastline.json', 'r', encoding='utf-8') as json_file:
        json_dict = json.load(json_file)
    
    islands = []
    island_total = []

    for island in json_dict['features']:
        for coordinates in island['geometry']['coordinates']:
            islands.append(coordinates)
        island_total.append(islands)
        islands = []
    return island_total
        
#del e
def plot_coastlines(lines):

    for value in lines:
        x_verdi = []
        y_verdi = []
        for x_y in value:
            x_verdi.append(x_y[0])
            y_verdi.append(x_y[1])
        
        plt.plot(x_verdi, y_verdi, color = 'grey')
    
    
if __name__ == "__main__":
    plt.figure(figsize=(12,8))
    plot_earthquakes(get_earthquake_list(get_earthquakes_csv_string(50, 4)))
    plot_coastlines(load_coastlines())
    plt.title('Blue = places you should reconsider visiting')
    plt.grid(True)
    plt.show()

#Jeg har samarbeidet med Niklas og fått hjelp av Jo, Åsmund og Noah