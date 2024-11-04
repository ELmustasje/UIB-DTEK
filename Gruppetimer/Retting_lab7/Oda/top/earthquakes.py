import requests 
from datetime import datetime, timezone, timedelta
import csv
import matplotlib.pyplot as plt 
import json


def get_earthquakes_csv_string(n, magnitude):
    baseurl = 'https://earthquake.usgs.gov/fdsnws/event/1/query'
    headers = {'User-Agent': 'no.uib.ii.inf100.h24.odhol2109'}

    end_time = datetime.now(timezone.utc)
    start_time = end_time - timedelta(days=n)

    end_time_streng = end_time.strftime('%Y-%m-%dT%H:%M:%S%z')
    start_time_streng = start_time.strftime('%Y-%m-%dT%H:%M:%S%z')

    params = {
        'format': 'csv',
        'starttime': start_time_streng,
        'endtime': end_time_streng,
        'minmagnitude': magnitude,
        'orderby': 'magnitude',
        'limit': 5000,
    }

    response = requests.get(baseurl, params=params, headers=headers)
    content = response.content.decode('utf-8')
    return content

def get_earthquake_list(csv_string):
    lines = csv_string.split("\n")
    reader = csv.DictReader(lines)
    
    earthquake_data = [
        (float(row['longitude']), float(row['latitude']), float(row['mag']))
        for row in reader if row['latitude'] and row['longitude'] and row['mag']
    ]
    
    return earthquake_data

def plot_earthquakes(data_points):
    xs = []
    ys = []
    sizes = []

    for point in data_points:
        longitude, latitude, magnitude = point
        xs.append(longitude)
        ys.append(latitude)
        sizes.append((3 ** magnitude) / 10)

    plt.scatter(xs, ys, s=sizes, color='blue', alpha=0.2)

def load_coastlines():
    with open("ne_110m_coastline.json", 'r', encoding='utf-8') as file:
        data = json.load(file)
    
    lines = []

    for island in data['features']:
        coordinates = island['geometry']['coordinates']
        lines.append(coordinates)
    
    return lines 

def plot_coastlines(islands):
    for island in islands:
       
        x_values = [coord[0] for coord in island]
        y_values = [coord[1] for coord in island]
        
        plt.plot(x_values, y_values,) #bedre uten grey

if __name__ == "__main__":
    plt.figure(figsize=(12, 8))
    csv_data = get_earthquakes_csv_string(50, 4)
    earthquake_list = get_earthquake_list(csv_data)
    coastlines = load_coastlines()  
    plot_coastlines(coastlines)  
    plot_earthquakes(earthquake_list)  
    plt.grid(True)
    plt.show() #nederst ja
        


    
    
    




