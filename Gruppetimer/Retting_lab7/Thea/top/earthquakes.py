
import requests
from datetime import datetime, timezone, timedelta
import csv
import io
import matplotlib.pyplot as plt
import json

def get_earthquakes_csv_string(n, magnitude):
    
    end_time = datetime.now(timezone.utc)
    start_time = end_time - timedelta(days=n)
    my_iso8601_string_end_time = end_time.strftime('%Y-%m-%dT%H:%M:%S%z')
    my_iso8601_string_start_time = start_time.strftime('%Y-%m-%dT%H:%M:%S%z')
    
    baseurl = 'https://earthquake.usgs.gov/fdsnws/event/1/query'
    headers = {'User-Agent': 'no.uib.ii.inf100.h24.lab7.Thea Louise'}
    params = {
    'format': 'csv',
    'starttime': my_iso8601_string_start_time,
    'endtime': my_iso8601_string_end_time,
    'minmagnitude': magnitude,
    'orderby': 'magnitude',
    'limit': 5000,
    }
    response = requests.get(baseurl, params=params, headers=headers)
    content = response.content.decode('utf-8')
    return content


def get_earthquake_list(csv_string):
    
    reader = csv.DictReader(io.StringIO(csv_string))
    earthquake_list = []

    for row in reader:
        latitude = float(row['latitude'])
        longitude = float(row['longitude'])
        magnitude = float(row['mag'])
        earthquake_list.append((longitude, latitude, magnitude))
    return earthquake_list


def plot_earthquakes(data_points):

    xs = []
    ys = []
    sizes = []
    for longitude, latitude, magnitude in data_points:
        xs.append(longitude)
        ys.append(latitude)
        sizes.append(3**magnitude/10)
    plt.scatter(xs, ys, s=sizes, alpha=0.2)


def load_coastlines():
    with open('ne_110m_coastline.json', 'r', encoding='utf-8') as json_file:
      data = json.load(json_file)
    lines = []
    for island in data['features']:
        a = island['geometry']['coordinates']
        lines.append(a)
    return lines

def plot_coastlines(islands):
    
    for line in islands:
        xs, ys = zip(*line)
        plt.plot(xs, ys, color='grey')
    

if __name__ == "__main__":
    plt.figure(figsize=(12, 8))
    plt.grid(True)
    c = load_coastlines()
    plot_coastlines(c)
    s = get_earthquakes_csv_string(50, 4.0)
    l = get_earthquake_list(s)
    p = plot_earthquakes(l)
    plt.title('Global Earthquake Activity')
    plt.show()

