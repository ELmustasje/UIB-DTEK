import requests
from datetime import datetime, timezone, timedelta
import json
import csv
import io
from matplotlib import pyplot as plt
from pathlib import Path


def load_coastlines():
    why = Path('ne_110m_coastline.json').read_text(encoding='utf-8')
    data = json.loads(why)
    isl = []
    for island in data['features']:
       isl.append(island['geometry']['coordinates'])
    return isl    

def get_earthquakes_csv_string(n, magnitude):
    baseurl = 'https://earthquake.usgs.gov/fdsnws/event/1/query'
    headers = {'User-Agent': 'no.uib.ii.inf100.h24.lab7.milflover5112'}
    endtime = datetime.today()
    start = timedelta(days=n)
    starttime = endtime-start
    my_iso8601_string = endtime.strftime('%Y-%m-%dT%H:%M:%S%z')
    my_iso8602_string = starttime.strftime('%Y-%m-%dT%H:%M:%S%z')
    params = {
        'format': 'csv',
        'minmagnitude': magnitude,
        'starttime': my_iso8602_string,
        'endtime': my_iso8601_string,
        'orderby': 'magnitude',
        'limit': 5000,
    }
    response = requests.get(baseurl, params=params, headers=headers)
    content = response.content.decode('utf-8')
    return (content)

def fix(content):
    reader = csv.DictReader(io.StringIO(content), delimiter=',', quotechar="'")
    headers = reader.fieldnames
    data = list(reader)
    return data

def floatft(t):
  if t % 1 == 0:
    return int(t)
  else:
    return t

def get_earthquake_list(csv_string):
    gwiz = fix(csv_string)
    h = []
    for i in range(len(gwiz)):
        l = float(gwiz[i]['longitude'])
        l = floatft(l)
        
        w = float(gwiz[i]['latitude'])
        w = floatft(w)
        
        m = float(gwiz[i]['mag'])
        m = floatft(m)
        
        b = (l, w, m)
        h.append(b)
    return h


def plot_earthquakes (h):
    data_points = get_earthquake_list(h)
    x_coordinates = []
    y_coordinates = []
    size_coordinates = []

    for xsys in data_points:
        x = xsys[0]
        x_coordinates.append(x)
        y = xsys[1]
        y_coordinates.append(y)
        mag = xsys[2]
        m = (3**mag) / 10
        size_coordinates.append(m)

    plt.scatter(x=x_coordinates, y=y_coordinates, s=size_coordinates, alpha=0.2)

def plot_coastlines(islands):
   x_coordinates = []
   y_coordinates = []
   for island in islands:
      for islan in island:
            x1 = islan[0]
            x_coordinates.append(x1)
            y1 = islan[1]
            y_coordinates.append(y1)
   
      plt.plot(x_coordinates, y_coordinates, color='red')
      x_coordinates.clear()
      y_coordinates.clear()
  

if __name__ == '__main__':
    plt.figure(figsize=(12,8))
    g = load_coastlines()
    plot_coastlines(g)
    b = get_earthquakes_csv_string(50, 4)
    plot_earthquakes (b)
    plt.grid(True)
    plt.title('')
    plt.show()



