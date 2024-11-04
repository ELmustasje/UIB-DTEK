import requests
from datetime import datetime, timedelta, timezone
from pathlib import Path
import os
import csv
import io
from matplotlib import pyplot as plt
import json


def get_earthquakes_csv_string(n, magnitude):
    end_time = datetime.now(timezone.utc)
    start_time = end_time - timedelta(days=n)  #   n er antall dager

    end_time = end_time.strftime("%Y-%m-%dT%H:%M:%S%z")
    start_time = start_time.strftime("%Y-%m-%dT%H:%M:%S%z")

    baseurl = "https://earthquake.usgs.gov/fdsnws/event/1/query"
    headers = {"User-Agent": "no.uib.ii.inf100.h24.lab7.rastr9649"}
    params = {
        "format": "csv",
        "starttime": start_time,  # ISO 8601 format
        "endtime": end_time,  # ISO 8601 format
        "minmagnitude": magnitude,
        "orderby": "magnitude",
        "limit": 5000,
    }

    response = requests.get(baseurl, params=params, headers=headers)
    content = response.content.decode("utf-8")
    return content


#   Del B: skal gjøre om csv-strengen til oppslagsverk.
def get_earthquake_list(csv_string):
    reader = csv.DictReader(io.StringIO(csv_string), delimiter=",", quotechar='"')
    headers = reader.fieldnames
    data = list(reader)

    longitudes = []
    for i in range(len(data)):
        longitudes.append(float(data[i]["longitude"]))

    latitudes = []
    for i in range(len(data)):
        latitudes.append(float(data[i]["latitude"]))

    magnitudes = []
    for i in range(len(data)):
        magnitudes.append(float(data[i]["mag"]))

    #   Nå har jeg lagd tre lister av flyttall. Jeg skal lage en liste av tupler (hvert jordskjelv)

    earthquakes = []
    for j in range(len(data)):
        earthquakes.append((longitudes[j], latitudes[j], magnitudes[j]))

    return earthquakes


#   Del C: plotting!
def plot_earthquakes(data_points):
    #   Starter med å gjøre koordinatene til x-er og y-er
    xs = []
    ys = []
    sizes = []
    for i in range(len(data_points)):
        xs.append(data_points[i][0])

    for j in range(len(data_points)):
        ys.append(data_points[j][1])

    for k in range(len(data_points)):
        sizes.append(
            3 ** data_points[k][2] / 10
        )  # 3 opphøyd i magnituden / 10 for å få en vesentlig størrelsesforskjell

    plt.scatter(
        xs, ys, s=sizes, alpha=0.2
    )  # Alpha 0.2 for å gjøre dem litt gjennomsiktige


#   Del C: kystlinje json


def load_coastlines():
    coastline_json = Path("ne_110m_coastline.json").read_text(encoding="utf-8")
    coastline_dict = json.loads(coastline_json)

    islands = []
    coordinates = []
    for i in range(len(coastline_dict["features"])):
        for j in range(len(coastline_dict["features"][i]["geometry"]["coordinates"])):
            coordinates.append(
                coastline_dict["features"][i]["geometry"]["coordinates"][j]
            )
        islands.append(coordinates)
        coordinates = []
        j = 0
    return islands


#   Del D: Plotte kystlinjer
def plot_coastlines(islands):
    for i in range(len(islands)):
        xs = []
        ys = []
        for coordinates in islands[i]:
            xs.append(coordinates[0])
            ys.append(coordinates[1])
        plt.plot(xs, ys, color="gray")


if __name__ == "__main__":
    s = get_earthquakes_csv_string(30, 5.8)
    # print(s) # vi fjerner print senere, men vi ser nå at vi er på rett vei
    get_earthquake_list(s)

    plt.figure(figsize=(12, 8))
    #   Test del C
    plot_earthquakes(get_earthquake_list(get_earthquakes_csv_string(50, 4)))

    #   Test del D
    islands = load_coastlines()
    plot_coastlines(islands)
    plt.show()

