from matplotlib import pyplot as plt
import requests
from datetime import datetime, timezone, timedelta
import csv
import json


def get_earthquakes_csv_string(n, magnitude):
    end_time = datetime.now(timezone.utc)
    start_time = end_time - timedelta(days=n)

    start_time_str = start_time.strftime("%Y-%m-%dT%H:%M:%S%z")
    end_time_str = end_time.strftime("%Y-%m-%dT%H:%M:%S%z")

    baseurl = "https://earthquake.usgs.gov/fdsnws/event/1/query"
    headers = {"User-Agent": "no.uib.ii.inf100.h24.lab7.mitt_uib_brukernavn"}
    params = {
        "format": "csv",
        "starttime": start_time_str,
        "endtime": end_time_str,
        "minmagnitude": magnitude,
        "orderby": "magnitude",
        "limit": 5000,
    }

    response = requests.get(baseurl, params=params, headers=headers)

    if response.status_code == 200:
        return response.content.decode("utf-8")
    else:
        return f"Får ikke takk i data (status code: {response.status_code})"


def get_earthquake_list(csv_string):
    earthquake_list = []

    csv_reader = csv.DictReader(csv_string.splitlines())

    for row in csv_reader:
        latitude = float(row["latitude"])
        longitude = float(row["longitude"])
        magnitude = float(row["mag"])
        earthquake_list.append((longitude, latitude, magnitude))

    return earthquake_list


def plot_earthquakes(data_points):
    xs = [point[0] for point in data_points]
    ys = [point[1] for point in data_points]
    size = [(3 ** point[2]) / 10 for point in data_points]

    plt.scatter(xs, ys, s=size, c="blue", alpha=0.2)
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")
    plt.title("Earthquake locations")


def plot_coastlines(islands):
    for island in islands:
        for coords in islands:
            longitudes = [point[0] for point in coords]
            latitudes = [point[1] for point in coords]
            plt.plot(longitudes, latitudes, color="grey")


def load_coastlines(file_path="ne_110m_coastline.json"):
    with open(file_path, "r") as file:
        json_data = file.read()

    data = json.loads(json_data)

    coastlines = []
    for feature in data["features"]:
        coordinates = feature["geometry"]["coordinates"]
        coastlines.append(coordinates)

    return coastlines


if __name__ == "__main__":
    plt.figure(figsize=(12, 8))

    plt.grid(True)
    s = get_earthquakes_csv_string(50, 4.0)
    earthquake_data = get_earthquake_list(s)

    islands = load_coastlines()
    plot_coastlines(islands)

    plot_earthquakes(earthquake_data)
    plt.show()

