import requests
import json

def weather_in_bergen_next_hour():
    baseurl = "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=60.361867&lon=5.369966"
    headers = {'User-Agent': 'no.uib.ii.inf100.h24.lab7.rastr9649'}

    response = requests.get(baseurl, headers=headers)
    content = json.loads(response.content.decode('utf-8'))
    timeseries = content["properties"]["timeseries"]
    weather_next_hour = timeseries[0]["data"]["next_1_hours"]["summary"]["symbol_code"]
    return weather_next_hour
    


print("Været i Bergen neste time:", weather_in_bergen_next_hour())  