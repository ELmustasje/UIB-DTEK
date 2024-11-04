import requests
import json
from datetime import datetime, timedelta


def weather_in_bergen_next_hour():
    
    url = 'https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=60.390358&lon=5.323065'
    headers = {'User-Agent': 'inf100.ii.uib.no.makar9281'}
    respone = requests.get(url, headers=headers)
    respone_decode =  respone.content.decode('utf-8')
    dict_weather = json.loads(respone_decode)

    now = datetime.now()
    current = now.strftime("%Y-%m-%dT%H:00:00Z")
    

    time = {}
    for i in range(len(dict_weather['properties']['timeseries'])):
        time [ dict_weather['properties']['timeseries'][i]['time'] ]= i
    
    return dict_weather['properties']['timeseries'][time[current]]['data']['next_1_hours']['summary']['symbol_code']
  

print('Været i Bergen sentrum neste time:', weather_in_bergen_next_hour())

#Samarbeidet med Niklas og fikk hjelp av Noah