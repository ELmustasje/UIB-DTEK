import json
import requests

def get_dog_facts(json_string):
    data = json.loads(json_string)
    facts = []
    for fact in data['data']:
        facts.append(fact['attributes']['body'])

    return facts

def download_dog_facts():
    url = 'https://dogapi.dog/api/v2/facts?limit=3'
    response = requests.get(url)

    if response.status_code == 200:

        return response.content.decode('utf-8')
    else:
        return None

if __name__ == '__main__':
    json_string = download_dog_facts()
    facts = get_dog_facts(json_string)
    print('\n'.join(facts))


