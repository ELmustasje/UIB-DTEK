import requests
import json

def download_dog_facts():
    url = "https://dogapi.dog/api/v2/facts?limit=3" # Nettsiden som skal lastes ned
    headers = {
        # Noen nettsider krever at 'User-Agent' har fått en verdi
        # for at man skal få respons.
        'User-Agent': 'inf100.ii.uib.no abc123', # Noe som forteller hvem du er
    }
    webpage = requests.get(url, headers=headers)
    webpage_content = webpage.content.decode('utf-8')
    return webpage_content

def get_dog_facts(we):
    we = json.loads(we)
    facts = []
    for i in range(3):
        facts.append(we['data'][i]['attributes']['body'])
    return facts
""" 
json_string = download_dog_facts()
facts = get_dog_facts(json_string)
print('\n'.join(facts)) """

arg = '''\
{
  "data": [
    {
      "id": "574c4504-250f-4743-9ff2-124e5c61e79e",
      "type": "fact",
      "attributes": {
        "body": "Dogs live an average of 15 years."
      }
    },
    {
      "id": "cd131ea1-3327-4c2c-93e6-488582a3e2e7",
      "type": "fact",
      "attributes": {
        "body": "Dogs have a wet nose to help them absorb scents."
      }
    },
    {
      "id": "58d82e49-0b2f-4911-bdde-0d686e092b72",
      "type": "fact",
      "attributes": {
        "body": "Dogs can see in color, but not as vividly as humans."
      }
    }
  ]
}
'''




""" if __name__ == '__main__':
    json_string = download_dog_facts()
    facts = get_dog_facts(json_string)
    print('\n'.join(facts)) """ 

