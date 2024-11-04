# Del A
import json
import requests

def get_dog_facts (json_string):
    d = json.loads(json_string)
    facts = [fact['attributes']['body']for fact in d['data']]
    return facts

def test_get_dog_facts():
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
    expected = [
        'Dogs live an average of 15 years.',
        'Dogs have a wet nose to help them absorb scents.',
        'Dogs can see in color, but not as vividly as humans.',
    ]
    actual = get_dog_facts(arg)
    assert expected == actual

# Del B
def download_dog_facts():
    url = 'https://dogapi.dog/api/v2/facts?limit=3' 
    response = requests.get(url)
    response.raise_for_status()
    return response.content.decode('utf-8')

if __name__ == '__main__':
    json_string = download_dog_facts()
    facts = get_dog_facts(json_string)
    for fact in facts:
        print(fact)

    test_get_dog_facts()
