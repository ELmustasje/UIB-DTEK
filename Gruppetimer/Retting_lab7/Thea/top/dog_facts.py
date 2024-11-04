import json 
import requests

def get_dog_facts(json_string):
    the_list = []

    the_dict = json.loads(json_string)

    for i in the_dict['data']:
       a = i['attributes']['body']
       the_list.append(a)
    return the_list


def download_dog_facts():
  url = 'https://dogapi.dog/api/v2/facts?limit=3'
  webpage = requests.get(url)
  webpage_content = webpage.content.decode('utf-8')
  return webpage_content



if __name__ == '__main__':
    json_string = download_dog_facts()
    facts = get_dog_facts(json_string)
    print('\n'.join(facts))

