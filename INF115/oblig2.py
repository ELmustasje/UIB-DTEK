import sqlite3
import pandas as pd
import re

from shiny import reactive
from shiny.express import render, ui, input 



# creates the database bysykkel
with open('bysykkel.sql', 'r') as sql_file:
    sql_script = sql_file.read()

con = sqlite3.connect(':memory:')
cur = con.cursor()
_ = cur.executescript(sql_script)


with ui.card():
    ui.card_header("Task 1")
    
    with ui.card():
        ui.card_header("a)")
        #--- Write your solution to 1a here ---
        ui.h3("Name: Thomas Barth")
        ui.h3("Id: 327979")



    with ui.card():
        ui.card_header("b)")
       #--- Write your solution to 1b here ---
        ui.input_text("name", "What's your name?")
        ui.input_text("phoneNum", "What's your phone number?")
        ui.input_text("Email", "What's your Email?")
        ui.input_action_button("submit", "Submit")

        @render.code
        @reactive.event(input.submit)
        def printInfo():
            text = f"{input.name().strip()}\n{input.phoneNum().strip()}\n{input.Email().strip()}"
            return text
        

    with ui.card():
        ui.card_header("c)")
        #--- Write your solution to 1c here ---
        @render.code
        @reactive.event(input.submit)
        def validate():
            if(len(input.name().strip()) > 0 and re.match("^[a-zA-Z\s]*$",input.name().strip())):  nameVal = f"{input.name().strip()} - Valid"
            ##if all(x.isalpha() or x.isspace() for x in input.name()):nameVal = f"{input.name()} - Valid"
            else: nameVal = f"{input.name().strip()} - Not Valid"

            if(re.match("^[0-9\s]+$", input.phoneNum().strip())and sum(x.isdigit() for x in input.phoneNum()) == 8): numVal = f"{input.phoneNum().strip()} - Valid"
            ##if(input.phoneNum().strip().isnumeric() and len(input.phoneNum().strip()) == 8):numVal = f"{input.phoneNum()} - Valid"
            else: numVal = f"{input.phoneNum().strip()} - Not Valid"

            if(re.search(r"@",input.Email().strip())): EmailVal = f"{input.Email().strip()} - Valid"
            ##if("@" in input.Email()):EmailVal = f"{input.Email()} - Valid"
            else: EmailVal = f"{input.Email().strip()} - Not Valid"
            
            text = f"{nameVal}\n{numVal}\n{EmailVal}"
            return text
        

with ui.card():
    ui.card_header("Task 2")
    
    with ui.panel_well():
        ui.card_header("a)")
        #--- Write your solution to 2a here ---

        @reactive.calc
        def users():
            query = "SELECT name FROM users ORDER BY name;"
            users = pd.read_sql(query,con)
            return users
        
        @render.data_frame
        def user_table():
            return users()
        

    with ui.panel_well():
        ui.card_header("b)")
        #--- Write your solution to 2b here ---

        @reactive.calc
        def bikes():
            query = "SELECT DISTINCT name, status FROM bikes;"
            bikes= pd.read_sql(query,con=con)
            return bikes
        
        @render.data_frame
        def bike_table():
            return bikes()
        
        
    with ui.panel_well():
        ui.card_header("c)")
        #--- Write your solution to 2c here ---

        @reactive.calc
        def subscriptions():
            query = "SELECT Type, COUNT(*) AS Purchased FROM subscriptions GROUP BY type;"
            subscriptions = pd.read_sql(query,con=con)
            return subscriptions
        
        @render.data_frame
        def subscription_table():
            return subscriptions()
        
        
with ui.card():
    ui.card_header("Task 3")
    
    with ui.panel_well():
        ui.card_header("a)")
        #--- Write your solution to 3a here ---
        ui.input_text("filter", "ADD FILTER")
        ui.input_action_button("filter_submit", "APPLY")

        @reactive.calc
        @reactive.event(input.filter_submit)
        def user_with_filter():
            filter = input.filter()
            #to include leading zeros i user substr('00000000' || phone_number,-8,8)
            #note it is bad practice to store phone numbers as an int
            query = "SELECT user_id, name, substr('00000000' || phone_number,-8,8) From users WHERE name LIKE '%" + filter + "%';"
            user_with_filer = pd.read_sql(query,con)
            user_with_filer = user_with_filer.rename(columns={"substr('00000000' || phone_number,-8,8)":"Phone Number"})
            return user_with_filer
        
        @render.data_frame
        def user_with_filter_table():
            return user_with_filter()
        

    with ui.panel_well():
        ui.card_header("b)")
        #--- Write your solution to 3b here ---

        @reactive.calc
        def station_trip_data():
            query = """
                    SELECT s.station_id, s.name, COUNT(t.end_station) AS Trips
                    FROM stations s
                    LEFT JOIN trips t ON s.station_id = t.end_station
                    GROUP BY s.station_id, s.name;
                    """
            station_trip_data = pd.read_sql(query,con)
            station_trip_data = station_trip_data.rename(columns={"name": "Name"})
            station_trip_data = station_trip_data.rename(columns={"Trips": "Number of trips"})
            return station_trip_data
            
        @render.data_frame
        def station_trip_table():
            return station_trip_data()
        
        
    with ui.panel_well():
        ui.card_header("c)")
        #--- Write your solution to 3c here ---

        @reactive.calc
        def user_subscription_history():
            query = """
                    SELECT
                    u.user_id,
                    u.name,
                    SUM(CASE WHEN strftime('%Y', s.start_time) = '2018' THEN 1 ELSE 0 END) AS subscriptions_2018,
                    SUM(CASE WHEN strftime('%Y', s.start_time) = '2019' THEN 1 ELSE 0 END) AS subscriptions_2019,
                    SUM(CASE WHEN strftime('%Y', s.start_time) = '2020' THEN 1 ELSE 0 END) AS subscriptions_2020,
                    SUM(CASE WHEN strftime('%Y', s.start_time) = '2021' THEN 1 ELSE 0 END) AS subscriptions_2021
                    FROM
                    users u
                    LEFT JOIN
                    subscriptions s ON u.user_id = s.user_id
                    GROUP BY
                    u.user_id,
                    u.name;
                    """
            user_subscription_history = pd.read_sql(query,con=con)
            return user_subscription_history
        
        @render.data_frame
        def user_subscription_history_table():
            return user_subscription_history()
        
        
with ui.panel_well():
    ui.card_header("Task 4")
    #--- Write your solution to 4 here ---
    
    query = "SELECT name FROM stations;"
    stations = pd.read_sql(query,con=con)
    stationNames = stations['name'].tolist()


    with ui.layout_columns():
            ui.input_selectize(
            "station", 
            "Select Station",
            stationNames + ["None"],
            multiple=False,
            selected="None",
            options=None
            )
            ui.input_switch("progress", "Is trip in Progress", value=False)

    @render.table(render_links=True, escape=False)
    @reactive.calc()
    @reactive.event(input.station, input.progress)
    def stationAvailabilityLocationData():
        if(input.station() == "None"):
            return 
        query = f"SELECT max_spots, available_spots, longitude, latitude FROM stations WHERE name='{input.station()}';"
        dataFrame = pd.read_sql(query,con=con)
        dataDict = {
                    "maxSpots":dataFrame['max_spots'].array[0],
                    "availableSpots":dataFrame['available_spots'].array[0],
                    "latitude":dataFrame['latitude'].array[0],
                    "longitude":dataFrame['longitude'].array[0]
        }

        if(input.progress()):  
            avalilability = str(round(dataDict.get("availableSpots")/dataDict.get("maxSpots") * 100)) + "%"
        else:
            bikes = dataDict.get("maxSpots") - dataDict.get("availableSpots")
            avalilability = str(round(bikes/dataDict.get("maxSpots") * 100)) + "%"
        name = input.station()
        link = f"""<a href="https://www.google.com/maps?q={dataDict.get("latitude")},{dataDict.get("longitude")}">Link</a>"""
    
        
        
        return pd.DataFrame([[name,avalilability,link]],columns=["Name","Availability","Location"])
        
        



