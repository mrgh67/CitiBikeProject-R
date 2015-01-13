#Predicting Citibike Bike Speeds

Can bikespeeds be predicted for those riders using the Citibikes in New York City?
Although the exact route, and therefore the precise distance travelled, is unknown for an given rider with some assumptions and enough data speed prediction is possible. The influence of weather and geography is also investigated.

By subsetting the data the aim is to find the real "go-getters" between any 2 stations. These are the folks who are not out for a pleasant cycle, taking a circuitous, winding or touristy route. These are the folks who use Citibikes day-in day-out to get from A to B in the fatest possible time. By subsetting the data to analysing only the fatestest journeys between any 2 Citibike stations it is hoped we are looking at a subset of riders who, indeed, use the bikes to go from A to B in the shortest time possible. A further assumption therefore, is that they follow the most efficient route possible between any 2 given stations. If these assumptions are correct, and we estimate distance in a consistent fashion, then we can estimate their speeds.

In addition to using only the fastest riders between any 2 stations the data is further restricted to men taking trips during  workdays.

The distance and elevation between 2 stations is estimated using the MapQuest API (Thank you MapQuest). Weather conditions for the trip were then sought from the Wunderground API (Thank you Wunderground) that provided historical weather data, including temperature, humidity and windspeed.

Results:
Citibike average speed estimate = 10.6 mph. This compares to the average rush hour speed in Lyon, France, of 9.3 mph.
Largest correlate with speed was temperature. 
Elevation was correlated but New York City is largely flat!

Prediction:
A 13 degree Fahrenheit change in temperature can effect your trip speed by approximately 0.3 mph
On a 1.85 mile trip allow yourself an extra 30 seconds in the cold!!

This project was presented at the following Meetup:
http://www.meetup.com/NYC-Open-Data/events/218737696/
