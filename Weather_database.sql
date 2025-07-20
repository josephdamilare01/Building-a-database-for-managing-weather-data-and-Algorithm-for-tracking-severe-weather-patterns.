

SELECT *
FROM location
JOIN geography ON location.ID_location = geography.ID_location
JOIN weather_condition ON geography.ID_location = weather_condition.ID_location
JOIN date_taken ON weather_condition.ID_location = date_taken.ID_location;
