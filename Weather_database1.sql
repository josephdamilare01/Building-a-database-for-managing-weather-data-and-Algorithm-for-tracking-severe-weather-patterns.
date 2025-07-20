create database severe;
use severe;
create table Location( ID_location int primary key,
country text, city text, region text, population int		
);
create table Geography( ID_geography int auto_increment primary key,
ID_location int ,
latitude double, longitude double,
foreign key (ID_location) references location(ID_location) 
);
create table Weather_Condition (ID_Weather_condition int auto_increment primary key,
ID_location int,
temp double, temp_min double,	
temp_max double, pressure int, humidity int,
sea_level int, ground_level int, wind_speed double,	
wind_degree int,	sunrise date,	sunset date, cloud int,
	description_1 text,
    foreign key (ID_location) references location(ID_location) 
);

create table date_taken( ID_date_taken int auto_increment primary key,
ID_location int ,
    date_taken date, timezone int, 
    foreign key (ID_location) references location(ID_location) 
);
SELECT * FROM severe.location;