## Mesonet:  KzLqu2HzkVTAnnddzg0OV5kXjxMd1Lf1OZi
import pandas as pd
from MesoPy import Meso
from datetime import datetime
import numpy
import csv
import os
from scipy import stats

path1="F:\Users\Jason\Desktop\Python code\sample_data\mesowest_downloads"
os.chdir(path1) #set directory

m = Meso(token='bae20ad3a6a045a58347dfb80a0c91f3')

##create list of years
starts = [200101010000,200201010000,200301010000,200401010000,200501010000,200601010000,200701010000,200801010000,200901010000,201001010000,201101010000,201201010000,201301010000,201401010000,201501010000]
ends   = [200112312300,200212312300,200312312300,200412312300,200512312300,200612312300,200712312300,200812312300,200912312300,201012312300,201112312300,201212312300,201312312300,201412312300,201512312300]

##open bounding box coordinates; convert to list of tuples#
bboxes = pd.read_csv('bboxes.csv')
subset = bboxes[['lon1', 'lat1', 'lon2','lat2']]
tuples = [tuple(x) for x in subset.values]
cities = bboxes[['name']]    #list of cities

tuples=[(-81.992876,    40.566564,  -80.992876, 41.566564),
(-74.298081,    42.245923,  -73.298081, 43.245923),
(-107.130105,   34.642077,  -106.130105,    35.642077),
(-75.933626,    40.147287,  -74.933626, 41.147287),
(-150.4,    60.7167 ,   -149.4,     61.7167),
(-84.832473,    33.324283,  -83.832473, 34.324283),
(-82.495747,    32.988324,  -81.495747, 33.988324),
(-98.262032,    29.859226,  -97.262032, 30.859226),
(-119.530301,   34.861544,  -118.530301,    35.861544),
(-77.147313,    38.733711,  -76.147313, 39.733711),
(-91.536778,    29.924093,  -90.536778, 30.924093),
(-87.290997,    32.969704,  -86.290997, 33.969704),
(-71.641441,    41.874132,  -70.641441, 42.874132),
(-73.80938, 40.735139,  -72.80938,  41.735139),
(-79.32386, 42.426634,  -78.32386,  43.426634),
(-82.342913,    26.099245,  -81.342913, 27.099245),
(-80.515331,    32.402253,  -79.515331, 33.402253),
(-81.316101,    34.750657,  -80.316101, 35.750657),
(-85.709605,    34.558469,  -84.709605, 35.558469),
(-88.398901,    41.327437,  -87.398901, 42.327437),
(-84.961421,    38.686445,  -83.961421, 39.686445),
(-82.104789,    40.942703,  -81.104789, 41.942703),
(-105.273976,   38.3719 ,   -104.273976,    39.3719),
(-81.575787,    33.53615,   -80.575787, 34.53615),
(-83.469191,    39.521391,  -82.469191, 40.521391),
(-122.508278,   37.350482,  -121.508278,    38.350482),
(-97.471293,    32.315015,  -96.471293, 33.315015),
(-84.66833, 39.255132,  -83.66833,  40.255132),
(-105.450758,   39.210391,  -104.450758,    40.210391),
(-94.156657,    41.118905,  -93.156657, 42.118905),
(-83.727797,    41.990015,  -82.727797, 42.990015),
(-114   ,   53.0333 ,   -113    ,   54.0333),
(-106.891094,   31.280802,  -105.891094,    32.280802),
(-148.2231, 64.3436 ,   -147.2231,  65.3436),
(-120.263105,   36.287878,  -119.263105,    37.287878),
(-86.16965, 42.442813,  -85.16965,  43.442813),
(-82.938981,    34.344189,  -81.938981, 35.344189),
(-77.368143,    39.746286,  -76.368143, 40.746286),
(-73.199095,    41.24075,   -72.199095, 42.24075),
(-95.893891,    29.283805,  -94.893891, 30.283805),
(-86.645552,    39.310876,  -85.645552, 40.310876),
(-90.6847,  31.7989 ,   -89.6847,   32.7989),
(-82.149802,    29.741835,  -81.149802, 30.741835),
(-95.096057,    38.539843,  -94.096057, 39.539843),
(-84.525572,    35.416903,  -83.525572, 36.416903),
(-76.811806,    39.599806,  -75.811806, 40.599806),
(-115.665726,   35.63259,   -114.665726,    36.63259),
(-92.820322,    34.256779,  -91.820322, 35.256779),
(-118.606428,   33.483551,  -117.606428,    34.483551),
(-86.174067,    37.730531,  -85.174067, 38.730531),
(-89.885082,    42.582697,  -88.885082, 43.582697),
(-98.67448, 25.740231,  -97.67448,  26.740231),
(-90.41766, 34.594653,  -89.41766,  35.594653),
(-80.731745,    25.677446,  -79.731745, 26.677446),
(-88.602646,    42.558116,  -87.602646, 43.558116),
(-93.780525,    44.478288,  -92.780525, 45.478288),
(-118.159044,   33.052579,  -117.159044,    34.052579),
(-74.0673,  45.0017 ,   -73.0673,   46.0017),
(-117.697753,   33.123585,  -116.697753,    34.123585),
(-87.192743,    35.642342,  -86.192743, 36.642342),
(-73.281158,    40.870042,  -72.281158, 41.870042),
(-90.644682,    29.457748,  -89.644682, 30.457748),
(-74.467284,    40.217505,  -73.467284, 41.217505),
(-112.485874,   40.640262,  -111.485874,    41.640262),
(-98.025067,    35.004641,  -97.025067, 36.004641),
(-96.534385,    40.734562,  -95.534385, 41.734562),
(-81.911146,    28.083819,  -80.911146, 29.083819),
(-81.174081,    27.689003,  -80.174081, 28.689003),
(-75.797141,    39.474067,  -74.797141, 40.474067),
(-112.47004,    32.994443,  -111.47004, 33.994443),
(-80.451369,    39.957319,  -79.451369, 40.957319),
(-80.802398,    26.765055,  -79.802398, 27.765055),
(-123.151091,   45.020241,  -122.151091,    46.020241),
(-74.483061,    41.052432,  -73.483061, 42.052432),
(-71.896816,    41.27683,   -70.896816, 42.27683),
(-112.229828,   39.775923,  -111.229828,    40.775923),
(-79.169201,    35.269801,  -78.169201, 36.269801),
(-120.297335,   39.032197,  -119.297335,    40.032197),
(-77.986489,    36.978711,  -76.986489, 37.978711),
(-117.844469,   33.486678,  -116.844469,    34.486678),
(-78.068429,    42.637383,  -77.068429, 43.637383),
(-121.827352,   38.139346,  -120.827352,    39.139346),
(-112.426673,   40.138248,  -111.426673,    41.138248),
(-98.976097,    29.015227,  -97.976097, 30.015227),
(-117.62913,    32.428335,  -116.62913, 33.428335),
(-122.789403,   37.235396,  -121.789403,    38.235396),
(-122.443801,   36.829715,  -121.443801,    37.829715),
(-82.972499,    26.802381,  -81.972499, 27.802381),
(-76.283777,    40.849313,  -75.283777, 41.849313),
(-122.774196,   46.969516,  -121.774196,    47.969516),
(-117.853025,   47.18001,   -116.853025,    48.18001),
(-73.074397,    41.625624,  -72.074397, 42.625624),
(-90.842212,    38.130725,  -89.842212, 39.130725),
(-121.787725,   37.461477,  -120.787725,    38.461477),
(-76.664096,    42.59022,   -75.664096, 43.59022),
(-83.013678,    27.50648,   -82.013678, 28.50648),
(-84.114061,    41.139064,  -83.114061, 42.139064),
(-79.9,     43.2    ,   -78.9   ,   44.2),
(-111.446345,   31.754861,  -110.446345,    32.754861),
(-96.403089,    35.606392,  -95.403089, 36.606392),
(-76.809016,    36.408456,  -75.809016, 37.408456),
(-77.689374,    38.397106,  -76.689374, 39.397106),
(-97.827317,    37.178997,  -96.827317, 38.178997),
(-80.766435,    35.571399,  -79.766435, 36.571399),
(-72.294686,    41.685946,  -71.294686, 42.685946),
(-81.181675,    40.645443,  -80.181675, 41.645443)]

cities = ['Akron','Albany--Schenectady','Albuquerque','Allentown','Anchorage','Atlanta','Augusta-Richmond County',
'Austin','Bakersfield','Baltimore','Baton Rouge','Birmingham','Boston','Bridgeport--Stamford','Buffalo',
'Cape Coral','Charleston--North Charleston','Charlotte','Chattanooga','Chicago','Cincinnati','Cleveland',
'Colorado Springs','Columbia','Columbus','Concord','Dallas--Fort Worth--Arlington','Dayton','Denver--Aurora',
'Des Moines','Detroit','Edmonton','El Paso','Fairbanks','Fresno','Grand Rapids','Greenville','Harrisburg',
'Hartford','Houston','Indianapolis','Jackson','Jacksonville','Kansas City','Knoxville','Lancaster',
'Las Vegas--Henderson','Little Rock','Los Angeles--Long Beach--Anaheim','Louisville--Jefferson County',
'Madison','McAllen','Memphis','Miami','Milwaukee','Minneapolis--St. Paul','Mission Viejo--Lake Forest--San Clemente',
'Montreal','Murrieta--Temecula--Menifee','Nashville-Davidson','New Haven','New Orleans','New York--Newark','Ogden--Layton',
'Oklahoma City','Omaha','Orlando','Palm Bay--Melbourne','Philadelphia','Phoenix--Mesa',
'Pittsburgh','Port St. Lucie','Portland','Poughkeepsie--Newburgh','Providence','Provo--Orem','Raleigh','Reno',
'Richmond','Riverside--San Bernardino','Rochester','Sacramento','Salt Lake City--West Valley City',
'San Antonio','San Diego','San Francisco--Oakland','San Jose','Sarasota--Bradenton','Scranton','Seattle',
'Spokane','Springfield','St. Louis','Stockton','Syracuse','Tampa--St. Petersburg','Toledo','Toronto','Tucson',
'Tulsa','Virginia Beach','Washington','Wichita','Winston-Salem','Worcester','Youngstown']

for h in range(len(tuples)):
    bbox1 = tuples[h]
    stations = m.metadata(bbox=bbox1)
    x = pd.DataFrame(stations["STATION"])
    city1 = cities['name'][h]   #city1 = cities.iloc[h]
    outname1=(city1+"_stations.csv")
    x.to_csv(outname1, sep=',')
    STIDs=x['STID']
    ind_station = pd.DataFrame()  #create empty dataframe for appending
    final = pd.DataFrame()
    for i in range(len(STIDs)):
        id1=STIDs[i]
        for j in range(len(starts)):
            try:
              data = m.timeseries(stid=id1, start=starts[j], end=ends[j], units='temp|F')
              df=pd.DataFrame(data['STATION'][0]['OBSERVATIONS']['date_time'])
              df['temp']=pd.DataFrame(data['STATION'][0]['OBSERVATIONS']['air_temp_set_1'])
              df.columns=['timestamp','temperature']
              df['timestamp'] = pd.to_datetime(df['timestamp'])
              df['date'] = pd.DatetimeIndex(df['timestamp']).date
              df2 = pd.DataFrame(df.pivot_table(index='date', values='temperature', aggfunc=numpy.min))
              df2['tmax'] = pd.DataFrame(df.pivot_table(index='date', values='temperature', aggfunc=numpy.max))
              df2['stid'] = id1
              ind_station=pd.concat([ind_station,df2],axis=0) #append rows
            except Exception, e:
                continue
    final = pd.concat([final,ind_station],axis=0)
    outname2=(city1+"_data.csv")
    final.to_csv(outname2, sep=',')


for h in range(len(tuples[1])):
    bbox1 = tuples[h]
    stations = m.metadata(bbox=bbox1)
    x = pd.DataFrame(stations["STATION"])
    city1 = cities[h]   #city1 = cities.iloc[h]
    outname1=(city1+"_stations.csv")
    x.to_csv(outname1, sep=',')



###individual cities
h=23
bbox1 = tuples[h]
stations = m.metadata(bbox=bbox1)
x = pd.DataFrame(stations["STATION"])
city1 = cities['name'][h]   #city1 = cities.iloc[h]
outname1=(city1+"_stations.csv")
x.to_csv(outname1, sep=',')
STIDs=x['STID']
ind_station = pd.DataFrame()  #create empty dataframe for appending
final = pd.DataFrame()
for i in range(len(STIDs)):
    id1=STIDs[i]
    for j in range(len(starts)):
        try:
          data = m.timeseries(stid=id1, start=starts[j], end=ends[j], units='temp|F')
          df=pd.DataFrame(data['STATION'][0]['OBSERVATIONS']['date_time'])
          df['temp']=pd.DataFrame(data['STATION'][0]['OBSERVATIONS']['air_temp_set_1'])
          df.columns=['timestamp','temperature']
          df['timestamp'] = pd.to_datetime(df['timestamp'])
          df['date'] = pd.DatetimeIndex(df['timestamp']).date
          df2 = pd.DataFrame(df.pivot_table(index='date', values='temperature', aggfunc=numpy.min))
          df2['tmax'] = pd.DataFrame(df.pivot_table(index='date', values='temperature', aggfunc=numpy.max))
          df2['stid'] = id1
          ind_station=pd.concat([ind_station,df2],axis=0) #append rows
        except Exception, e:
            continue
final = pd.concat([final,ind_station],axis=0)
outname2=(city1+"_data.csv")
final.to_csv(outname2, sep=',')
