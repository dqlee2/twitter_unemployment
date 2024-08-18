##############################################################
# Collect city level data on UI claims

tracker = merge(
  read.csv(
    file = paste0("./data/ui/EconomicTracker-main/data/UI Claims - City - Weekly.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE)[,c(
      'cityid','year','month','day_endofweek','initclaims_count_regular'
      )],
  read.csv(
    file = paste0("./data/ui/EconomicTracker-main/data/GeoIDs - City.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE)[,c(
      'cityid','cityname','stateabbrev'
      )],
  by = 'cityid', all.x = TRUE, all.y = FALSE
)

tracker = tracker[order(tracker$cityid,tracker$year,tracker$month,tracker$day_endofweek),]
tracker$date = as.Date(ISOdate(
  year = tracker$year,
  month = tracker$month,
  day = tracker$day_endofweek))
tracker$week = strftime(tracker$date, format = '%V')
tracker = tracker[,c('cityname','stateabbrev','year','week','initclaims_count_regular')]
colnames(tracker) = c('cityname','state','year','week','iclaimnsa')

# table(tracker$cityname)
# tracker[tracker$cityname=='Los Angeles',]

# HI Hawaii

data = rbind(
  read_excel(
    path = paste0("./data/ui/local/HI/IC2021-for-upload-1.xls"),
    range = 'A62:L114', sheet='2021', col_types=c('date',rep('numeric',11))
  ),
  read_excel(
    path = paste0("./data/ui/local/HI/IC2022-for-upload-1.xls"),
    range = 'A62:L114', sheet='2022', col_types=c('date',rep('numeric',11))
  ),
  read_excel(
    path = paste0("./data/ui/local/HI/IC2022-for-upload-1.xls"),
    range = 'A7:L51', sheet='2022', col_types=c('date',rep('numeric',11))
  )
)
colnames(data) = c(
  'week_end_date',
  'Honolulu','Kaneohe','Waipahu','OAHU','Hilo',
  'Kona','HAWAII','Wailuku','Molokai','MAUI','KAUAI'
  )
data$date = as.Date(data$week_end_date)
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data = data[,c('year','week','OAHU','MAUI')]
colnames(data) = c('year','week','Honolulu','Kahului') # -Wailuku-Lahaina
data = data[,c(
  'year','week',
  'Honolulu',
  'Kahului' # -Wailuku-Lahaina
)] %>% pivot_longer(cols=c(
  'Honolulu',
  'Kahului' # -Wailuku-Lahaina
),names_to='cityname',values_to='iclaimnsa')
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'ID'
data = data[data$cityname!='',c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker[tracker$cityname!='Honolulu',],data)

# IA Iowa

data = read.csv(
  file = paste0("./data/ui/local/IA/Iowa_Unemployment_Insurance_Claims_Data_by_County__Weekly_.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)[,c(
    'County','Week.Ending','Initial.Claims'
  )]
colnames(data) = c('county','date','iclaimnsa')
data$date = as.Date(data$date,"%m/%d/%Y")
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data$cityname=""
data[data$county %in% c(
  'Polk','Dallas','Warren','Jasper','Madison','Guthrie','Story','Boone','Marion','Mahaska'
  ),'cityname'] = 'Des Moines'
data[data$county %in% c(
  'Linn','Benton','Jones','Johnson','Washington','Scott','Rock Island','Henry','Mercer','Clinton','Muscatine'
),'cityname'] = 'Cedar Rapids'
data[data$county %in% c(
  'Scott','Rock Island','Henry','Mercer','Clinton','Muscatine'
),'cityname'] = 'Davenport'
data[data$county %in% c(
  'Black Hawk','Bremer','Grundy'
),'cityname'] = 'Waterloo'
data[data$county %in% c(
  'Woodbury'
),'cityname'] = 'Sioux City'
data[data$county %in% c(
  'Dubuque'
),'cityname'] = 'Dubuque'
data[data$county %in% c(
  'Lee','Des Moines'
),'cityname'] = 'Burlington'
data[data$county %in% c(
  'Dickinson','Clay'
),'cityname'] = 'Spencer'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
  )] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'IA'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker,data)

# ID Idaho

data = read_excel(
  path = paste0("./data/ui/local/ID/UI_Claims_Data.xlsx"),
  range = 'A2:G251', sheet='MSA'
)
colnames(data) = c(
  'date','unclassified',
  'Boise',
  'Coeur dAlene',
  'Idaho Falls',
  'Lewston',
  'Pocatello'
)
data$date = as.Date(data$date)
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data = data[,c(
  'year','week',
  'Boise',
  'Coeur dAlene',
  'Idaho Falls',
  'Lewston',
  'Pocatello'
)] %>% pivot_longer(cols=c(
  'Boise',
  'Coeur dAlene',
  'Idaho Falls',
  'Lewston',
  'Pocatello'
),names_to='cityname',values_to='iclaimnsa')
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'ID'
data = data[data$cityname!='',c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker[tracker$cityname!='Boise',],data)

# plot(data[data$cityname=="Boise" & data$year>=2020,'iclaimnsa'],type='l',col='black')
# lines(tracker[tracker$cityname=="Boise" & tracker$year>=2020,'iclaimnsa'],col='blue')

# IN Indiana

data = read_excel(
  path = paste0("./data/ui/local/IN/idwd_data_496.xlsx"),
  range = 'A5:H70567', sheet='idwd_data_496'
)
colnames(data) = c('county','statefips','countyfips','year','monthname','month','week','iclaimnsa')
data$county = gsub(' County, IN','',data$county)
data$cityname=""
data[data$county %in% c(
  'Marion','Hamilton','Hendricks','Johnson','Madison','Hancock','Morgan','Boone','Shelby',
  'Putnam','Brown','Delaware','Bartholomew','Henry','Jackson','Montgomery','Jennings','Decatur'
),'cityname'] = 'Indianapolis'
data[data$county %in% c(
  'Allen','Whitley','Noble','DeKalb','Huntington','Adams','Steuben','Wells'
),'cityname'] = 'Fort Wayne'
data[data$county %in% c(
  'St. Joseph','Cass','Elkhart','Berrien','Kosciusko','Marshall'
),'cityname'] = 'South Bend'
data[data$county %in% c(
  'Tippecanoe','Carroll','Benton','Warren','Clinton'
),'cityname'] = 'Lafayette'
data[data$county %in% c(
  'Monroe','Owen','Lawrence'
),'cityname'] = 'Bloomington'
data[data$county %in% c(
  'Vigo','Clay','Sullivan','Parke','Vermillion'
),'cityname'] = 'Terre Haute'
data[data$county %in% c(
  'Howard','Miami'
),'cityname'] = 'Kokomo'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'IN'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker[tracker$cityname!='Indianapolis',],data)

# MA Massachusetts

data = rbind(
  read_excel(
    path = paste0("./data/ui/local/MA/ClaimsDataCounty.xlsx"),
    range = 'A2:C730', sheet='2019InitialClaims'
  ),
  read_excel(
    path = paste0("./data/ui/local/MA/ClaimsDataCounty.xlsx"),
    range = 'A2:C2031', sheet='InitialClaims'
  )
)
colnames(data) = c('date','county','iclaimnsa')
data$iclaimnsa = as.numeric(data$iclaimnsa)
data$date = as.Date(data$date)
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data$county = gsub(' County','',data$county)
data$cityname=""
data[data$county %in% c(
  'Middlesex','Essex','Suffolk','Norfolk','Plymouth','Bristol','Worcester','Barnstable'
),'cityname'] = 'Boston'
data[data$county %in% c(
  'Hampden','Hampshire','Franklin'
),'cityname'] = 'Springfield'
data[data$county %in% c(
  'Berkshire'
),'cityname'] = 'Pittsfield'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'MA'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker[tracker$cityname!='Boston',],data)

# NE Nebraska

data = read_excel(
  path = paste0("./data/ui/local/NE/NEworks UI claims by industry occupation county.xlsx"),
  range = 'A1:CP67', sheet='CountyClean', col_types='text'
)
data = as.data.frame(data.table::melt(data.table(data),id=1))
data = data[,c('date','variable','value')]
colnames(data) = c('date','county','iclaimnsa')
data$iclaimnsa = as.numeric(data$iclaimnsa)
data$date = as.Date(data$date,'%m/%d/%Y')
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data$cityname=""
data[data$county %in% c(
  'Douglas','Sarpy','Pottawattamie','Cass','Saunders','Washington','Mills','Harrison','Dodge'
),'cityname'] = 'Omaha'
data[data$county %in% c(
  'Lancaster','Seward','Gage'
),'cityname'] = 'Lincoln'
data[data$county %in% c(
  'Hall','Merrick','Howard'
),'cityname'] = 'Grand Island'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'NE'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker,data)

# NV Nevada

data = read.csv(
  file = paste0("./data/ui/local/NV/State of Nevada Weekly Unemployment Insurance Trends by County.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)[,c(
    'County','Program','Week.Ending','Claim.Type','Claims')]
data = data[data$Program=='Regular' & data$Claim.Type=='initial',]
data = data[,c('County','Week.Ending','Claims')]
colnames(data) = c('county','date','iclaimnsa')
data$iclaimnsa = as.numeric(data$iclaimnsa)
data$date = as.Date(data$date)
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data$cityname=""
data[data$county %in% c(
  'Clark','Mohave','Nye'
),'cityname'] = 'Las Vegas'
data[data$county %in% c(
  'Washoe','Storey','Lyon','Carson','Douglas'
),'cityname'] = 'Reno'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'NV'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker[tracker$cityname!='Las Vegas',],data)

# TX Texas

data = read_excel(
  path = paste0("./data/ui/local/TX/weekly-claims-by-county-twc.xlsx"),
  range = 'A1:IV200', sheet='Texas Claims' # , col_types='text'
)
data = as.data.frame(data.table::melt(data.table(data),id=1))
data = data[,c('Week Ending Date','variable','value')]
colnames(data) = c('date','county','iclaimnsa')
data$iclaimnsa = as.numeric(data$iclaimnsa)
data$date = as.Date(data$date)
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data$cityname=""
data[data$county %in% c(
  'Dallas','Collin','Denton','Ellis','Kaufman','Rockwall','Hunt','Tarrant','Johnson',
  'Parker','Wise','Grayson','Henderson','Hood','Navarro','Bryan','Cooke','Fannin','Palo Pinto'
),'cityname'] = 'Dallas'
data[data$county %in% c(
  'Harris','Fort Bend','Montgomery','Brazoria','Galveston','Liberty','Waller',
  'Chambers','Austin','Walker','Wharton','Matagorda','Washington'
),'cityname'] = 'Houston'
data[data$county %in% c(
  'Bexar','Guadalupe','Comal','Medina','Atascosa','Wilson','Kendall','Bandera','Frio'
),'cityname'] = 'San Antonio'
data[data$county %in% c(
  'Travis','Williamson','Hays','Bastrop','Caldwell'
),'cityname'] = 'Austin'
data[data$county %in% c(
  'Hidalgo','Starr'
),'cityname'] = 'McAllen'
data[data$county %in% c(
  'El Paso','Hudspeth','Dona Ana'
),'cityname'] = 'El Paso'
data[data$county %in% c(
  'Nueces','San Patricio','Jim Wells','Duval','Kleberg','Kenedy','Aransas'
),'cityname'] = 'Corpus Christi'
data[data$county %in% c(
  'Bell','Coryell','Lampasas'
),'cityname'] = 'Killeen'
data[data$county %in% c(
  'Cameron','Willacy'
),'cityname'] = 'Brownsville'
data[data$county %in% c(
  'Jefferson','Orange','Hardin'
),'cityname'] = 'Beaumont'
data[data$county %in% c(
  'Lubbock','Lynn','Crosby','Hale','Hockley'
),'cityname'] = 'Lubbock'
data[data$county %in% c(
  'Midland','Martin','Ector'
),'cityname'] = 'Midland'
data[data$county %in% c(
  'Randall','Potter','Carson','Oldham','Armstrong','Gray','Roberts','Hutchinson'
),'cityname'] = 'Amarillo'
data[data$county %in% c(
  'Gregg','Harrison','Rusk','Upshur'
),'cityname'] = 'Longview'
data[data$county %in% c(
  'Smith','Cherokee'
),'cityname'] = 'Tyler'
data[data$county %in% c(
  'Webb'
),'cityname'] = 'Laredo'
data[data$county %in% c(
  'McLennan','Falls'
),'cityname'] = 'Waco'
data[data$county %in% c(
  'Brazos','Burleson','Robertson'
),'cityname'] = 'College Station'
data[data$county %in% c(
  'Taylor','Jones','Callahan'
),'cityname'] = 'Abilene'
data[data$county %in% c(
  'Wichita','Clay','Archer'
),'cityname'] = 'Wichita Falls'
data[data$county %in% c(
  'Tom Green','Irion','Sterling'
),'cityname'] = 'San Angelo'
data[data$county %in% c(
  'Victoria','Goliad','Calhoun'
),'cityname'] = 'Victoria'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'TX'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker,data)

# CT Connecticut

data = read_excel(
  path = paste0("./data/ui/local/CT/Processed_Initial_Claims_by_Town_update_2022_06_27.xlsx"),
  # range = '', sheet='' # , 
  col_types='text'
)
data = as.data.frame(data.table::melt(data.table(data[!is.na(data$`Town Name`),]),id=1))
data = data[,c('variable','Town Name','value')]
colnames(data) = c('date','county','iclaimnsa')
data$iclaimnsa = as.numeric(data$iclaimnsa)
data$date = as.Date(data$date,format="%b %d,%Y")
data$year = strftime(data$date, format = '%Y')
data$week = strftime(data$date, format = '%V')
data$cityname=""
data[data$county %in% c(
  'Hartford','Middlesex','Tolland','New London'
),'cityname'] = 'Hartford'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'CT'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker,data)

# WI Wisconsin

data = read_excel(
  path = paste0("./data/ui/local/WI/initial-claims-by-county-2021.xlsx"),
  range = 'A1:BA74', # sheet='' # , 
  col_types='text'
)
data = as.data.frame(data.table::melt(data.table(data[!is.na(data$`County`),]),id=1))
data = data[,c('variable','County','value')]
colnames(data) = c('date','county','iclaimnsa')
data$iclaimnsa = as.numeric(data$iclaimnsa)
data$year = 2021
data$week = as.numeric(sub('UI Week ', '', data$date, perl=T))
data$cityname=""
data[data$county %in% c(
  'MILWAUKEE'
),'cityname'] = 'Milwaukee'
data = data[data$cityname!='',c(
  'cityname','year','week','iclaimnsa'
)] %>% group_by(cityname,year,week) %>% summarise_all(sum,na.rm=TRUE)
data = as.data.frame(data[order(data$cityname,data$year,data$week),])
data$state = 'CT'
data = data[,c('cityname','state','year','week','iclaimnsa')]
tracker = rbind(tracker[tracker$cityname!='Milwaukee',],data)

tracker$year = as.numeric(tracker$year)
tracker$week = as.numeric(tracker$week)
tracker$citystate = paste0(tracker$cityname,', ',tracker$state)
table(tracker[tracker$year>=2016,c('citystate','year')])

locations = read.csv(
  file = paste0('./data/locations_US.csv'),
  header = TRUE, sep = ",", 
  stringsAsFactors = FALSE # , fileEncoding = "UTF-8"
)
# table(locations$metro_area_name)

metro_area_names = data.frame(
  metro_area_name=unique(locations[!is.na(locations[['metro_area_name']]),'metro_area_name'])
  )
metro_area_names[['merged']] = 1

tracker[['metro_area_name']] = tracker[['cityname']]
check = merge(tracker,metro_area_names,by='metro_area_name',all=TRUE)
table(check$cityname,check$merged)

tracker[tracker[['metro_area_name']]=='Laredo','metro_area_name'] = 'Nuevo Laredo'
tracker[tracker[['metro_area_name']]=='New York City','metro_area_name'] = 'New York'
tracker[tracker[['metro_area_name']]=='Washington','metro_area_name'] = 'Washington D.C.'
tracker[tracker[['metro_area_name']]=='San Francisco','metro_area_name'] = 'San Jose'
tracker[tracker[['metro_area_name']]=='San Diego','metro_area_name'] = 'Tijuana'
tracker[['iclaimnsa']] = as.numeric(tracker[['iclaimnsa']])
uidata = tracker[!is.na(tracker[['metro_area_name']]),c(
  'metro_area_name','year','week','iclaimnsa'
  )] %>% group_by(metro_area_name,year,week) %>% summarise_all(sum,na.rm=TRUE)

write.csv(
  uidata, 
  file = paste0("./data/ui/ui_city.csv"),
  row.names = FALSE)

table(tracker$cityname)

length(unique(tracker[!is.na(tracker$iclaimnsa),'state']))
length(unique(tracker[!is.na(tracker$iclaimnsa),'cityname']))
length(unique(tracker[!is.na(tracker$iclaimnsa) & tracker$year<=2019,'state']))
length(unique(tracker[!is.na(tracker$iclaimnsa) & tracker$year<=2019,'cityname']))
length(unique(tracker[!is.na(tracker$iclaimnsa) & tracker$year<=2018,'state']))
length(unique(tracker[!is.na(tracker$iclaimnsa) & tracker$year<=2018,'cityname']))
length(unique(tracker[!is.na(tracker$iclaimnsa) & tracker$year<=2017,'cityname']))

# tracker[tracker$metro_area_name=='Fort Wayne' & tracker$year>="2019",c('year','week','iclaimnsa')]
plot(tracker[tracker$metro_area_name=='Milwaukee' & tracker$year>="2019",c('iclaimnsa')],type='l')

#######################################################
# By demographics

uidemo = data.frame()

# AK

temp = read_excel(
  paste0("./data/ui/local/AK/UI_Claimants_Alaska_Jan2015thruNov2022.xlsx"),
  range = "A5:R3039"
)[-1,c(1,2,4,6,13,14,15,16,17,18)] %>% as.data.frame()
colnames(temp) = c('tm','area','ui_female','ui_male','<25','25-34','35-44','45-54','55-64','65+')
temp = temp[temp$area=='Alaska',]
for (var in c('tm','ui_female','ui_male','<25','25-34','35-44','45-54','55-64','65+')) {
  temp[[var]] = as.numeric(temp[[var]])
}
temp[['tm']] = as.yearmon(floor(temp[['tm']]/100)+(temp[['tm']]-floor(temp[['tm']]/100)*100)/12)
temp[['ui_age_lt20']] = (temp[['<25']]/2)
temp[['ui_age_2030']] = (temp[['<25']]/2) + (temp[['25-34']]/2)
temp[['ui_age_3040']] = (temp[['25-34']]/2) + (temp[['35-44']]/2)
temp[['ui_age_gt40']] = (temp[['35-44']]/2) + temp[['45-54']] + temp[['55-64']] + temp[['65+']]
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp = temp %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp[['state']] = 'AK'
uidemo = rbind(uidemo,temp)

# AZ

temp0 = read_excel(
  paste0("./data/ui/local/AZ/ui-04-monthly-data.xlsx"),
  sheet = "203", range = "B1:LW12", col_types = 'text'
)
temp0[['...2']] = NULL
temp0 = temp0[temp0[,1]!='INA*',]
colnames(temp0) = c('group',as.character(as.yearmon(seq(as.yearmon('Feb 1995'),as.yearmon('Sep 2022'),1/12))))
temp0 = data.table::melt(data.table(temp0),id=1)
colnames(temp0) = c('group','tm','value')
temp0[['tm']] = as.yearmon(temp0[['tm']])
temp0[['value']] = as.numeric(temp0[['value']])
temp = merge(
  temp0[temp0$group=='Male',c('tm','value')] %>% rename(ui_male = value),
  temp0[temp0$group=='Female',c('tm','value')] %>% rename(ui_female = value),
  by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Under 22',c('tm','value')] %>% rename(ui_lt22 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='22-24',c('tm','value')] %>% rename(ui_2224 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='25-34',c('tm','value')] %>% rename(ui_2534 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='35-44',c('tm','value')] %>% rename(ui_3544 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='45-54',c('tm','value')] %>% rename(ui_4554 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='55-59',c('tm','value')] %>% rename(ui_5559 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='60-64',c('tm','value')] %>% rename(ui_6064 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Over 64',c('tm','value')] %>% rename(ui_gt64 = value),by=c('tm'),all=TRUE
)
temp[['ui_age_lt20']] = (temp[['ui_lt22']]*8/10)
temp[['ui_age_2030']] = (temp[['ui_lt22']]*2/10) + temp[['ui_2224']] + (temp[['ui_2534']]/2)
temp[['ui_age_3040']] = (temp[['ui_2534']]/2) + (temp[['ui_3544']]/2)
temp[['ui_age_gt40']] = (temp[['ui_3544']]/2) + temp[['ui_4554']] + temp[['ui_5559']] + temp[['ui_6064']] + temp[['ui_gt64']]
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp = temp %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp[['state']] = 'AZ'
uidemo = rbind(uidemo,temp)

# CA

temp0 = read_excel(
  paste0("./data/ui/local/CA/state-county-dashboard-110222.xlsx"),
  sheet = "Statewide_Demographics", range = "A4:D2944" # , col_types = 'text'
)
colnames(temp0) = c('date','group','category','value')
temp0[['tm']] = as.yearmon(temp0[['date']])
temp0 = temp0[,c('tm','category','value')] %>% group_by(tm,category) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(
  temp0[temp0$category=='Male',c('tm','value')] %>% rename(ui_male = value),
  temp0[temp0$category=='Female',c('tm','value')] %>% rename(ui_female = value),
  by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='16-19',c('tm','value')] %>% rename(ui_1619 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='20-24',c('tm','value')] %>% rename(ui_2024 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='25-34',c('tm','value')] %>% rename(ui_2534 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='35-44',c('tm','value')] %>% rename(ui_3544 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='45-54',c('tm','value')] %>% rename(ui_4554 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='55-64',c('tm','value')] %>% rename(ui_5564 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$category=='65-85',c('tm','value')] %>% rename(ui_6585 = value),by=c('tm'),all=TRUE
)
temp[['ui_age_lt20']] = (temp[['ui_1619']])
temp[['ui_age_2030']] = temp[['ui_2024']] + (temp[['ui_2534']]/2)
temp[['ui_age_3040']] = (temp[['ui_2534']]/2) + (temp[['ui_3544']]/2)
temp[['ui_age_gt40']] = (temp[['ui_3544']]/2) + temp[['ui_4554']] + temp[['ui_5564']] + temp[['ui_6585']]
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp = temp %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp[['state']] = 'CA'
uidemo = rbind(uidemo,temp)

# CO

temp0 = read_excel(
  paste0("./data/ui/local/CO/Initial_Claims_by_Week_by_Age_Sex_Race_Ethnicity_p(5).xlsx"),
  sheet = "Initial Claims by Sex", range = "A2:E45" # , col_types = 'text'
)[-1,]
temp0[['tm']] = as.yearmon(temp0[['Week Ending Date']],'%m/%d/%Y')
temp0 = temp0[,c('tm','Male','Female')]
colnames(temp0) = c('tm','ui_male','ui_female')
for (var in c('ui_female','ui_male')) {
  temp0[[var]] = as.numeric(temp0[[var]])
}
temp0 = temp0 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp1 = read_excel(
  paste0("./data/ui/local/CO/Initial_Claims_by_Week_by_Age_Sex_Race_Ethnicity_p(5).xlsx"),
  sheet = "Initial Claims by Age Group", range = "A2:J45" # , col_types = 'text'
)[-1,]
temp1[['tm']] = as.yearmon(temp1[['Week Ending Date']],'%m/%d/%Y')
for (var in c('16-19','20-24','25-34','35-44','45-54','55-64','65 and older')) {
  temp1[[var]] = as.numeric(temp1[[var]])
}
temp1[['ui_age_lt20']] = (temp1[['16-19']])
temp1[['ui_age_2030']] = temp1[['20-24']] + (temp1[['25-34']]/2)
temp1[['ui_age_3040']] = (temp1[['25-34']]/2) + (temp1[['35-44']]/2)
temp1[['ui_age_gt40']] = (temp1[['35-44']]/2) + temp1[['45-54']] + temp1[['55-64']] + temp1[['65 and older']]
temp1 = temp1[,c('tm','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp1 = temp1 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(temp0,temp1,by=c('tm'),all=TRUE)
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'CO'
uidemo = rbind(uidemo,temp)

# CT

temp0 = read_excel(
  paste0("./data/ui/local/CT/Processed_Initial_Claims_by_Sex_update_2022_06_27.xlsx"),
  range = "B1:E897" # , sheet = "", col_types = 'text'
)
temp0[['tm']] = as.yearmon(temp0[['Date']])
temp0 = temp0[,c('tm','Male','Female')]
colnames(temp0) = c('tm','ui_male','ui_female')
temp0 = temp0 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp1 = read_excel(
  paste0("./data/ui/local/CT/Processed_Initial_Claims_by_Age_update_2022_06_27.xlsx"),
  range = "B2:K898" # , sheet = "", col_types = 'text'
)
temp1[['tm']] = as.yearmon(temp1[['New Claim Date']])
temp1[['ui_age_lt20']] = (temp1[['Under 20']])
temp1[['ui_age_2030']] = (temp1[['20 to 29']])
temp1[['ui_age_3040']] = (temp1[['30 to 39']])
temp1[['ui_age_gt40']] = (temp1[['40 to 49']]) + temp1[['50 to 59']] + temp1[['60 to 69']] + temp1[['70 to 79']] + temp1[['80 to 89']] + temp1[['90 +']]
temp1 = temp1[,c('tm','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp1 = temp1 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(temp0,temp1,by=c('tm'),all=TRUE)
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'CT'
uidemo = rbind(uidemo,temp)

# ID

temp0 = read_excel(
  paste0("./data/ui/local/ID/UI_Claims_Data.xlsx"),
  range = "A1:C251", sheet = "Gender" # , col_types = 'text'
) %>% as.data.frame()
temp0[['tm']] = as.yearmon(temp0[,1])
temp0 = temp0[,c('tm','Male','Female')]
colnames(temp0) = c('tm','ui_male','ui_female')
temp0 = temp0 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp1 = read_excel(
  paste0("./data/ui/local/ID/UI_Claims_Data.xlsx"),
  range = "A2:H251", sheet = "Age Groups" # , col_types = 'text'
) %>% as.data.frame()
temp1[['tm']] = as.yearmon(temp1[,1])
temp1[['ui_age_lt20']] = (temp1[['< 25']]/2)
temp1[['ui_age_2030']] = (temp1[['< 25']]/2) + (temp1[['25 - 34']]/2)
temp1[['ui_age_3040']] = (temp1[['25 - 34']]/2) + (temp1[['35 - 44']]/2)
temp1[['ui_age_gt40']] = (temp1[['35 - 44']]/2) + temp1[['45 - 54']] + temp1[['55 - 59']] + temp1[['60 - 64']] + temp1[['65 +']]
temp1 = temp1[,c('tm','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp1 = temp1 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(temp0,temp1,by=c('tm'),all=TRUE)
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'ID'
uidemo = rbind(uidemo,temp)

# MA

temp = rbind(
  read_excel(
    path = paste0("./data/ui/local/MA/ClaimsDataCounty.xlsx"),
    range = 'A2:E730', sheet='2019InitialClaims'
  ),
  read_excel(
    path = paste0("./data/ui/local/MA/ClaimsDataCounty.xlsx"),
    range = 'A2:E2031', sheet='InitialClaims'
  )
) %>% as.data.frame()
temp[['tm']] = as.yearmon(temp[,1])
temp = temp[,c('tm','Male','Female')]
colnames(temp) = c('tm','ui_male','ui_female')
for (var in c('ui_female','ui_male')) {
  temp[[var]] = as.numeric(temp[[var]])
}
temp = temp %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
for (var in c('ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')) {
  temp[[var]] = NA
}
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'MA'
uidemo = rbind(uidemo,temp)

# ME

temp00 = read_excel(
  paste0("./data/ui/local/ME/UIClaimantCharacteristics.xlsx"),
  range = "A1:D170", sheet = "gender" # , col_types = 'text'
)
temp00 = temp00[temp00[['Program Type']]=='State Unemployment Insurance',] %>% as.data.frame()
temp00[['tm']] = as.yearmon(temp00[['Claim Date']])
temp0 = merge(
  temp00[temp00$Description=='Male',c('tm','Count')] %>% rename(ui_male = Count),
  temp00[temp00$Description=='Female',c('tm','Count')] %>% rename(ui_female = Count),
  by=c('tm'),all=TRUE
)
temp0 = temp0[,c('tm','ui_male','ui_female')]
colnames(temp0) = c('tm','ui_male','ui_female')
temp0 = temp0 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp10 = read_excel(
  paste0("./data/ui/local/ME/UIClaimantCharacteristics.xlsx"),
  range = "A1:D411", sheet = "age" # , col_types = 'text'
)
temp10 = temp10[temp10[['Program Type']]=='State Unemployment Insurance',] %>% as.data.frame()
temp10[['tm']] = as.yearmon(temp10[['Claim Date']])
temp1 = merge(
  temp10[temp10$Description=='<25',c('tm','Count')] %>% rename(ui_lt25 = Count),
  temp10[temp10$Description=='25-34',c('tm','Count')] %>% rename(ui_2534 = Count),
  by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$Description=='35-44',c('tm','Count')] %>% rename(ui_3544 = Count),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$Description=='45-54',c('tm','Count')] %>% rename(ui_4554 = Count),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$Description=='55-64',c('tm','Count')] %>% rename(ui_5564 = Count),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$Description=='>=65',c('tm','Count')] %>% rename(ui_gt65 = Count),by=c('tm'),all=TRUE
)
temp1[['ui_age_lt20']] = (temp1[['ui_lt25']]/2)
temp1[['ui_age_2030']] = (temp1[['ui_lt25']]/2) + (temp1[['ui_2534']]/2)
temp1[['ui_age_3040']] = (temp1[['ui_2534']]/2) + (temp1[['ui_3544']]/2)
temp1[['ui_age_gt40']] = (temp1[['ui_3544']]/2) + temp1[['ui_4554']] + temp1[['ui_5564']] + temp1[['ui_gt65']]
temp1 = temp1[,c('tm','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp1 = temp1 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(temp0,temp1,by=c('tm'),all=TRUE)
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'ME'
uidemo = rbind(uidemo,temp)

# NE

temp00 = read_excel(
  paste0("./data/ui/local/NE/NEworks Weeks Claimed by County and Demographics.xlsx"),
  range = "A3:BN5", sheet = "Gender" # , col_types = 'text'
)
temp00 = data.table::melt(data.table(temp00),id=1)
colnames(temp00) = c('group','tm','value')
temp00[['tm']] = as.yearmon(temp00[['tm']],'%m/%d/%Y')
temp00 = temp00 %>% group_by(group,tm) %>% summarise_all(sum,na.rm=TRUE)
temp0 = merge(
  temp00[temp00$group=='Male',c('tm','value')] %>% rename(ui_male = value),
  temp00[temp00$group=='Female',c('tm','value')] %>% rename(ui_female = value),
  by=c('tm'),all=TRUE
)
temp0 = temp0[,c('tm','ui_male','ui_female')]
colnames(temp0) = c('tm','ui_male','ui_female')
temp10 = read_excel(
  paste0("./data/ui/local/NE/NEworks Weeks Claimed by County and Demographics.xlsx"),
  range = "A3:BN12", sheet = "Age" # , col_types = 'text'
)
temp10 = data.table::melt(data.table(temp10),id=1)
colnames(temp10) = c('group','tm','value')
temp10[['tm']] = as.yearmon(temp10[['tm']],'%m/%d/%Y')
temp10 = temp10 %>% group_by(group,tm) %>% summarise_all(sum,na.rm=TRUE)
temp1 = merge(
  temp10[temp10$group=='Under 20',c('tm','value')] %>% rename(ui_lt20 = value),
  temp10[temp10$group=='20-24 Years Old',c('tm','value')] %>% rename(ui_2024 = value),
  by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='25-34 Years Old',c('tm','value')] %>% rename(ui_2534 = value),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='35-44 Years Old',c('tm','value')] %>% rename(ui_3544 = value),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='45-54 Years Old',c('tm','value')] %>% rename(ui_4554 = value),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='55-59 Years Old',c('tm','value')] %>% rename(ui_5559 = value),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='60-64 Years Old',c('tm','value')] %>% rename(ui_6064 = value),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='65-74 Years Old',c('tm','value')] %>% rename(ui_6574 = value),by=c('tm'),all=TRUE
)
temp1 = merge(
  temp1,temp10[temp10$group=='75 Years Old and Older',c('tm','value')] %>% rename(ui_gt75 = value),by=c('tm'),all=TRUE
)
temp1[['ui_age_lt20']] = (temp1[['ui_lt20']])
temp1[['ui_age_2030']] = (temp1[['ui_2024']]) + (temp1[['ui_2534']]/2)
temp1[['ui_age_3040']] = (temp1[['ui_2534']]/2) + (temp1[['ui_3544']]/2)
temp1[['ui_age_gt40']] = (temp1[['ui_3544']]/2) + temp1[['ui_4554']] + temp1[['ui_5559']] + temp1[['ui_6064']] + temp1[['ui_6574']] + temp1[['ui_gt75']]
temp1 = temp1[,c('tm','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp1 = temp1 %>% group_by(tm) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(temp0,temp1,by=c('tm'),all=TRUE)
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'NE'
uidemo = rbind(uidemo,temp)

# NJ

temp0 = read_excel(
  paste0("./data/ui/local/NJ/claimant_characteristics2020.xlsx"),
  range = "A2:AX75"
)
colnames(temp0)[1] = 'group'
temp0 = temp0[grepl('Male|Female|Age',temp0$group),]
temp0 = data.table::melt(data.table(temp0),id=1)
colnames(temp0) = c('group','tm','value')
temp0[['tm']] = as.yearmon(temp0[['tm']],'%b-%y')
temp = merge(
  temp0[temp0$group=='Female',c('tm','value')] %>% rename(ui_female = value),
  temp0[temp0$group=='Male',c('tm','value')] %>% rename(ui_male = value),
  by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age   16-19',c('tm','value')] %>% rename(ui_1619 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age  20-24',c('tm','value')] %>% rename(ui_2024 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age  25-34',c('tm','value')] %>% rename(ui_2534 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age  35-44',c('tm','value')] %>% rename(ui_3544 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age 45-54',c('tm','value')] %>% rename(ui_4554 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age  55-64',c('tm','value')] %>% rename(ui_5564 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Age  65 and Older',c('tm','value')] %>% rename(ui_gt65 = value),by=c('tm'),all=TRUE
)
temp[['ui_age_lt20']] = (temp[['ui_1619']])
temp[['ui_age_2030']] = (temp[['ui_2024']]) + (temp[['ui_2534']]/2)
temp[['ui_age_3040']] = (temp[['ui_2534']]/2) + (temp[['ui_3544']]/2)
temp[['ui_age_gt40']] = (temp[['ui_3544']]/2) + temp[['ui_4554']] + temp[['ui_5564']] + temp[['ui_gt65']]
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'NJ'
uidemo = rbind(uidemo,temp)

# NY

temp0 = read_excel(
  path = paste0("./data/ui/local/NY/weekly-claims-and-benefits-report_0.xlsx"),
  range = 'B2:AU32', sheet='Monthly IC by Gender'
) %>% group_by(Gender) %>% summarise_all(sum,na.rm=TRUE) %>% as.data.frame()
temp0 = data.table::melt(data.table(temp0),id=1)
temp0[['tm']] = as.yearmon(temp0[['variable']])
temp0[['value']] = as.numeric(temp0[['value']])
temp = merge(
  temp0[temp0$Gender=='Female',c('tm','value')] %>% rename(ui_female = value),
  temp0[temp0$Gender=='Male',c('tm','value')] %>% rename(ui_male = value),
  by=c('tm'),all=TRUE
)
for (var in c('ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')) {
  temp[[var]] = NA
}
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'NY'
uidemo = rbind(uidemo,temp)

# OK

temp = read_excel(
  path = paste0("./data/ui/local/OK/ETA203_Oklahoma.xlsx"),
  range = 'A1:N355' # , sheet=''
) %>% rename(ui_female = Female, ui_male = Male)
temp[['tm']] = as.yearmon(temp[['rptdate']])
temp[['ui_age_lt20']] = (temp[['Under 22']] * 8/10)
temp[['ui_age_2030']] = (temp[['Under 22']] * 2/10) + temp[['22-24']] + (temp[['25-34']]/2)
temp[['ui_age_3040']] = (temp[['25-34']]/2) + (temp[['35-44']]/2)
temp[['ui_age_gt40']] = (temp[['35-44']]/2) + temp[['45-54']] + temp[['55-59']] + temp[['60-64']] + temp[['65+']]
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'OK'
uidemo = rbind(uidemo,temp)

# RI

temp0 = read_excel(
  paste0("./data/ui/local/RI/ic20.xlsx"),
  range = "A3:BA19"
)
colnames(temp0)[1] = 'group'
temp0 = temp0[!is.na(temp0[,2]),]
temp0 = data.table::melt(data.table(temp0),id=1)
temp0[['variable']] = gsub('Week ending ','',temp0[['variable']])
temp0[['variable']] = gsub('Sept','Sep',temp0[['variable']])
temp0[['tm']] = as.yearmon(paste0(temp0[['variable']],', 2020'),'%b %d, %Y')
temp0[['variable']] = NULL
temp0 = temp0 %>% group_by(group,tm) %>% summarise_all(sum,na.rm=TRUE)
temp = merge(
  temp0[temp0$group=='2 - Female',c('tm','value')] %>% rename(ui_female = value),
  temp0[temp0$group=='1 - Male',c('tm','value')] %>% rename(ui_male = value),
  by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='Under 22',c('tm','value')] %>% rename(ui_lt22 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='22-24',c('tm','value')] %>% rename(ui_2224 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='25-34',c('tm','value')] %>% rename(ui_2534 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='35-44',c('tm','value')] %>% rename(ui_3544 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='45-54',c('tm','value')] %>% rename(ui_4554 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='55-64',c('tm','value')] %>% rename(ui_5564 = value),by=c('tm'),all=TRUE
)
temp = merge(
  temp,temp0[temp0$group=='65 & Over',c('tm','value')] %>% rename(ui_gt65 = value),by=c('tm'),all=TRUE
)
temp[['ui_age_lt20']] = (temp[['ui_lt22']]*8/10)
temp[['ui_age_2030']] = (temp[['ui_lt22']]*2/10) + (temp[['ui_2224']]) + (temp[['ui_2534']]/2)
temp[['ui_age_3040']] = (temp[['ui_2534']]/2) + (temp[['ui_3544']]/2)
temp[['ui_age_gt40']] = (temp[['ui_3544']]/2) + temp[['ui_4554']] + temp[['ui_5564']] + temp[['ui_gt65']]
temp = temp[,c('tm','ui_female','ui_male','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
temp[['state']] = 'RI'
uidemo = rbind(uidemo,temp)

write.csv(
  uidemo, 
  file = paste0("./data/ui/ui_state_demo.csv"),
  row.names = FALSE)



