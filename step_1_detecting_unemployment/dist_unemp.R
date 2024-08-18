#####################################################
# Distribution of unemployed users across states, age, gender between Rule-Based vs bert relative to population

rm(list=ls())

# User profile information
demo = fread('../preprocessing/data/restricted/demo_inf_final_20240106.csv')
pf = fread('../preprocessing/data/restricted/latest_profiles.csv')
pf[['age']] = NULL
pf[['gender']] = NULL
pf[['org']] = NULL
pf = merge(pf,demo,by=c('user_id'),all.x=TRUE,all.y=FALSE)
pf[['tweet_timestamp']] = as.Date(
  as.POSIXct(pf[['tweet_timestamp']],origin='1970-01-01'))
pf[['acctage']] = 2022 - year(pf[['tweet_timestamp']])
pf[['tweet_timestamp']] = NULL
pf[['n_users']] = 1

# Number of users
n_users = pf[,c(
  'state','age','gender','n_users'
)] %>% group_by(state,age,gender) %>% summarise_all(sum,na.rm=TRUE)
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]
n_gender = n_users[,c(
  'gender','n_users'
)] %>% group_by(gender) %>% summarise_all(sum,na.rm=TRUE)
n_gender[['n_users']] = n_gender[['n_users']] / sum(n_gender[['n_users']],na.rm=TRUE)
n_age = n_users[,c(
  'age','n_users'
)] %>% group_by(age) %>% summarise_all(sum,na.rm=TRUE)
n_age[['n_users']] = n_age[['n_users']] / sum(n_age[['n_users']],na.rm=TRUE)
pf[is.na(pf[['gender']]),'gender'] = sample(
  0:1,sum(is.na(pf[['gender']]),na.rm=TRUE),
  prob=n_gender[['n_users']],replace=TRUE)
pf[is.na(pf[['age']]),'age'] = sample(
  0:3,sum(is.na(pf[['age']]),na.rm=TRUE),
  prob=n_age[['n_users']],replace=TRUE)
n_users = pf[,c(
  'state','age','gender','n_users'
)] %>% group_by(state,age,gender) %>% summarise_all(sum,na.rm=TRUE)
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]

# Unemployment tweets
tw_regex = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_regex.csv'))
tw_regex = tw_regex[,c('user_id','tweet_timestamp','is_unemployed')]
colnames(tw_regex) = c('user_id','tweet_timestamp','regex')
tw_bert = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_bert.csv'))
tw_bert = tw_bert[,c('user_id','tweet_timestamp','is_unemployed')]
colnames(tw_bert) = c('user_id','tweet_timestamp','bert')
tw = merge(tw_bert,tw_regex,by=c('user_id','tweet_timestamp'),all=TRUE)
rm(tw_regex,tw_bert)
tw[['date']] = as.Date(as.POSIXct(tw[['tweet_timestamp']],origin='1970-01-01'))
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw[[paste0('bert',990)]] = (tw[['bert']] > 990/1000) & !is.na(tw[['bert']])
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw = tw[,c(
  'user_id','date','regex',paste0('bert',990)
),with=FALSE] %>% group_by(user_id,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,pf,by=c('user_id'),all.x=TRUE,all.y=FALSE)
rm(pf,demo)
tw = tw[tw[['state']]!="",]
tw = tw[!is.na(tw[['gender']]),]
tw[['tm']] = as.yearmon(as.Date(tw[['date']],format='%m/%d/%Y'))
tw[['year']] = year(tw[['date']])
tw[['week']] = strftime(tw[['date']],format='%V')
tw[['date']] = NULL
tw[['tm']] = NULL
tw = tw[,c(
  'state','gender','age','year','week','regex',paste0('bert',990)
)] %>% group_by(state,gender,age,year,week) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,n_users,by=c('state','gender','age'),all.x=TRUE,all.y=TRUE)
tw$week = as.numeric(tw$week)

# Share of unemployed by group 
dist = tw[,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
dist_gender0 = tw[tw$gender==0,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
colnames(dist_gender0) = c(
  'state','year','week','n_users_gender0',
  'regex_gender0',paste0('bert',990,'_gender0'))
dist_gender1 = tw[tw$gender==1,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
colnames(dist_gender1) = c(
  'state','year','week','n_users_gender1',
  'regex_gender1',paste0('bert',990,'_gender1'))
dist_age0 = tw[tw$age==0,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
colnames(dist_age0) = c(
  'state','year','week','n_users_age0',
  'regex_age0',paste0('bert',990,'_age0'))
dist_age1 = tw[tw$age==1,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
colnames(dist_age1) = c(
  'state','year','week','n_users_age1',
  'regex_age1',paste0('bert',990,'_age1'))
dist_age2 = tw[tw$age==2,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
colnames(dist_age2) = c(
  'state','year','week','n_users_age2',
  'regex_age2',paste0('bert',990,'_age2'))
dist_age3 = tw[tw$age==3,c(
  'state','year','week','n_users',
  'regex',paste0('bert',990)
)] %>% group_by(state,year,week) %>% summarise_all(mean,na.rm=TRUE)
colnames(dist_age3) = c(
  'state','year','week','n_users_age3',
  'regex_age3',paste0('bert',990,'_age3'))
dist = merge(dist,dist_gender0,by=c('state','year','week'),all=TRUE)
dist = merge(dist,dist_gender1,by=c('state','year','week'),all=TRUE)
dist = merge(dist,dist_age0,by=c('state','year','week'),all=TRUE)
dist = merge(dist,dist_age1,by=c('state','year','week'),all=TRUE)
dist = merge(dist,dist_age2,by=c('state','year','week'),all=TRUE)
dist = merge(dist,dist_age3,by=c('state','year','week'),all=TRUE)
dist[is.na(dist)] = 0
dist$tm = dist$year * 100 + dist$week
dist[['regex']] = dist[['regex']] * dist[['n_users']]
dist[['regex_age0']] = dist[['regex_age0']] / dist[['n_users_age0']]
dist[['regex_age1']] = dist[['regex_age1']] / dist[['n_users_age1']]
dist[['regex_age2']] = dist[['regex_age2']] / dist[['n_users_age2']]
dist[['regex_age3']] = dist[['regex_age3']] / dist[['n_users_age3']]
dist[['regex_gender0']] = dist[['regex_gender0']] / dist[['n_users_gender0']] 
dist[['regex_gender1']] = dist[['regex_gender1']] / dist[['n_users_gender1']]
dist = dist[,c(
  'state','tm','year','week',
  'n_users',paste0('n_users_age',0:3),paste0('n_users_gender',0:1),
  'regex','bert990',
  paste0('regex_age',0:3),paste0('bert990_age',0:3),
  paste0('regex_gender',0:1),paste0('bert990_gender',0:1)
)]
dist = dist %>% group_by(state,tm,year,week) %>% summarise_all(sum,na.rm=TRUE)
dist = dist[dist$state!="",]
pop = sum(as.matrix(dist[dist$tm==202001,'n_users']),na.rm=TRUE)/10^3

# Actual UI claims data by state
temp1 = fread(paste0("../preprocessing/data/ui_state_iso2_","US",".csv"))
temp1$tm = temp1$year * 100 + temp1$week
dist = merge(
  dist, temp1[,c('tm','state','iclaimnsa'),with=FALSE],
  by = c('tm','state'), 
  all.x = TRUE, all.y = FALSE)
dist = as.data.frame(dist)
dist[['year']] = floor(dist[['tm']]/100)
dist = dist %>% group_by(year,state) %>% summarise_all(sum,na.rm=TRUE)

# Actual UI claims data by age or gender
demo = fread(paste0("../preprocessing/data/unemp_dist0.csv"))
colnames(demo) = c(
  'tm','u','ufull','upart',
  'u1617','u1619','ufull1619','upart1619','u1624','u1819','u20p','u2024',
  'u25p','u2534','u2554','u3544','u4554','u55p',
  'umales','umaleu','umalefulls','umalefullu','umaleparts','umalepartu',
  'ufemales','ufemaleu','ufemalefulls','ufemalefullu','ufemaleparts','ufemalepartu'
)
demo[['tm']] = as.yearmon(as.Date(demo[['tm']],format='%m/%d/%Y'))
demo[['year']] = year(demo[['tm']])
demo[['ui_female']] = demo[['ufemales']]
demo[['ui_male']] = demo[['umales']]
demo[['ui_age_lt20']] = demo[['u1619']]
demo[['ui_age_2030']] = demo[['u2024']]
demo[['ui_age_3040']] = demo[['u2534']]
demo[['ui_age_gt40']] = demo[['u3544']]
demo = demo[,c(
  'year','ui_female','ui_male',
  'ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
demo = demo %>% group_by(year) %>% summarise_all(sum,na.rm=TRUE)
dist = merge(
  dist, demo,
  by = c('year'), 
  all.x = TRUE, all.y = FALSE)

# Population size
census = read_excel(
  paste0("../preprocessing/data/NST-EST2021-POP.xlsx"),
  range = "A4:D60"
)
census = census[,c(1,3)]
colnames(census) = c('srd_text','pop')
census$srd_text = gsub('\\.','',census$srd_text)
census = merge(
  census,
  read.csv(
    file = paste0("../preprocessing/data/srd_code_to_state.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE),
  by = 'srd_text',all=FALSE)
census[['srd_text']] = NULL
idx = (census$state=="DC")
census[idx,'pop'] = census[idx,'pop']*4
dist = merge(
  dist, census,
  by = 'state', all.x = TRUE, all.y = FALSE
)

#######################################################
# Distribution of unemployed users by state

demo = dist[
  dist$year>=2020 & dist$year<=2021 & 
    !is.na(dist[['iclaimnsa']]) & dist[['iclaimnsa']] > 0 
  & !is.na(dist[[paste0('bert990')]]) & dist[[paste0('bert990')]] > 0,c(
    'state','iclaimnsa','regex','bert990'
  )] 
nrows = nrow(dist)
demo_sd = demo %>% group_by(state) %>% summarise_all(sd,na.rm=TRUE)
demo = demo %>% group_by(state) %>% summarise_all(sum,na.rm=TRUE)
colnames(demo) = c('state','ui','regex','bert')
colnames(demo_sd) = colnames(demo)
demo[['pop']] = NULL
demo[['ui']] = sqrt(demo[['ui']])
demo[['regex']] = sqrt(demo[['regex']])
demo[['bert']] = sqrt(demo[['bert']])
demo[['ui_dist']] = demo[['ui']] / sum(demo[['ui']],na.rm=TRUE)
demo[['regex_dist']] = demo[['regex']] / sum(demo[['regex']],na.rm=TRUE)
demo[['bert_dist']] = demo[['bert']] / sum(demo[['bert']],na.rm=TRUE)
demo_sd[['ui_dist']] = demo_sd[['ui']] / sqrt(sum(demo_sd[['ui']]^2,na.rm=TRUE)) / sqrt(nrows)
demo_sd[['regex_dist']] = demo_sd[['regex']] / sqrt(sum(demo_sd[['regex']]^2,na.rm=TRUE)) / sqrt(nrows)
demo_sd[['bert_dist']] = demo_sd[['bert']] / sqrt(sum(demo_sd[['bert']]^2,na.rm=TRUE)) / sqrt(nrows)
demo[['regex_kl']] = demo[['ui_dist']] * log(demo[['ui_dist']] / demo[[paste0('regex_dist')]])
demo[['bert_kl']] = demo[['ui_dist']] * log(demo[['ui_dist']] / demo[[paste0('bert_dist')]])
KL_regex = sum(demo[['regex_kl']],na.rm=TRUE)
KL_bert = sum(demo[['bert_kl']],na.rm=TRUE)
print(KL_bert/KL_regex)

write.csv(demo,file='figure1b_dist_state.csv',row.names=FALSE)

#######################################################
# Distribution of unemployed users by age

demo = dist[
  dist$year>=2020 & dist$year<=2021 & 
    !is.na(dist[['ui_age_lt20']]) & dist[['ui_age_lt20']] > 0 
  & !is.na(dist[[paste0('bert990_age',0)]]) 
  & dist[[paste0('bert990_age',0)]] > 0,c(
    'state','pop','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40',
    paste0('regex_age',0:3),paste0('bert990_age',0:3)
  )]
nrows = nrow(dist)
demo[['state']] = 'US'
demo_sd = demo %>% group_by(state) %>% summarise_all(sd,na.rm=TRUE)
demo = demo %>% group_by(state) %>% summarise_all(sum,na.rm=TRUE)
colnames(demo) = c(
  'state','pop',
  'ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40',
  'regex_age_lt20','regex_age_2030','regex_age_3040','regex_age_gt40',
  'bert_age_lt20','bert_age_2030','bert_age_3040','bert_age_gt40')
colnames(demo_sd) = colnames(demo)
ui = demo[,c(
  'state','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
colnames(ui) = c('state','lt20','2030','3040','gt40')
ui = data.table::melt(data.table(ui),id.var='state')
colnames(ui) = c('state','age','ui')
ui_sd = demo_sd[,c(
  'state','ui_age_lt20','ui_age_2030','ui_age_3040','ui_age_gt40')]
colnames(ui_sd) = c('state','lt20','2030','3040','gt40')
ui_sd = data.table::melt(data.table(ui_sd),id.var='state')
colnames(ui_sd) = c('state','age','ui_sd')
ui = merge(ui,ui_sd,by=c('state','age'))
regex = demo[,c(
  'state','regex_age_lt20','regex_age_2030','regex_age_3040','regex_age_gt40')]
colnames(regex) = c('state','lt20','2030','3040','gt40')
regex[['lt20']] = demo[['regex_age_lt20']]
regex[['2030']] = 0.5*demo[['regex_age_2030']]
regex[['3040']] = 0.5*demo[['regex_age_2030']] + 0.5*demo[['regex_age_3040']]
regex[['gt40']] = 0.5*demo[['regex_age_3040']] + demo[['regex_age_gt40']]
regex = data.table::melt(data.table(regex),id.var='state')
colnames(regex) = c('state','age','regex')
regex_sd = demo_sd[,c(
  'state','regex_age_lt20','regex_age_2030','regex_age_3040','regex_age_gt40')]
colnames(regex_sd) = c('state','lt20','2030','3040','gt40')
regex_sd[['lt20']] = demo_sd[['regex_age_lt20']]
regex_sd[['2030']] = 0.5*demo_sd[['regex_age_2030']]
regex_sd[['3040']] = 0.5*demo_sd[['regex_age_2030']] + 0.5*demo_sd[['regex_age_3040']]
regex_sd[['gt40']] = 0.5*demo_sd[['regex_age_3040']] + demo_sd[['regex_age_gt40']]
regex_sd = data.table::melt(data.table(regex_sd),id.var='state')
colnames(regex_sd) = c('state','age','regex_sd')
regex = merge(regex,regex_sd,by=c('state','age'))
bert = demo[,c(
  'state','bert_age_lt20','bert_age_2030','bert_age_3040','bert_age_gt40')]
colnames(bert) = c('state','lt20','2030','3040','gt40')
bert[['lt20']] = demo[['bert_age_lt20']]
bert[['2030']] = 0.5*demo[['bert_age_2030']]
bert[['3040']] = 0.5*demo[['bert_age_2030']] + 0.5*demo[['bert_age_3040']]
bert[['gt40']] = 0.5*demo[['bert_age_3040']] + demo[['bert_age_gt40']]
bert = data.table::melt(data.table(bert),id.var='state')
colnames(bert) = c('state','age','bert')
bert_sd = demo_sd[,c(
  'state','bert_age_lt20','bert_age_2030','bert_age_3040','bert_age_gt40')]
colnames(bert_sd) = c('state','lt20','2030','3040','gt40')
bert_sd[['lt20']] = demo_sd[['bert_age_lt20']]
bert_sd[['2030']] = 0.5*demo_sd[['bert_age_2030']]
bert_sd[['3040']] = 0.5*demo_sd[['bert_age_2030']] + 0.5*demo_sd[['bert_age_3040']]
bert_sd[['gt40']] = 0.5*demo_sd[['bert_age_3040']] + demo_sd[['bert_age_gt40']]
bert_sd = data.table::melt(data.table(bert_sd),id.var='state')
colnames(bert_sd) = c('state','age','bert_sd')
bert = merge(bert,bert_sd,by=c('state','age'))
uitw = merge(ui,regex,by=c('state','age'),all=TRUE)
uitw = merge(uitw,bert,by=c('state','age'),all=TRUE)
uitw[['ui_dist']] = uitw[['ui']] / sum(uitw[['ui']],na.rm=TRUE)
uitw[['regex_dist']] = uitw[['regex']] / sum(uitw[['regex']],na.rm=TRUE)
uitw[['bert_dist']] = uitw[['bert']] / sum(uitw[['bert']],na.rm=TRUE)
uitw[['ui_sd_dist']] = uitw[['ui_sd']] / sqrt(sum(uitw[['ui_sd']]^2,na.rm=TRUE)) / sqrt(nrows)
uitw[['regex_sd_dist']] = uitw[['regex_sd']] / sqrt(sum(uitw[['regex_sd']]^2,na.rm=TRUE)) / sqrt(nrows)
uitw[['bert_sd_dist']] = uitw[['bert_sd']] / sqrt(sum(uitw[['bert_sd']]^2,na.rm=TRUE)) / sqrt(nrows)
uitw[['regex_kl']] = uitw[['ui_dist']] * log(uitw[['ui_dist']] / uitw[[paste0('regex_dist')]])
uitw[['bert_kl']] = uitw[['ui_dist']] * log(uitw[['ui_dist']] / uitw[[paste0('bert_dist')]])
KL_regex = sum(uitw[['regex_kl']],na.rm=TRUE)
KL_bert = sum(uitw[['bert_kl']],na.rm=TRUE)
print(KL_bert/KL_regex)

write.csv(uitw,file='figure1c_dist_age.csv',row.names=FALSE)

#######################################################
# Distribution of unemployed users by gender

demo = dist[
  dist$year>=2020 & dist$year<=2021 & 
    !is.na(dist[['ui_female']]) & dist[['ui_female']] > 0 
  & !is.na(dist[[paste0('bert990_age',0)]]) 
  & dist[[paste0('bert990_age',0)]] > 0,c(
    'state','pop','ui_female','ui_male',
    paste0('regex_gender',0:1),
    paste0('bert990_gender',0:1)
  )] 
nrows = nrow(dist)
demo[['state']] = 'US'
demo_sd = demo %>% group_by(state) %>% summarise_all(sd,na.rm=TRUE)
demo = demo %>% group_by(state) %>% summarise_all(sum,na.rm=TRUE)
colnames(demo) = c(
  'state','pop',
  'ui_female','ui_male',
  'regex_female','regex_male',
  'bert_female','bert_male')
colnames(demo_sd) = colnames(demo)
ui = demo[,c('state','ui_female','ui_male')]
colnames(ui) = c('state','female','male')
ui = data.table::melt(data.table(ui),id.var='state')
colnames(ui) = c('state','gender','ui')
ui_sd = demo_sd[,c('state','ui_female','ui_male')]
colnames(ui_sd) = c('state','female','male')
ui_sd = data.table::melt(data.table(ui_sd),id.var='state')
colnames(ui_sd) = c('state','gender','ui_sd')
ui = merge(ui,ui_sd,by=c('state','gender'))
regex = demo[,c('state','regex_female','regex_male')]
colnames(regex) = c('state','female','male')
regex = data.table::melt(data.table(regex),id.var='state')
colnames(regex) = c('state','gender','regex')
regex_sd = demo_sd[,c('state','regex_female','regex_male')]
colnames(regex_sd) = c('state','female','male')
regex_sd = data.table::melt(data.table(regex_sd),id.var='state')
colnames(regex_sd) = c('state','gender','regex_sd')
regex = merge(regex,regex_sd,by=c('state','gender'))
bert = demo[,c('state','bert_female','bert_male')]
colnames(bert) = c('state','female','male')
bert = data.table::melt(data.table(bert),id.var='state')
colnames(bert) = c('state','gender','bert')
bert_sd = demo_sd[,c('state','bert_female','bert_male')]
colnames(bert_sd) = c('state','female','male')
bert_sd = data.table::melt(data.table(bert_sd),id.var='state')
colnames(bert_sd) = c('state','gender','bert_sd')
bert = merge(bert,bert_sd,by=c('state','gender'))
uitw = merge(ui,regex,by=c('state','gender'),all=TRUE)
uitw = merge(uitw,bert,by=c('state','gender'),all=TRUE)
uitw[['ui_dist']] = uitw[['ui']] / sum(uitw[['ui']],na.rm=TRUE)
uitw[['regex_dist']] = uitw[['regex']] / sum(uitw[['regex']],na.rm=TRUE)
uitw[['bert_dist']] = uitw[['bert']] / sum(uitw[['bert']],na.rm=TRUE)
uitw[['ui_sd_dist']] = uitw[['ui_sd']] / sqrt(sum(uitw[['ui_sd']]^2,na.rm=TRUE)) / sqrt(nrows)
uitw[['regex_sd_dist']] = uitw[['regex_sd']] / sqrt(sum(uitw[['regex_sd']]^2,na.rm=TRUE)) / sqrt(nrows)
uitw[['bert_sd_dist']] = uitw[['bert_sd']] / sqrt(sum(uitw[['bert_sd']]^2,na.rm=TRUE)) / sqrt(nrows)
uitw[['regex_kl']] = uitw[['ui_dist']] * log(uitw[['ui_dist']] / uitw[[paste0('regex_dist')]])
uitw[['bert_kl']] = uitw[['ui_dist']] * log(uitw[['ui_dist']] / uitw[[paste0('bert_dist')]])
KL_regex = sum(uitw[['regex_kl']],na.rm=TRUE)
KL_bert = sum(uitw[['bert_kl']],na.rm=TRUE)
print(KL_bert/KL_regex)

write.csv(uitw,file='figure1d_dist_gender.csv',row.names=FALSE)
