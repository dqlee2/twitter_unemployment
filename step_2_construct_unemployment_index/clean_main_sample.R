##############################################################
# Construct unemployment indices

rm(list = ls())

## National level

# Maximum lag order
lagmaxs = c(4)

# List of twitter models
cutoffs = c('990')
models = c("regex",'bert990')
spells = c(0)
labels = c(paste0('adj',spells,'_','is_unemployed'))
mas = c(4)
ds = c(5)
d = 5
stats = c('n_users')
statmas = paste0('n_users','_ma',mas)

# Sample periods
trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252
windows = c(3*52)

# Alternative models
specs = c('AR','consensus','regex','BERT')

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
n_users = pf[,c(
  'state','age','gender','n_users'
)] %>% group_by(state,age,gender) %>% summarise_all(sum,na.rm=TRUE)
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]
n_gender = n_users[,c(
  'gender','n_users'
)] %>% group_by(gender) %>% summarise_all(sum,na.rm=TRUE)
n_gender[['n_users']] = (
  n_gender[['n_users']] / sum(n_gender[['n_users']],na.rm=TRUE))
n_age = n_users[,c(
  'age','n_users'
)] %>% group_by(age) %>% summarise_all(sum,na.rm=TRUE)
n_age[['n_users']] = (
  n_age[['n_users']] / sum(n_age[['n_users']],na.rm=TRUE))
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
tw_regex = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_regex.csv'))
tw_regex = tw_regex[,c('user_id','tweet_timestamp','is_unemployed')]
colnames(tw_regex) = c('user_id','tweet_timestamp','regex')
tw_bert = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_bert.csv'))
tw_bert = tw_bert[,c('user_id','tweet_timestamp','is_unemployed')]
colnames(tw_bert) = c('user_id','tweet_timestamp','bert')
tw = merge(tw_bert,tw_regex,by=c('user_id','tweet_timestamp'),all=TRUE)
rm(tw_regex,tw_bert)
tw[['date']] = as.Date(
  as.POSIXct(tw[['tweet_timestamp']],origin='1970-01-01'))
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw[['bert990']] = (tw[['bert']] > 990/1000) & !is.na(tw[['bert']])
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw = tw[,c(
  'user_id','date','regex','bert990'
),with=FALSE] %>% group_by(user_id,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,pf,by=c('user_id'),all.x=TRUE,all.y=FALSE)
tw[['user_statuses_count']] = tw[['user_statuses_count']] / (1+tw[['acctage']])
for (var in c(
  'gender','age','org'
)) {
  tw[is.na(tw[[var]]),var] = -1
  tw[[var]] = factor(tw[[var]])
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count' # ,
)) {
  tw[is.na(tw[[var]]),var] = 0
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count'
)) {
  tw[[var]] = log(1+tw[[var]])
}
tw[['state']] = factor(tw[['state']])

ds = c(5)
tw = tw[tw[['state']]!="",]
tw = tw[!is.na(tw[['gender']]),]
tw = tw[,c(
  'state','gender','age','date','regex','bert990'
)] %>% group_by(state,gender,age,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,n_users,by=c('state','gender','age'),all.x=TRUE,all.y=TRUE)
census = as.data.frame(fread('../preprocessing/data/sc-est2022-agesex-civ.csv'))
census[['age']] = NA
census[(census[['AGE']]>=15) & (census[['AGE']]<=20),'age'] = 0
census[(census[['AGE']]>=21) & (census[['AGE']]<=30),'age'] = 1
census[(census[['AGE']]>=31) & (census[['AGE']]<=40),'age'] = 2
census[(census[['AGE']]>=41) & (census[['AGE']]<=50),'age'] = 3
census = census[
  (census$SUMLEV==40) & (census$SEX!=0) 
  & (census$AGE>=15) & (census$AGE<=50),c(
    'NAME','SEX','age','ESTBASE2020_CIV'
  )]
colnames(census) = c('srd_text','gender','age','pop')
census[['gender']] = (census[['gender']]==2)*1 # Female dummy
census = merge(
  census,
  read.csv(
    file = paste0("../preprocessing/data/srd_code_to_state.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE),
  by = 'srd_text',all=FALSE)
census[['srd_text']] = NULL
census[census$state=="DC",'pop'] = census[census$state=="DC",'pop'] * 4
census = census %>% group_by(state,gender,age) %>% summarise_all(sum,na.rm=TRUE)
tw[['age']] = as.integer(as.character(tw[['age']]))
tw[['gender']] = as.integer(as.character(tw[['gender']]))
tw = merge(tw,census,by=c('state','age','gender'),all.x=TRUE,all.y=FALSE)
pop_total = sum(tw[['pop']],na.rm=TRUE)
tw[['adj0_is_unemployed_regex_mrp']] = (
  tw[['pop']] * (tw[['regex']] / tw[['n_users']]) / pop_total)
tw[[paste0('adj0_is_unemployed_bert990_mrp')]] = (
  tw[['pop']] * (tw[['bert990']] / tw[['n_users']]) / pop_total)
tw_gender = tw[,c(
  'gender','state','date',
  'adj0_is_unemployed_regex_mrp',
  'adj0_is_unemployed_bert990_mrp'
)] %>% group_by(gender,state,date) %>% summarise_all(sum,na.rm=TRUE)
tw_gender = reshape(
  as.data.frame(tw_gender),
  idvar=c('state','date'),timevar='gender',direction='wide',sep='_')
tw_age = tw[,c(
  'age','state','date',
  'adj0_is_unemployed_regex_mrp',
  'adj0_is_unemployed_bert990_mrp'
)] %>% group_by(age,state,date) %>% summarise_all(sum,na.rm=TRUE)
tw_age[['age']] = tw_age[['age']] + 2
tw_age = reshape(
  as.data.frame(tw_age),
  idvar=c('state','date'),timevar='age',direction='wide',sep='_')
tw_agg = merge(tw_gender,tw_age,by=c('state','date'),all=TRUE)
tw_agg[is.na(tw_agg)] = 0
tw_agg[['adj0_is_unemployed_regex_mrp']] = (
  tw_agg[['adj0_is_unemployed_regex_mrp_1']] 
  + tw_agg[['adj0_is_unemployed_regex_mrp_2']]
  + tw_agg[['adj0_is_unemployed_regex_mrp_3']] 
  + tw_agg[['adj0_is_unemployed_regex_mrp_4']]
  + tw_agg[['adj0_is_unemployed_regex_mrp_5']]
)
tw_agg[['adj0_is_unemployed_bert990_mrp']] = (
  tw_agg[['adj0_is_unemployed_bert990_mrp_1']] 
  + tw_agg[['adj0_is_unemployed_bert990_mrp_2']]
  + tw_agg[['adj0_is_unemployed_bert990_mrp_3']] 
  + tw_agg[['adj0_is_unemployed_bert990_mrp_4']]
  + tw_agg[['adj0_is_unemployed_bert990_mrp_5']]
)
tw_agg = data.table(tw_agg[order(tw_agg$state,tw_agg$date),])
setkey(tw_agg,state,date)
for (var in c(
  'adj0_is_unemployed_regex_mrp',
  'adj0_is_unemployed_bert990_mrp'
)) {
  for (dd in 0:13) {
    tw_agg[,templag:=shift(.SD,n=dd,fill=0,type='lag'),by=state,.SDcols=(var)]
    tw_agg[[paste0(var,'_now',dd)]] = tw_agg[['templag']]
    tw_agg[['templag']] = NULL
  }
}
tw_agg[['tm']] = as.yearmon(as.Date(tw_agg[['date']],format='%m/%d/%Y'))
tw_agg[['year']] = year(tw_agg[['date']])
tw_agg[['week']] = strftime(tw_agg[['date']],format='%V')
tw_agg[['date']] = NULL
tw_agg[['tm']] = NULL
tw_agg = tw_agg %>% group_by(state,year,week) %>% summarise_all(sum,na.rm=TRUE)

# List of maximum lags
lagmaxs = c(4)

# List of twitter models
cutoffs = c('990')
models = c("regex","bert990")
ds = c(5)
spells = c(0)
labels = paste0('adj',spells,'_','is_unemployed')
mas = c(4)
stats = c('n_users')
statmas = paste0('n_users','_ma',mas)

trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252
windows = c(4*52-52)

specs = c('AR','consensus','regex','BERT')
samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
uivars = c('iclaimnsa')
activities = 0:7
ages = 0:4

temp0 = fread(
  paste0("../preprocessing/data/locations_","US","_num_state.csv")
)[,c('state','num_state')] %>% unique()
p5 = tw_agg
p5 = merge(p5,temp0,by=c('state'),all.x=TRUE,all.y=FALSE)
p5[['state']] = NULL
dt = fread(paste0("../preprocessing/data/n_users_national_week_demo_US.csv"))
colnames(dt) = sub("age-1","age4",colnames(dt))
dt = dt[,c(
  'year','week','num_state',
  outer(paste0(stats,'_','activity',activities,'_age'),ages,paste0)
),with=FALSE]
p5$week = as.numeric(p5$week) 
dt = merge(
  dt,p5,by=c('year','week','num_state'),all=TRUE
)
dt$tm = dt$year * 100 + dt$week
dt[dt$tm==201653,'tm'] = 201652
dt[dt$tm==201753,'tm'] = 201752
dt[dt$tm==201853,'tm'] = 201852
dt[dt$tm==201953,'tm'] = 201952
dt[dt$tm==202153,'tm'] = 202152
dt[dt$tm==202253,'tm'] = 202252
dt[['year']] = floor(dt[['tm']] / 100)
dt[['week']] = dt[['tm']] - dt[['year']] * 100
dt = dt %>% group_by(num_state,tm,year,week) %>% summarise_all(mean,na.rm=TRUE)
temp0 = fread(
  paste0("../preprocessing/data/locations_","US","_num_state.csv")
)[,c('state','num_state')] %>% unique()
dt = merge(dt,temp0,by=c('num_state'),all.x=TRUE,all.y=TRUE)
dt[['num_state']] = NULL
dt = dt[dt$state!="",]
dt = dt[dt$state!=-1,]
dt = data.table(dt[order(dt$state,dt$tm),])
setkey(dt,state,tm)
for (i in activities) {
  for (a in ages) {
    for (stat in stats) {
      subvar = paste0(stat,'_','activity',i,'_','age',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
    }
  }
}
for (a in 1:d) {
  for (label in labels) {
    for (model in 'bert990') {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (
        (dt[[subvar]]-dt[['templag']])/(dt[[subvar]]+dt[['templag']])*2)
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}
for (dd in 0:13) {
  for (label in labels) {
    for (model in 'bert990') {
      subvar = paste0(label,'_',model,'_','mrp','_now',dd)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (
        (dt[[subvar]]-dt[['templag']])/(dt[[subvar]]+dt[['templag']])*2)
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}
for (a in 1:d) {
  for (label in labels) {
    for (model in "regex") {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (
        (dt[[subvar]]-dt[['templag']])/(dt[[subvar]]+dt[['templag']])*2)
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}
data_iso2 = dt
data_iso2 = as.data.frame(data_iso2)
data_iso2[['state']] = NULL
data_iso2 = data_iso2 %>% group_by(
  year,week
) %>% summarise_all(sum,na.rm=TRUE)
data_iso2$tm = data_iso2$year * 100 + data_iso2$week
pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019
temp1 = read.csv(
  file = paste0("../preprocessing/data/ICNSA.csv"), 
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1[['DATE']] = as.Date(temp1[['DATE']])
temp1[['year']] = year(temp1[['DATE']])
temp1[['week']] = week(temp1[['DATE']])
temp1$tm = temp1$year * 100 + temp1$week
temp1 = temp1[,c('year','week','tm','ICNSA')]
colnames(temp1) = c('year','week','tm','iclaimnsa')
uivars = "iclaimnsa"
temp1[[uivars]] = temp1[[uivars]] / pop
data_iso2 = merge(
  data_iso2, temp1[,c("tm",uivars)],
  by = "tm", all.x = TRUE, all.y = TRUE)
data_iso2[[uivars]] = NULL
data_iso2[data_iso2$tm==201653,'tm'] = 201652
data_iso2[data_iso2$tm==201753,'tm'] = 201752
data_iso2[data_iso2$tm==201853,'tm'] = 201852
data_iso2[data_iso2$tm==201953,'tm'] = 201952
data_iso2[data_iso2$tm==202153,'tm'] = 202152
data_iso2[data_iso2$tm==202253,'tm'] = 202252
data_iso2[['year']] = floor(data_iso2[['tm']] / 100)
data_iso2[['week']] = data_iso2[['tm']] - data_iso2[['year']] * 100
data_iso2 = data_iso2 %>% group_by(
  tm,year,week
) %>% summarise_all(mean,na.rm=TRUE)
data_iso2 = merge(
  data_iso2, temp1[,c("tm",uivars)],
  by = "tm", all.x = TRUE, all.y = FALSE)
temp1 = read.csv(
  file = paste0("../preprocessing/data/restricted/ui_consensus_forecast.csv"), 
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1$tm = temp1$year * 100 + temp1$week
data_iso2 = merge(
  data_iso2, temp1[,c("tm","consensus")], 
  by = "tm", all.x = TRUE, all.y = FALSE)
data_iso2 = data_iso2 %>% fill(consensus)
temp1 = read_excel(
  paste0("../preprocessing/data/release_dates_50.xls"),
  range='A36:A855')
colnames(temp1) = c('release_date')
temp1[['release_date']] = as.Date(temp1[['release_date']])
temp1[['year']] = year(temp1[['release_date']])
temp1[['week']] = week(temp1[['release_date']])
temp1$tm = temp1$year * 100 + temp1$week
temp1[['emp_sit']] = 1
data_iso2 = merge(
  data_iso2, temp1[,c("tm","emp_sit")],
  by = "tm", all.x = TRUE, all.y = FALSE)
data_iso2[is.na(data_iso2[['emp_sit']]),'emp_sit'] = 0
data_iso2 = data_iso2 %>% group_by(
  tm,year,week
) %>% summarise_all(mean,na.rm=TRUE)
data_iso2[['eip']] = 0
data_iso2[data_iso2$tm==202015,'eip'] = 1
data_iso2[data_iso2$tm==202052,'eip'] = 1
data_iso2[data_iso2$tm==202053,'eip'] = 1
data_iso2[data_iso2$tm==202111,'eip'] = 1
data_iso2[data_iso2$tm==202128,'eip'] = 1
data_iso2$X = NULL
data_iso2$year = floor(data_iso2$tm / 100)
data_iso2$week = data_iso2$tm - data_iso2$year * 100
data_iso2$quarter = floor(data_iso2$week / 14) + 1
data_iso2$month = floor(data_iso2$week / 4) + 1
data_iso2[['templag']] = shift(data_iso2[['consensus']],n=1)
idx = (is.na(data_iso2[['consensus']]))
data_iso2[idx,'consensus'] = data_iso2[idx,'templag']
data_iso2[['templag']] = NULL
data_iso2[['consensus']] = shift(data_iso2[['consensus']],n=-1)
for (var in c(uivars)) {
  data_iso2[[var]] = data_iso2[[var]] / 10^6
  data_iso2[['templag']] = shift(data_iso2[[var]],n=1)
  idx = is.na(data_iso2[[var]])
  data_iso2[idx,var] = data_iso2[idx,'templag']
  data_iso2[['templag']] = NULL
  data_iso2[['ce']] = data_iso2[[var]] - data_iso2[['consensus']]/10^3
}

# Fill in missing values with lags
for (i in activities) {
  for (a in ages) {
    for (stat in stats) {
      subvar = paste0(stat,'_','activity',i,'_','age',a)
      data_iso2[['templag']] = shift(data_iso2[[subvar]],n=1)
      idx = is.na(data_iso2[[subvar]])
      data_iso2[idx,stat] = data_iso2[idx,'templag']
      data_iso2[['templag']] = NULL
    }
  }
}
for (a in 1:d) {
  for (label in c(labels)) {
    for (model in models) {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      data_iso2[['templag']] = shift(data_iso2[[subvar]],n=1)
      idx = is.na(data_iso2[[subvar]])
      data_iso2[idx,subvar] = data_iso2[idx,'templag']
      data_iso2[['templag']] = NULL
    }
  }
}

# Count total users across groups
for (stat in stats) {
  data_iso2[[paste0(stat,'_ma',0)]] = 0
  data_iso2[[paste0(stat,'_ma',4)]] = 0
  data_iso2[[paste0(stat,'_ma',8)]] = 0
  for (a in ages) {
    data_iso2[[paste0(stat,'_','age',a)]] = 0
    for (i in activities) {
      data_iso2[[paste0(stat,'_','age',a)]] = (
        data_iso2[[paste0(stat,'_','age',a)]] 
        + data_iso2[[paste0(stat,'_','activity',i,'_','age',a)]]
      )
    }
  }
  for (a in ages) {
    subvar = paste0(stat,'_','age',a)
    data_iso2[['templag1']] = shift(data_iso2[[subvar]],n=1)
    data_iso2[['templag2']] = shift(data_iso2[[subvar]],n=2)
    data_iso2[['templag3']] = shift(data_iso2[[subvar]],n=3)
    data_iso2[['templag4']] = shift(data_iso2[[subvar]],n=4)
    data_iso2[['templag5']] = shift(data_iso2[[subvar]],n=5)
    data_iso2[['templag6']] = shift(data_iso2[[subvar]],n=6)
    data_iso2[['templag7']] = shift(data_iso2[[subvar]],n=7)
    data_iso2[[paste0(subvar,'_ma',0)]] = data_iso2[[paste0(subvar)]]
    data_iso2[[paste0(subvar,'_ma',4)]] = data_iso2[[paste0(subvar)]]
    data_iso2[[paste0(subvar,'_ma',8)]] = data_iso2[[paste0(subvar)]]
    data_iso2[,paste0('templag',1:7)] = NULL
    data_iso2[[paste0(stat,'_ma',0)]] = (
      data_iso2[[paste0(stat,'_ma',0)]] 
      + data_iso2[[paste0(subvar,'_ma',0)]]
    )
    data_iso2[[paste0(stat,'_ma',4)]] = (
      data_iso2[[paste0(stat,'_ma',4)]] 
      + data_iso2[[paste0(subvar,'_ma',4)]]
    )
    data_iso2[[paste0(stat,'_ma',8)]] = (
      data_iso2[[paste0(stat,'_ma',8)]] 
      + data_iso2[[paste0(subvar,'_ma',8)]]
    )
  }
}

# Count unemployed users across groups
idx = (data_iso2$tm==201840)
idx2 = (data_iso2$tm==201839)
for (p in 1:d) {
  vname = paste0('adj0_is_unemployed_bert990_mrp_',p)
  data_iso2[idx,vname] = data_iso2[idx2,vname]
}
for (dd in 0:13) {
  vname = paste0('adj0_is_unemployed_bert990_mrp_now',dd)
  data_iso2[idx,vname] = data_iso2[idx2,vname]
}
for (label in labels) {
  for (stat in statmas) {
    for (model in models) {
      for (p in 1:d) {
        data_iso2[[paste0(label,'_',model,'_','mrp','_',p,'_',stat)]] = (
          data_iso2[[paste0(label,'_',model,'_','mrp','_',p)]]
        )
      }
      for (dd in 0:13) {
        data_iso2[[paste0(label,'_',model,'_','mrp','_now',dd,'_',stat)]] = (
          data_iso2[[paste0(label,'_',model,'_','mrp','_now',dd)]]
        )
      }
    }
  }
}
data_iso2 = data_iso2[data_iso2$tm<=testend,]
data_iso2 = data_iso2[!is.na(data_iso2$tm),]
data_iso2 = data_iso2[order(data_iso2$tm),]
data_iso2 = data_iso2[data_iso2$tm<=testend & !is.na(data_iso2[[uivars[1]]]),]
data_iso2 = data_iso2[order(data_iso2$tm),]
data_iso2[['dummy_covid']] = (data_iso2$tm>=covidstart & data_iso2$tm<=covidend)
data_iso2 = as.data.frame(data_iso2)
traintest = data_iso2[,c(
  'tm','dummy_covid',uivars,'consensus','emp_sit','eip',
  statmas,
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_',1:5,'_','n_users','_ma'),paste0),mas,paste0),
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_now',0:13,'_','n_users','_ma'),paste0),mas,paste0)
)]
write.csv(
  traintest, 
  file = paste0("../preprocessing/data/restricted/data_national.csv"),
  row.names = FALSE)

## Unweighted unemployment index

cutoffs = c(
  500,600,700,
  800,900,950,990,994
)
pcutoffs = rev(c(seq(0.001,0.017,0.002),seq(0.02,1.00,0.02)))
d = 5
ds0 = c(1,2,3,4,5,6)
pc = 1

temp1 = fread(paste0("../preprocessing/data/ui_state_iso2_","US",".csv"))
temp1 = temp1[
  temp1$year>=2016,
  c('state','iclaimnsa')
  ] %>% group_by(state) %>% summarise_all(mean,na.rm=TRUE)
pf = fread('../preprocessing/data/restricted/latest_profiles.csv')
pf[['tweet_timestamp']] = as.Date(
  as.POSIXct(pf[['tweet_timestamp']],origin='1970-01-01'))
pf[['acctage']] = 2022 - year(pf[['tweet_timestamp']])
pf[['tweet_timestamp']] = NULL
pf[['n_users']] = 1
n_users = pf[,c(
  'state','age','gender','n_users'
  )] %>% group_by(state,age,gender) %>% summarise_all(sum,na.rm=TRUE) 
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]
n_gender = n_users[,c('gender','n_users')] %>% group_by(gender) %>% summarise_all(sum,na.rm=TRUE)
n_gender[['n_users']] = n_gender[['n_users']] / sum(n_gender[['n_users']],na.rm=TRUE)
n_age = n_users[,c('age','n_users')] %>% group_by(age) %>% summarise_all(sum,na.rm=TRUE)
n_age[['n_users']] = n_age[['n_users']] / sum(n_age[['n_users']],na.rm=TRUE)
pf[is.na(pf[['gender']]),'gender'] = sample(0:1,sum(is.na(pf[['gender']]),na.rm=TRUE),prob=n_gender[['n_users']],replace=TRUE)
pf[is.na(pf[['age']]),'age'] = sample(0:3,sum(is.na(pf[['age']]),na.rm=TRUE),prob=n_age[['n_users']],replace=TRUE)
n_users = pf[,c('state','age','gender','n_users')] %>% group_by(state,age,gender) %>% summarise_all(sum,na.rm=TRUE) 
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]
tw_regex = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_regex.csv'))
tw_regex = tw_regex[,c('user_id','tweet_timestamp','is_unemployed')]
colnames(tw_regex) = c('user_id','tweet_timestamp','regex')
tw_bert = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_bert.csv'))
tw_bert = tw_bert[,c('user_id','tweet_timestamp','is_unemployed')]
colnames(tw_bert) = c('user_id','tweet_timestamp','bert')
tw = merge(tw_bert,tw_regex,by=c('user_id','tweet_timestamp'),all=TRUE)
rm(tw_regex,tw_bert)
tw[['date']] = as.Date(as.POSIXct(tw[['tweet_timestamp']],origin='1970-01-01'))
tw[['tm']] = as.yearmon(as.Date(tw[['date']],format='%m/%d/%Y'))
tw[['year']] = year(tw[['date']])
tw[['week']] = strftime(tw[['date']],format='%V')
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
for (c in cutoffs) {
  tw[[paste0('bert',c)]] = (tw[['bert']] > c/1000) & !is.na(tw[['bert']])
}
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw = tw[,c(
  'user_id','year','week','regex',paste0('bert',cutoffs)
),with=FALSE] %>% group_by(user_id,year,week) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,pf,by=c('user_id'),all.x=TRUE,all.y=FALSE)
tw[['user_statuses_count']] = tw[['user_statuses_count']] / (1+tw[['acctage']])
for (var in c(
  'gender','age','org' 
)) {
  tw[is.na(tw[[var]]),var] = -1
  tw[[var]] = factor(tw[[var]])
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count' # ,
)) {
  tw[is.na(tw[[var]]),var] = 0
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count'
)) {
  tw[[var]] = log(1+tw[[var]])
}
tw[['state']] = factor(tw[['state']])
ds = c(5)
tw = tw[tw[['state']]!="",]
tw = tw[!is.na(tw[['gender']]),]
tw = tw[,c(
  'state','gender','age','year','week','regex',paste0('bert',cutoffs)
)] %>% group_by(state,gender,age,year,week) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,n_users,by=c('state','gender','age'),all.x=TRUE,all.y=TRUE)
census = as.data.frame(fread('../preprocessing/data/sc-est2022-agesex-civ.csv'))
census[['age']] = NA
census[(census[['AGE']]>=15) & (census[['AGE']]<=20),'age'] = 0
census[(census[['AGE']]>=21) & (census[['AGE']]<=30),'age'] = 1
census[(census[['AGE']]>=31) & (census[['AGE']]<=40),'age'] = 2
census[(census[['AGE']]>=41) & (census[['AGE']]<=50),'age'] = 3
census = census[
  (census$SUMLEV==40) & (census$SEX!=0) & (census$AGE>=15) & (census$AGE<=50),c(
    'NAME','SEX','age','ESTBASE2020_CIV'
  )]
colnames(census) = c('srd_text','gender','age','pop')
census[['gender']] = (census[['gender']]==2)*1 # Female dummy
census = merge(
  census,
  read.csv(
    file = paste0("../preprocessing/data/srd_code_to_state.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE),
  by = 'srd_text',all=FALSE)
census[['srd_text']] = NULL
census[census$state=="DC",'pop'] = census[census$state=="DC",'pop'] * 4
census = census %>% group_by(state,gender,age) %>% summarise_all(sum,na.rm=TRUE)
tw[['age']] = as.integer(as.character(tw[['age']]))
tw[['gender']] = as.integer(as.character(tw[['gender']]))
tw = merge(tw,census,by=c('state','age','gender'),all.x=TRUE,all.y=FALSE)
pop_total = sum(tw[['pop']],na.rm=TRUE)
tw[['adj0_is_unemployed_regex_mrp']] = (tw[['regex']] / tw[['n_users']])
for (c in cutoffs) {
  tw[[paste0('adj0_is_unemployed_bert',c,'_mrp')]] = (tw[[paste0('bert',c)]] / tw[['n_users']])
}
tw_gender = tw[,c(
  'gender','state','year','week',
  'adj0_is_unemployed_regex_mrp',paste0('adj0_is_unemployed_bert',cutoffs,'_mrp')
)] %>% group_by(gender,state,year,week) %>% summarise_all(sum,na.rm=TRUE)
tw_gender = reshape(
  as.data.frame(tw_gender),
  idvar=c('state','year','week'),timevar='gender',direction='wide',sep='_')
tw_age = tw[,c(
  'age','state','year','week',
  'adj0_is_unemployed_regex_mrp',paste0('adj0_is_unemployed_bert',cutoffs,'_mrp')
)] %>% group_by(age,state,year,week) %>% summarise_all(sum,na.rm=TRUE)
tw_age[['age']] = tw_age[['age']] + 2
tw_age = reshape(as.data.frame(tw_age),idvar=c('state','year','week'),timevar='age',direction='wide',sep='_')
tw_agg = merge(tw_gender,tw_age,by=c('state','year','week'),all=TRUE)
tw_agg[is.na(tw_agg)] = 0

# List of maximum lags
lagmaxs = c(4)
ds = c(5)

# List of twitter models
cutoffs = c(
  '800','900','950','990','994'
)
models = c(
  "regex",
  paste0("bert",cutoffs)
)
spells = c(0)
labels = c(
  paste0('adj',spells,'_','is_unemployed')
)
mas = c(4)
stats = c('n_users')
statmas = paste0('n_users','_ma',mas)

trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252
windows = c(2*52)

specs = c('AR','consensus','regex','BERT')
samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
uivars = c('iclaimnsa')
activities = 0:7
ages = 0:4

temp0 = fread(
  paste0("../preprocessing/data/locations_","US","_num_state.csv")
)[,c('state','num_state')] %>% unique()
p5 = tw_agg
p5 = merge(p5,temp0,by=c('state'),all.x=TRUE,all.y=FALSE)
p5[['state']] = NULL

dt = fread(paste0("../preprocessing/data/n_users_national_week_demo_US.csv"))
colnames(dt) = sub("age-1","age4",colnames(dt))
dt = dt[,c(
  'year','week','num_state',
  outer(paste0(stats,'_','activity',activities,'_age'),ages,paste0) # ,
),with=FALSE]
p5$week = as.numeric(p5$week) 
dt = merge(
  dt,p5,by=c('year','week','num_state'),all=TRUE
)
dt$tm = dt$year * 100 + dt$week
dt[dt$tm==201653,'tm'] = 201652
dt[dt$tm==201753,'tm'] = 201752
dt[dt$tm==201853,'tm'] = 201852
dt[dt$tm==201953,'tm'] = 201952
dt[dt$tm==202153,'tm'] = 202152
dt[dt$tm==202253,'tm'] = 202252
dt[['year']] = floor(dt[['tm']] / 100)
dt[['week']] = dt[['tm']] - dt[['year']] * 100
dt = dt %>% group_by(num_state,tm,year,week) %>% summarise_all(mean,na.rm=TRUE)
temp0 = fread(
  paste0("../preprocessing/data/locations_","US","_num_state.csv")
)[,c('state','num_state')] %>% unique()
dt = merge(dt,temp0,by=c('num_state'),all.x=TRUE,all.y=TRUE)
dt[['num_state']] = NULL
dt = dt[dt$state!="",]
dt = dt[dt$state!=-1,]
dt = data.table(dt[order(dt$state,dt$tm),])
setkey(dt,state,tm)
for (i in activities) { #
  for (a in ages) {
    for (stat in stats) {
      subvar = paste0(stat,'_','activity',i,'_','age',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
    }
  }
}

for (a in 1:d) {
  for (label in labels) {
    for (model in paste0("bert",cutoffs)) {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[dt$tm==201840,c(subvar)] = dt[dt$tm==201839,c(subvar)]
      
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (dt[[subvar]] - dt[['templag']]) / (dt[[subvar]] + dt[['templag']]) * 2
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}
data_iso2 = dt
data_iso2 = as.data.frame(data_iso2)
data_iso2[['state']] = NULL
data_iso2 = data_iso2 %>% group_by(year,week) %>% summarise_all(sum,na.rm=TRUE)
data_iso2$tm = data_iso2$year * 100 + data_iso2$week
pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019
temp1 = read.csv(
  file = paste0("../preprocessing/data/ICNSA.csv"), 
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1[['DATE']] = as.Date(temp1[['DATE']])
temp1[['year']] = year(temp1[['DATE']])
temp1[['week']] = week(temp1[['DATE']])
temp1$tm = temp1$year * 100 + temp1$week
temp1 = temp1[,c('year','week','tm','ICNSA')]
colnames(temp1) = c('year','week','tm','iclaimnsa')
uivars = "iclaimnsa"
temp1[[uivars]] = temp1[[uivars]] / pop
data_iso2 = merge(
  data_iso2, temp1[,c("tm",uivars)],
  by = "tm", all.x = TRUE, all.y = TRUE)
data_iso2[[uivars]] = NULL
data_iso2[data_iso2$tm==201653,'tm'] = 201652
data_iso2[data_iso2$tm==201753,'tm'] = 201752
data_iso2[data_iso2$tm==201853,'tm'] = 201852
data_iso2[data_iso2$tm==201953,'tm'] = 201952
data_iso2[data_iso2$tm==202153,'tm'] = 202152
data_iso2[data_iso2$tm==202253,'tm'] = 202252
data_iso2[['year']] = floor(data_iso2[['tm']] / 100)
data_iso2[['week']] = data_iso2[['tm']] - data_iso2[['year']] * 100
data_iso2 = data_iso2 %>% group_by(tm,year,week) %>% summarise_all(mean,na.rm=TRUE)
data_iso2 = merge(
  data_iso2, temp1[,c("tm",uivars)],
  by = "tm", all.x = TRUE, all.y = FALSE)
temp1 = read.csv(
  file = paste0("../preprocessing/data/restricted/ui_consensus_forecast.csv"), 
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1$tm = temp1$year * 100 + temp1$week
data_iso2 = merge(
  data_iso2, temp1[,c("tm","consensus")], 
  by = "tm", all.x = TRUE, all.y = FALSE)
data_iso2 = data_iso2 %>% fill(consensus)
temp1 = read_excel(
  paste0("../preprocessing/data/release_dates_50.xls"),
  range='A36:A855')
colnames(temp1) = c('release_date')
temp1[['release_date']] = as.Date(temp1[['release_date']])
temp1[['year']] = year(temp1[['release_date']])
temp1[['week']] = week(temp1[['release_date']])
temp1$tm = temp1$year * 100 + temp1$week
temp1[['emp_sit']] = 1
data_iso2 = merge(
  data_iso2, temp1[,c("tm","emp_sit")],
  by = "tm", all.x = TRUE, all.y = FALSE)
data_iso2[is.na(data_iso2[['emp_sit']]),'emp_sit'] = 0
data_iso2 = data_iso2 %>% group_by(tm,year,week) %>% summarise_all(mean,na.rm=TRUE)
data_iso2[['eip']] = 0
data_iso2[data_iso2$tm==202015,'eip'] = 1
data_iso2[data_iso2$tm==202052,'eip'] = 1
data_iso2[data_iso2$tm==202053,'eip'] = 1
data_iso2[data_iso2$tm==202111,'eip'] = 1
data_iso2[data_iso2$tm==202128,'eip'] = 1
data_iso2$X = NULL
data_iso2$year = floor(data_iso2$tm / 100)
data_iso2$week = data_iso2$tm - data_iso2$year * 100
data_iso2$quarter = floor(data_iso2$week / 14) + 1
data_iso2$month = floor(data_iso2$week / 4) + 1
data_iso2[['templag']] = shift(data_iso2[['consensus']],n=1)
data_iso2[is.na(data_iso2[['consensus']]),'consensus'] = data_iso2[is.na(data_iso2[['consensus']]),'templag']
data_iso2[['templag']] = NULL
data_iso2[['consensus']] = shift(data_iso2[['consensus']],n=-1)
for (var in c(uivars)) {
  data_iso2[[var]] = data_iso2[[var]]
  data_iso2[['templag']] = shift(data_iso2[[var]],n=1)
  data_iso2[is.na(data_iso2[[var]]),var] = data_iso2[is.na(data_iso2[[var]]),'templag']
  data_iso2[['templag']] = NULL
  data_iso2[['ce']] = data_iso2[[var]] - data_iso2[['consensus']] / 10^3
}

# Fill in missing values with lags
for (i in activities) { # 
  for (a in ages) {
    for (stat in stats) {
      subvar = paste0(stat,'_','activity',i,'_','age',a)
      data_iso2[['templag']] = shift(data_iso2[[subvar]],n=1)
      data_iso2[is.na(data_iso2[[subvar]]),stat] = data_iso2[is.na(data_iso2[[subvar]]),'templag']
      data_iso2[['templag']] = NULL
    }
  }
}
for (a in 1:d) {
  for (label in c(labels)) {
    for (model in models) {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      data_iso2[['templag']] = shift(data_iso2[[subvar]],n=1)
      data_iso2[is.na(data_iso2[[subvar]]),subvar] = data_iso2[is.na(data_iso2[[subvar]]),'templag']
      data_iso2[['templag']] = NULL
    }
  }
}

# Count total users across groups
for (stat in stats) {
  data_iso2[[paste0(stat,'_ma',0)]] = 0
  data_iso2[[paste0(stat,'_ma',4)]] = 0
  data_iso2[[paste0(stat,'_ma',8)]] = 0
  for (a in ages) {
    data_iso2[[paste0(stat,'_','age',a)]] = 0
    for (i in activities) { # 
      data_iso2[[paste0(stat,'_','age',a)]] = (
        data_iso2[[paste0(stat,'_','age',a)]] 
        + data_iso2[[paste0(stat,'_','activity',i,'_','age',a)]]
      )
    }
  }
  for (a in ages) { # 
    subvar = paste0(stat,'_','age',a)
    data_iso2[['templag1']] = shift(data_iso2[[subvar]],n=1)
    data_iso2[['templag2']] = shift(data_iso2[[subvar]],n=2)
    data_iso2[['templag3']] = shift(data_iso2[[subvar]],n=3)
    data_iso2[['templag4']] = shift(data_iso2[[subvar]],n=4)
    data_iso2[['templag5']] = shift(data_iso2[[subvar]],n=5)
    data_iso2[['templag6']] = shift(data_iso2[[subvar]],n=6)
    data_iso2[['templag7']] = shift(data_iso2[[subvar]],n=7)
    data_iso2[[paste0(subvar,'_ma',0)]] = data_iso2[[paste0(subvar)]]
    data_iso2[[paste0(subvar,'_ma',4)]] = data_iso2[[paste0(subvar)]]
    data_iso2[[paste0(subvar,'_ma',8)]] = data_iso2[[paste0(subvar)]]
    data_iso2[,paste0('templag',1:7)] = NULL
    data_iso2[[paste0(stat,'_ma',0)]] = (
      data_iso2[[paste0(stat,'_ma',0)]] 
      + data_iso2[[paste0(subvar,'_ma',0)]]
    )
    data_iso2[[paste0(stat,'_ma',4)]] = (
      data_iso2[[paste0(stat,'_ma',4)]] 
      + data_iso2[[paste0(subvar,'_ma',4)]]
    )
    data_iso2[[paste0(stat,'_ma',8)]] = (
      data_iso2[[paste0(stat,'_ma',8)]] 
      + data_iso2[[paste0(subvar,'_ma',8)]]
    )
  }
}

# Count unemployed users across groups
for (label in c(labels)) {
  for (stat in statmas) {
    for (model in models) {
      for (p in 1:d) {
        data_iso2[[paste0(label,'_',model,'_','mrp','_',p,'_',stat)]] = (
          data_iso2[[paste0(label,'_',model,'_','mrp','_',p)]]
        )
      }
    }
  }
}
data_iso2 = data_iso2[data_iso2$tm<=testend,]
data_iso2 = data_iso2[!is.na(data_iso2$tm),]
data_iso2 = data_iso2[order(data_iso2$tm),]
data_iso2 = data_iso2[data_iso2$tm<=testend & !is.na(data_iso2[[uivars[1]]]),]
data_iso2 = data_iso2[order(data_iso2$tm),]
data_iso2[['dummy_covid']] = (data_iso2$tm>=covidstart & data_iso2$tm<=covidend)
data_iso2 = as.data.frame(data_iso2)
traintest = data_iso2[,c(
  'tm','dummy_covid',uivars,'consensus','emp_sit','eip',
  statmas, 
  outer(outer(outer(labels,paste0('_',models,'_'),paste0),paste0('mrp_',1:5,'_','n_users','_ma'),paste0),mas,paste0) # ,
)]
uw = traintest

# Post-stratified unemployment index
mrp = read.csv(
  file = paste0("../preprocessing/data/restricted/data_national.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
mrp[['regex_mrp']] = (
  mrp[['adj0_is_unemployed_regex_mrp_1_n_users_ma4']] 
  + mrp[['adj0_is_unemployed_regex_mrp_2_n_users_ma4']] 
  + mrp[['adj0_is_unemployed_regex_mrp_3_n_users_ma4']] 
  + mrp[['adj0_is_unemployed_regex_mrp_4_n_users_ma4']] 
  + mrp[['adj0_is_unemployed_regex_mrp_5_n_users_ma4']]
)
mrp[[paste0('bert990_mrp')]] = (
  mrp[[paste0('adj0_is_unemployed_bert990_mrp_1_n_users_ma4')]] 
  + mrp[[paste0('adj0_is_unemployed_bert990_mrp_2_n_users_ma4')]] 
  + mrp[[paste0('adj0_is_unemployed_bert990_mrp_3_n_users_ma4')]] 
  + mrp[[paste0('adj0_is_unemployed_bert990_mrp_4_n_users_ma4')]] 
  + mrp[[paste0('adj0_is_unemployed_bert990_mrp_5_n_users_ma4')]]
)

# Unweighted unemployment index
# uw = read.csv(
#   # file = paste0("../preprocessing/data/restricted/data_iso2_70b.csv"),
#   file = paste0("../preprocessing/data/restricted/data_national_uw.csv"),
#   header = TRUE, sep = ",", stringsAsFactors = FALSE)
uw[['regex_uw']] = (
  uw[['adj0_is_unemployed_regex_mrp_1_n_users_ma4']] 
  + uw[['adj0_is_unemployed_regex_mrp_2_n_users_ma4']] 
  + uw[['adj0_is_unemployed_regex_mrp_3_n_users_ma4']] 
  + uw[['adj0_is_unemployed_regex_mrp_4_n_users_ma4']] 
  + uw[['adj0_is_unemployed_regex_mrp_5_n_users_ma4']]
)
uw[[paste0('bert990_uw')]] = (
  uw[[paste0('adj0_is_unemployed_bert990_mrp_1_n_users_ma4')]] 
  + uw[[paste0('adj0_is_unemployed_bert990_mrp_2_n_users_ma4')]] 
  + uw[[paste0('adj0_is_unemployed_bert990_mrp_3_n_users_ma4')]] 
  + uw[[paste0('adj0_is_unemployed_bert990_mrp_4_n_users_ma4')]] 
  + uw[[paste0('adj0_is_unemployed_bert990_mrp_5_n_users_ma4')]]
)

# Actual UI claims data
temp1 = read.csv(
  file = paste0("../preprocessing/data/ICNSA.csv"), 
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1[['DATE']] = as.Date(temp1[['DATE']])
temp1[['year']] = year(temp1[['DATE']])
temp1[['week']] = week(temp1[['DATE']])
temp1$tm = temp1$year * 100 + temp1$week
temp1 = temp1[,c('year','week','tm','ICNSA')]
colnames(temp1) = c('year','week','tm','iclaimnsa')
pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019
temp1[["iclaimnsa"]] = temp1[["iclaimnsa"]] / pop

dt = merge(
  mrp[,c('tm','regex_mrp','bert990_mrp')], 
  uw[,c('tm','regex_uw','bert990_uw')], # ,
  by = "tm", all.x = TRUE, all.y = TRUE)
dt = merge(
  dt, temp1[,c("tm","iclaimnsa")],
  by = "tm", all.x = TRUE, all.y = TRUE)
dt = dt[order(dt$tm),]
dt[['iclaimnsa_raw']] = dt[['iclaimnsa']]
dt0 = dt

write.csv(
  dt0,
  file='time_series.csv',row.names=FALSE)

## State level

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
tw[['bert990']] = (tw[['bert']] > 990/1000) & !is.na(tw[['bert']])
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw = tw[,c(
  'user_id','date','regex','bert990'
),with=FALSE] %>% group_by(user_id,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,pf,by=c('user_id'),all.x=TRUE,all.y=FALSE)
tw[['user_statuses_count']] = tw[['user_statuses_count']] / (1+tw[['acctage']])
for (var in c('gender','age','org')) {
  tw[is.na(tw[[var]]),var] = -1
  tw[[var]] = factor(tw[[var]])
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count' # ,
)) {
  tw[is.na(tw[[var]]),var] = 0
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count'
)) {
  tw[[var]] = log(1+tw[[var]])
}
tw[['state']] = factor(tw[['state']])
tw = tw[tw[['state']]!="",]
tw = tw[!is.na(tw[['gender']]),]
tw = tw[,c(
  'state','gender','age','date','regex','bert990'
)] %>% group_by(state,gender,age,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,n_users,by=c('state','gender','age'),all.x=TRUE,all.y=TRUE)

# Population by demographic group
census = as.data.frame(fread('../preprocessing/data/sc-est2022-agesex-civ.csv'))
census[['age']] = NA
census[(census[['AGE']]>=15) & (census[['AGE']]<=20),'age'] = 0
census[(census[['AGE']]>=21) & (census[['AGE']]<=30),'age'] = 1
census[(census[['AGE']]>=31) & (census[['AGE']]<=40),'age'] = 2
census[(census[['AGE']]>=41) & (census[['AGE']]<=50),'age'] = 3
census = census[
  (census$SUMLEV==40) & (census$SEX!=0) 
  & (census$AGE>=15) & (census$AGE<=50),c(
    'NAME','SEX','age','ESTBASE2020_CIV'
  )]
colnames(census) = c('srd_text','gender','age','pop')
census[['gender']] = (census[['gender']]==2)*1 # Female dummy
census = merge(
  census,
  read.csv(
    file = paste0("../preprocessing/data/srd_code_to_state.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE),
  by = 'srd_text',all=FALSE)
census[['srd_text']] = NULL
idx = (census$state=="DC")
census[idx,'pop'] = census[idx,'pop'] * 4
census = census %>% group_by(state,gender,age) %>% summarise_all(sum,na.rm=TRUE)
tw[['age']] = as.integer(as.character(tw[['age']]))
tw[['gender']] = as.integer(as.character(tw[['gender']]))
tw = merge(tw,census,by=c('state','age','gender'),all.x=TRUE,all.y=FALSE)

# Post-stratified unemployment index
pop_total = sum(tw[['pop']],na.rm=TRUE)
tw[['adj0_is_unemployed_regex_mrp']] = (
  tw[['pop']] * (tw[['regex']] / tw[['n_users']]) / pop_total)
tw[['adj0_is_unemployed_bert990_mrp']] = (
  tw[['pop']] * (tw[['bert990']] / tw[['n_users']]) / pop_total)
tw_gender = tw[,c(
  'gender','state','date',
  'adj0_is_unemployed_regex_mrp',
  'adj0_is_unemployed_bert990_mrp'
)] %>% group_by(gender,state,date) %>% summarise_all(sum,na.rm=TRUE)
tw_gender = reshape(
  as.data.frame(tw_gender),
  idvar=c('state','date'),timevar='gender',
  direction='wide',sep='_')
tw_age = tw[,c(
  'age','state','date',
  'adj0_is_unemployed_regex_mrp','adj0_is_unemployed_bert990_mrp'
)] %>% group_by(age,state,date) %>% summarise_all(sum,na.rm=TRUE)
tw_age[['age']] = tw_age[['age']] + 2
tw_age = reshape(
  as.data.frame(tw_age),
  idvar=c('state','date'),timevar='age',
  direction='wide',sep='_')
tw_agg = merge(tw_gender,tw_age,by=c('state','date'),all=TRUE)
tw_agg[is.na(tw_agg)] = 0
tw_agg[['adj0_is_unemployed_regex_mrp']] = (
  tw_agg[['adj0_is_unemployed_regex_mrp_1']] 
  + tw_agg[['adj0_is_unemployed_regex_mrp_2']]
  + tw_agg[['adj0_is_unemployed_regex_mrp_3']] 
  + tw_agg[['adj0_is_unemployed_regex_mrp_4']]
  + tw_agg[['adj0_is_unemployed_regex_mrp_5']]
)
tw_agg[['adj0_is_unemployed_bert990_mrp']] = (
  tw_agg[['adj0_is_unemployed_bert990_mrp_1']] 
  + tw_agg[['adj0_is_unemployed_bert990_mrp_2']]
  + tw_agg[['adj0_is_unemployed_bert990_mrp_3']] 
  + tw_agg[['adj0_is_unemployed_bert990_mrp_4']]
  + tw_agg[['adj0_is_unemployed_bert990_mrp_5']]
)
tw_agg = data.table(tw_agg[order(tw_agg$state,tw_agg$date),])
setkey(tw_agg,state,date)
for (var in c(
  'adj0_is_unemployed_regex_mrp','adj0_is_unemployed_bert990_mrp'
)) {
  for (dd in 0:13) {
    tw_agg[,templag:=shift(.SD,n=dd,fill=0,type='lag'),by=state,.SDcols=(var)]
    tw_agg[[paste0(var,'_now',dd)]] = tw_agg[['templag']]
    tw_agg[['templag']] = NULL
  }
}
tw_agg[['tm']] = as.yearmon(as.Date(tw_agg[['date']],format='%m/%d/%Y'))
tw_agg[['year']] = year(tw_agg[['date']])
tw_agg[['week']] = strftime(tw_agg[['date']],format='%V')
tw_agg[['date']] = NULL
tw_agg[['tm']] = NULL
tw_agg = tw_agg %>% group_by(state,year,week) %>% summarise_all(sum,na.rm=TRUE)

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

# List of maximum lags
lagmaxs = c(4)

# List of twitter models
cutoffs = c('990')
models = c("regex","bert990")
spells = c(0)
labels = c('adj0_is_unemployed')

# List of statistsics: Number of users or number of tweets
mas = c(4)
stats = c('n_users')
statmas = paste0('n_users','_ma',mas)

trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252

d = 5
hmax = 0
specs = c('AR','consensus','regex','BERT') # c('AR','regex','BERT')
samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
uivars = c('iclaimnsa')
activities = 0:7
ages = 0:4

# Prepare state level data

temp0 = fread(
  paste0("../preprocessing/data/locations_","US","_num_state.csv")
)[,c('state','num_state')] %>% unique()
p5 = tw_agg
p5 = merge(p5,temp0,by=c('state'),all.x=TRUE,all.y=FALSE)
p5[['state']] = NULL
dt = fread(paste0("../preprocessing/data/n_users_state_week_demo_US.csv"))
colnames(dt) = sub("age-1","age4",colnames(dt))
dt = dt[,c(
  'year','week','num_state',
  outer(paste0(stats,'_age'),ages,paste0)
),with=FALSE]
p5$week = as.numeric(p5$week) 
dt = merge(
  dt,p5,by=c('year','week','num_state'),all=TRUE
)
dt$tm = dt$year * 100 + dt$week
dt[dt$tm==201653,'tm'] = 201652
dt[dt$tm==201753,'tm'] = 201752
dt[dt$tm==201853,'tm'] = 201852
dt[dt$tm==201953,'tm'] = 201952
dt[dt$tm==202153,'tm'] = 202152
dt[dt$tm==202253,'tm'] = 202252
dt[['year']] = floor(dt[['tm']] / 100)
dt[['week']] = dt[['tm']] - dt[['year']] * 100

dt = dt %>% group_by(num_state,tm,year,week) %>% summarise_all(sum,na.rm=TRUE)

temp0 = fread(
  paste0("../preprocessing/data/locations_","US","_num_state.csv")
)[,c('state','num_state')] %>% unique()
dt = merge(dt,temp0,by=c('num_state'),all.x=TRUE,all.y=TRUE)
dt[['num_state']] = NULL
dt = dt[dt$state!="",]
dt = dt[dt$state!=-1,]
dt = data.table(dt[order(dt$state,dt$tm),])
setkey(dt,state,tm)

for (a in ages) {
  for (stat in stats) {
    subvar = paste0(stat,'_','age',a)
    dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
    dt[['templag']] = NULL
    dt[is.na(dt[[subvar]]),subvar] = 0
  }
}

for (a in 1:5) {
  for (label in labels) {
    for (model in "bert990") {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[dt$tm==201840,c(subvar)] = dt[dt$tm==201839,c(subvar)]
      
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (dt[[subvar]] - dt[['templag']]) / (dt[[subvar]] + dt[['templag']]) * 2
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}

for (dd in 0:13) {
  for (label in labels) {
    for (model in "bert990") {
      subvar = paste0(label,'_',model,'_','mrp','_now',dd)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (dt[[subvar]] - dt[['templag']]) / (dt[[subvar]] + dt[['templag']]) * 2
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}

# National level data

data_iso2 = read.csv(
  file = paste0("../preprocessing/data/restricted/data_national.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)

for (var in c(
  uivars,'consensus'
)) {
  for (lag in 0:(max(lagmaxs)+2)) {
    data_iso2[[paste0('iso2_L',lag,'_',var)]] = (
      log(shift(data_iso2[[var]],n=lag)) 
      - log(shift(data_iso2[[var]],n=(lag+1))))
    data_iso2[[paste0('iso2_L',lag,'_',var,'_','dummy_covid')]] = (
      data_iso2[[paste0('iso2_L',lag,'_',var)]] * data_iso2[['dummy_covid']])
  }
}

for (var in c(
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_',1:5,'_','n_users','_ma'),paste0),
    mas,paste0),
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_now',0:13,'_','n_users','_ma'),paste0),
    mas,paste0)
)) {
  for (lag in 0:(max(lagmaxs)+2)) {
    data_iso2[[paste0('iso2_L',lag,'_',var)]] = (
      shift(data_iso2[[var]],n=lag) - shift(data_iso2[[var]],n=(lag+1)))
    data_iso2[[paste0('iso2_L',lag,'_',var,'_','dummy_covid')]] = (
      data_iso2[[paste0('iso2_L',lag,'_',var)]] * data_iso2[['dummy_covid']])
  }
}

data_iso2 = data_iso2[,c(
  'tm','emp_sit','eip',
  colnames(data_iso2)[grepl('iso2_L',colnames(data_iso2))]
)]

# state level UI claims data
temp1 = fread(paste0("../preprocessing/data/ar539.csv"))
temp1$rptdate = as.Date(temp1$rptdate,'%m/%d/%Y')
temp1$year = year(temp1$rptdate)
temp1$week = week(temp1$rptdate)
temp1 = temp1[,c('st','year','week','c3')]
colnames(temp1) = c('state','year','week',uivars)
temp1$tm = temp1$year * 100 + temp1$week

dt[[uivars]] = NULL
dt = merge(
  dt, temp1[,c('tm','state',uivars),with=FALSE],
  by = c('tm','state'), 
  all.x = TRUE, all.y = FALSE)
dt = dt[order(dt$state,dt$tm),]
setkey(dt,state,tm)

for (var in c(uivars)) {
  for (lag in 1:8) {
    dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(var)]
    dt[is.na(dt[[var]]),var] = dt[is.na(dt[[var]]),'templag']
    dt[['templag']] = NULL
  } 
  dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(var)]
  dt[[var]] = (log(dt[[var]]) - log(dt[['templag']])) # * 100
  dt[['templag']] = NULL
}

for (stat in stats) {
  
  dt[[paste0(stat,'_ma',0)]] = 0
  dt[[paste0(stat,'_ma',4)]] = 0
  dt[[paste0(stat,'_ma',8)]] = 0
  
  for (i in ages) { # 
    
    subvar = paste0(stat,'_','age',i)
    dt[,templag1:=shift(.SD,n=1,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[,templag2:=shift(.SD,n=2,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[,templag3:=shift(.SD,n=3,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[,templag4:=shift(.SD,n=4,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[,templag5:=shift(.SD,n=5,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[,templag6:=shift(.SD,n=6,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[,templag7:=shift(.SD,n=7,fill=NA,type='lag'),by=state,.SDcols=(subvar)]
    dt[[paste0(subvar,'_ma',0)]] = dt[[paste0(subvar)]]
    dt[[paste0(subvar,'_ma',4)]] = dt[[paste0(subvar)]]
    dt[[paste0(subvar,'_ma',8)]] = dt[[paste0(subvar)]]
    dt[,paste0('templag',1:7)] = NULL
    dt[[paste0(stat,'_ma',0)]] = (
      dt[[paste0(stat,'_ma',0)]] 
      + dt[[paste0(subvar,'_ma',0)]]
    )
    dt[[paste0(stat,'_ma',4)]] = (
      dt[[paste0(stat,'_ma',4)]] 
      + dt[[paste0(subvar,'_ma',4)]]
    )
    dt[[paste0(stat,'_ma',8)]] = (
      dt[[paste0(stat,'_ma',8)]] 
      + dt[[paste0(subvar,'_ma',8)]]
    )
  }
}

setkey(dt,state,tm)

for (label in labels) {
  for (stat in statmas) {
    for (model in models) {
      
      for (p in 1:d) {
        dt[[paste0(label,'_',model,'_','mrp','_',p,'_',stat)]] = (
          dt[[paste0(label,'_',model,'_','mrp','_',p)]]
        )
      }
      for (dd in 0:13) {
        dt[[paste0(label,'_',model,'_','mrp','_now',dd,'_',stat)]] = (
          dt[[paste0(label,'_',model,'_','mrp','_now',dd)]]
        )
      }
    }
  }
}
dt = dt[dt$tm<=testend,]
dt = dt[!is.na(dt$tm),]
dt = dt[order(dt$state,dt$tm),]
dt = dt[dt$tm<=testend,]
dt = dt[order(dt$state,dt$tm),]
dt[['dummy_covid']] = (dt$tm>=covidstart & dt$tm<=covidend)

for (var in c(
  uivars,
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_',1:5,'_','n_users','_ma'),paste0),mas,paste0),
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_now',0:13,'_','n_users','_ma'),paste0),mas,paste0)
)) {
  for (lag in 0:(max(lagmaxs)+2)) {
    dt[,(paste0('L',lag,'_',var)):=shift(.SD,n=lag,fill=NA,type='lag'),by=state,.SDcols=(var)]
    dt[[paste0('L',lag,'_',var,'_','dummy_covid')]] = (
      dt[[paste0('L',lag,'_',var)]] * dt[['dummy_covid']]
    )
  }
}
dt = as.data.frame(dt)
dt$year = floor(dt$tm / 100)
dt$week = dt$tm - dt$year * 100
dt$quarter = floor(dt$week / 14) + 1
dt$month = floor(dt$week / 4) + 1

for (var in c('state',"year","quarter","month","week")) {
  dt[[paste0("dummy_", var)]] = factor(dt[[var]])
}

dt = merge(
  dt,data_iso2,
  by='tm',all.x=TRUE,all.y=FALSE
)
dt = dt[order(dt$state,dt$tm),]

# state by state rolling regressions
windows = c(4*52-26)

RMSEs = c()
forecasts = c()

dates_to_cover = c(201902,202002,202102,202202)
cities_long = dt[
  !is.na(dt[[uivars]]) & dt$tm %in% dates_to_cover
  ,c('state','tm','n_users_ma4')]
cities_long$count = 1
cities_long = cities_long[,c('state','count','n_users_ma4')] %>% group_by(
  state
) %>% summarise_all(sum,na.rm=TRUE)
cities_long[['sample']] = (cities_long$count==length(dates_to_cover))
cities_long = cities_long[cities_long$count > 1,]
cities_long[['ntile_n_users_ma4']] = ntile(cities_long[['n_users_ma4']],10)
cities_long[['n_users_ma4']] = NULL

dt[['count']] = NULL
dt[['sample']] = NULL
dt[['ntile_n_users_ma4']] = NULL
dt = merge(
  dt,cities_long,by='state',all.x=TRUE,all.y=FALSE
)

data = dt[dt$tm>=201601,]
data = data[!is.na(data$state) & !is.na(data[[uivars]]) & !is.na(data$tm),]
data = data[order(data$state,data$tm),]

write.csv(
  data, 
  file = paste0("../preprocessing/data/restricted/data_state.csv"),
  row.names = FALSE)

## City level

locations = fread(
  paste0("../preprocessing/data/locations_","US",".csv")
)[,c('user_location','geo_id','metro_area_name')] %>% unique()
temp1 = fread(paste0("../preprocessing/data/ui_state_iso2_","US",".csv"))
temp1 = temp1[
  temp1$year>=2016,
  c('state','iclaimnsa')
  ] %>% group_by(state) %>% summarise_all(mean,na.rm=TRUE)
pf_locations = fread('../preprocessing/data/restricted/latest_profiles_user_location.csv')
pf = merge(
  fread('../preprocessing/data/restricted/latest_profiles.csv'),pf_locations,
  by=c('user_id'),all.x=TRUE,all.y=FALSE)
pf = merge(pf,locations,by=c('user_location'),all.x=TRUE,all.y=FALSE)
pf[['user_location']] = NULL
pf[is.na(pf$geo_id),'geo_id'] = -1
demo = fread('../preprocessing/data/restricted/demo_inf_final_20240106.csv')
pf[['age']] = NULL
pf[['gender']] = NULL
pf[['org']] = NULL
pf = merge(pf,demo,by=c('user_id'),all.x=TRUE,all.y=FALSE)
pf[['tweet_timestamp']] = as.Date(as.POSIXct(pf[['tweet_timestamp']],origin='1970-01-01'))
pf[['acctage']] = 2022 - year(pf[['tweet_timestamp']])
pf[['tweet_timestamp']] = NULL
pf[['metro_area_name']] = NULL
pf[['n_users']] = 1
n_users = pf[,c(
  'state','geo_id','age','gender','n_users'
  )] %>% group_by(state,geo_id,age,gender) %>% summarise_all(sum,na.rm=TRUE) 
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]
n_gender = n_users[,c('gender','n_users')] %>% group_by(gender) %>% summarise_all(sum,na.rm=TRUE)
n_gender[['n_users']] = n_gender[['n_users']] / sum(n_gender[['n_users']],na.rm=TRUE)
n_age = n_users[,c('age','n_users')] %>% group_by(age) %>% summarise_all(sum,na.rm=TRUE)
n_age[['n_users']] = n_age[['n_users']] / sum(n_age[['n_users']],na.rm=TRUE)
pf[is.na(pf[['gender']]),'gender'] = sample(
  0:1,sum(is.na(pf[['gender']]),na.rm=TRUE),
  prob=n_gender[['n_users']],replace=TRUE)
pf[is.na(pf[['age']]),'age'] = sample(
  0:3,sum(is.na(pf[['age']]),na.rm=TRUE),
  prob=n_age[['n_users']],replace=TRUE)
n_users = pf[,c(
  'state','geo_id','age','gender','n_users'
  )] %>% group_by(state,geo_id,age,gender) %>% summarise_all(sum,na.rm=TRUE) 
n_users = n_users[n_users[['state']]!="",]
n_users = n_users[!is.na(n_users[['gender']]),]
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
tw[['bert990']] = (tw[['bert']] > 990/1000) & !is.na(tw[['bert']])
tw[['regex']] = (tw[['regex']]==1) & !is.na(tw[['regex']])
tw = tw[,c(
  'user_id','date','regex','bert990'
),with=FALSE] %>% group_by(user_id,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,pf,by=c('user_id'),all.x=TRUE,all.y=FALSE)

tw[['user_statuses_count']] = tw[['user_statuses_count']] / (1+tw[['acctage']])
for (var in c(
  'gender','age','org' 
)) {
  tw[is.na(tw[[var]]),var] = -1
  tw[[var]] = factor(tw[[var]])
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count' # ,
)) {
  tw[is.na(tw[[var]]),var] = 0
}
for (var in c(
  'user_favourites_count','user_followers_count',
  'user_friends_count','user_statuses_count'
)) {
  tw[[var]] = log(1+tw[[var]])
}
tw[['state']] = factor(tw[['state']])

tw = tw[tw[['state']]!="",]
tw = tw[!is.na(tw[['gender']]),]
tw = tw[,c(
  'state','geo_id','gender','age','date','regex','bert990'
)] %>% group_by(state,geo_id,gender,age,date) %>% summarise_all(sum,na.rm=TRUE)
tw = merge(tw,n_users,by=c('state','geo_id','gender','age'),all.x=TRUE,all.y=TRUE)
census = as.data.frame(fread('../preprocessing/data/sc-est2022-agesex-civ.csv'))
census[['age']] = NA
census[(census[['AGE']]>=15) & (census[['AGE']]<=20),'age'] = 0
census[(census[['AGE']]>=21) & (census[['AGE']]<=30),'age'] = 1
census[(census[['AGE']]>=31) & (census[['AGE']]<=40),'age'] = 2
census[(census[['AGE']]>=41) & (census[['AGE']]<=50),'age'] = 3
census = census[
  (census$SUMLEV==40) & (census$SEX!=0) & (census$AGE>=15) & (census$AGE<=50),c(
    'NAME','SEX','age','ESTBASE2020_CIV'
  )]
colnames(census) = c('srd_text','gender','age','pop')
census[['gender']] = (census[['gender']]==2)*1 # Female dummy
census = merge(
  census,
  read.csv(
    file = paste0("../preprocessing/data/srd_code_to_state.csv"),
    header = TRUE, sep = ",", stringsAsFactors = FALSE),
  by = 'srd_text',all=FALSE)
census[['srd_text']] = NULL
census[census$state=="DC",'pop'] = census[census$state=="DC",'pop'] * 4
census = census %>% group_by(state,gender,age) %>% summarise_all(sum,na.rm=TRUE)
tw[['age']] = as.integer(as.character(tw[['age']]))
tw[['gender']] = as.integer(as.character(tw[['gender']]))
tw = merge(tw,census,by=c('state','age','gender'),all.x=TRUE,all.y=FALSE)
pop_total = sum(tw[['pop']],na.rm=TRUE)
tw[['adj0_is_unemployed_regex_mrp']] = (tw[['regex']] / tw[['n_users']])
tw[[paste0('adj0_is_unemployed_bert990_mrp')]] = (tw[['bert990']] / tw[['n_users']])
tw_gender = tw[,c(
  'gender','geo_id','date',
  'adj0_is_unemployed_regex_mrp',paste0('adj0_is_unemployed_bert990_mrp')
)] %>% group_by(gender,geo_id,date) %>% summarise_all(sum,na.rm=TRUE)
tw_gender = reshape(
  as.data.frame(tw_gender),
  idvar=c('geo_id','date'),timevar='gender',direction='wide',sep='_')
tw_age = tw[,c(
  'age','geo_id','date',
  'adj0_is_unemployed_regex_mrp',paste0('adj0_is_unemployed_bert990_mrp')
)] %>% group_by(age,geo_id,date) %>% summarise_all(sum,na.rm=TRUE)
tw_age[['age']] = tw_age[['age']] + 2
tw_age = reshape(
  as.data.frame(tw_age),
  idvar=c('geo_id','date'),timevar='age',direction='wide',sep='_')
tw_agg = merge(tw_gender,tw_age,by=c('geo_id','date'),all=TRUE)
tw_agg[is.na(tw_agg)] = 0
tw_agg[[paste0('adj0_is_unemployed_regex_mrp')]] = (
  tw_agg[[paste0('adj0_is_unemployed_regex_mrp','_',1)]] 
  + tw_agg[[paste0('adj0_is_unemployed_regex_mrp','_',2)]]
  + tw_agg[[paste0('adj0_is_unemployed_regex_mrp','_',3)]] 
  + tw_agg[[paste0('adj0_is_unemployed_regex_mrp','_',4)]]
  + tw_agg[[paste0('adj0_is_unemployed_regex_mrp','_',5)]]
)
tw_agg[[paste0('adj0_is_unemployed_bert990_mrp')]] = (
  tw_agg[[paste0(paste0('adj0_is_unemployed_bert990_mrp'),'_',1)]] 
  + tw_agg[[paste0(paste0('adj0_is_unemployed_bert990_mrp'),'_',2)]]
  + tw_agg[[paste0(paste0('adj0_is_unemployed_bert990_mrp'),'_',3)]] 
  + tw_agg[[paste0(paste0('adj0_is_unemployed_bert990_mrp'),'_',4)]]
  + tw_agg[[paste0(paste0('adj0_is_unemployed_bert990_mrp'),'_',5)]]
)
tw_agg = data.table(tw_agg[order(tw_agg$geo_id,tw_agg$date),])
setkey(tw_agg,geo_id,date)
for (var in c(
  paste0('adj0_is_unemployed_regex_mrp'),
  paste0(paste0('adj0_is_unemployed_bert990_mrp'))
)) {
  for (dd in 0:13) {
    tw_agg[,templag:=shift(.SD,n=dd,fill=0,type='lag'),by=geo_id,.SDcols=(var)]
    tw_agg[[paste0(var,'_now',dd)]] = tw_agg[['templag']]
    tw_agg[['templag']] = NULL
  }
}
tw_agg[['tm']] = as.yearmon(as.Date(tw_agg[['date']],format='%m/%d/%Y'))
tw_agg[['year']] = year(tw_agg[['date']])
tw_agg[['week']] = strftime(tw_agg[['date']],format='%V')
tw_agg[['date']] = NULL
tw_agg[['tm']] = NULL
tw_agg = tw_agg %>% group_by(geo_id,year,week) %>% summarise_all(sum,na.rm=TRUE)

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

# List of maximum lags
lagmaxs = c(4)

# List of twitter models
cutoffs = c('990')
models = c("regex","bert990")
spells = c(0)
labels = c(paste0('adj',spells,'_','is_unemployed'))

# List of statistsics: Number of users or number of tweets
mas = c(4)
stats = c('n_users')
statmas = paste0('n_users','_ma',mas)

trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202224
d = 5
specs = c('AR','consensus','regex','BERT') # c('AR','regex','BERT')
samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
activities = 0:7
ages = 0:4

ui = fread("../preprocessing/data/ui_city.csv")
ui = merge(
  ui,fread(
    paste0("../preprocessing/data/locations_","US",".csv")
  )[,c('geo_id','metro_area_name')] %>% unique(),
  by='metro_area_name',all.x=TRUE,all.y=FALSE
)
ui = ui[!is.na(ui$geo_id),]
p5 = tw_agg
dt = fread(paste0("../preprocessing/data/n_users_city_week_demo_US.csv"))
colnames(dt) = sub("age-1","age4",colnames(dt))
dt = merge(
  dt,ui,by=c("geo_id","year","week"),all.x=FALSE,all.y=TRUE
)
dt = dt[,c(
  'year','week','metro_area_name','geo_id',
  outer(paste0(stats,'_age'),ages,paste0)
),with=FALSE]
p5$week = as.numeric(p5$week) 
dt = merge(
  dt,p5,by=c('year','week','geo_id'),all=TRUE
)
dt$tm = dt$year * 100 + dt$week
dt[dt$tm==201653,'tm'] = 201652
dt[dt$tm==201753,'tm'] = 201752
dt[dt$tm==201853,'tm'] = 201852
dt[dt$tm==201953,'tm'] = 201952
dt[dt$tm==202153,'tm'] = 202152
dt[dt$tm==202253,'tm'] = 202252
dt[['year']] = floor(dt[['tm']] / 100)
dt[['week']] = dt[['tm']] - dt[['year']] * 100
dt = dt %>% group_by(
  metro_area_name,tm,year,week
  ) %>% summarise_all(sum,na.rm=TRUE)
dt = dt[dt$metro_area_name!="",]
dt = dt[dt$geo_id!=-1,]
dt = data.table(dt[order(dt$metro_area_name,dt$tm),])
setkey(dt,metro_area_name,tm)
for (i in ages) { #
  for (stat in stats) {
    subvar = paste0(stat,'_','age',i)
    dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
    dt[['templag']] = NULL
    dt[is.na(dt[[subvar]]),subvar] = 0
  }
}
for (a in 1:5) {
  for (label in labels) {
    for (model in models) {
      subvar = paste0(label,'_',model,'_','mrp','_',a)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[dt$tm==201840,c(subvar)] = dt[dt$tm==201839,c(subvar)]
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (dt[[subvar]] - dt[['templag']]) / (dt[[subvar]] + dt[['templag']]) * 2
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}
for (dd in 0:13) {
  for (label in labels) {
    for (model in "bert990") {
      subvar = paste0(label,'_',model,'_','mrp','_now',dd)
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=geo_id,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['templag']] = NULL
      dt[is.na(dt[[subvar]]),subvar] = 0
      dt[dt$tm==202053,subvar] = dt[dt$tm==202053,subvar,with=FALSE]*(7/5)
      dt[dt$tm==201840,c(subvar)] = dt[dt$tm==201839,c(subvar)]
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=geo_id,.SDcols=(subvar)]
      dt[is.na(dt[[subvar]]),subvar] = dt[is.na(dt[[subvar]]),'templag']
      dt[['growth']] = (dt[[subvar]] - dt[['templag']]) / (dt[[subvar]] + dt[['templag']]) * 2
      dt[is.na(dt[['growth']]),'growth'] = 0
      outlier = 0.5
      dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=geo_id,.SDcols=(subvar)]
      dt[
        (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
        ,subvar] = dt[
          (dt[['growth']]) > 1*outlier & !(dt$tm>=202004 & dt$tm<=202016)
          ,'templag']
      dt[['templag']] = NULL
      dt[['growth']] = NULL
    }
  }
}

data_iso2 = read.csv(
  file = paste0("../preprocessing/data/restricted/data_national.csv"), 
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
data_iso2[['dummy_covid']] = (data_iso2$tm>=covidstart & data_iso2$tm<=covidend)

for (var in c(
  'iclaimnsa','consensus'
)) {
  for (lag in 0:((max(lagmaxs)+1))) {
    data_iso2[[paste0('iso2_L',lag,'_',var)]] = log(shift(data_iso2[[var]],n=lag)) - log(shift(data_iso2[[var]],n=(lag+1)))
    data_iso2[[paste0('iso2_L',lag,'_',var,'_','dummy_covid')]] = (
      data_iso2[[paste0('iso2_L',lag,'_',var)]] * data_iso2[['dummy_covid']]
    )
  }
}

for (var in c(
  outer(outer(outer(labels,paste0('_',models,'_'),paste0),paste0('mrp_',1:5,'_','n_users','_ma'),paste0),mas,paste0),
  outer(outer(outer(labels,paste0('_',models,'_'),paste0),paste0('mrp_now',0:13,'_','n_users','_ma'),paste0),mas,paste0)
)) {
  for (lag in 0:((max(lagmaxs)+1))) {
    data_iso2[[paste0('iso2_L',lag,'_',var)]] = shift(data_iso2[[var]],n=lag) - shift(data_iso2[[var]],n=(lag+1))
    data_iso2[[paste0('iso2_L',lag,'_',var,'_','dummy_covid')]] = (
      data_iso2[[paste0('iso2_L',lag,'_',var)]] * data_iso2[['dummy_covid']]
    )
  }
}

data_iso2 = data_iso2[,c(
  'tm','emp_sit','eip',
  colnames(data_iso2)[grepl('iso2_L',colnames(data_iso2))]
)]

# metro_area_name level data
temp1 = fread(paste0("../preprocessing/data/ui_city.csv"))
temp1$tm = temp1$year * 100 + temp1$week

dt[['iclaimnsa']] = NULL
dt = merge(
  dt, temp1[,c('tm','metro_area_name','iclaimnsa'),with=FALSE],
  by = c('tm','metro_area_name'), 
  all.x = TRUE, all.y = FALSE)
dt = dt[order(dt$metro_area_name,dt$tm),]
setkey(dt,metro_area_name,tm)

for (var in c('iclaimnsa')) {
  dt[[var]] = dt[[var]]
  for (lag in 1:8) {
    dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(var)]
    dt[is.na(dt[[var]]),var] = dt[is.na(dt[[var]]),'templag']
    dt[['templag']] = NULL
  } 
  dt[,templag:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(var)]
  dt[[var]] = (log(dt[[var]]) - log(dt[['templag']])) # * 100
  dt[['templag']] = NULL
}

for (stat in stats) {
  dt[[paste0(stat,'_ma',0)]] = 0
  dt[[paste0(stat,'_ma',4)]] = 0
  dt[[paste0(stat,'_ma',8)]] = 0
  for (i in ages) { # 
    subvar = paste0(stat,'_','age',i)
    dt[,templag1:=shift(.SD,n=1,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[,templag2:=shift(.SD,n=2,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[,templag3:=shift(.SD,n=3,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[,templag4:=shift(.SD,n=4,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[,templag5:=shift(.SD,n=5,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[,templag6:=shift(.SD,n=6,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[,templag7:=shift(.SD,n=7,fill=NA,type='lag'),by=metro_area_name,.SDcols=(subvar)]
    dt[[paste0(subvar,'_ma',0)]] = dt[[paste0(subvar)]]
    dt[[paste0(subvar,'_ma',4)]] = dt[[paste0(subvar,'_ma',0)]]
    dt[[paste0(subvar,'_ma',8)]] = dt[[paste0(subvar,'_ma',0)]]
    dt[,paste0('templag',1:7)] = NULL
    dt[[paste0(stat,'_ma',0)]] = (
      dt[[paste0(stat,'_ma',0)]] 
      + dt[[paste0(subvar,'_ma',0)]]
    )
    dt[[paste0(stat,'_ma',4)]] = (
      dt[[paste0(stat,'_ma',4)]] 
      + dt[[paste0(subvar,'_ma',4)]]
    )
    dt[[paste0(stat,'_ma',8)]] = (
      dt[[paste0(stat,'_ma',8)]] 
      + dt[[paste0(subvar,'_ma',8)]]
    )
  }
}

setkey(dt,metro_area_name,tm)
for (label in labels) {
  for (stat in statmas) {
    for (model in models) {
      for (p in 1:d) {
        dt[[paste0(label,'_',model,'_','mrp','_',p,'_',stat)]] = (
          dt[[paste0(label,'_',model,'_','mrp','_',p)]]
        )
      }
      for (dd in 0:13) {
        dt[[paste0(label,'_',model,'_','mrp','_now',dd,'_',stat)]] = (
          dt[[paste0(label,'_',model,'_','mrp','_now',dd)]]
        )
      }
    }
  }
}
dt = dt[dt$tm<=testend,]
dt = dt[!is.na(dt$tm),]
dt = dt[order(dt$metro_area_name,dt$tm),]
dt = dt[dt$tm<=testend,]
dt = dt[order(dt$metro_area_name,dt$tm),]
dt[['dummy_covid']] = (dt$tm>=covidstart & dt$tm<=covidend)

for (var in c(
  'iclaimnsa',
  outer(outer(outer(labels,paste0('_',models,'_'),paste0),paste0('mrp_',1:5,'_','n_users','_ma'),paste0),mas,paste0),
  outer(outer(outer(labels,paste0('_',models,'_'),paste0),paste0('mrp_now',0:13,'_','n_users','_ma'),paste0),mas,paste0) # ,
)) {
  for (lag in 0:(max(lagmaxs)+1)) {
    dt[,(paste0('L',lag,'_',var)):=shift(.SD,n=lag,fill=NA,type='lag'),by=metro_area_name,.SDcols=(var)]
    dt[[paste0('L',lag,'_',var,'_','dummy_covid')]] = (
      dt[[paste0('L',lag,'_',var)]] * dt[['dummy_covid']]
    )
  }
}
dt = as.data.frame(dt)
dt$year = floor(dt$tm / 100)
dt$week = dt$tm - dt$year * 100
dt$quarter = floor(dt$week / 14) + 1
dt$month = floor(dt$week / 4) + 1
for (var in c('metro_area_name',"year","quarter","month","week")) {
  dt[[paste0("dummy_", var)]] = factor(dt[[var]])
}
dt = merge(
  dt,data_iso2,
  by='tm',all.x=TRUE,all.y=FALSE
)

locations = fread(
  paste0("../preprocessing/data/locations_","US",".csv")
)[,c('administrative_area_level_1_short','metro_area_name','n_pings')] %>% unique()
colnames(locations) = c('state','metro_area_name','n_pings')
locations = locations[locations$metro_area_name!="",]
locations = locations[order(locations$metro_area_name,-locations$n_pings),]
locations = locations[!duplicated(locations$metro_area_name),]

dt = merge(
  dt,locations,
  by=c('metro_area_name'),all.x=TRUE,all.y=FALSE
)

dt = dt[order(dt$metro_area_name,dt$tm),]
dt = dt[
  ,c(
    'tm','metro_area_name','dummy_covid','iclaimnsa','emp_sit','eip',
    statmas,
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),'iclaimnsa',paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),'iclaimnsa',paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),'consensus',paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),paste0('iclaimnsa','_','dummy_covid'),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),paste0('iclaimnsa','_','dummy_covid'),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),paste0('consensus','_','dummy_covid'),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0)
  )]

data_state = read.csv(
  file = paste0("../preprocessing/data/restricted/data_state.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
data_state = data_state[
  ,c(
    'tm','state','dummy_covid','iclaimnsa','emp_sit','eip',
    statmas,
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),'iclaimnsa',paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),'iclaimnsa',paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),'consensus',paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),paste0('iclaimnsa','_','dummy_covid'),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),paste0('iclaimnsa','_','dummy_covid'),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),paste0('consensus','_','dummy_covid'),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0)
  )]
colnames(data_state)[colnames(data_state)=='state'] = 'metro_area_name'
dt = rbind(dt,data_state)

# metro_area_name by metro_area_name rolling regressions

windows = c(2*52)

dates_to_cover = c(201902,202002,202102,202202)
cities_long = dt[
  !is.na(dt[['iclaimnsa']]) & dt$tm %in% dates_to_cover
  ,c('metro_area_name','tm','n_users_ma4')]
cities_long$count = 1
cities_long = cities_long[,c(
  'metro_area_name','count','n_users_ma4'
  )] %>% group_by(
  metro_area_name
) %>% summarise_all(sum,na.rm=TRUE)
cities_long[['sample']] = (cities_long$count==length(dates_to_cover))
cities_long = cities_long[cities_long$count > 1,]
cities_long[['ntile_n_users_ma4']] = ntile(cities_long[['n_users_ma4']],10)
cities_long[['n_users_ma4']] = NULL

dt[['count']] = NULL
dt[['sample']] = NULL
dt[['ntile_n_users_ma4']] = NULL
dt = merge(
  dt,cities_long,by='metro_area_name',all.x=TRUE,all.y=FALSE
)

data = dt[
  dt$tm>=201601
  ,c(
    'tm','sample','ntile_n_users_ma4','metro_area_name','dummy_covid',
    'iclaimnsa','emp_sit', # 'eip',
    statmas,
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),'iclaimnsa',paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),'iclaimnsa',paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),'consensus',paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),paste0('iclaimnsa','_','dummy_covid'),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),paste0('iclaimnsa','_','dummy_covid'),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),paste0('consensus','_','dummy_covid'),paste0),
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0), 
    outer(paste0('iso2_L',0:(max(lagmaxs)+1),'_'),outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_n_users_ma',mas,'_','dummy_covid'),paste0),paste0)
  )]
data = data[!is.na(data$metro_area_name) & !is.na(data[['iclaimnsa']]) & !is.na(data$tm),]
data = data[order(data$metro_area_name,data$tm),]

write.csv(
  data,
  file = paste0("../preprocessing/data/restricted/data_city.csv"),
  row.names = FALSE
)






