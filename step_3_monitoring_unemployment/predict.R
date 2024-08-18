##############################################################
# Predict UI claims

rm(list = ls())

## Predict national level UI claims

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

# Sample periods = year*100 + week
trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252
windows = c(3*52)

# Alternative models
specs = c('AR','consensus','regex','BERT')

data = read.csv(
  file = paste0("../preprocessing/data/restricted/data_national.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)

for (var in c(
  'iclaimnsa','consensus'
)) {
  for (lag in 0:(max(lagmaxs))) {
    data[[paste0('L',lag,'_',var)]] = shift(data[[var]],n=lag)
    data[[paste0('L',lag,'_',var,'_','dummy_covid')]] = (
      data[[paste0('L',lag,'_',var)]] * data[['dummy_covid']]
    )
  }
}
for (var in c(
  outer(outer(labels,outer(paste0('_',models,'_','mrp_'),1:5,paste0),paste0),paste0('_','n_users','_ma',mas),paste0) # ,
)) {
  for (lag0 in 1:(max(lagmaxs)+1)) {
    for (lag in 1:lag0) {
      if (lag==1) {
        data[[paste0('L',lag0,'_',var)]] = shift(data[[var]],n=lag)
      } else {
        data[[paste0('L',lag0,'_',var)]] = data[[paste0('L',lag0,'_',var)]] + shift(data[[var]],n=lag)
      }
      data[[paste0('L',lag0,'_',var)]] = data[[paste0('L',lag0,'_',var)]] / lag0
      data[[paste0('L',lag0,'_',var,'_','dummy_covid')]] = (
        data[[paste0('L',lag0,'_',var)]] * data[['dummy_covid']]
      )
    }
  }
}
for (var in c(
  outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_','n_users','_ma',mas),paste0) # ,
)) {
  for (lag0 in 0:(max(lagmaxs)+1)) {
    for (lag in 0:lag0) {
      data[[paste0('L',lag0,'_',var)]] = shift(data[[var]],n=lag)
      data[[paste0('L',lag0,'_',var,'_','dummy_covid')]] = (
        data[[paste0('L',lag0,'_',var)]] * data[['dummy_covid']]
      )
    }
  }
}
for (var in c(
  outer(outer(labels,outer(paste0('_',models,'_','mrp_now'),0:13,paste0),paste0),paste0('_','n_users','_ma',mas),paste0) # ,
)) {
  data[[paste0(var,'_','dummy_covid')]] = (
    data[[paste0(var)]] * data[['dummy_covid']]
  )
}

periods = data[data$tm>=teststart & data$tm<=testend,'tm']
for (dd in 0:13) {
  if (dd>=8) {
    lagmin = 2
    lagmin2 = 2
    lagmin3 = 1
    dds = dd:13
  } else if (dd>=7) {
    lagmin = 1
    lagmin2 = 2
    lagmin3 = 0
    dds = dd:dd
  } else {
    lagmin = 1
    lagmin2 = 1
    lagmin3 = 0
    dds = dd:5
  }
  hmax = 0
  for (h in 0:hmax) {
    forecasts = c()
    RMSEs = c()
    for (uivar in 'iclaimnsa') {
      cv_lagmaxs = c()
      cv_mas = c()
      cv_spells = c()
      cv_cutoffs = c()
      cv_windows = c()
      cv_ds = c()
      for (period in periods) {
        if (period==periods[1] || ((period-floor(period/100)*100) %% 52)==0) {
          cv_RMSEs = c()
          for (cv_window in windows) {
            for (cv_lagmax in lagmaxs) {
              for (cv_spell in spells) {
                for (cv_cutoff in cutoffs) {
                  for (cv_ma in mas) {
                    for (cv_d in ds) {
                      label = paste0('adj',cv_spell,'_','is_unemployed')
                      cv_normal_model_BERT = c(
                        paste0('L',lagmin2:cv_lagmax,'_',uivar),
                        outer(
                          paste0('L',lagmin:cv_lagmax,'_',label,'_','bert',cv_cutoff,'_','mrp','_'),
                          paste0(1:cv_d,'_n_users','_ma',cv_ma),paste0),
                        outer(
                          paste0('L',0,'_',label,'_','bert',cv_cutoff,'_','mrp','_now'),
                          paste0(dds,'_n_users','_ma',cv_ma),paste0),
                        paste0('L',lagmin:lagmin,'_','consensus')
                      )
                      if (period>=covidend) {
                        cv_covid_model_BERT = c(
                          paste0('L',lagmin2:cv_lagmax,'_',uivar,'_','dummy_covid'),
                          paste0('L',lagmin:lagmin,'_','consensus','_','dummy_covid'),
                          outer(
                            paste0('L',lagmin:cv_lagmax,'_',label,'_','bert',cv_cutoff,'_','mrp','_'),
                            paste0(1:cv_d,'_n_users','_ma',cv_ma,'_','dummy_covid'),paste0),
                          outer(
                            paste0('L',0,'_',label,'_','bert',cv_cutoff,'_','mrp','_now'),
                            paste0(dds,'_n_users','_ma',cv_ma,'_','dummy_covid'),paste0),
                          'dummy_covid'
                        )
                      } else {
                        cv_covid_model_BERT = c()
                      }
                      cvperiods = data[data$tm<=period,'tm']
                      cvperiods = cvperiods[(length(cvperiods)-floor(cv_window/8)):(length(cvperiods))]
                      for (cvperiod in cvperiods) {
                        estdata = data[data$tm>=trainstart & data$tm<=cvperiod,]
                        cv_model_BERT = lm(as.formula(paste0(
                          uivar,' ~ ',paste(c(
                            cv_normal_model_BERT,cv_covid_model_BERT
                          ),sep='',collapse=' + '))),
                          data = estdata[max(1,(nrow(estdata)-cv_window)):nrow(estdata),]
                        )
                        data[data$tm>=cvperiod,'cv_forecast_BERT'] = predict(
                          cv_model_BERT,
                          data[data$tm>=cvperiod,]
                        )
                        data[['cv_resid_BERT']] = data[[uivar]] - data[['cv_forecast_BERT']]
                      }
                      cv_RMSE = data[data$tm>=teststart,c('tm',paste0('cv_resid_','BERT'))]
                      colnames(cv_RMSE) = c('tm','RMSE_BERT')
                      cv_RMSE[['tm']] = NULL
                      cv_RMSE = cv_RMSE^2 %>% summarise_all(mean,na.rm=TRUE)
                      cv_RMSE[['cv_window']] = cv_window
                      cv_RMSE[['cv_spell']] = cv_spell
                      cv_RMSE[['cv_cutoff']] = cv_cutoff
                      cv_RMSE[['cv_lagmax']] = cv_lagmax
                      cv_RMSE[['cv_ma']] = cv_ma
                      cv_RMSE[['cv_d']] = cv_d
                      cv_RMSEs = rbind(cv_RMSEs,cv_RMSE)
                    }
                  }
                }
              }
            }
          }
          min_cv_RMSEs = cv_RMSEs[cv_RMSEs[['RMSE_BERT']]==min(cv_RMSEs[['RMSE_BERT']]),]
          window = min_cv_RMSEs[['cv_window']]
          spell = min_cv_RMSEs[['cv_spell']]
          cutoff = min_cv_RMSEs[['cv_cutoff']]
          lagmax = min_cv_RMSEs[['cv_lagmax']]
          ma = min_cv_RMSEs[['cv_ma']]
          d = min_cv_RMSEs[['cv_d']]
          cv_windows = c(cv_windows,window)
          cv_lagmaxs = c(cv_lagmaxs,lagmax)
          cv_spells = c(cv_spells,spell)
          cv_cutoffs = c(cv_cutoffs,cutoff)
          cv_mas = c(cv_mas,ma)
          cv_ds = c(cv_ds,d)
        }
        normal_model_AR = c(
          paste0('L',((lagmin2:lagmax)+h),'_',uivar)
        )
        normal_model_consensus = c(
          paste0('L',((lagmin2:lagmax)+h),'_',uivar),
          paste0('L',((lagmin:2)+h),'_','consensus')
        )
        normal_model_regex = c(
          paste0('L',(((lagmin2):lagmax)+h),'_',uivar),
          outer(paste0('L',(c((lagmin):(lagmax-lagmin3*2))+h),'_',label,'_','regex','_','mrp','_'),paste0(1:d,'_n_users','_ma',ma),paste0),
          outer(paste0('L',(c(lagmin:((lagmax-lagmin3*2)))+h),'_',label,'_','regex','_','mrp_now'),paste0(dds,'_n_users','_ma',ma),paste0)
        )
        normal_model_BERT = c(
          paste0('L',((lagmin2:lagmax)+h),'_',uivar),
          outer(paste0('L',(c(lagmin:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_','mrp','_'),paste0(1:d,'_n_users','_ma',ma),paste0),
          outer(paste0('L',lagmin3:(lagmax-lagmin3*2+1),'_',label,'_','bert',cutoff,'_','mrp','_now'),paste0(dds,'_n_users','_ma',ma),paste0)
        )
        if (period>=covidend) {
          covid_model_AR = c(
            paste0('L',((lagmin2:lagmax)+h),'_',uivar,'_','dummy_covid'),
            'dummy_covid'
          )
          covid_model_consensus = c(
            paste0('L',((lagmin2:lagmax)+h),'_',uivar,'_','dummy_covid'),
            paste0('L',((lagmin:lagmin)+h),'_','consensus','_','dummy_covid'),
            'dummy_covid'
          )
          covid_model_regex = c(
            paste0('L',(((lagmin2):lagmax)+h),'_',uivar,'_','dummy_covid'),
            outer(paste0('L',(c((lagmin):(lagmax-lagmin3*2))+h),'_',label,'_','regex','_','mrp','_'),paste0(1:d,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
            outer(paste0('L',lagmin3:(lagmax-lagmin3*2),'_',label,'_','regex','_','mrp','_now'),paste0(dds,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
            'dummy_covid'
          )
          covid_model_BERT = c(
            paste0('L',((lagmin2:lagmax)+h),'_',uivar,'_','dummy_covid'),
            outer(paste0('L',(c(lagmin:(lagmax-lagmin3*2+1))+h),'_',label,'_','bert',cutoff,'_','mrp','_'),paste0(1:d,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
            outer(paste0('L',lagmin3:(lagmax-lagmin3*2+1),'_',label,'_','bert',cutoff,'_','mrp','_now'),paste0(dds,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
            'dummy_covid'
          )
        } else {
          covid_model_AR = covid_model_consensus = covid_model_regex = covid_model_BERT = c()
        }
        estdata = data[data$tm>=trainstart & data$tm<=period,]
        model_AR = lm(as.formula(paste0(
          uivar,' ~ ',paste(c(
            normal_model_AR,covid_model_AR
          ),sep='',collapse=' + '))),
          data = estdata[(nrow(estdata)-window):nrow(estdata),]
        )
        data[data$tm>=period,paste0('forecast',h,'_','AR')] = predict(
          model_AR,
          data[data$tm>=period,]
        )
        data[[paste0('resid',h,'_','AR')]] = (
          data[[uivar]] 
          - data[[paste0('forecast',h,'_','AR')]]
        )
        model_consensus = lm(as.formula(paste0(
          uivar,' ~ ',paste(c(
            normal_model_consensus,covid_model_consensus
          ),sep='',collapse=' + '))),
          data = estdata[(nrow(estdata)-window):nrow(estdata),]
        )
        data[data$tm>=period,paste0('forecast',h,'_','consensus')] = predict(
          model_consensus,
          data[data$tm>=period,]
        )
        data[[paste0('resid',h,'_','consensus')]] = (
          data[[uivar]] 
          - data[[paste0('forecast',h,'_','consensus')]]
        )
        model_regex = lm(as.formula(paste0(
          uivar,' ~ ',paste(c(
            normal_model_regex,covid_model_regex
          ),sep='',collapse=' + '))),
          data = estdata[(nrow(estdata)-window):nrow(estdata),]
        )
        data[data$tm>=period,paste0('forecast',h,'_','regex')] = predict(
          model_regex,
          data[data$tm>=period,]
        )
        data[[paste0('resid',h,'_','regex')]] = (
          data[[uivar]] 
          - data[[paste0('forecast',h,'_','regex')]]
        )
        model_BERT = lm(as.formula(paste0(
          uivar,' ~ ',paste(c(
            normal_model_BERT,covid_model_BERT
          ),sep='',collapse=' + '))),
          data = estdata[(nrow(estdata)-window):nrow(estdata),]
        )
        data[data$tm>=period,paste0('forecast',h,'_','BERT')] = predict(
          model_BERT,
          data[data$tm>=period,]
        )
        data[[paste0('resid',h,'_','BERT')]] = (
          data[[uivar]] 
          - data[[paste0('forecast',h,'_','BERT')]]
        )
      }
      forecast = data[data$tm>=teststart,c(
        'tm','iclaimnsa','n_users_ma4',paste0('forecast',h,'_',specs)
      )]
      forecasts = rbind(forecasts,forecast)
    }
    write.csv(
      forecasts,
      file = paste0("./predict/forecasts_national_d",dd,".csv"),
      row.names = FALSE
    )
  }
}


## Predict state level UI claims

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

data0 = read.csv(
  file = paste0("../preprocessing/data/restricted/data_state.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
ddss = c(0,1,3,5,6,7,8,9,11,13)
for (n in 1:length(states)) {
  print(n)
  
  # List of maximum lags
  lagmaxs = c(4)
  
  # List of twitter models
  cutoffs = c('990')
  models = c("regex","bert990")
  
  # List of twitter labels
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
  testend = 202252
  
  hmax = 0
  specs = c('AR','consensus','regex','BERT')
  samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
  uivars = c('iclaimnsa')
  activities = 0:7
  ages = 0:4
  
  # state by state rolling regressions
  windows = c(4*52-52)
  
  state0 = states[n]
  data = data0[data0$state==state0,]
  allperiods = sort(unique(data[!is.na(data$tm) & data$sample==TRUE,'tm']))
  periods = sort(unique(data[
    !is.na(data$tm) & data$sample==TRUE 
    & data$tm>=teststart & data$tm<=testend,'tm']))
  for (dd in ddss) {
    if (dd>=8) {
      lagmin = 2
      lagmin2 = 2
      lagmin3 = 1
      dds = dd:13
    } else if (dd>=7) {
      lagmin = 1
      lagmin2 = 1
      lagmin3 = 0
      dds = dd:dd
    } else {
      lagmin = 1
      lagmin2 = 1
      lagmin3 = 0
      dds = dd:5
    }
    RMSEs = c()
    forecasts = c()
    for (h in 0:hmax) {
      for (uivar in 'iclaimnsa') {
        cv_lagmaxs = c()
        cv_spells = c()
        cv_cutoffs = c()
        cv_windows = c()
        cv_mas = c()
        for (period in periods) {
          if (period==periods[1] || ((period-floor(period/100)*100) %% 52)==0) {
            cv_RMSEs = c()
            for (cv_window in windows) {
              for (cv_lagmax in lagmaxs) {
                for (cv_spell in spells) {
                  for (cv_cutoff in cutoffs) {
                    for (cv_ma in mas) {
                      label = paste0('adj',c(cv_spell),'_','is_unemployed')
                      cv_normal_model_BERT = c(
                        paste0('L',((lagmin:cv_lagmax)+h),'_',uivar),
                        outer(
                          paste0('L',(c(lagmin:cv_lagmax)+h),'_',label,'_','bert',cv_cutoff,'_'),
                          paste0('mrp_',1:5,'_n_users','_ma',cv_ma),paste0),
                        outer(
                          paste0('L',0,'_',label,'_','bert',cv_cutoff,'_'),
                          paste0('mrp_now',dds,'_n_users','_ma',cv_ma),paste0),
                        paste0('iso2_L',((lagmin:cv_lagmax)+h),'_',uivar),
                        paste0('iso2_L',((lagmin:cv_lagmax)+h),'_','consensus'),
                        outer(
                          paste0('iso2_L',(c(lagmin:cv_lagmax)+h),'_',label,'_','bert',cv_cutoff,'_'),
                          paste0('mrp_',1:5,'_n_users','_ma',cv_ma),paste0),
                        outer(
                          paste0('iso2_L',lagmin:cv_lagmax,'_',label,'_','bert',cv_cutoff,'_'),
                          paste0('mrp_now',dds,'_n_users','_ma',cv_ma),paste0) # ,
                      )
                      if (period>=covidend) {
                        cv_covid_model_BERT = c(
                          paste0('L',((lagmin:cv_lagmax)+h),'_',uivar,'_','dummy_covid'),
                          outer(
                            paste0('L',(c(lagmin:cv_lagmax)+h),'_',label,'_','bert',cv_cutoff,'_'),
                            paste0('mrp_',1:5,'_n_users','_ma',cv_ma,'_','dummy_covid'),paste0),
                          outer(
                            paste0('L',0,'_',label,'_','bert',cv_cutoff,'_'),
                            paste0('mrp_now',dds,'_n_users','_ma',cv_ma,'_','dummy_covid'),paste0),
                          paste0('iso2_L',((lagmin:cv_lagmax)+h),'_',uivar,'_','dummy_covid'),
                          paste0('iso2_L',((lagmin:cv_lagmax)+h),'_','consensus','_','dummy_covid'),
                          outer(
                            paste0('iso2_L',(c(lagmin:cv_lagmax)+h),'_',label,'_','bert',cv_cutoff,'_'),
                            paste0('mrp_',1:5,'_n_users','_ma',cv_ma,'_','dummy_covid'),paste0),
                          outer(
                            paste0('iso2_L',lagmin:cv_lagmax,'_',label,'_','bert',cv_cutoff,'_'),
                            paste0('mrp_now',dds,'_n_users','_ma',cv_ma,'_','dummy_covid'),paste0),
                          'dummy_covid'
                        )
                      } else {
                        cv_covid_model_BERT = c()
                      }
                      cvperiods = c(201952)
                      
                      for (cvperiod in cvperiods) {
                        
                        cv_t0 = allperiods[allperiods<=cvperiod]
                        cv_t0 = cv_t0[max(1,length(cv_t0)-cv_window+1)]
                        tempdata = data[
                          data$tm<=cvperiod & data$sample==TRUE
                          ,]
                        cv_model_BERT = lm(as.formula(paste0(
                          uivar,' ~ ',paste(c(
                            cv_normal_model_BERT,cv_covid_model_BERT
                          ),sep='',collapse=' + '))),
                          data = tempdata
                        )
                        data[data$tm>=cvperiod,'cv_forecast_BERT'] = predict(
                          cv_model_BERT,
                          data[data$tm>=cvperiod,]
                        )
                        data[['cv_resid_BERT']] = data[[uivar]] - data[['cv_forecast_BERT']]
                        
                      }
                      cv_RMSE = data[data$tm>=teststart,c('tm',paste0('cv_resid_','BERT'))]
                      colnames(cv_RMSE) = c('tm','RMSE_BERT')
                      cv_RMSE[['tm']] = NULL
                      cv_RMSE = cv_RMSE^2 %>% summarise_all(mean,na.rm=TRUE)
                      cv_RMSE[['cv_spell']] = cv_spell
                      cv_RMSE[['cv_cutoff']] = cv_cutoff
                      cv_RMSE[['cv_lagmax']] = cv_lagmax
                      cv_RMSE[['cv_window']] = cv_window
                      cv_RMSE[['cv_ma']] = cv_ma
                      cv_RMSEs = rbind(cv_RMSEs,cv_RMSE)
                    }
                  }
                }
              }
            }
            min_cv_RMSEs = cv_RMSEs[
              cv_RMSEs[['RMSE_BERT']]==min(cv_RMSEs[['RMSE_BERT']]),]
            min_cv_RMSEs = min_cv_RMSEs[1,]
            spell = min_cv_RMSEs[['cv_spell']]
            cutoff = min_cv_RMSEs[['cv_cutoff']]
            lagmax = min_cv_RMSEs[['cv_lagmax']]
            window = min_cv_RMSEs[['cv_window']]
            ma = min_cv_RMSEs[['cv_ma']]
            cv_lagmaxs = c(cv_lagmaxs,lagmax)
            cv_spells = c(cv_spells,spell)
            cv_cutoffs = c(cv_cutoffs,cutoff)
            cv_windows = c(cv_windows,window)
            cv_mas = c(cv_mas,ma)
          }
          normal_model_AR = c(
            paste0('L',lagmin:lagmax,'_',uivar),
            paste0('iso2_L',lagmin:lagmax,'_',uivar)
          )
          normal_model_consensus = c(
            paste0('L',lagmin:lagmax,'_',uivar),
            paste0('iso2_L',lagmin:lagmax,'_',uivar),
            paste0('iso2_L',lagmin2:lagmin2,'_','consensus')
          )
          normal_model_regex = c(
            paste0('L',lagmin:(lagmax-lagmin3*2),'_',uivar),
            outer(
              paste0('L',lagmin:(lagmax-lagmin3*2),'_',label,'_','regex','_'),
              paste0('mrp_',1:5,'_n_users','_ma',ma),paste0),
            outer(
              paste0('L',lagmin,'_',label,'_','regex','_'),
              paste0('mrp_now',dds,'_n_users','_ma',ma),paste0),
            paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',uivar),
            outer(
              paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',label,'_','regex','_'),
              paste0('mrp_',1:5,'_n_users','_ma',ma),paste0),
            outer(
              paste0('iso2_L',lagmin,'_',label,'_','regex','_'),
              paste0('mrp_now',dds,'_n_users','_ma',ma),paste0)
          )
          normal_model_BERT = c(
            paste0('L',lagmin:(lagmax-lagmin3*2+1),'_',uivar),
            outer(
              paste0('L',lagmin:(lagmax-lagmin3*2+1),'_',label,'_','bert',cutoff,'_'),
              paste0('mrp_',1:5,'_n_users','_ma',ma),paste0),
            outer(
              paste0('L',lagmin3:(lagmax-lagmin3*2),'_',label,'_','bert',cutoff,'_'),
              paste0('mrp_now',dds,'_n_users','_ma',ma),paste0),
            paste0('iso2_L',lagmin:(lagmax-lagmin3*2+1),'_',uivar),
            outer(
              paste0('iso2_L',lagmin:(lagmax-lagmin3*2+1),'_',label,'_','bert',cutoff,'_'),
              paste0('mrp_',1:5,'_n_users','_ma',ma),paste0),
            outer(
              paste0('iso2_L',lagmin3:(lagmax-lagmin3*2),'_',label,'_','bert',cutoff,'_'),
              paste0('mrp_now',dds,'_n_users','_ma',ma),paste0)
          )
          if (period>=covidend) {
            
            covid_model_AR = c(
              paste0('L',lagmin:lagmax,'_',uivar,'_','dummy_covid'),
              paste0('iso2_L',lagmin:lagmax,'_',uivar,'_','dummy_covid'),
              'dummy_covid'
            )
            covid_model_consensus = c(
              paste0('L',lagmin:lagmax,'_',uivar,'_','dummy_covid'),
              paste0('iso2_L',lagmin:lagmax,'_',uivar,'_','dummy_covid'),
              paste0('iso2_L',lagmin2:lagmin2,'_','consensus','_','dummy_covid'),
              'dummy_covid'
            )
            covid_model_regex = c(
              paste0('L',lagmin:(lagmax-lagmin3*2),'_',uivar,'_','dummy_covid'),
              paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',uivar,'_','dummy_covid'),
              outer(
                paste0('L',lagmin:(lagmax-lagmin3*2),'_',label,'_','regex','_'),
                paste0('mrp_',1:5,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              outer(
                paste0('L',lagmin,'_',label,'_','regex','_'),
                paste0('mrp_now',dds,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              outer(
                paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',label,'_','regex','_'),
                paste0('mrp_',1:5,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              outer(
                paste0('iso2_L',lagmin,'_',label,'_','regex','_'),
                paste0('mrp_now',dds,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              'dummy_covid'
            )
            covid_model_BERT = c(
              paste0('L',lagmin:(lagmax-lagmin3*2+1),'_',uivar,'_','dummy_covid'),
              paste0('iso2_L',lagmin:(lagmax-lagmin3*2+1),'_',uivar,'_','dummy_covid'),
              outer(
                paste0('L',lagmin:(lagmax-lagmin3*2),'_',label,'_','bert',cutoff,'_'),
                paste0('mrp_',1:5,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              outer(
                paste0('L',lagmin3:(lagmax-lagmin3*2),'_',label,'_','bert',cutoff,'_'),
                paste0('mrp_now',dds,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              outer(
                paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',label,'_','bert',cutoff,'_'),
                paste0('mrp_',1:5,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              outer(
                paste0('iso2_L',lagmin3:(lagmax-lagmin3*2),'_',label,'_','bert',cutoff,'_'),
                paste0('mrp_now',dds,'_n_users','_ma',ma,'_','dummy_covid'),paste0),
              'dummy_covid'
            )
          } else {
            covid_model_AR = covid_model_consensus = covid_model_regex = covid_model_BERT = c()
          }
          t0 = allperiods[allperiods<=period]
          t0 = t0[max(1,length(t0)-window+1)]
          tempdata = data[data$tm<=period & data$sample==TRUE,]
          
          # Produce forecasts for each model
          
          model_AR = lm(as.formula(paste0(
            uivar,' ~ ',paste(c(
              normal_model_AR,covid_model_AR
            ),sep='',collapse=' + '))),
            data = tempdata
          )
          data[data$tm>=period,'forecast_AR'] = predict(
            model_AR,
            data[data$tm>=period,]
          )
          
          model_consensus = lm(as.formula(paste0(
            uivar,' ~ ',paste(c(
              normal_model_consensus,covid_model_consensus
            ),sep='',collapse=' + '))),
            data = tempdata
          )
          data[data$tm>=period,'forecast_consensus'] = predict(
            model_consensus,
            data[data$tm>=period,]
          )
          
          model_regex = lm(as.formula(paste0(
            uivar,' ~ ',paste(c(
              normal_model_regex,covid_model_regex
            ),sep='',collapse=' + '))),
            data = tempdata
          )
          data[data$tm>=period,'forecast_regex'] = predict(
            model_regex,
            data[data$tm>=period,]
          )
          
          model_BERT = lm(as.formula(paste0(
            uivar,' ~ ',paste(c(
              normal_model_BERT,covid_model_BERT
            ),sep='',collapse=' + '))),
            data = tempdata
          )
          data[data$tm>=period,'forecast_BERT'] = predict(
            model_BERT,
            data[data$tm>=period,]
          )
        }
        forecast = data[data$tm>=teststart,c(
          'tm','state','sample','iclaimnsa',statmas,paste0('forecast_',specs)
        )]
        forecasts = rbind(forecasts,forecast)
      }
      write.csv(
        forecasts,
        file = paste0("./predict/forecasts_state_d",dd,"_",state0,".csv"),
        row.names = FALSE
      )
    }
  }
}

## Predict city level UI claims

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
cities = c(
  "Abilene","Amarillo","Austin","Beaumont","Bloomington","Boise",
  "Boston","Brownsville","Cedar Rapids","College Station","Corpus Christi",
  "Dallas","Davenport","Des Moines","Dubuque","Fort Wayne","Hartford","Houston",
  "Idaho Falls","Indianapolis","Killeen","Lafayette","Las Vegas","Longview",
  "Lubbock","McAllen","Midland","Nuevo Laredo","Reno","San Angelo","San Antonio",
  "Sioux City","South Bend","Springfield","Waco","Waterloo","Wichita Falls"
)
sample_cities = c(
  "Abilene","Amarillo","Atlanta","Austin","Beaumont",
  "Bloomington","Boise","Boston","Brownsville","Cedar Rapids",
  "Chicago","Cleveland","College Station","Columbus","Corpus Christi",
  "Dallas","Davenport","Des Moines","Dubuque","Fort Wayne",
  "Hartford","Honolulu","Houston","Idaho Falls","Indianapolis",
  "Kansas City","Killeen","Lafayette","Las Vegas","Longview",
  "Lubbock","McAllen","Midland","Nuevo Laredo","Reno",
  "San Angelo","San Antonio","Seattle","Sioux City","South Bend",
  "Springfield","Waco","Washington D.C.","Waterloo","Wichita Falls"
)

# List of maximum lags
lagmaxs = c(4)

# List of twitter models
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
testend = 202224

specs = c('AR','consensus','regex','BERT') # c('AR','regex','BERT')
samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
activities = 0:7
ages = 0:4
windows = c(4*52-26)

data = read.csv(
  file = paste0("../preprocessing/data/restricted/data_city.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
data_state = read.csv(
  file = paste0("../preprocessing/data/restricted/data_state.csv"),
  header = TRUE, sep = ",", stringsAsFactors = FALSE)
data_state = as.data.table(data_state[order(data_state$state,data_state$tm),])
setkey(data_state,state,tm)
for (var in c(
  'iclaimnsa'
)) {
  for (lag in 0:(max(lagmaxs)+1)) {
    data_state[[paste0('state_L',lag,'_',var)]] = data_state[[paste0('L',lag,'_',var)]]
    data_state[[paste0('state_L',lag,'_',var,'_','dummy_covid')]] = (
      data_state[[paste0('state_L',lag,'_',var)]] * data_state[['dummy_covid']]
    )
  }
}

for (var in c(
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_',1:5,'_','n_users','_ma'),paste0),mas,paste0),
  outer(outer(
    outer(labels,paste0('_',models,'_'),paste0),
    paste0('mrp_now',0:13,'_','n_users','_ma'),paste0),mas,paste0) # ,
)) {
  data_state[[var]] = data_state[[paste0('L',0,'_',var)]]
  for (lag in 0:(max(lagmaxs)*2)) {
    data_state[,templag0:=shift(.SD,n=(lag),fill=NA,type='lag'),by=state,.SDcols=(var)]
    data_state[,templag1:=shift(.SD,n=(lag+1),fill=NA,type='lag'),by=state,.SDcols=(var)]
    data_state[[paste0('state_L',lag,'_',var)]] = data_state[['templag0']]
    data_state[['templag0']] = NULL
    data_state[['templag1']] = NULL
    data_state[[paste0('state_L',lag,'_',var,'_','dummy_covid')]] = (
      data_state[[paste0('state_L',lag,'_',var)]] * data_state[['dummy_covid']]
    )
  }
}
data_state = as.data.frame(data_state)
data_state = data_state[,c(
  'state','tm',
  colnames(data_state)[grepl('state_L',colnames(data_state))]
)]
locations = fread(
  paste0("../preprocessing/data/locations_","US",".csv")
)[,c(
  'administrative_area_level_1_short','metro_area_name','n_pings'
  )] %>% unique()
colnames(locations) = c('state','metro_area_name','n_pings')
locations = locations[locations$metro_area_name!="",]
locations = locations[order(locations$metro_area_name,-locations$n_pings),]
locations = locations[!duplicated(locations$metro_area_name),]
data = merge(
  data,locations,
  by=c('metro_area_name'),all.x=TRUE,all.y=FALSE
)
data[is.na(data$state),'state'] = data[is.na(data$state),'metro_area_name']
data = merge(
  data,data_state,
  by=c('state','tm'),all.x=TRUE,all.y=FALSE
)
data = data[!is.na(data$state),]
data = data[order(data$metro_area_name,data$tm),]
ddss = c(0,1,3,5,6,7,8,9,11,13)

for (dd in ddss) {
  if (dd>=8) {
    lagmin = 2
    lagmin2 = 2
    lagmin3 = 1
    dds = dd:13
  } else if (dd>=7) {
    lagmin = 1
    lagmin2 = 2
    lagmin3 = 0
    dds = dd:dd
  } else {
    lagmin = 1
    lagmin2 = 1
    lagmin3 = 0
    dds = dd:5
  }
  forecasts = c()
  allperiods = sort(unique(data[!is.na(data$tm) & data$sample==TRUE,'tm']))
  periods = sort(unique(data[
    !is.na(data$tm) & data$sample==TRUE 
    & data$tm>=teststart & data$tm<=testend
    ,'tm']))
  h = 0
  for (uivar in 'iclaimnsa') {
    cv_lagmaxs = c()
    cv_spells = c()
    cv_cutoffs = c()
    cv_windows = c()
    cv_mas = c()
    for (period in periods) {
      if (period==periods[1] || ((period-floor(period/100)*100) %% 52)==0) {
        cv_RMSEs = c()
        for (cv_window in windows) {
          for (cv_lagmax in lagmaxs) {
            for (cv_spell in spells) {
              for (cv_cutoff in cutoffs) {
                for (cv_ma in mas) {
                  label = paste0('adj',c(cv_spell),'_','is_unemployed')
                  cv_normal_model_BERT = c(
                    paste0('L',lagmin2:cv_lagmax,'_',uivar),
                    paste0('state_L',lagmin2:cv_lagmax,'_',uivar),
                    paste0('iso2_L',lagmin2:cv_lagmax,'_',uivar),
                    outer(
                      paste0('L',0,'_',label,'_','bert',cv_cutoff,'_mrp_now'),
                      paste0(dds,'_n_users_ma',cv_ma),paste0),
                    outer(
                      paste0('state_L',0,'_',label,'_','bert',cv_cutoff,'_mrp_now'),
                      paste0(dds,'_n_users_ma',cv_ma),paste0),
                    outer(
                      paste0('iso2_L',0,'_',label,'_','bert',cv_cutoff,'_mrp_now'),
                      paste0(dds,'_n_users_ma',cv_ma),paste0)
                  )
                  if (period>=covidend) {
                    cv_covid_model_BERT = c(
                      paste0('L',lagmin2:cv_lagmax,'_',uivar,'_','dummy_covid'),
                      paste0('state_L',lagmin2:cv_lagmax,'_',uivar,'_','dummy_covid'),
                      paste0('iso2_L',lagmin2:cv_lagmax,'_',uivar,'_','dummy_covid'),
                      outer(
                        paste0('L',0,'_',label,'_','bert',cv_cutoff,'_mrp_now'),
                        paste0(dds,'_n_users_ma',cv_ma,'_','dummy_covid'),paste0),
                      outer(
                        paste0('state_L',0,'_',label,'_','bert',cv_cutoff,'_mrp_now'),
                        paste0(dds,'_n_users_ma',cv_ma,'_','dummy_covid'),paste0),
                      outer(
                        paste0('iso2_L',0,'_',label,'_','bert',cv_cutoff,'_mrp_now'),
                        paste0(dds,'_n_users_ma',cv_ma,'_','dummy_covid'),paste0),
                      'dummy_covid'
                    )
                  } else {
                    cv_covid_model_BERT = c()
                  }
                  cvperiods = c(201952)
                  
                  for (cvperiod in cvperiods) {
                    
                    cv_t0 = allperiods[allperiods<=cvperiod]
                    cv_t0 = cv_t0[max(1,length(cv_t0)-cv_window+1)]
                    tempdata = data[
                      data$tm<=cvperiod & data$sample==TRUE
                      ,]
                    cv_model_BERT = lm(as.formula(paste0(
                      uivar,' ~ ',paste(c(
                        cv_normal_model_BERT,cv_covid_model_BERT
                      ),sep='',collapse=' + '))),
                      data = tempdata
                    )
                    data[data$tm>=cvperiod,'cv_forecast_BERT'] = predict(
                      cv_model_BERT,
                      data[data$tm>=cvperiod,]
                    )
                    data[['cv_resid_BERT']] = data[[uivar]] - data[['cv_forecast_BERT']]
                  }
                  cv_RMSE = data[data$tm>=teststart,c('tm',paste0('cv_resid_','BERT'))]
                  colnames(cv_RMSE) = c('tm','RMSE_BERT')
                  cv_RMSE[['tm']] = NULL
                  cv_RMSE = cv_RMSE^2 %>% summarise_all(mean,na.rm=TRUE)
                  cv_RMSE[['cv_spell']] = cv_spell
                  cv_RMSE[['cv_cutoff']] = cv_cutoff
                  cv_RMSE[['cv_lagmax']] = cv_lagmax
                  cv_RMSE[['cv_window']] = cv_window
                  cv_RMSE[['cv_ma']] = cv_ma
                  cv_RMSEs = rbind(cv_RMSEs,cv_RMSE)
                }
              }
            }
          }
        }
        min_cv_RMSEs = cv_RMSEs[
          cv_RMSEs[['RMSE_BERT']]==min(cv_RMSEs[['RMSE_BERT']]),]
        min_cv_RMSEs = min_cv_RMSEs[1,]
        spell = min_cv_RMSEs[['cv_spell']]
        cutoff = min_cv_RMSEs[['cv_cutoff']]
        lagmax = min_cv_RMSEs[['cv_lagmax']]
        window = min_cv_RMSEs[['cv_window']]
        ma = min_cv_RMSEs[['cv_ma']]
        cv_lagmaxs = c(cv_lagmaxs,lagmax)
        cv_spells = c(cv_spells,spell)
        cv_cutoffs = c(cv_cutoffs,cutoff)
        cv_windows = c(cv_windows,window)
        cv_mas = c(cv_mas,ma)
      }
      normal_model_AR = c(
        paste0('L',lagmin:(lagmax-lagmin3*2),'_',uivar),
        paste0('state_L',lagmin:(lagmax-lagmin3*2),'_',uivar),
        paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',uivar),
        'emp_sit'
      )
      normal_model_consensus = c(
        paste0('L',lagmin:(lagmax-lagmin3*2),'_',uivar),
        paste0('state_L',lagmin:(lagmax-lagmin3*2),'_',uivar),
        paste0('iso2_L',lagmin:(lagmax-lagmin3*2),'_',uivar),
        paste0('iso2_L',lagmin:lagmin,'_','consensus'),
        'emp_sit'
      )
      normal_model_regex = c(
        paste0('L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar),
        paste0('state_L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar),
        paste0('iso2_L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar),
        outer(paste0('L',(c(1,(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_'),paste0(1:5,'_n_users_ma',ma),paste0),
        outer(paste0('state_L',(c(1,(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_'),paste0(1:5,'_n_users_ma',ma),paste0),
        outer(paste0('iso2_L',(c(1,(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_'),paste0(1:5,'_n_users_ma',ma),paste0),
        outer(paste0('L',(c(lagmin3:(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_now'),paste0(dds,'_n_users_ma',ma),paste0),
        paste0('iso2_L',((lagmin2:lagmin2)+h),'_','consensus'),
        'emp_sit'
      )
      normal_model_BERT = c(
        paste0('L',((lagmin:((lagmax-lagmin3*2)+1))+h),'_',uivar),
        paste0('state_L',((lagmin:((lagmax-lagmin3*2)+1))+h),'_',uivar),
        paste0('iso2_L',((lagmin:((lagmax-lagmin3*2)+1))+h),'_',uivar),
        outer(paste0('L',(c(1:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_'),paste0(1:5,'_n_users_ma',ma),paste0),
        outer(paste0('state_L',(c(1:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_'),paste0(1:5,'_n_users_ma',ma),paste0),
        outer(paste0('iso2_L',(c(1:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_'),paste0(1:5,'_n_users_ma',ma),paste0),
        outer(paste0('L',(c(lagmin3:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_now'),paste0(dds,'_n_users_ma',ma),paste0),
        outer(paste0('state_L',(c(lagmin3:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_now'),paste0(dds,'_n_users_ma',ma),paste0),
        outer(paste0('iso2_L',(c(lagmin3:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_now'),paste0(dds,'_n_users_ma',ma),paste0),
        paste0('iso2_L',((lagmin2:((lagmax-lagmin3*2)+1))+h),'_','consensus'),
        'emp_sit'
      )
      if (period>=covidend) {
        covid_model_AR = c(
          paste0('L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('state_L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          'dummy_covid'
        )
        covid_model_consensus = c(
          paste0('L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('state_L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('iso2_L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('iso2_L',((lagmin:lagmin)+h),'_','consensus','_','dummy_covid'),
          'dummy_covid'
        )
        covid_model_regex = c(
          paste0('L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('state_L',(c(lagmin,(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('iso2_L',((lagmin:(lagmax-lagmin3*2))+h),'_',uivar,'_','dummy_covid'),
          paste0('iso2_L',((lagmin2:lagmin2)+h),'_','consensus','_','dummy_covid'),
          outer(paste0('state_L',(c(1,(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_'),paste0(1:5,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('L',(c(1:(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_'),paste0(1:5,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('iso2_L',(c(1,(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_'),paste0(1:5,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('L',(c(lagmin3:(lagmax-lagmin3*2))+h),'_',label,'_','regex','_mrp_now'),paste0(dds,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          'dummy_covid'
        )
        covid_model_BERT = c(
          paste0('L',((lagmin:((lagmax-lagmin3*2)+1))+h),'_',uivar,'_','dummy_covid'),
          paste0('state_L',(c(lagmin:((lagmax-lagmin3*2)+1))+h),'_',uivar,'_','dummy_covid'),
          paste0('iso2_L',((lagmin:((lagmax-lagmin3*2)+1))+h),'_',uivar,'_','dummy_covid'),
          paste0('iso2_L',((lagmin2:((lagmax-lagmin3*2)+1))+h),'_','consensus','_','dummy_covid'),
          outer(paste0('state_L',(c(1:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_'),paste0(1:5,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('L',(c(1:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_'),paste0(1:5,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('iso2_L',(c(1:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_'),paste0(1:5,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('state_L',(c(lagmin3:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_now'),paste0(dds,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('L',(c(lagmin3:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_now'),paste0(dds,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          outer(paste0('iso2_L',(c(lagmin3:((lagmax-lagmin3*2)+1))+h),'_',label,'_','bert',cutoff,'_mrp_now'),paste0(dds,'_n_users_ma',ma,'_','dummy_covid'),paste0),
          'dummy_covid'
        )
      } else {
        covid_model_AR = covid_model_consensus = covid_model_regex = covid_model_BERT = c()
      }
      t0 = allperiods[allperiods<=period]
      t0 = t0[max(1,length(t0)-window+1)]
      tempdata = data[
        data$tm<=period & data$sample==TRUE
        ,]
      
      # Predictions for each model
      
      model_AR = lm(as.formula(paste0(
        uivar,' ~ ',paste(c(
          normal_model_AR,covid_model_AR
        ),sep='',collapse=' + '))),
        data = tempdata
      )
      data[data$tm>=period,'forecast_AR'] = predict(
        model_AR,
        data[data$tm>=period,]
      )
      model_consensus = lm(as.formula(paste0(
        uivar,' ~ ',paste(c(
          normal_model_consensus,covid_model_consensus
        ),sep='',collapse=' + '))),
        data = tempdata
      )
      data[data$tm>=period,'forecast_consensus'] = predict(
        model_consensus,
        data[data$tm>=period,]
      )
      model_regex = lm(as.formula(paste0(
        uivar,' ~ ',paste(c(
          normal_model_regex,covid_model_regex
        ),sep='',collapse=' + '))),
        data = tempdata
      )
      data[data$tm>=period,'forecast_regex'] = predict(
        model_regex,
        data[data$tm>=period,]
      )
      model_BERT = lm(as.formula(paste0(
        uivar,' ~ ',paste(c(
          normal_model_BERT,covid_model_BERT
        ),sep='',collapse=' + '))),
        data = tempdata
      )
      data[data$tm>=period,'forecast_BERT'] = predict(
        model_BERT,
        data[data$tm>=period,]
      )
    }
    forecast = data[data$tm>=teststart,c(
      'tm','metro_area_name','sample',statmas,'iclaimnsa',
      paste0('forecast_',specs)
    )]
    forecasts = rbind(forecasts,forecast)
  }
  write.csv(
    forecasts,
    file = paste0("./predict/forecasts_city_d",dd,".csv"),
    row.names = FALSE
  )
}






