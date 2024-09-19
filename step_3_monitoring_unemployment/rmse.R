##############################################################
# Calculate RMSE of predictions

## RMSE national level forecasts

# List of twitter models
models = c("regex","bert990"); spells = c(0); labels = c(paste0('adj',spells,'_','is_unemployed'))
mas = c(4); ds = c(5); stats = c('n_users'); statmas = paste0('n_users','_ma',mas)

# Sample periods
trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252

# Alternative models
specs = c('AR','consensus','regex','BERT')

# Compute RMSE for each model and forecast horizon
RMSE_fulls = c(); SE_fulls = c()
for (dd0 in 0:14) {
  if (dd0==14) {
    dd = 13
  } else {
    dd = dd0
  }
  # Load forecasts
  forecasts = as.data.frame(fread(paste0("./predict/forecasts_national_d",dd,".csv")))
  RMSE2 = data.table(forecasts)
  
  # Calculate forecast errors
  for (spec in specs) {
    RMSE2[[paste0('resid_',spec)]] = (RMSE2[[paste0('forecast0_',spec)]] - RMSE2[['iclaimnsa']])
  }
  RMSE2[[paste0('resid_','AR')]] = RMSE2[['iclaimnsa']]
  setkey(RMSE2,tm)
  RMSE2[,templag:=shift(.SD,n=1,fill=NA,type='lag'),.SDcols=('iclaimnsa')]
  RMSE2[['growth']] = abs((RMSE2[['iclaimnsa']]) - (RMSE2[['templag']]))
  RMSE2[['templag']] = NULL
  pctiles = quantile(RMSE2[['growth']],probs=c(0.2,0.8),na.rm=TRUE)
  RMSE2[['group']] = dd0
  RMSE2 = na.omit(RMSE2[RMSE2$tm>=teststart,c('tm','group',paste0('resid_',specs)),with=FALSE])
  n0 = length(unique(RMSE2$tm))
  n1 = nrow(RMSE2)
  
  # Diebold-Mariano test statistic
  setkey(RMSE2,tm)
  for (spec in specs) {
    
	# Calculate squared errors
	RMSE2[[paste0('full_',spec)]] = RMSE2[[paste0('resid_',spec)]]^2
    for (k in 0:(ceiling(n0)^(1/3)+1)) {
      RMSE2[,templag:=shift(.SD,n=k,fill=NA,type='lag'),.SDcols=(paste0('full_',spec))]
      RMSE2[[paste0('gamma',k,'_',spec)]] = ((RMSE2[[paste0('full_',spec)]] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE)) * (RMSE2[['templag']] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE)))
      RMSE2[['templag']] = NULL
    }
  }
  RMSE2[['tm']] = NULL
  
  # Calculate standard errors implied by DM statistics
  for (spec in specs) {
    RMSE2[[paste0('se_',spec)]] = RMSE2[[paste0('gamma',0,'_',spec)]]
    for (k in 1:(ceiling(n0)^(1/3)+1)) {
      RMSE2[[paste0('se_',spec)]] = (RMSE2[[paste0('se_',spec)]] + 2 * (RMSE2[[paste0('gamma',k,'_',spec)]]))
    }
  }
  RMSE2 = RMSE2[,c('group',paste0('full_',specs),paste0('se_',specs)),with=FALSE]
  
  # Calculate RMSEs
  RMSE2 = RMSE2 %>% group_by(group) %>% summarise_all(mean,na.rm=TRUE)
  for (spec in specs) {
    RMSE2[[paste0('se_',spec)]] = (sqrt(RMSE2[[paste0('se_',spec)]]/n0))
    min_SE = min(RMSE2[[paste0('se_',spec)]],na.rm=TRUE)
    RMSE2[is.nan(RMSE2[[paste0('se_',spec)]]),paste0('se_',spec)] = min_SE
    RMSE2[[paste0('full_',spec)]] = sqrt(RMSE2[[paste0('full_',spec)]])
    RMSE2[[paste0('full_',spec)]] = sqrt(RMSE2[[paste0('full_',spec)]])
  }
  RMSE2 = RMSE2[,c('group',paste0('full_',specs),paste0('se_',specs))]
  colnames(RMSE2) = c('group','AR','Consensus','Rule','BERT',paste0('se_',specs))
  RMSE2_melt = data.table::melt(data.table(RMSE2[,c('group','AR','Consensus','Rule','BERT')]),id.vars=c('group'))
  SE2_melt = RMSE2[,c('group',paste0('se_',specs))] 
  colnames(SE2_melt) = c('group','AR','Consensus','Rule','BERT')
  SE2_melt = data.table::melt(data.table(SE2_melt),id.vars=c('group'))
  colnames(SE2_melt) = c('group','variable','SE')
  max_SE = max(SE2_melt[['SE']],na.rm=TRUE)
  RMSE2_melt = merge(RMSE2_melt,SE2_melt,by=c('group','variable'),all=TRUE)
  RMSE2_melt_AR = RMSE2_melt[RMSE2_melt$variable=='AR',c('group','value','SE')]
  colnames(RMSE2_melt_AR) = c('group','value_AR','SE_AR')
  RMSE2_melt = merge(RMSE2_melt,RMSE2_melt_AR,by=c('group'),all=TRUE)
  
  # RMSE as % of standard deviation of UI claims
  RMSE2_melt[['rel_to_CF']] = (RMSE2_melt$value / RMSE2_melt$value_AR) * 100
  
  # Calculate standard error for the ratio using Delta method
  RMSE2_melt[['SE0']] = sqrt((RMSE2_melt[['value']]^2*RMSE2_melt[['SE']]^2 + RMSE2_melt[['value_AR']]^2*RMSE2_melt[['SE_AR']]^2)/RMSE2_melt[['value_AR']]^3 * 100)
  ngroup = RMSE2_melt[RMSE2_melt$variable=='BERT',c('group','rel_to_CF')] %>% arrange(-rel_to_CF)
  ngroup[['ngroup']] = factor(1:nrow(ngroup),labels=ngroup$group)
  ngroup[['rel_to_CF']] = NULL
  RMSE2_melt[['ngroup']] = NULL
  RMSE2_melt = merge(RMSE2_melt,as.data.frame(ngroup),by='group',all.x=TRUE,all.y=FALSE)
  
  # Reshape table to wide format
  RMSE_full = merge(
    RMSE2_melt[RMSE2_melt$variable=='Rule',c('group','rel_to_CF')] %>% rename_at('rel_to_CF',~'Gain_full_sample_Keyword'),
    RMSE2_melt[RMSE2_melt$variable=='BERT',c('group','rel_to_CF')] %>% rename_at('rel_to_CF',~'Gain_full_sample_BERT'),
    by=c('group'),all=TRUE
  )
  RMSE_full = merge(
    RMSE_full,
    RMSE2_melt[RMSE2_melt$variable=='Consensus',c('group','rel_to_CF')] %>% rename_at('rel_to_CF',~'Gain_full_sample_Consensus'),
    by=c('group'),all=TRUE
  ) %>% rename_at('group',~'h')
  RMSE_fulls = rbind(RMSE_fulls,RMSE_full)
  SE_full = merge(
    RMSE2_melt[RMSE2_melt$variable=='Rule',c('group','SE0')] %>% rename_at('SE0',~'SE0_full_sample_Keyword'),
    RMSE2_melt[RMSE2_melt$variable=='BERT',c('group','SE0')] %>% rename_at('SE0',~'SE0_full_sample_BERT'),
    by=c('group'),all=TRUE
  )
  SE_full = merge(
    SE_full,
    RMSE2_melt[RMSE2_melt$variable=='Consensus',c('group','SE0')] %>% rename_at('SE0',~'SE0_full_sample_Consensus'),
    by=c('group'),all=TRUE
  ) %>% rename_at('group',~'h')
  SE_fulls = rbind(SE_fulls,SE_full)
}
colnames(RMSE_fulls) = c('h','Rule-Based','Conversational BERT','Consensus')
dt = data.table::melt(data.table(RMSE_fulls),id.vars=c('h'))
colnames(SE_fulls) = c('h','Rule-Based','Conversational BERT','Consensus')
SE_fulls_melt = data.table::melt(data.table(SE_fulls),id.vars=c('h'))
SE_fulls_melt$value = sqrt(SE_fulls_melt$value) * 10^4
colnames(SE_fulls_melt) = c('h','variable','SE0')
dt = merge(dt,SE_fulls_melt,by=c('h','variable'),all=TRUE)
dt = dt[order(dt$variable,-dt$value),]
dt$h = rep(14:0,3)
tempdt = dt[dt$h==0,]
tempdt$h = tempdt$h - 1
dt = rbind(dt,tempdt)
dt = dt[order(dt$variable,dt$h),]
setkey(dt,variable,h)
for (i in 1:1) {
  dt[,templag:=shift(.SD,n=i,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  dt[,templead:=shift(.SD,n=-i,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  dt[,templag2:=shift(.SD,n=i+1,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  dt[,templead2:=shift(.SD,n=-i-1,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  idx = (!is.na(dt$templag) & !is.na(dt$templead)) & ((dt$h>=9) | (dt$h<=5))
  dt[idx,'value'] = (dt[idx,'value']+dt[idx,'templag']+dt[idx,'templead'])/3
}

# 95% confidence intervals
dt[['ub']] = dt[['value']] + 1.96*dt[['SE0']]
dt[['lb']] = dt[['value']] - 1.96*dt[['SE0']]
dt = dt[,c('variable','h','value','lb','ub')]
dt$variable = factor(as.character(dt$variable),levels=c('Consensus','Rule-Based','Conversational BERT'))
dt$h = -(dt$h-5)
uirelease = -(7 - 5)
cfrelease = -(6 - 5)
dt = dt[dt$h<=5,]
temp_RMSE = dt

## Numbers cited in main text

pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019
varnames = c('iclaimnsa','forecast0_consensus','forecast0_regex','forecast0_BERT')
forecasts = as.data.frame(fread(paste0("./predict/forecasts_national_d",5,".csv")))
forecasts[,varnames] = forecasts[,varnames]*pop

# Actual UI claims data
temp1 = read.csv(file = paste0("../preprocessing/data/ICNSA.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1[['DATE']] = as.Date(temp1[['DATE']])
temp1[['year']] = year(temp1[['DATE']])
temp1[['week']] = week(temp1[['DATE']])
temp1$tm = temp1$year * 100 + temp1$week
temp1 = temp1[,c('year','week','tm','ICNSA')]
colnames(temp1) = c('year','week','tm','iclaimnsa')
temp1 = temp1[temp1$tm>=201601,]
temp1[['iclaimnsa']] = temp1[['iclaimnsa']]/10^6
ui_sd = sd(temp1[temp1$tm < 202301,'iclaimnsa'],na.rm=TRUE)

# During the first week after COVID-19 was declared a pandemic (March 14 to March 21, 2020)
# UI claims jumped from 252 thousand at the beginning of the week 
# to 2.9 million claims at the end of it (4.1 standard deviations)
ui_start = forecasts[forecasts$tm==202011,'iclaimnsa']
ui_end = forecasts[forecasts$tm==202012,'iclaimnsa']
print(c(ui_start,ui_end,(ui_end-ui_start)/ui_sd))

# Consensus model's estimate two days before the week ended
pred_consensus = forecasts[forecasts$tm==202012,'forecast0_consensus']
print(c(pred_consensus,(1-pred_consensus/ui_end)*100))

# Rule-based model's estimate two days before the week ended
pred_regex = forecasts[forecasts$tm==202012,'forecast0_regex']
print(c(pred_regex,(1-pred_regex/ui_end)*100))

# JoblessBERT's estimate two days before the week ended
pred_BERT = forecasts[forecasts$tm==202012,'forecast0_BERT']
print(c(pred_BERT,(1-pred_BERT/ui_end)*100))

# JoblessBERT's estimate on the day before official release of 2.9 million
forecasts = as.data.frame(fread(paste0("./predict/forecasts_national_d",0,".csv")))
forecasts[,varnames] = forecasts[,varnames]*pop
pred_BERT = forecasts[forecasts$tm==202012,'forecast0_BERT']
print(c(pred_BERT,(1-pred_BERT/ui_end)*100))

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
)
ddss = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14); ddsplot = ddss

# Actual UI claims data
temp1 = read.csv(file = "../preprocessing/data/ICNSA.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
temp1[['DATE']] = as.Date(temp1[['DATE']])
temp1[['year']] = year(temp1[['DATE']])
temp1[['week']] = week(temp1[['DATE']])
temp1$tm = temp1$year * 100 + temp1$week
temp1 = temp1[,c('year','week','tm','ICNSA')]
colnames(temp1) = c('year','week','tm','iclaimnsa')
pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019
uivars = "iclaimnsa"
temp1[[uivars]] = temp1[[uivars]] / pop
temp1[[uivars]] = log(temp1[[uivars]]) - log(shift(temp1[[uivars]],1))

# Compute RMSE for each model and forecast horizon
h = 0
tables = c()
for (dd0 in ddsplot) {
  if (dd0==2) {
    dd = 1
  } else if (dd0==4) {
    dd = 3
  } else if (dd0==10) {
    dd = 9
  } else if (dd0==12) {
    dd = 11
  } else if (dd0==14) {
    dd = 13
  } else {
    dd = dd0
  }
  # Load forecasts
  forecasts = c()
  for (state0 in states) {
    temp = as.data.frame(fread(paste0("./predict/forecasts_state_d",dd,"_",state0,".csv")))
    forecasts = rbind(forecasts,temp)
  }
  forecasts[,c('state')] = NULL
  forecasts = forecasts %>% group_by(tm,sample) %>% summarize_all(mean,na.rm=TRUE)
  forecasts[['iclaimnsa']] = NULL
  forecasts = merge(forecasts,temp1,by='tm',all.x=TRUE,all.y=FALSE)
  RMSE2 = forecasts[forecasts$tm>=teststart,]

  # Calculate forecast errors
  for (spec in specs) {
    RMSE2[[paste0('resid_',spec)]] = (RMSE2[[paste0('forecast_',spec)]] - RMSE2[[uivars]])
  }
  RMSE2[[paste0('resid_','AR')]] = RMSE2[[uivars]]
  RMSE2 = na.omit(RMSE2[RMSE2$tm>=teststart,c('tm','sample',paste0('resid_',specs))])
  n0 = length(unique(forecasts$tm))
  n1 = nrow(forecasts)
  RMSE2 = RMSE2 %>% group_by(sample) %>% add_count(sample) %>% as.data.table()

  # Diebold-Mariano test statistic
  setkey(RMSE2,sample,tm)
  for (spec in specs) {

    # Calculate squared errors
    RMSE2[[paste0('full_',spec)]] = RMSE2[[paste0('resid_',spec)]]^2
    for (k in 0:(ceiling(n0)^(1/3)+1)) {
      RMSE2[,templag:=shift(.SD,n=k,fill=NA,type='lag'),by=sample,.SDcols=(paste0('full_',spec))]
      RMSE2[[paste0('gamma',k,'_',spec)]] = ((RMSE2[[paste0('full_',spec)]] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE)) * (RMSE2[['templag']] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE)))
      RMSE2[['templag']] = NULL
      RMSE2[(ceiling(RMSE2[['n']])^(1/3)+1) < k,paste0('gamma',k,'_',spec)] = 0
    }
  }
  RMSE2[['tm']] = NULL
  
  # Calculate RMSEs
  RMSE2 = RMSE2[!is.na(RMSE2[['sample']]),] %>% group_by(sample) %>% summarise_all(mean,na.rm=TRUE)
  
  # Calculate standard errors implied by DM statistics
  for (spec in specs) {
    RMSE2[[paste0('se_',spec)]] = RMSE2[[paste0('gamma',0,'_',spec)]]
    for (k in 1:(ceiling(n0)^(1/3)+1)) {
      RMSE2[[paste0('se_',spec)]] = (RMSE2[[paste0('se_',spec)]] + 2 * RMSE2[[paste0('gamma',k,'_',spec)]])
    }
    RMSE2[[paste0('se_',spec)]] = (sqrt(RMSE2[[paste0('se_',spec)]]/sqrt(n1)))
    RMSE2[[paste0('full_',spec)]] = sqrt(RMSE2[[paste0('full_',spec)]])
  }
  RMSE2 = RMSE2[,c('sample',paste0('full_',specs),paste0('se_',specs))]
  colnames(RMSE2) = c('sample','AR','Consensus','Rule','BERT',paste0('se_',specs))
  RMSE2_melt = data.table::melt(data.table(RMSE2[,c('sample','AR','Consensus','Rule','BERT')]),id.vars=c('sample'))
  SE2_melt = RMSE2[,c('sample',paste0('se_',specs))]
  colnames(SE2_melt) = c('sample','AR','Consensus','Rule','BERT')
  SE2_melt = data.table::melt(data.table(SE2_melt),id.vars=c('sample'))
  colnames(SE2_melt) = c('sample','variable','SE')
  max_SE = max(SE2_melt[['SE']],na.rm=TRUE)
  RMSE2_melt = merge(RMSE2_melt,SE2_melt,by=c('sample','variable'),all=TRUE)
  RMSE2_melt_AR = RMSE2_melt[RMSE2_melt$variable=='AR',c('sample','value','SE')]
  colnames(RMSE2_melt_AR) = c('sample','value_AR','SE_AR')
  RMSE2_melt = merge(RMSE2_melt,RMSE2_melt_AR,by=c('sample'),all=TRUE)
  
  # RMSE as % of standard deviation of UI claims
  RMSE2_melt[['rel_to_AR']] = (RMSE2_melt$value / RMSE2_melt$value_AR) * 100
  
  # Calculate standard error for the ratio using Delta method
  RMSE2_melt[['SE']] = sqrt((RMSE2_melt[['value']]^2*RMSE2_melt[['SE']]^2 + RMSE2_melt[['value_AR']]^2*RMSE2_melt[['SE_AR']]^2)/RMSE2_melt[['value_AR']]^2 * 100)
  nsample = RMSE2_melt[RMSE2_melt$variable=='BERT',c('sample','rel_to_AR')] %>% arrange(rel_to_AR)
  nsample[['nsample']] = factor(1:nrow(nsample),labels=nsample$sample)
  nsample[['rel_to_AR']] = NULL
  RMSE2_melt[['nsample']] = NULL
  RMSE2_melt = merge(RMSE2_melt,data.frame(nsample),by='sample',all.x=TRUE,all.y=FALSE)
  RMSE2_melt = data.frame(RMSE2_melt[RMSE2_melt$variable!='AR',])
  RMSE2_melt[['variable2']] = factor(paste0(as.character(RMSE2_melt[,'variable'])),levels=c('BERT','Rule','Consensus'))
  RMSE2_melt_wgt = RMSE2_melt[,c('nsample','variable2','value','value_AR','rel_to_AR','SE')]
  RMSE2_melt_wgt = RMSE2_melt_wgt[RMSE2_melt_wgt$variable2!="AR",]
  RMSE2_melt_wgt[['nsample']] = NULL
  table = RMSE2_melt_wgt %>% group_by(variable2) %>% summarise_all(mean)
  table[['h']] = dd0
  tables = rbind(tables,table)
}
tables[['variable2']] = gsub('Consensus','AR',tables[['variable2']])
tables[['variable2']] = gsub('Rule','Rule-Based',tables[['variable2']])
tables[['variable2']] = gsub('BERT','Conversational BERT',tables[['variable2']])
dt = tables[,c('h','variable2','rel_to_AR')]
colnames(dt) = c('h','variable','value')
SE_fulls_melt = tables[,c('h','variable2','SE')]
colnames(SE_fulls_melt) = c('h','variable','value')
colnames(SE_fulls_melt) = c('h','variable','SE0')
dt = merge(dt,SE_fulls_melt,by=c('h','variable'),all=TRUE)
dt = dt[order(dt$variable,-dt$value),]
dt$h = rep(rev(ddsplot),3)
tempdt = dt[dt$h==0,]
tempdt$h = tempdt$h - 1
dt = rbind(dt,tempdt)
dt = dt[order(dt$variable,dt$h),]
dt$h = -(dt$h-5)
dt[dt$variable=='AR','variable'] = 'Autoregressive'
dt = data.table(dt)
setkey(dt,variable,h)
for (i in c(1,1:3)) {
  dt[,templag:=shift(.SD,n=i,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  dt[,templead:=shift(.SD,n=-i,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  dt[,templag2:=shift(.SD,n=i+1,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  dt[,templead2:=shift(.SD,n=-i-1,fill=NA,type='lag'),by=variable,.SDcols=('value')]
  idx = (!is.na(dt$templag) & !is.na(dt$templead)) & (dt$variable!='Autoregressive')
  dt[idx,'value'] = (dt[idx,'value']+dt[idx,'templag']+dt[idx,'templead'])/3
}
uirelease = -(7 - 5)
cfrelease = -(6 - 5)
dt = dt[dt$h<=5,]
dt = dt[order(dt$variable,dt$h),]

temp_RMSE$SE0 = (temp_RMSE$ub - temp_RMSE$lb) / 2.54
temp_RMSE = temp_RMSE[order(as.character(temp_RMSE$variable),temp_RMSE$h),]
dt$value = (dt$value + temp_RMSE$value) / 2
dt$SE0 = (dt$SE0 + temp_RMSE$SE0) / 2
temp_RMSE_CF = temp_RMSE[temp_RMSE$variable=='Consensus',c('variable','h','value','SE0')]
temp_RMSE_exCF = temp_RMSE[temp_RMSE$variable!='Consensus',c('variable','h','value','SE0')]
dt = dt[dt$variable!='Autoregressive',c('variable','h','value','SE0')]
temp_RMSE_exCF = temp_RMSE_exCF[temp_RMSE_exCF$h==0 & temp_RMSE_exCF$variable=='Conversational BERT',c('variable','value')]
colnames(temp_RMSE_exCF) = c('variable','value0')
temp0_RMSE_exCF = dt[dt$h==0 & dt$variable=='Conversational BERT',c('variable','value')]
colnames(temp0_RMSE_exCF) = c('variable','value1')
dt$value = dt$value + (temp_RMSE_exCF$value0 - temp0_RMSE_exCF$value1)
dt = rbind(dt[,c('variable','h','value','SE0')],temp_RMSE_CF[,c('variable','h','value','SE0')])

write.csv(dt,file='figure2c_rmse_by_d_national.csv',row.names=FALSE)

## RMSE state level forecasts

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
specs = c('AR','consensus','regex','BERT')

trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252

ddss = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14); ddsplot = ddss

# Compute RMSE for each model and forecast horizon
h = 0
tables = c()
for (dd0 in ddsplot) {
  if (dd0==2) {
    dd = 1
  } else if (dd0==4) {
    dd = 3
  } else if (dd0==10) {
    dd = 9
  } else if (dd0==12) {
    dd = 11
  } else if (dd0==14) {
    dd = 13
  } else {
    dd = dd0
  }
  # Load forecasts
  forecasts = c()
  for (state0 in states) {
    temp = as.data.frame(fread(paste0("./predict/forecasts_state_d",dd,"_",state0,".csv")))
    forecasts = rbind(forecasts,temp)
  }
  RMSE2 = forecasts[forecasts$tm>=teststart,]

  # Calculate forecast errors
  for (spec in specs) {
    RMSE2[[paste0('resid_',spec)]] = (RMSE2[[paste0('forecast_',spec)]] - RMSE2[['iclaimnsa']])
  }
  RMSE2[[paste0('resid_','AR')]] = RMSE2[['iclaimnsa']]
  RMSE2 = na.omit(RMSE2[RMSE2$tm>=teststart,c('tm','state','sample',paste0('resid_',specs))])
  n0 = length(unique(forecasts$tm))
  n1 = nrow(forecasts)
  RMSE2 = RMSE2 %>% group_by(state) %>% add_count(state) %>% as.data.table()

  # Diebold-Mariano test statistic
  setkey(RMSE2,state,tm)
  for (spec in specs) {

    # Calculate squared errors
    RMSE2[[paste0('full_',spec)]] = RMSE2[[paste0('resid_',spec)]]^2
    for (k in 0:(ceiling(n0)^(1/3)+1)) {
      RMSE2[,templag:=shift(.SD,n=k,fill=NA,type='lag'),by=state,.SDcols=(paste0('full_',spec))]
      RMSE2[[paste0('gamma',k,'_',spec)]] = ((RMSE2[[paste0('full_',spec)]] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE)) * (RMSE2[['templag']] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE)))
      RMSE2[['templag']] = NULL
      RMSE2[(ceiling(RMSE2[['n']])^(1/3)+1) < k,paste0('gamma',k,'_',spec)] = 0
    }
  }
  RMSE2[['tm']] = NULL
  
  # Calculate RMSEs
  RMSE2 = RMSE2[!is.na(RMSE2[['state']]),] %>% group_by(state,sample) %>% summarise_all(mean,na.rm=TRUE)
  
  # Calculate standard errors implied by DM statistics
  for (spec in specs) {
    RMSE2[[paste0('se_',spec)]] = RMSE2[[paste0('gamma',0,'_',spec)]]
    for (k in 1:(ceiling(n0)^(1/3)+1)) {
      RMSE2[[paste0('se_',spec)]] = (RMSE2[[paste0('se_',spec)]] + 2 * RMSE2[[paste0('gamma',k,'_',spec)]])
    }
    RMSE2[[paste0('se_',spec)]] = (sqrt(RMSE2[[paste0('se_',spec)]]/sqrt(n1)))
    RMSE2[[paste0('full_',spec)]] = sqrt(RMSE2[[paste0('full_',spec)]])
  }
  RMSE2 = RMSE2[,c('state','sample',paste0('full_',specs),paste0('se_',specs))]
  colnames(RMSE2) = c(
    'State','Sample','AR','Consensus','Rule','BERT',paste0('se_',specs)
  )
  RMSE2_melt = data.table::melt(
    data.table(RMSE2[,c('State','Sample','AR','Consensus','Rule','BERT')]),
    id.vars=c('State','Sample'))
  SE2_melt = RMSE2[,c('State',paste0('se_',specs))]
  colnames(SE2_melt) = c('State','AR','Consensus','Rule','BERT')
  SE2_melt = data.table::melt(data.table(SE2_melt),id.vars=c('State'))
  colnames(SE2_melt) = c('State','variable','SE')
  max_SE = max(SE2_melt[['SE']],na.rm=TRUE)
  RMSE2_melt = merge(
    RMSE2_melt,SE2_melt,
    by=c('State','variable'),all=TRUE)
  RMSE2_melt_AR = RMSE2_melt[RMSE2_melt$variable=='AR',c('State','value','SE')]
  colnames(RMSE2_melt_AR) = c('State','value_AR','SE_AR')
  RMSE2_melt = merge(
    RMSE2_melt,RMSE2_melt_AR,
    by=c('State'),all=TRUE)
  
  # RMSE as % of standard deviation of UI claims
  RMSE2_melt[['rel_to_AR']] = (RMSE2_melt$value / RMSE2_melt$value_AR) * 100

  # Calculate standard error for the ratio using Delta method
  RMSE2_melt[['SE']] = sqrt((
    RMSE2_melt[['value']]^2*RMSE2_melt[['SE']]^2
    + RMSE2_melt[['value_AR']]^2*RMSE2_melt[['SE_AR']]^2
  )/RMSE2_melt[['value_AR']]^2 * 100)
  nState = RMSE2_melt[
    RMSE2_melt$variable=='BERT',
    c('State','rel_to_AR')
  ] %>% arrange(rel_to_AR)
  nState[['nState']] = factor(1:nrow(nState),labels=nState$State)
  nState[['rel_to_AR']] = NULL
  RMSE2_melt[['nState']] = NULL
  RMSE2_melt = merge(
    RMSE2_melt,data.frame(nState),
    by='State',all.x=TRUE,all.y=FALSE)
  RMSE2_melt = data.frame(RMSE2_melt[RMSE2_melt$variable!='AR',])
  RMSE2_melt[['variable2']] = factor(paste0(
    as.character(RMSE2_melt[,'variable'])
  ),levels=c('BERT','Rule','Consensus'))
  RMSE2_melt_wgt = RMSE2_melt[,c(
    'nState','variable2','value','value_AR','rel_to_AR','SE'
  )]
  RMSE2_melt_wgt = RMSE2_melt_wgt[RMSE2_melt_wgt$variable2!="AR",]
  RMSE2_melt_wgt[['nState']] = NULL
  table = RMSE2_melt_wgt %>% group_by(variable2) %>% summarise_all(mean)
  table[['h']] = dd0
  tables = rbind(tables,table)
}
tables[['variable2']] = gsub('Consensus','AR',tables[['variable2']])
tables[['variable2']] = gsub('Rule','Rule-Based',tables[['variable2']])
tables[['variable2']] = gsub('BERT','Conversational BERT',tables[['variable2']])
dt = tables[,c('h','variable2','rel_to_AR')]
colnames(dt) = c('h','variable','v')
SE_fulls_melt = tables[,c('h','variable2','SE')]
colnames(SE_fulls_melt) = c('h','variable','v')
colnames(SE_fulls_melt) = c('h','variable','SE0')
dt = merge(dt,SE_fulls_melt,by=c('h','variable'),all=TRUE)
dt = dt[order(dt$variable,-dt$v),]
dt$h = rep(rev(ddsplot),3)
tempdt = dt[dt$h==0,]
tempdt$h = tempdt$h - 1
dt = rbind(dt,tempdt)
dt = dt[order(dt$variable,dt$h),]
dt$h = -(dt$h-5)
dt[dt$variable=='AR','variable'] = 'Autoregressive'
dt = data.table(dt)
setkey(dt,variable,h)
for (i in c(1,1:3)) {
  dt[,templag:=shift(.SD,n=i,fill=NA,type='lag'),by=variable,.SDcols=('v')]
  dt[,templead:=shift(.SD,n=-i,fill=NA,type='lag'),by=variable,.SDcols=('v')]
  dt[,templag2:=shift(.SD,n=i+1,fill=NA,type='lag'),by=variable,.SDcols=('v')]
  dt[,templead2:=shift(.SD,n=-i-1,fill=NA,type='lag'),by=variable,.SDcols=('v')]
  idx = (!is.na(dt$templag) & !is.na(dt$templead) & (dt$variable!='Autoregressive'))
  dt[idx,'v'] = (dt[idx,'v'] + dt[idx,'templag'] + dt[idx,'templead']) / 3
}

# 95% confidence intervals
dt[['ub']] = dt[['v']] + 2.54*dt[['SE0']]
dt[['lb']] = dt[['v']] - 2.54*dt[['SE0']]
uirelease = -(7 - 5)
cfrelease = -(6 - 5)
dt = dt[dt$h<=5,]
dt[dt$variable=='Conversational BERT','variable'] = 'JoblessBERT'
dt$variable = factor(
  as.character(dt$variable),
  levels=c('Autoregressive','Rule-Based','JoblessBERT'))
dt[dt$h==5,'h'] = 4+8.5/24

write.csv(dt,file='figure3b_rmse_by_d_state.csv',row.names=FALSE)

## Numbers cited in main text

# RMSE of JoblessBERT on the day prior to data release
mean_consensus = mean(as.matrix(dt[dt$variable=='Autoregressive' & dt$h==3,'v']),na.rm=TRUE)
mean_rule = mean(as.matrix(dt[dt$variable=='Rule-Based' & dt$h==3,'v']),na.rm=TRUE)
mean_bert = mean(as.matrix(dt[dt$variable=='JoblessBERT' & dt$h==3,'v']),na.rm=TRUE)
print(mean_bert/100)

# RMSE on an average two-week period before data release of actual UI claims
mean_baseline = mean(as.matrix(dt[dt$variable=='Autoregressive','v']),na.rm=TRUE)
mean_rule = mean(as.matrix(dt[dt$variable=='Rule-Based','v']),na.rm=TRUE)
mean_bert = mean(as.matrix(dt[dt$variable=='JoblessBERT','v']),na.rm=TRUE)
print((1-mean_bert/mean_baseline)*100)
print((1-mean_bert/mean_rule)*100)
err_cf = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_consensus'])
err_regex = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_regex'])
err_bert990 = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_BERT'])
dm.test(err_cf,err_bert990,alternative='two.sided',h=1,power=1)
dm.test(err_regex,err_bert990,alternative='two.sided',h=1,power=1)

# List of states
states = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID", 
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
  "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
specs = c('AR','consensus','regex','BERT')

trainstart = 201601
trainend = 201952
covidstart = 202009
covidend = 202022
teststart = 202001
testend = 202252

# Compute RMSE for each model and forecast horizon
ddss = c(5,6,7)
ddsplot = ddss
h = 0
tables = c()
forecasts = c()
for (dd0 in ddsplot) {
  if (dd0==2) {
    dd = 1
  } else if (dd0==4) {
    dd = 3
  } else if (dd0==10) {
    dd = 9
  } else if (dd0==12) {
    dd = 11
  } else if (dd0==14) {
    dd = 13
  } else {
    dd = dd0
  }
  # Load forecasts
  for (state0 in states) {
    temp = as.data.frame(fread(
      paste0("./predict/forecasts_state_d",dd,"_",state0,".csv")))
    forecasts = rbind(forecasts,temp)
  }
}
specs = c('AR','consensus','regex','BERT')
forecasts = forecasts %>% group_by(state,tm,sample) %>% summarise_all(mean,na.rm=TRUE)
RMSE2 = forecasts[forecasts$tm>=teststart,]

# Calculate forecast errors
for (spec in specs) {
  RMSE2[[paste0('resid_',spec)]] = (
    RMSE2[[paste0('forecast_',spec)]] - RMSE2[['iclaimnsa']]
  )
}
RMSE2[[paste0('resid_','AR')]] = RMSE2[['iclaimnsa']]
RMSE2 = na.omit(RMSE2[RMSE2$tm>=teststart,c(
  'tm','state','sample',paste0('resid_',specs)
)])
n0 = length(unique(forecasts$tm))
n1 = nrow(forecasts)
RMSE2 = RMSE2 %>% group_by(state) %>% add_count(state) %>% as.data.table()

# Diebold-Mariano test statistic
setkey(RMSE2,state,tm)
for (spec in specs) {
  
  # Calculate squared errors
  RMSE2[[paste0('full_',spec)]] = RMSE2[[paste0('resid_',spec)]]^2
  for (k in 0:(ceiling(n0)^(1/3)+1)) {
    RMSE2[,templag:=shift(.SD,n=k,fill=NA,type='lag'),by=state,.SDcols=(paste0('full_',spec))]
    RMSE2[[paste0('gamma',k,'_',spec)]] = (
      (RMSE2[[paste0('full_',spec)]] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE))
      * (RMSE2[['templag']] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE))
    )
    RMSE2[['templag']] = NULL
    RMSE2[(ceiling(RMSE2[['n']])^(1/3)+1) < k,paste0('gamma',k,'_',spec)] = 0
  }
}
RMSE2[['tm']] = NULL

# Calculate RMSEs
RMSE2 = RMSE2[
  !is.na(RMSE2[['state']]),
] %>% group_by(state,sample) %>% summarise_all(mean,na.rm=TRUE)

# Calculate standard errors implied by DM statistics
for (spec in specs) {
  RMSE2[[paste0('se_',spec)]] = RMSE2[[paste0('gamma',0,'_',spec)]]
  for (k in 1:(ceiling(n0)^(1/3)+1)) {
    RMSE2[[paste0('se_',spec)]] = (
      RMSE2[[paste0('se_',spec)]] + 2 * RMSE2[[paste0('gamma',k,'_',spec)]]
    )
  }
  RMSE2[[paste0('se_',spec)]] = (sqrt(RMSE2[[paste0('se_',spec)]]/n0))
  RMSE2[[paste0('full_',spec)]] = sqrt(RMSE2[[paste0('full_',spec)]])
}
RMSE2 = RMSE2[,c('state','sample',paste0('full_',specs),paste0('se_',specs))]
colnames(RMSE2) = c(
  'State','Sample','AR','Consensus','Rule','BERT',paste0('se_',specs)
)
RMSE2_melt = data.table::melt(
  data.table(RMSE2[,c('State','Sample','AR','Consensus','Rule','BERT')]),
  id.vars=c('State','Sample'))
SE2_melt = RMSE2[,c('State',paste0('se_',specs))]
colnames(SE2_melt) = c('State','AR','Consensus','Rule','BERT')
SE2_melt = data.table::melt(data.table(SE2_melt),id.vars=c('State'))
colnames(SE2_melt) = c('State','variable','SE')
max_SE = max(SE2_melt[['SE']],na.rm=TRUE)
RMSE2_melt = merge(
  RMSE2_melt,SE2_melt,
  by=c('State','variable'),all=TRUE
)
RMSE2_melt_AR = RMSE2_melt[RMSE2_melt$variable=='AR',c('State','value','SE')]
colnames(RMSE2_melt_AR) = c('State','value_AR','SE_AR')
RMSE2_melt = merge(
  RMSE2_melt,RMSE2_melt_AR,
  by=c('State'),all=TRUE
)
# RMSE as % of standard deviation of UI claims
RMSE2_melt[['rel_to_AR']] = (RMSE2_melt$value / RMSE2_melt$value_AR) * 100

# Calculate standard error for the ratio using Delta method
RMSE2_melt[['SE']] = sqrt((
  RMSE2_melt[['value']]^2*RMSE2_melt[['SE']]^2
  + RMSE2_melt[['value_AR']]^2*RMSE2_melt[['SE_AR']]^2
)/RMSE2_melt[['value_AR']]^4 * 100)
nState = RMSE2_melt[
  RMSE2_melt$variable=='BERT',c('State','rel_to_AR')
] %>% arrange(rel_to_AR)
nState[['nState']] = factor(1:nrow(nState),labels=nState$State)
nState[['rel_to_AR']] = NULL
RMSE2_melt[['nState']] = NULL
RMSE2_melt = merge(
  RMSE2_melt,data.frame(nState),
  by='State',all.x=TRUE,all.y=FALSE)
RMSE2_melt = data.frame(RMSE2_melt[RMSE2_melt$variable!='AR',])
RMSE2_melt[RMSE2_melt$variable=='Consensus','variable'] = 'AR'
RMSE2_melt[['variable2']] = factor(
  RMSE2_melt[,'variable'],
  levels=c('BERT','Rule','AR'))
RMSE2_melt$variable2 = paste0(RMSE2_melt$variable2,' (Weighted)')
RMSE2_melt[RMSE2_melt[['variable2']]=='AR (Weighted)','variable2'] = 'AR'
RMSE2_melt_wgt = RMSE2_melt[,c('nState','variable2','rel_to_AR','SE')]
RMSE2_melt_wgt = RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2!="AR (Unweighted)" 
  & RMSE2_melt_wgt$variable2!="AR (Weighted)",]
RMSE2_melt_wgt = RMSE2_melt_wgt[
  order(RMSE2_melt_wgt$nState,RMSE2_melt_wgt$variable2),]
RMSE2_melt_wgt[['variable2']] = rep(c(
  'AR','BERT (Weighted)','Rule (Weighted)'
),length(unique(RMSE2_melt_wgt$nState)))
RMSE2_melt_wgt[['variable2']] = gsub(
  'BERT','Conversational BERT',
  RMSE2_melt_wgt[['variable2']])
RMSE2_melt_wgt[['variable2']] = gsub(
  'Rule','Rule-Based',
  RMSE2_melt_wgt[['variable2']])
RMSE2_melt_wgt[['variable2']] = factor(
  RMSE2_melt_wgt[['variable2']],
  levels=c('AR','Conversational BERT (Weighted)','Rule-Based (Weighted)'))
RMSE2_melt_wgt = RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2!='Rule-Based (Unweighted)',]
RMSE2_melt_wgt = RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2!='Conversational BERT (Unweighted)',]
RMSE2_melt_wgt$variable2 = as.character(RMSE2_melt_wgt$variable2)
RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2=='Conversational BERT (Weighted)','variable2'
] = 'Conversational BERT'
RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2=='Rule-Based (Weighted)','variable2'
] = 'Rule-Based'
RMSE2_melt_wgt$State = as.character(RMSE2_melt_wgt$nState)
nState = RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable=='Conversational BERT',c('State','rel_to_AR')
] %>% arrange(rel_to_AR)
nState[['nState']] = factor(1:nrow(nState),labels=nState$State)
nState[['rel_to_AR']] = NULL
RMSE2_melt_wgt[['nState']] = NULL
RMSE2_melt_wgt = merge(
  RMSE2_melt_wgt,data.frame(nState),
  by='State',all.x=TRUE,all.y=FALSE)
RMSE2_melt_wgt$variable2 = as.character(RMSE2_melt_wgt$variable2)
RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2=='AR',
  'variable2'] = 'Autoregressive'
RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2=='Conversational BERT',
  'variable2'] = 'JoblessBERT'
RMSE2_melt_wgt[['variable2']] = factor(
  RMSE2_melt_wgt[['variable2']],
  levels=c('Autoregressive','Rule-Based','JoblessBERT'))

write.csv(RMSE2_melt_wgt,file='figure3a_rmse_by_state.csv',row.names=FALSE)

err_cf = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_regex'])
err_regex = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_consensus'])
err_bert990 = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_BERT'])
dm.test(err_cf,err_bert990,alternative='two.sided',h=1,power=1)
dm.test(err_cf,err_regex,alternative='two.sided',h=1,power=1)
dm.test(err_regex,err_bert990,alternative='two.sided',h=1,power=1)

## RMSE city level forecasts

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

# List of twitter models
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

specs = c('AR','consensus','regex','BERT')
samples = c('full','2020h1','2020h2','2021h1','2021h2','2022h1')
activities = 0:7
ages = 0:4

ddss = c(0,1,3,5,6,7,8,9,11,13,14)
ddsplot = ddss

# Compute RMSE for each model and forecast horizon
h = 0
tables = c()
for (dd0 in ddsplot) {
  if (dd0==14) {
    dd = 13
  } else {
    dd = dd0
  }
  # Load forecasts
  forecasts = as.data.frame(fread(
    paste0("./predict/forecasts_city_d",dd,".csv")))
  RMSE2 = forecasts[forecasts$tm>=teststart,]
  
  # Calculate forecast errors
  for (spec in specs) {
    RMSE2[[paste0('resid_',spec)]] = (
      RMSE2[[paste0('forecast_',spec)]] - RMSE2[['iclaimnsa']]
    )
  }
  RMSE2[[paste0('resid_','AR')]] = RMSE2[['iclaimnsa']]
  RMSE2 = na.omit(RMSE2[RMSE2$tm>=teststart,c(
    'tm','metro_area_name','sample',paste0('resid_',specs)
  )])
  n0 = length(unique(forecasts$tm))
  n1 = nrow(forecasts)
  RMSE2 = RMSE2 %>% group_by(metro_area_name) %>% add_count(metro_area_name) %>% as.data.table()
  
  # Diebold-Mariano test statistic
  setkey(RMSE2,metro_area_name,tm)
  for (spec in specs) {

    # Calculate squared errors
    RMSE2[[paste0('full_',spec)]] = RMSE2[[paste0('resid_',spec)]]^2
    for (k in 0:(ceiling(n0)^(1/3)+1)) {
      RMSE2[,templag:=shift(.SD,n=k,fill=NA,type='lag'),by=metro_area_name,.SDcols=(paste0('full_',spec))]
      RMSE2[[paste0('gamma',k,'_',spec)]] = (
        (RMSE2[[paste0('full_',spec)]] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE))
        * (RMSE2[['templag']] - mean(RMSE2[[paste0('full_',spec)]],na.rm=TRUE))
      )
      RMSE2[['templag']] = NULL
      RMSE2[(ceiling(RMSE2[['n']])^(1/3)+1) < k,paste0('gamma',k,'_',spec)] = 0
    }
  }
  RMSE2[['tm']] = NULL
  
  # Calculate RMSEs
  RMSE2 = RMSE2[
    !is.na(RMSE2[['metro_area_name']]),
  ] %>% group_by(metro_area_name,sample) %>% summarise_all(mean,na.rm=TRUE)
  
  # Calculate standard errors implied by DM statistics
  for (spec in specs) {
    RMSE2[[paste0('se_',spec)]] = RMSE2[[paste0('gamma',0,'_',spec)]]
    for (k in 1:(ceiling(n0)^(1/3)+1)) {
      RMSE2[[paste0('se_',spec)]] = (
        RMSE2[[paste0('se_',spec)]] + 2 * RMSE2[[paste0('gamma',k,'_',spec)]]
      )
    }
    RMSE2[[paste0('se_',spec)]] = (sqrt(RMSE2[[paste0('se_',spec)]]/sqrt(n1)))
    RMSE2[[paste0('full_',spec)]] = sqrt(RMSE2[[paste0('full_',spec)]])
  }
  RMSE2 = RMSE2[,c(
    'metro_area_name','sample',paste0('full_',specs),paste0('se_',specs))]
  colnames(RMSE2) = c(
    'metro_area_name','Sample','AR','Consensus','Rule','BERT',paste0('se_',specs))
  RMSE2_melt = data.table::melt(
    data.table(RMSE2[,c(
      'metro_area_name','Sample','AR','Consensus','Rule','BERT')]),
    id.vars=c('metro_area_name','Sample'))
  SE2_melt = RMSE2[,c('metro_area_name',paste0('se_',specs))]
  colnames(SE2_melt) = c('metro_area_name','AR','Consensus','Rule','BERT')
  SE2_melt = data.table::melt(data.table(SE2_melt),id.vars=c('metro_area_name'))
  colnames(SE2_melt) = c('metro_area_name','variable','SE')
  max_SE = max(SE2_melt[['SE']],na.rm=TRUE)
  RMSE2_melt = merge(
    RMSE2_melt,SE2_melt,
    by=c('metro_area_name','variable'),all=TRUE
  )
  RMSE2_melt_AR = RMSE2_melt[
    RMSE2_melt$variable=='AR',c('metro_area_name','value','SE')]
  colnames(RMSE2_melt_AR) = c('metro_area_name','value_AR','SE_AR')
  RMSE2_melt = merge(
    RMSE2_melt,RMSE2_melt_AR,
    by=c('metro_area_name'),all=TRUE
  )
  
  # RMSE as % of standard deviation of UI claims
  RMSE2_melt[['rel_to_AR']] = (RMSE2_melt$value / RMSE2_melt$value_AR) * 100
  
  # Calculate standard error for the ratio using Delta method
  RMSE2_melt[['SE']] = sqrt((
    RMSE2_melt[['value']]^2*RMSE2_melt[['SE']]^2
    + RMSE2_melt[['value_AR']]^2*RMSE2_melt[['SE_AR']]^2
  )/RMSE2_melt[['value_AR']]^2 * 100)
  nmetro_area_name = RMSE2_melt[
    RMSE2_melt$variable=='BERT',
    c('metro_area_name','rel_to_AR')
  ] %>% arrange(rel_to_AR)
  nmetro_area_name[['nmetro_area_name']] = factor(
    1:nrow(nmetro_area_name),
    labels=nmetro_area_name$metro_area_name)
  nmetro_area_name[['rel_to_AR']] = NULL
  RMSE2_melt[['nmetro_area_name']] = NULL
  RMSE2_melt = merge(
    RMSE2_melt,data.frame(nmetro_area_name),
    by='metro_area_name',all.x=TRUE,all.y=FALSE)
  RMSE2_melt = data.frame(RMSE2_melt[RMSE2_melt$variable!='AR',])
  RMSE2_melt[['variable2']] = factor(paste0(
    as.character(RMSE2_melt[,'variable']) # ,
  ),levels=c('BERT','Rule','Consensus'))
  RMSE2_melt_wgt = RMSE2_melt[,c(
    'nmetro_area_name','variable2','value','value_AR','rel_to_AR','SE')]
  RMSE2_melt_wgt = RMSE2_melt_wgt[RMSE2_melt_wgt$variable2!="AR",]
  RMSE2_melt_wgt[['nmetro_area_name']] = NULL
  table = RMSE2_melt_wgt %>% group_by(variable2) %>% summarise_all(mean)
  table[['h']] = dd0
  tables = rbind(tables,table)
}
tables[['variable2']] = gsub('Consensus','AR',tables[['variable2']])
tables[['variable2']] = gsub('Rule','Rule-Based',tables[['variable2']])
tables[['variable2']] = gsub('BERT','Conversational BERT',tables[['variable2']])
dt = tables[,c('h','variable2','rel_to_AR')]
colnames(dt) = c('h','variable','value')
SE_fulls_melt = tables[,c('h','variable2','SE')]
colnames(SE_fulls_melt) = c('h','variable','value')
colnames(SE_fulls_melt) = c('h','variable','SE0')
dt = merge(dt,SE_fulls_melt,by=c('h','variable'),all=TRUE)
dt = dt[order(dt$variable,-dt$value),]
dt$h = rep(rev(ddsplot),3)
tempdt = dt[dt$h==0,]
tempdt$h = tempdt$h - 1
dt = rbind(dt,tempdt)
dt = dt[order(dt$variable,dt$h),]
dt[dt$variable=='AR' & dt$h>=8,'value'] = mean(
  as.matrix(dt[dt$variable=='AR' & dt$h>=8,'value']),na.rm=TRUE)
dt[dt$variable=='AR' & dt$h<8,'value'] = mean(
  as.matrix(dt[dt$variable=='AR' & dt$h<2,'value']),na.rm=TRUE)
dt[dt$variable=='AR' & dt$h>=8,'SE0'] = mean(
  as.matrix(dt[dt$variable=='AR' & dt$h>=8,'SE0']),na.rm=TRUE)
dt[dt$variable=='AR' & dt$h<8,'SE0'] = mean(
  as.matrix(dt[dt$variable=='AR' & dt$h<2,'SE0']),na.rm=TRUE)

# 95% confidence intervals
dt[['ub']] = dt[['value']] + 2.54*dt[['SE0']]
dt[['lb']] = dt[['value']] - 2.54*dt[['SE0']]
dt$h = -(dt$h-5)
uirelease = -(7 - 5)
cfrelease = -(6 - 5)
dt = dt[dt$h<=5,]
dt[dt$variable=='AR','variable'] = 'Autoregressive'
dt[dt$variable=='Conversational BERT','variable'] = 'JoblessBERT'
dt$variable = factor(
  as.character(dt$variable),
  levels=c('Autoregressive','Rule-Based','JoblessBERT'))
dt[dt$h==5,'h'] = 4+8.5/24

write.csv(dt,file='figure3c_rmse_by_d_city.csv',row.names=FALSE)

## Numbers cited in main text

# RMSE of JoblessBERT on the day prior to data release
mean_consensus = mean(as.matrix(dt[dt$variable=='Autoregressive' & dt$h==3,'value']),na.rm=TRUE)
mean_rule = mean(as.matrix(dt[dt$variable=='Rule-Based' & dt$h==3,'value']),na.rm=TRUE)
mean_bert = mean(as.matrix(dt[dt$variable=='JoblessBERT' & dt$h==3,'value']),na.rm=TRUE)
print(mean_bert/100)

# RMSE on an average two-week period before data release of actual UI claims
mean_baseline = mean(as.matrix(dt[dt$variable=='Autoregressive','value']),na.rm=TRUE)
mean_rule = mean(as.matrix(dt[dt$variable=='Rule-Based','value']),na.rm=TRUE)
mean_bert = mean(as.matrix(dt[dt$variable=='JoblessBERT','value']),na.rm=TRUE)
print((1-mean_bert/mean_baseline)*100)
print((1-mean_bert/mean_rule)*100)
forecasts = na.omit(forecasts)
err_cf = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_consensus'])
err_regex = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_regex'])
err_bert990 = as.matrix(forecasts[,'iclaimnsa'] - forecasts[,'forecast_BERT'])
dm.test(err_cf,err_bert990,alternative='two.sided',h=1,power=1)
dm.test(err_regex,err_bert990,alternative='two.sided',h=1,power=1)





























