#####################################################
# Plot time series of actual UI claims and unemployment indices

rm(list = ls())

dt0 = as.data.frame(fread('time_series.csv'))
dt0 = dt0[(dt0$tm>=201901) & (dt0$tm<=202252),]
dt0 = dt0[(dt0$tm!=202053),]

# Average value during 2020 week 1 to week 4
for (var in c('iclaimnsa','regex_uw','bert990_uw','regex_mrp','bert990_mrp')) {
  dt0[52+(1:4),var] = mean(dt0[52+(1:4),var],na.rm=TRUE)
}

# Normalize each time series relative to a base period
dt0[['iclaimnsa']] = (dt0[['iclaimnsa']] / dt0[52+4,'iclaimnsa'])
dt0[['regex_mrp']] = (dt0[['regex_mrp']] / dt0[52+4,'regex_mrp'])
dt0[['bert990_mrp']] = (dt0[['bert990_mrp']] / dt0[52+4,'bert990_mrp'])
dt0[['regex_uw']] = (dt0[['regex_uw']] / dt0[52+4,'regex_uw'])
dt0[['bert990_uw']] = (dt0[['bert990_uw']] / dt0[52+4,'bert990_uw'])
dt0$time = as.numeric(1:nrow(dt0))

## Numbers cited in the main text 

# RMSE during full evaluation period
idx = (dt0$tm>=202001) & (dt0$tm<=202252)
cor_regex_uw = round(cor(dt0[idx,c('regex_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_uw = round(cor(dt0[idx,c('bert990_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_regex_mrp = round(cor(dt0[idx,c('regex_mrp')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_mrp = round(cor(dt0[idx,c('bert990_mrp')],dt0[idx,c('iclaimnsa')]),2)
mu_ui = round(mean(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
sd_ui = round(sd(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
snr_ui = round(mu_ui / sd_ui,2)
mu_regex_uw = round(mean(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
sd_regex_uw = round(sd(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
snr_regex_uw = round(mu_regex_uw / sd_regex_uw,2)
mu_bert990_uw = round(mean(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
sd_bert990_uw = round(sd(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
snr_bert990_uw = round(mu_bert990_uw / sd_bert990_uw,2)
mu_regex_mrp = round(mean(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
sd_regex_mrp = round(sd(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
snr_regex_mrp = round(mu_regex_mrp / sd_regex_mrp,2)
mu_bert990_mrp = round(mean(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
sd_bert990_mrp = round(sd(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
snr_bert990_mrp = round(mu_bert990_mrp / sd_bert990_mrp,2)
mae_regex_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]),na.rm=TRUE),2)
mae_bert990_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]),na.rm=TRUE),2)
mae_regex_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]),na.rm=TRUE),2)
mae_bert990_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]),na.rm=TRUE),2)
rmse_regex_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')])^2,na.rm=TRUE)),2)
rmse_bert990_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')])^2,na.rm=TRUE)),2)
rmse_regex_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')])^2,na.rm=TRUE)),2)
rmse_bert990_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')])^2,na.rm=TRUE)),2)
table_full = data.frame(Sample='Full Sample (2020 Week 1 - 2022 Week 52)',rbind(
  c("Rule-Based (Unweighted)",cor_regex_uw,mu_regex_uw,sd_regex_uw,snr_regex_uw,mae_regex_uw),
  c("JoblessBERT (Unweighted)",cor_bert990_uw,mu_bert990_uw,sd_bert990_uw,snr_bert990_uw,mae_bert990_uw),
  c("Rule-Based (Post-Stratified)",cor_regex_mrp,mu_regex_mrp,sd_regex_mrp,snr_regex_mrp,mae_regex_mrp),
  c("JoblessBERT (Post-Stratified)",cor_bert990_mrp,mu_bert990_mrp,sd_bert990_mrp,snr_bert990_mrp,mae_bert990_mrp),
  c("Actual UI Claims",NA,mu_ui,sd_ui,snr_ui,NA) # ,
))
colnames(table_full) = c('Sample','Variable','Cor','mu','sigma','SNR','MAE')
rmses_full = data.frame(Sample='Full Sample (2020 Week 1 - 2022 Week 52)',rbind(
  c("Rule-Based (Unweighted)",rmse_regex_uw),
  c("JoblessBERT (Unweighted)",rmse_bert990_uw),
  c("Rule-Based (Post-Stratified)",rmse_regex_mrp),
  c("JoblessBERT (Post-Stratified)",rmse_bert990_mrp)
))
colnames(rmses_full) = c('Sample','Variable','RMSE')

idx = (dt0$tm>=201901) & (dt0$tm<=2020011)
cor_regex_uw = round(cor(dt0[idx,c('regex_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_uw = round(cor(dt0[idx,c('bert990_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_regex_mrp = round(cor(dt0[idx,c('regex_mrp')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_mrp = round(cor(dt0[idx,c('bert990_mrp')],dt0[idx,c('iclaimnsa')]),2)
mu_ui = round(mean(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
sd_ui = round(sd(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
snr_ui = round(mu_ui / sd_ui,2)
mu_regex_uw = round(mean(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
sd_regex_uw = round(sd(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
snr_regex_uw = round(mu_regex_uw / sd_regex_uw,2)
mu_bert990_uw = round(mean(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
sd_bert990_uw = round(sd(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
snr_bert990_uw = round(mu_bert990_uw / sd_bert990_uw,2)
mu_regex_mrp = round(mean(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
sd_regex_mrp = round(sd(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
snr_regex_mrp = round(mu_regex_mrp / sd_regex_mrp,2)
mu_bert990_mrp = round(mean(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
sd_bert990_mrp = round(sd(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
snr_bert990_mrp = round(mu_bert990_mrp / sd_bert990_mrp,2)
mae_regex_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]),na.rm=TRUE),2)
mae_bert990_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]),na.rm=TRUE),2)
mae_regex_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]),na.rm=TRUE),2)
mae_bert990_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]),na.rm=TRUE),2)
rmse_regex_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')])^2,na.rm=TRUE)),2)
rmse_bert990_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')])^2,na.rm=TRUE)),2)
rmse_regex_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')])^2,na.rm=TRUE)),2)
rmse_bert990_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')])^2,na.rm=TRUE)),2)
table_pre = data.frame(Sample='Pre-Covid (2019 Week 1 - 2020 Week 11)',rbind(
  c("Rule-Based (Unweighted)",cor_regex_uw,mu_regex_uw,sd_regex_uw,snr_regex_uw,mae_regex_uw),
  c("JoblessBERT (Unweighted)",cor_bert990_uw,mu_bert990_uw,sd_bert990_uw,snr_bert990_uw,mae_bert990_uw),
  c("Rule-Based (Post-Stratified)",cor_regex_mrp,mu_regex_mrp,sd_regex_mrp,snr_regex_mrp,mae_regex_mrp),
  c("JoblessBERT (Post-Stratified)",cor_bert990_mrp,mu_bert990_mrp,sd_bert990_mrp,snr_bert990_mrp,mae_bert990_mrp),
  c("Actual UI Claims",NA,mu_ui,sd_ui,snr_ui,NA) # ,
))
colnames(table_pre) = c('Sample','Variable','Cor','mu','sigma','SNR','MAE')
rmses_pre = data.frame(Sample='Pre-Covid (2019 Week 1 - 2020 Week 11)',rbind(
  c("Rule-Based (Unweighted)",rmse_regex_uw),
  c("JoblessBERT (Unweighted)",rmse_bert990_uw),
  c("Rule-Based (Post-Stratified)",rmse_regex_mrp),
  c("JoblessBERT (Post-Stratified)",rmse_bert990_mrp)
))
colnames(rmses_pre) = c('Sample','Variable','RMSE')

idx = (dt0$tm>=202012) & (dt0$tm<=202022)
cor_regex_uw = round(cor(dt0[idx,c('regex_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_uw = round(cor(dt0[idx,c('bert990_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_regex_mrp = round(cor(dt0[idx,c('regex_mrp')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_mrp = round(cor(dt0[idx,c('bert990_mrp')],dt0[idx,c('iclaimnsa')]),2)
mu_ui = round(mean(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
sd_ui = round(sd(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
snr_ui = round(mu_ui / sd_ui,2)
mu_regex_uw = round(mean(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
sd_regex_uw = round(sd(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
snr_regex_uw = round(mu_regex_uw / sd_regex_uw,2)
mu_bert990_uw = round(mean(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
sd_bert990_uw = round(sd(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
snr_bert990_uw = round(mu_bert990_uw / sd_bert990_uw,2)
mu_regex_mrp = round(mean(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
sd_regex_mrp = round(sd(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
snr_regex_mrp = round(mu_regex_mrp / sd_regex_mrp,2)
mu_bert990_mrp = round(mean(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
sd_bert990_mrp = round(sd(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
snr_bert990_mrp = round(mu_bert990_mrp / sd_bert990_mrp,2)
mae_regex_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]),na.rm=TRUE),2)
mae_bert990_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]),na.rm=TRUE),2)
mae_regex_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]),na.rm=TRUE),2)
mae_bert990_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]),na.rm=TRUE),2)
rmse_regex_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')])^2,na.rm=TRUE)),2)
rmse_bert990_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')])^2,na.rm=TRUE)),2)
rmse_regex_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')])^2,na.rm=TRUE)),2)
rmse_bert990_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')])^2,na.rm=TRUE)),2)
table_covid = data.frame(Sample='Covid (2020 Week 12 - 2021 Week 26)',rbind(
  c("Rule-Based (Unweighted)",cor_regex_uw,mu_regex_uw,sd_regex_uw,snr_regex_uw,mae_regex_uw),
  c("JoblessBERT (Unweighted)",cor_bert990_uw,mu_bert990_uw,sd_bert990_uw,snr_bert990_uw,mae_bert990_uw),
  c("Rule-Based (Post-Stratified)",cor_regex_mrp,mu_regex_mrp,sd_regex_mrp,snr_regex_mrp,mae_regex_mrp),
  c("JoblessBERT (Post-Stratified)",cor_bert990_mrp,mu_bert990_mrp,sd_bert990_mrp,snr_bert990_mrp,mae_bert990_mrp),
  c("Actual UI Claims",NA,mu_ui,sd_ui,snr_ui,NA) # ,
))
colnames(table_covid) = c('Sample','Variable','Cor','mu','sigma','SNR','MAE')
rmses_covid = data.frame(Sample='Covid (2020 Week 12 - 2021 Week 26)',rbind(
  c("Rule-Based (Unweighted)",rmse_regex_uw),
  c("JoblessBERT (Unweighted)",rmse_bert990_uw),
  c("Rule-Based (Post-Stratified)",rmse_regex_mrp),
  c("JoblessBERT (Post-Stratified)",rmse_bert990_mrp)
))
colnames(rmses_covid) = c('Sample','Variable','RMSE')
pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019

idx = (dt0$tm>=202023) & (dt0$tm<=202252)
cor_regex_uw = round(cor(dt0[idx,c('regex_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_uw = round(cor(dt0[idx,c('bert990_uw')],dt0[idx,c('iclaimnsa')]),2)
cor_regex_mrp = round(cor(dt0[idx,c('regex_mrp')],dt0[idx,c('iclaimnsa')]),2)
cor_bert990_mrp = round(cor(dt0[idx,c('bert990_mrp')],dt0[idx,c('iclaimnsa')]),2)
mu_ui = round(mean(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
sd_ui = round(sd(dt0[idx,c('iclaimnsa')],na.rm=TRUE),2)
snr_ui = round(mu_ui / sd_ui,2)
mu_regex_uw = round(mean(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
sd_regex_uw = round(sd(dt0[idx,c('regex_uw')],na.rm=TRUE),2)
snr_regex_uw = round(mu_regex_uw / sd_regex_uw,2)
mu_bert990_uw = round(mean(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
sd_bert990_uw = round(sd(dt0[idx,c('bert990_uw')],na.rm=TRUE),2)
snr_bert990_uw = round(mu_bert990_uw / sd_bert990_uw,2)
mu_regex_mrp = round(mean(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
sd_regex_mrp = round(sd(dt0[idx,c('regex_mrp')],na.rm=TRUE),2)
snr_regex_mrp = round(mu_regex_mrp / sd_regex_mrp,2)
mu_bert990_mrp = round(mean(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
sd_bert990_mrp = round(sd(dt0[idx,c('bert990_mrp')],na.rm=TRUE),2)
snr_bert990_mrp = round(mu_bert990_mrp / sd_bert990_mrp,2)
mae_regex_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]),na.rm=TRUE),2)
mae_bert990_uw = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]),na.rm=TRUE),2)
mae_regex_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]),na.rm=TRUE),2)
mae_bert990_mrp = round(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]),na.rm=TRUE),2)
rmse_regex_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')])^2,na.rm=TRUE)),2)
rmse_bert990_uw = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')])^2,na.rm=TRUE)),2)
rmse_regex_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')])^2,na.rm=TRUE)),2)
rmse_bert990_mrp = round(sqrt(mean(abs(dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')])^2,na.rm=TRUE)),2)
table_post = data.frame(Sample='Post-Covid (2021 Week 27 - 2022 Week 52)',rbind(
  c("Rule-Based (Unweighted)",cor_regex_uw,mu_regex_uw,sd_regex_uw,snr_regex_uw,mae_regex_uw),
  c("JoblessBERT (Unweighted)",cor_bert990_uw,mu_bert990_uw,sd_bert990_uw,snr_bert990_uw,mae_bert990_uw),
  c("Rule-Based (Post-Stratified)",cor_regex_mrp,mu_regex_mrp,sd_regex_mrp,snr_regex_mrp,mae_regex_mrp),
  c("JoblessBERT (Post-Stratified)",cor_bert990_mrp,mu_bert990_mrp,sd_bert990_mrp,snr_bert990_mrp,mae_bert990_mrp),
  c("Actual UI Claims",NA,mu_ui,sd_ui,snr_ui,NA) # ,
))
colnames(table_post) = c('Sample','Variable','Cor','mu','sigma','SNR','MAE')
rmses_post = data.frame(Sample='Post-Covid (2021 Week 27 - 2022 Week 52)',rbind(
  c("Rule-Based (Unweighted)",rmse_regex_uw),
  c("JoblessBERT (Unweighted)",rmse_bert990_uw),
  c("Rule-Based (Post-Stratified)",rmse_regex_mrp),
  c("JoblessBERT (Post-Stratified)",rmse_bert990_mrp)
))
colnames(rmses_post) = c('Sample','Variable','RMSE')
rmses = rbind(rmses_full,rmses_pre,rmses_covid,rmses_post)
print(rmses)

idx = (dt0$tm>=202001) & (dt0$tm<=202252)
err_regex_uw = dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]
err_bert990_uw = dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]
err_regex_mrp = dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]
err_bert990_mrp = dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]
dm.test(err_regex_uw,err_bert990_uw,alternative='two.sided',h=1,power=1)
dm.test(err_regex_uw,err_bert990_mrp,alternative='two.sided',h=1,power=1)
dm.test(err_regex_mrp,err_bert990_mrp,alternative='two.sided',h=1,power=1)
dm.test(err_bert990_uw,err_bert990_mrp,alternative='two.sided',h=1,power=1)
print(length(err_regex_uw)-1)

idx = (dt0$tm>=202015) & (dt0$tm<=202026)
err_regex_uw = dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]
err_bert990_uw = dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]
err_regex_mrp = dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]
err_bert990_mrp = dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]
dm.test(err_regex_uw,err_bert990_uw,alternative='two.sided',h=1,power=1)
dm.test(err_regex_mrp,err_bert990_mrp,alternative='two.sided',h=1,power=1)
sqrt(mean((err_regex_mrp)^2,na.rm=TRUE))
sqrt(mean((err_bert990_mrp)^2,na.rm=TRUE))
sqrt(mean((err_regex_mrp)^2,na.rm=TRUE))/sqrt(mean((err_bert990_mrp)^2,na.rm=TRUE))-1
print(length(err_regex_uw)-1)
print(
  mean(dt0[idx,c('iclaimnsa_raw')],na.rm=TRUE) 
  * (mean(dt0[idx,c('regex_mrp')],na.rm=TRUE) 
     / mean(dt0[idx,c('bert990_mrp')],na.rm=TRUE))*pop)

idx = (dt0$tm>202026) & (dt0$tm<=202252)
err_regex_uw = dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_uw')]
err_bert990_uw = dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_uw')]
err_regex_mrp = dt0[idx,c('iclaimnsa')]-dt0[idx,c('regex_mrp')]
err_bert990_mrp = dt0[idx,c('iclaimnsa')]-dt0[idx,c('bert990_mrp')]
dm.test(err_regex_uw,err_bert990_uw,alternative='two.sided',h=1,power=1)
dm.test(err_regex_mrp,err_bert990_mrp,alternative='two.sided',h=1,power=1)
sqrt(mean((err_regex_mrp)^2,na.rm=TRUE))
sqrt(mean((err_bert990_mrp)^2,na.rm=TRUE))
sqrt(mean((err_regex_mrp)^2,na.rm=TRUE))/sqrt(mean((err_bert990_mrp)^2,na.rm=TRUE))-1
print(length(err_regex_uw)-1)
print(
  mean(dt0[idx,c('iclaimnsa_raw')],na.rm=TRUE) 
  * (mean(dt0[idx,c('regex_mrp')],na.rm=TRUE) 
     / mean(dt0[idx,c('bert990_mrp')],na.rm=TRUE))*pop
)

print(dt0[dt0$tm==202013,'iclaimnsa']/dt0[dt0$tm==202011,'iclaimnsa'])
print(dt0[dt0$tm==202013,'iclaimnsa'])

##########################
# Plot Time Series

dt0[dt0$tm==202252,] = dt0[dt0$tm==202251,]
dt0$time = dt0$time - 52
dt0_melt = data.table::melt(data.table(dt0[dt0$tm>=202001,c(
  'tm','time',
  'iclaimnsa','regex_uw','bert990_uw','regex_mrp','bert990_mrp'
)]),id.vars=c('tm','time'))
idx = (dt0$tm>=201901) & (dt0$tm<=202252)
dt0_melt[['variable2']] = factor(
  as.character(dt0_melt[['variable']]),
  levels=c('iclaimnsa','regex_mrp','bert990_mrp','regex_uw','bert990_uw'),
  labels=c(
    'Actual UI claims',
    paste0('Rule-Based (Post-Stratified)'),
    paste0('JoblessBERT (Post-Stratified)'),
    paste0('Rule-Based (Unweighted)'),
    paste0('JoblessBERT (Unweighted)') # ,
  ))
dt0_melt$tm = paste0(
  floor(dt0_melt$tm/100),'W',dt0_melt$tm-floor(dt0_melt$tm/100)*100)
dt0_melt = dt0_melt[,c('tm','time','variable2','value')]
plot = ggplot(data = dt0_melt,aes(x=time,y=value,color=variable2)) + 
  geom_line(aes(linetype=variable2),linewidth=0.8) +
  labs(y = "Index (2020M1 = 1) Log Scale", x = "", title = '') +
  scale_y_continuous(trans='log10',breaks=c(0.2,0.5,1,2,5,10,20)) + 
  scale_x_continuous(breaks=c((0:5)*26+1,6*26-2),labels=c(
    '01/2020','07/2020','01/2021','07/2021','01/2022','07/2022','01/2023'
  ),expand=c(0,0)) + 
  scale_linetype_manual(values=c(
    'solid','solid','solid','32','32'
  )) + 
  scale_color_manual(values=c(
    '#000000','#f98400','#3498db','#f1c40f','#3498db'
  )) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.x = element_text(size=13,angle=0,hjust=0.6,vjust=0.0,colour='black'),
    axis.text.y = element_text(size=13,colour='black'),
    axis.title = element_text(size=13,colour='black'),
    legend.text=element_text(size=13,colour='black'),
    legend.title=element_blank(),legend.key=element_blank(),
    legend.position=c(0.70,0.75),legend.direction="vertical",
    legend.key.width = unit(2,"line"),
    legend.margin=margin(0,0,10,0),
    legend.box.margin=margin(-15,-10,-10,-10),
    plot.margin = unit(c(-0.7,1.0,-0.3,0.7), "cm")
  )
print(plot)
ggsave(
  "figure2a_time_series.pdf",plot,
  width=24,height=10,units="cm",
  dpi=1200
)

