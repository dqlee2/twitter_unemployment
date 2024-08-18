#####################################################
# Plot precision recall curve

d = as.data.frame(fread('figure1a_pr_eval.csv'))
d = d[order(d[['p']]),]
point_x = mean(d[d[['label']]==1,'is_unemployed_regex'],na.rm=TRUE)
point_y = mean(d[d[['is_unemployed_regex']]==1,'label'],na.rm=TRUE)
d[['variable']] = 'JoblessBERT'
d = d[,c('variable','r','p')]
d = rbind(d,data.frame(variable='Rule-Based',r=point_x,p=point_y))
d$variable = factor(d$variable,levels=c('Rule-Based','JoblessBERT'))

plot = ggplot(
  data = d, aes(x = r, y = p, color = variable)) +
  geom_line(
    data = subset(d,variable=='JoblessBERT'),
    size=1,show.legend=TRUE) +
  geom_point(
    data = subset(d,variable=='Rule-Based'),
    shape=4,size=4,stroke=1.5,show.legend=TRUE) +
  scale_color_manual(
    values=c('#f98400','#3498db'),
    breaks=c('Rule-Based', 'JoblessBERT')) + 
  scale_x_continuous(breaks=seq(0.0,1.0,0.2),limits=c(0.0,1)) +
  scale_y_continuous(breaks=seq(0.0,1.0,0.2),limits=c(0.0,1)) +
  labs(y = "Precision", x = "Recall", title = '') + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.x = element_text(size=16,angle=0),
    axis.text.y = element_text(size=16),
    axis.title = element_text(size=16),
    # legend.position='none',
    legend.position=c(0.80,0.10),
    legend.direction="vertical",
    legend.text=element_text(size=16),
    legend.title=element_blank(),
    legend.key=element_blank()
  ) + 
  guides(colour = guide_legend(
    override.aes = list(shape=c(4,NA),linetype=c(NA,1))))
print(plot)
ggsave(
  "figure1a_pr_eval.pdf",plot,
  width=15,height=15,units="cm",
  dpi=1200
  )

## Numbers cited in the main text

d = d[order(d[['p']]),]
p = d[d$variable=='JoblessBERT','p']
r = d[d$variable=='JoblessBERT','r']
f1 = 2 * r * p / (r + p)
pb = p[which.max(f1)]
rb = r[which.max(f1)]
pk = 0.942
rk = 0.238
print(c(pb,rb))
print((pk-pb)/pk)
print((rb-rk)/rk)

# This number should correspond to the classification threshold used to maximize the F1-score. 
d = as.data.frame(fread('figure1a_pr_eval.csv'))
d = d[order(d[['p']]),]
threshold = d[which.max(f1),'is_unemployed_iter13']
tw_regex = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_regex.csv'))
tw_regex = tw_regex[,c(
  'user_id','tweet_timestamp','is_unemployed','tweet_text'
  )]
colnames(tw_regex) = c(
  'user_id','tweet_timestamp','regex','tweet_text'
  )
tw_bert = fread(paste0('../preprocessing/data/restricted/unemployed_tweets_bert.csv'))
tw_bert = tw_bert[,c(
  'user_id','tweet_timestamp','is_unemployed','tweet_text'
  )]
colnames(tw_bert) = c(
  'user_id','tweet_timestamp','bert','tweet_text'
  )
tw = merge(
  tw_bert,tw_regex,
  by=c('user_id','tweet_timestamp','tweet_text'),all=TRUE)
tw[is.na(tw$regex),'regex'] = 0
tw[is.na(tw$bert),'bert'] = 0
tw$bert = (tw$bert>=threshold)
tw = tw[!(tw$bert==0 & tw$regex==0),]
tab = table(tw$bert,tw$regex)
print(sum(tw$bert)/nrow(tw))
print(sum(tw$regex)/nrow(tw))
tw$lost_job = grepl('lost.* job',tw$tweet_text)
tw$laid_off = grepl('laid.* off',tw$tweet_text)
tw$layoff = grepl('layoff',tw$tweet_text)
print((sum(tw$lost_job + tw$laid_off + tw$layoff))/nrow(tw))
tw$fire = grepl('fire',tw$tweet_text)
tw$just = grepl('just',tw$tweet_text)
tw$today = grepl('today',tw$tweet_text)
print((sum(
  tw$just + tw$today + tw$fire + tw$lost_job + tw$laid_off + tw$layoff
  ))/nrow(tw))

## Test comparing rule vs JoblessBERT
precision = point_y
recall = point_x
total_positives = sum(tw_regex$regex)
total_instances = 15*10^9

# Calculate TP and FN
TP = recall * total_positives
FN = total_positives - TP

# Calculate FP
FP = TP * (1/precision - 1)

# Calculate TN
total_negatives = total_instances - total_positives
TN = total_negatives - FP

# Contingency table
contingency_table_regex = matrix(
  c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
  dimnames = list(
    'Prediction' = c('Positive', 'Negative'),
    'Actual' = c('Positive', 'Negative')))
precision = pb
recall = rb
total_positives = sum(tw$bert)
total_instances = 15*10^9

# Calculate TP and FN
TP = recall * total_positives
FN = total_positives - TP

# Calculate FP
FP = TP * (1/precision - 1)

# Calculate TN
total_negatives = total_instances - total_positives
TN = total_negatives - FP

# Contingency table
contingency_table_bert = matrix(
  c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
  dimnames = list(
    'Prediction' = c('Positive', 'Negative'),
    'Actual' = c('Positive', 'Negative')))
chisq.test(contingency_table_bert,contingency_table_regex)



