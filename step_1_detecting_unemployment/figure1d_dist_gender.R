#####################################################
# Distribution of unemployed users by gender

uitw = as.data.frame(fread('figure1d_dist_gender.csv'))

uitw2 = uitw[,c(
  'gender','ui_dist','regex_dist','bert_dist'
)] %>% group_by(gender) %>% summarise_all(sum,na.rm=TRUE)
colnames(uitw2) = c('Gender','Actual','Rule-Based','JoblessBERT')
uitw2 = data.table::melt(data.table(uitw2),id.vars=c('Gender'))
uitw2_sd = uitw[,c(
  'gender','ui_sd_dist','regex_sd_dist','bert_sd_dist'
)] %>% group_by(gender) %>% summarise_all(sum,na.rm=TRUE)
colnames(uitw2_sd) = c('Gender','Actual','Rule-Based','JoblessBERT')
uitw2_sd = data.table::melt(data.table(uitw2_sd),id.vars=c('Gender'))
colnames(uitw2_sd) = c('Gender','variable','SE')
uitw2 = merge(uitw2,uitw2_sd,by=c('Gender','variable'))
genders = c('Female','Male')
uitw2 = uitw2[,c('Gender','variable','value','SE')]
uitw0 = uitw2
regex_kl_label = paste0('Rule-Based')
bert_kl_label = paste0('JoblessBERT')
uitw2$variable = as.character(uitw2$variable)
uitw2[uitw2$variable=='Rule-Based','variable'] = regex_kl_label
uitw2[uitw2$variable=='JoblessBERT','variable'] = bert_kl_label
uitw2$variable = factor(
  uitw2$variable,
  levels=c('Actual',regex_kl_label,bert_kl_label))
uitw2$value = uitw2$value * 100
uitw2$SE = uitw2$SE * 100

plot = ggplot(data = uitw2, aes(x = Gender, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
  geom_errorbar(
    aes(ymin=value-1.96*SE,ymax=value+1.96*SE,color=variable), 
    position = position_dodge(0.95), width = 0.2, alpha = 1.00, linetype=1) + 
  labs(x = "Gender", y = "Share Across Categories (%)", title = '') +
  scale_color_manual(values=c('black','#f98400','#3498db')) + 
  scale_fill_manual(values=c('black','#f98400','#3498db')) + 
  scale_x_discrete(labels = genders) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.65)*100) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text = element_text(size=16),
    axis.title = element_text(size=16),
    legend.position=c(0.23,0.93),
    legend.direction="vertical",
    legend.text=element_text(size=16),
    legend.title=element_blank())
print(plot)
ggsave(
  "figure1d_dist_gender.pdf",plot,
  width=15,height=15,units="cm",
  dpi=1200
)

# Prop. test cited in main text 

pop = 164704 #  Civilian Labor Force Level (CLF16OV)	Dec 2019
prop.test(
  x = pop*c(
    as.numeric(uitw0[uitw0$Gender=='female' & uitw0$variable=='Actual','value']),
    as.numeric(uitw0[uitw0$Gender=='female' & uitw0$variable=='Rule-Based','value'])),
  n = c(pop,pop),
  alternative='greater')$p.value
prop.test(
  x = pop*c(
    as.numeric(uitw0[uitw0$Gender=='female' & uitw0$variable=='Actual','value']),
    as.numeric(uitw0[uitw0$Gender=='female' & uitw0$variable=='JoblessBERT','value'])),
  n = c(pop,pop),
  alternative='greater')$p.value
