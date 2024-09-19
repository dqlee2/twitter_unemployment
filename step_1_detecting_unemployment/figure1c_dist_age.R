#####################################################
# Distribution of unemployed users by age

uitw = as.data.frame(fread('figure1c_dist_age.csv'))

uitw2 = uitw[,c(
  'age','ui_dist','regex_dist','bert_dist'
)] %>% group_by(age) %>% summarise_all(sum,na.rm=TRUE)
colnames(uitw2) = c('Age','Actual','Rule-Based','JoblessBERT')
uitw2 = data.table::melt(data.table(uitw2),id.vars=c('Age'))
uitw2_sd = uitw[,c(
  'age','ui_sd_dist','regex_sd_dist','bert_sd_dist'
)] %>% group_by(age) %>% summarise_all(sum,na.rm=TRUE)
colnames(uitw2_sd) = c('Age','Actual','Rule-Based','JoblessBERT')
uitw2_sd = data.table::melt(data.table(uitw2_sd),id.vars=c('Age'))
colnames(uitw2_sd) = c('Age','variable','SE')
uitw2 = merge(uitw2,uitw2_sd,by=c('Age','variable'))
ages = c('Below 20','20 to 29','30 to 39','40 and Above')
uitw2 = uitw2[,c('Age','variable','value','SE')]
uitw0 = uitw2
regex_kl_label = paste0('Rule-Based')
bert_kl_label = paste0('JoblessBERT')
uitw2$variable = as.character(uitw2$variable)
uitw2[uitw2$variable=='Rule-Based','variable'] = regex_kl_label
uitw2[uitw2$variable=='JoblessBERT','variable'] = bert_kl_label
uitw2$Age = factor(uitw2$Age,levels=c('lt20','2030','3040','gt40'))
uitw2$variable = factor(
  uitw2$variable,
  levels=c('Actual',regex_kl_label,bert_kl_label))
uitw2$value = uitw2$value * 100
uitw2$SE = uitw2$SE * 100

plot = ggplot(data = uitw2, aes(x = Age, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) +
  geom_errorbar(
    aes(ymin=value-1.96*SE,ymax=value+1.96*SE,color=variable), 
    position = position_dodge(0.95), width = 0.2, alpha = 1.00, linetype=1) + 
  labs(x = "Age Group", y = "Share Across Categories (%)", title = '') +
  scale_color_manual(values=c('black','#f98400','#3498db')) + 
  scale_fill_manual(values=c('black','#f98400','#3498db')) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.45)*100) + 
  scale_x_discrete(labels = ages) + 
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
  "figure1c_dist_age.pdf",plot,
  width=15,height=15,units="cm",
  dpi=1200
)

# Test cited in main text 
# JoblessBERT closer to distribution of unemployment across age brackets in general population

mu_actual = uitw2[uitw2$Age=='lt20' & uitw2$variable=='Actual','value']/100
se_actual = uitw2[uitw2$Age=='lt20' & uitw2$variable=='Actual','SE']/100
mu_rule = uitw2[uitw2$Age=='lt20' & uitw2$variable=='Rule-Based','value']/100
se_rule = uitw2[uitw2$Age=='lt20' & uitw2$variable=='Rule-Based','SE']/100
mu_bert = uitw2[uitw2$Age=='lt20' & uitw2$variable=='JoblessBERT','value']/100
se_bert = uitw2[uitw2$Age=='lt20' & uitw2$variable=='JoblessBERT','SE']/100
pnorm(as.numeric((mu_rule-mu_actual)/((se_rule+se_actual))),lower.tail=FALSE)
pnorm(as.numeric((mu_bert-mu_actual)/((se_bert+se_actual))),lower.tail=FALSE)

mu_actual = uitw2[uitw2$Age=='2030' & uitw2$variable=='Actual','value']/100
se_actual = uitw2[uitw2$Age=='2030' & uitw2$variable=='Actual','SE']/100
mu_rule = uitw2[uitw2$Age=='2030' & uitw2$variable=='Rule-Based','value']/100
se_rule = uitw2[uitw2$Age=='2030' & uitw2$variable=='Rule-Based','SE']/100
mu_bert = uitw2[uitw2$Age=='2030' & uitw2$variable=='JoblessBERT','value']/100
se_bert = uitw2[uitw2$Age=='2030' & uitw2$variable=='JoblessBERT','SE']/100
pnorm(as.numeric((mu_rule-mu_actual)/((se_rule+se_actual))),lower.tail=TRUE)
pnorm(as.numeric((mu_bert-mu_actual)/((se_bert+se_actual))),lower.tail=TRUE)

mu_actual = uitw2[uitw2$Age=='3040' & uitw2$variable=='Actual','value']/100
se_actual = uitw2[uitw2$Age=='3040' & uitw2$variable=='Actual','SE']/100
mu_rule = uitw2[uitw2$Age=='3040' & uitw2$variable=='Rule-Based','value']/100
se_rule = uitw2[uitw2$Age=='3040' & uitw2$variable=='Rule-Based','SE']/100
mu_bert = uitw2[uitw2$Age=='3040' & uitw2$variable=='JoblessBERT','value']/100
se_bert = uitw2[uitw2$Age=='3040' & uitw2$variable=='JoblessBERT','SE']/100
pnorm(as.numeric((mu_rule-mu_actual)/((se_rule+se_actual))),lower.tail=TRUE)
pnorm(as.numeric((mu_bert-mu_actual)/((se_bert+se_actual))),lower.tail=TRUE)

mu_actual = uitw2[uitw2$Age=='gt40' & uitw2$variable=='Actual','value']/100
se_actual = uitw2[uitw2$Age=='gt40' & uitw2$variable=='Actual','SE']/100
mu_rule = uitw2[uitw2$Age=='gt40' & uitw2$variable=='Rule-Based','value']/100
se_rule = uitw2[uitw2$Age=='gt40' & uitw2$variable=='Rule-Based','SE']/100
mu_bert = uitw2[uitw2$Age=='gt40' & uitw2$variable=='JoblessBERT','value']/100
se_bert = uitw2[uitw2$Age=='gt40' & uitw2$variable=='JoblessBERT','SE']/100
pnorm(as.numeric((mu_rule-mu_actual)/((se_rule+se_actual))),lower.tail=FALSE)
pnorm(as.numeric((mu_bert-mu_actual)/((se_bert+se_actual))),lower.tail=FALSE)
