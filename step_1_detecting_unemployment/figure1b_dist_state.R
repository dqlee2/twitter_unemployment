#####################################################
# Distribution of unemployed users by state

demo = as.data.frame(fread('figure1b_dist_state.csv'))

uitw2 = demo[,c(
  'state','ui_dist','regex_dist','bert_dist'
)] %>% group_by(state) %>% summarise_all(sum,na.rm=TRUE)
colnames(uitw2) = c(
  'state',
  paste0('Actual'),
  paste0('Rule-Based (Cor = ',round(cor(demo[['ui_dist']],demo[['regex_dist']]),4),')'),
  paste0('JoblessBERT (Cor = ',round(cor(demo[['ui_dist']],demo[['bert_dist']]),4),')')
)
uitw2 = data.table::melt(data.table(uitw2),id.vars=c('state'))
states = unique(uitw2$state)

uitw2 = demo[,c('state','regex_kl','bert_kl')]
colnames(uitw2) = c(
  'State',
  paste0('Rule-Based (Cor = ',round(cor(demo[['ui_dist']],demo[['regex_dist']]),4),')'),
  paste0('JoblessBERT (Cor = ',round(cor(demo[['ui_dist']],demo[['bert_dist']]),4),')')
)
uitw2 = data.table::melt(data.table(uitw2),id.vars=c('State'))

demo_melt = data.table::melt(
  data.table(demo[,c('state','ui_dist','regex_dist','bert_dist')]),
  id.vars=c('state'))
demo_melt$value = demo_melt$value * 100
demo_melt_ui = demo_melt[demo_melt$variable=='ui_dist',c('state','value')]
colnames(demo_melt_ui) = c('state','value_ui')
demo_melt = merge(
  demo_melt[demo_melt$variable!='ui_dist',],
  demo_melt_ui,
  by=c('state'),all.x=TRUE,all.y=FALSE
)

demo_melt$variable2 = ""
demo_melt[demo_melt$variable=='regex_dist','variable2'] = paste0('Rule-Based')
demo_melt[demo_melt$variable=='bert_dist','variable2'] = paste0('JoblessBERT')
demo_melt = demo_melt[,c('state','variable2','value','value_ui')]
demo_melt$variable2 = factor(demo_melt$variable2,levels=c(
  paste0('Rule-Based'),
  paste0('JoblessBERT')
))

plot = ggplot(
  data = demo_melt, 
  aes(x = value, y = value_ui, color = variable2)) +
  geom_abline(intercept=0,slope=1,color='grey',linetype='dashed',size=1.0) + 
  geom_point(aes(shape=variable2),size=3) + 
  geom_smooth(method='lm',se=FALSE,fullrange=TRUE,size=1.0) + 
  geom_text_repel(
    aes(label=state), vjust=-0.9, size=3.5, max.overlaps = 10, 
    force = 10.0, force_pull = 0.01, label.padding = 1.0, 
    max.time = 10.0, max.iter = 10^8, verbose = TRUE, alpha = 0.6) + 
  labs(
    x = "Share of Unemployed Users (% Log Scale)", 
    y = "Share of Actual Unemployment (% Log Scale)", 
    title = '') + 
  scale_color_manual(values=c('#f98400','#3498db')) + 
  scale_x_log10(labels = ~ format(.x, scientific = FALSE), limits = c(0.07,25)) +
  scale_y_log10(labels = ~ format(.x, scientific = FALSE), limits = c(0.07,25)) +
  annotation_logticks() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.x = element_text(size=16,angle=0),
    axis.text.y = element_text(size=16),
    axis.title = element_text(size=16),
    legend.position=c(0.80,0.10),
    legend.direction="vertical",
    legend.text=element_text(size=16),
    legend.title=element_blank(),
    legend.key=element_blank())
print(plot)
ggsave(
  "figure1b_dist_state.pdf",plot,
  width=15,height=15,units="cm",
  dpi=1200
)

# Compare linear fit of Rule-Based vs JoblessBERT
fit_rule = lm(
  'value_ui ~ value',
  data=demo_melt[grepl('Rule-Based',demo_melt$variable2),])
fit_bert = lm(
  'value_ui ~ value',
  data=demo_melt[grepl('JoblessBERT',demo_melt$variable2),])
coefnum = 1
val = 1
library(lmtest)
library(sandwich)
ttest <- function(reg, coefnum, val){
  co = coeftest(reg,vcov = vcovHC(reg, type="HC3"))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE)
}

# Test whether regression slope different from one
fit_rule
ttest(fit_rule,2,1)
fit_bert
ttest(fit_bert,2,1)

