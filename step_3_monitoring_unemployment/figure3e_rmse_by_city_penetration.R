##########################################################################
# RMSE by city sorted by penetration rate

dt2 = as.data.frame(fread('figure3e_rmse_by_city_penetration.csv'))
dt2$variable2 = factor(dt2$variable2,levels=c(
  'Rule-Based (Trained)','Rule-Based (Holdout)',
  'JoblessBERT (Trained)','JoblessBERT (Holdout)'
))

plot = ggplot(
  data = dt2, aes(x = penetration, y = rel_to_AR, color = variable2)) +
  geom_point(aes(shape=variable2,fill=variable2),size=3,stroke=1.0) +
  geom_errorbar(
    aes(ymin=rel_to_AR-2.54*SE,ymax=rel_to_AR+2.54*SE,color=variable2), 
    position = position_dodge(0.0), linetype=1, width = 0.005, alpha = 0.60) +
  stat_smooth(
    data = subset(dt2,variable=='Conversational BERT (Weighted/)'), 
    aes(y=rel_to_AR),color='#3498db',se=FALSE,size=1,linetype='solid') + 
  geom_hline(yintercept=100,linetype="dashed",color='grey') +
  labs(y = "", x = "Penetration Rate (% of Population)", title = '') +
  scale_y_continuous(limits=c(55,105),breaks=seq(55,105,5)) + 
  scale_shape_manual(values=c(21,21)) +
  scale_fill_manual(values=c('#3498db','white')) +
  scale_color_manual(values=c('#3498db','#3498db')) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.y = element_text(size=15,angle=0,hjust=1.0,vjust=0.3,colour='black'),
    axis.text.x = element_text(size=15,colour='black'),
    axis.title = element_text(size=15,colour='black'),
    legend.position=c(0.80,0.75), # 
    # legend.position='bottom', # 
    legend.direction="vertical",
    legend.text=element_text(size=13,colour='black'),
    legend.title=element_blank(),
    legend.key=element_blank() # ,
  ) + 
  guides(shape = guide_legend(nrow = 2,byrow=FALSE))
print(plot)
ggsave(
  "figure3e_rmse_by_city_penetration.pdf", # plot,
  width=18,height=12,units="cm",
  dpi=1200
)
