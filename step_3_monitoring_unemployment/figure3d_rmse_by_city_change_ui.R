##########################################################################
# RMSE by city sorted by UI changes

dt2 = as.data.frame(fread('figure3d_rmse_by_city_change_ui.csv'))
dt2$variable2 = factor(dt2$variable2,levels=c(
  'Rule-Based (Trained)','Rule-Based (Holdout)',
  'JoblessBERT (Trained)','JoblessBERT (Holdout)'
))

plot = ggplot(
  data = dt2[
    dt2$variable2 %in% c('JoblessBERT (Trained)','JoblessBERT (Holdout)'),], 
  aes(x = avg_change_ui, y = rel_to_AR, color = variable2)) +
  geom_point(aes(shape=variable2,fill=variable2),size=3,stroke=1.0) +
  geom_errorbar(
    aes(ymin=rel_to_AR-2.54*SE,ymax=rel_to_AR+2.54*SE,color=variable2), 
    position = position_dodge(0.0), linetype=1, width = 0.005, alpha = 0.60) +
  stat_smooth(
    data = subset(dt2,variable=='Conversational BERT (Weighted/)'), 
    aes(y=rel_to_AR),color='#3498db',se=FALSE,size=1,linetype='solid') + 
  geom_hline(yintercept=100,linetype="dashed",color='grey') +
  labs(
    y = "RMSE (% of STDEV of UI Claims)", 
    x = "Average change in UI Claims (% of Labor Force)", 
    title = '') +
  scale_x_continuous(limits=c(0,8),breaks=0:8) + 
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
    legend.direction="vertical",
    legend.text=element_text(size=13,colour='black'),
    legend.title=element_blank(),
    legend.key=element_blank() # ,
  ) + 
  guides(shape = guide_legend(nrow = 2,byrow=FALSE))
print(plot)
ggsave(
  "figure3d_rmse_by_city_change_ui.pdf", # plot,
  width=18,height=12,units="cm",
  dpi=1200
)

# print(dt2[dt2$variable2=='Conversational BERT (Trained)' & dt2$rel_to_AR > 90,])
# print(dt2[dt2$variable2=='Conversational BERT (Trained)' & dt2$rel_to_AR < 64,])

