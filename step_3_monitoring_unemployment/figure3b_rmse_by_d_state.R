##############################################################
# RMSE by horizon d state level

dt = as.data.frame(fread('figure3b_rmse_by_d_state.csv'))
dt$variable = factor(
  as.character(dt$variable),
  levels=c('Autoregressive','Rule-Based','JoblessBERT'))

plot = ggplot(
  data = dt,aes(x=h,y=v,color=variable)) + 
  geom_hline(yintercept=100,linewidth=0.6,color='gray',linetype='dotted') +
  geom_line(aes(linetype=variable),linewidth=0.7) +
  geom_ribbon(
    aes(ymin=lb,ymax=ub,fill=variable,linetype=variable),
    alpha=0.1,linewidth=0.2) + 
  labs(
    y = "RMSE (% of STDEV of UI Claims)", 
    x = "Time After Measurement Week (d)", 
    title = '') +
  scale_y_continuous(expand=c(0,3),limits=c(30,110),breaks=seq(30,110,10)) +
  scale_x_continuous(breaks=-10:5,limits=c(-10,5),expand=c(0,0.1)) +
  scale_linetype_manual(values=c('dotted','solid','longdash')) + 
  scale_color_manual(values=c('darkolivegreen3','#f98400','#3498db')) +
  scale_fill_manual(values=c('darkolivegreen3','#f98400','#3498db')) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.x = element_text(size=17,angle=0,hjust=0.6,vjust=0.0,colour='black'),
    axis.text.y = element_text(size=17,colour='black'),
    axis.title = element_text(size=17,colour='black'),
    legend.text=element_text(size=17,colour='black'),
    legend.title=element_blank(),
    legend.key=element_blank(),
    legend.position=c(0.19,0.17),
    legend.direction="horizontal",
    legend.key.width = unit(1,"line")
  ) + 
  guides(linetype = guide_legend(nrow = 3,byrow=TRUE))
print(plot)
ggsave(
  "figure3b_rmse_by_d_state.pdf",plot,
  width=18,height=12,units="cm",
  dpi=1200
  )



