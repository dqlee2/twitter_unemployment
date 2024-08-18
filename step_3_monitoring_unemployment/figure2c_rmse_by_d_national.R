##############################################################
# RMSE by horizon d national level

dt = as.data.frame(fread('figure2c_rmse_by_d_national.csv'))

dt[['ub']] = dt[['value']] + 2.54*dt[['SE0']]
dt[['lb']] = dt[['value']] - 2.54*dt[['SE0']]
dt[dt$variable=='Conversational BERT','variable'] = 'JoblessBERT'
dt$variable = factor(
  as.character(dt$variable),
  levels=c('Consensus','Rule-Based','JoblessBERT'))
dt[dt$h==5,'h'] = 4+8.5/24

plot = ggplot(
  data = dt,
  aes(x=h,y=value,color=variable)) + 
  geom_rect(
    aes(ymin=-Inf,ymax=Inf,xmin=-(12-5),xmax=-(5-5)),
    fill='lightgray',color=NA,alpha=0.02) + 
  geom_line(
    aes(linetype=variable),
    linewidth=0.7) +
  geom_ribbon(
    aes(ymin=lb,ymax=ub,fill=variable,linetype=variable),
    alpha=0.1,linewidth=0.2) + 
  labs(
    y = "RMSE (% of STDEV of UI Claims)", 
    x = "Time After Measurement Week", 
    title = '') +
  scale_y_continuous(expand=c(0,0),limits=c(0,75),breaks=seq(0,70,10)) +
  scale_x_continuous(breaks=-9:5,expand=c(0,0.1)) +
  scale_linetype_manual(values=c('dotted','longdash','solid')) + 
  scale_color_manual(values=c(
    '#9b59b6','#f98400','#3498db'
  )) +
  scale_fill_manual(values=c(
    '#9b59b6','#f98400','#3498db'
  )) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.x = element_text(size=13,angle=0,hjust=0.6,vjust=0.0,colour='black'),
    axis.text.y = element_text(size=13,colour='black'),
    axis.title = element_text(size=13,colour='black'),
    legend.text=element_text(size=13,colour='black'),
    legend.title=element_blank(),
    legend.key=element_blank(),
    legend.position='bottom',
    legend.direction="horizontal",
    legend.key.width = unit(2,"line") # ,
  ) + 
  guides(linetype = guide_legend(nrow=1,byrow=TRUE))
print(plot)
ggsave(
  "figure2c_rmse_by_d_national.pdf",plot,
  width=18,height=12,units="cm",
  dpi=1200
)

