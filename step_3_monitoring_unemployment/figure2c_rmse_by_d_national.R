##############################################################
# RMSE by horizon d national level

dt = as.data.frame(fread('figure2c_rmse_by_d_national.csv'))

dt[dt$variable=='Conversational BERT','variable'] = 'JoblessBERT'
dt$variable = factor(
  as.character(dt$variable),
  levels=c('Consensus','Rule-Based','JoblessBERT'))

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
  scale_x_continuous(breaks=-10:5,limits=c(-10,5),expand=c(0,0.1)) +
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

## Numbers cited in main text

# consensus model's RMSE on day -10
print(dt[dt$variable=='Consensus' & dt$h<=-9,'value']/100)

# consensus model's RMSE on day -3
print(dt[dt$variable=='Consensus' & dt$h==-3,'value']/100)

# consensus model's RMSE on day -2
print(dt[dt$variable=='Consensus' & dt$h==-2,'value']/100)

# Rule-based RMSE relative to baseline RMSE on an average two-week period before data release of actual UI claims
print(1-mean(dt[dt$variable=='Rule-Based','value']/dt[dt$variable=='Consensus','value']))

# JoblessBERT RMSE relative to baseline RMSE on an average two-week period before data release of actual UI claims
print(1-mean(dt[dt$variable=='JoblessBERT','value']/dt[dt$variable=='Consensus','value']))

