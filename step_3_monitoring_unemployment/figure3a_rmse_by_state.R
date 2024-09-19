##############################################################
# RMSE by state

RMSE2_melt_wgt = as.data.frame(fread('figure3a_rmse_by_state.csv'))
RMSE2_melt_wgt$State = as.character(RMSE2_melt_wgt$nState)
nState = RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable=='JoblessBERT',c('State','rel_to_AR')
] %>% arrange(rel_to_AR)
nState[['nState']] = factor(1:nrow(nState),labels=nState$State)
nState[['rel_to_AR']] = NULL
RMSE2_melt_wgt[['nState']] = NULL
RMSE2_melt_wgt = merge(
  RMSE2_melt_wgt,data.frame(nState),
  by='State',all.x=TRUE,all.y=FALSE)
RMSE2_melt_wgt$variable2 = as.character(RMSE2_melt_wgt$variable2)
RMSE2_melt_wgt[
  RMSE2_melt_wgt$variable2=='AR',
  'variable2'] = 'Autoregressive'
RMSE2_melt_wgt[['variable2']] = factor(
  RMSE2_melt_wgt[['variable2']],
  levels=c('Autoregressive','Rule-Based','JoblessBERT'))

plot = ggplot(
  data = RMSE2_melt_wgt, 
  aes(y = rel_to_AR, x = nState, color = variable2)) +
  geom_point(aes(shape=variable2,fill=variable2),size=2.5,stroke=1.5) +
  geom_errorbar(
    aes(ymin=rel_to_AR-2.54*SE,ymax=rel_to_AR+2.54*SE,color=variable2), 
    position = position_dodge(0.2), linetype=1, width = 0.1, alpha = 0.60) +
  geom_hline(yintercept=100,linetype="dashed",color='gray25') +
  labs(y = "RMSE (% of STDEV of UI Claims)", x = "", title = '') +
  scale_y_continuous(limits=c(30,110),breaks=seq(30,110,10)) + 
  scale_shape_manual(values=c(25,22,21)) + 
  scale_fill_manual(values=c(
    'darkolivegreen3','#f98400','#3498db'
  )) +
  scale_color_manual(values=c(
    'darkolivegreen3','#f98400','#3498db'
  )) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "#000000"),
    axis.text.y = element_text(size=14,angle=0,hjust=1.0,vjust=0.3,colour='black'),
    axis.text.x = element_text(size=10,colour='black'),
    axis.title = element_text(size=14,colour='black'),
    legend.position='bottom',
    legend.direction="horizontal",
    legend.text=element_text(size=13,colour='black'),
    legend.title=element_blank(),
    legend.key=element_blank(),
    legend.margin=margin(0,0,10,0),
    legend.box.margin=margin(-10,-10,-10,-10),
    plot.margin = unit(c(0.0,0.0,0.0,0.2), "cm")
  ) +
  guides(shape = guide_legend(nrow = 1,byrow=TRUE))
print(plot)
ggsave(
  "figure3a_rmse_by_state.pdf",plot,
  width=30,height=10,units="cm",
  dpi=1200
  )



