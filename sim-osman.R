library(ggbeeswarm)
set.seed(1)
df = data.frame(x = 1, 
                y = c(rnorm(30, 0.95, 0.025), 
                      rnorm(5, 0.45, 0.03),
                      rnorm(30, 0.2, 0.05)))
ggplot(df, aes(x=x, y=y)) + geom_beeswarm()
df2 = data.frame(x1 = mean(df$y), x2 = df$y, y1 = 0, y2 = 1)
sf = 4
png("osman-sim.png", width=200*sf, height=300*sf, res=72*sf)
ggplot(df2, aes(x=x1,y=y1,xend=x2,yend=y2)) + 
  geom_segment(color="purple", alpha=0.5) +
  geom_point(aes(x=x2, y=y2), color="purple") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90), legend.position="none") + 
  labs(x="h", y = "Time (24 h period)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
dev.off()
