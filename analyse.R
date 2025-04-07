library(ggplot2)
library(ggpubr)

expt = "sample"

if(expt == "sample") {
  df = read.csv("output-sample.csv")
} else {
  df = read.csv("output-turnover.csv")
}
df$Vph = (df$SDh)**2/(df$Eh*(1-df$Eh))

ggplot(df, aes(x=t, y=Eh, color=factor(ds))) + geom_line() + facet_grid(h0 ~ nstar)
ggplot(df, aes(x=t, y=SDh, color=factor(ds))) + geom_line() + facet_grid(h0 ~ nstar)
ggplot(df, aes(x=t, y=Vph, color=factor(ds))) + geom_line() + facet_grid(h0 ~ nstar)
ggplot(df, aes(x=t, y=Vph*nstar, color=factor(ds))) + geom_line() + facet_grid(h0 ~ nstar)

df[df$h0 == 0.5 & df$ds == 0.25 & df$nstar == 160 & df$t == 999,]
# d Vph / dt ~ 1/N
# Vph ~ t / N
ggarrange(
  ggplot(df, aes(x=t, y=Vph, color=factor(ds), group=paste(nstar,h0))) + 
    geom_line() + facet_grid(~ ds) + xlim(0,10),
  ggplot(df, aes(x=t, y=Vph*nstar, color=factor(ds), group=paste(nstar,h0))) + 
    geom_line() + facet_grid(~ ds) + xlim(0,10) ,
  nrow=2
)

ggplot(df, aes(x=t, y=Vph, color=factor(ds), group=paste(nstar,h0))) + 
  geom_line() + facet_grid(nstar~ ds) + xlim(0,10)

g.v0ts = ggplot() +
  geom_line(data=df, aes(x=t, y=Vph, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=t, y=Vph, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) +
  theme_minimal()
g.v0ts

g.vts = ggplot() +
  geom_line(data=df, aes(x=t, y=Vph*nstar, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=t, y=Vph*nstar, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) +
  theme_minimal()
g.vts

ggplot(df, aes(x=Eh, y=Vph)) + 
  #geom_point(shape=21, color="#FFFFFF00", aes(fill = factor(nstar))) +
  geom_line(linewidth=0.5, aes(color=factor(ds), group=paste(nstar,h0))) 


ggplot() +
  geom_line(data=df, aes(x=Eh, y=Vph, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df[df$t == max(df$t),], aes(x=Eh, y=Vph, shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal()

g.sd = ggplot() +
  geom_line(data=df, aes(x=Eh, y=SDh, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=Eh, y=SDh, color=factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal()
print(g.sd)

g.vp = ggplot() +
  geom_line(data=df, aes(x=Eh, y=Vph, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=Eh, y=Vph, color=factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal()
print(g.vp)

g.vp1 = ggplot() +
  geom_line(data=df, aes(x=Eh, y=Vph*nstar, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=Eh, y=Vph*nstar, color = factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal() + ylim(0,100)
print(g.vp1)
g.vp1 + scale_y_log10()

sf = 2
png(paste0("set-illus-", expt", .png", collapse=""), width=600*sf, height=600*sf, res=72*sf)
ggarrange(g.sd,
          ggarrange(g.vp, g.vp1, nrow=1), nrow=2)
dev.off()

g.ts = ggplot(df, aes(x = t, y=Eh))
ggplot(df, aes(x=Eh, y=Vph/(t/nstar), color=factor(ds), fill=factor(nstar), group=h0)) +
  geom_point(shape=21)
