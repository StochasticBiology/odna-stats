library(ggplot2)
library(ggpubr)

expt = "sample"
expt = "turnover"
expt = "moran"
expt = "general"

if(expt == "sample") {
  df = read.csv("output-sample.csv")
} else if(expt == "turnover") {
  df = read.csv("output-turnover.csv")
} else if(expt == "moran") {
  df = read.csv("output-moran.csv")
} else if(expt == "general") {
  df = read.csv("output-general.csv")
  colnames(df)[1] = "ds"
}
  
df$Vph = (df$SDh)**2/(df$Eh*(1-df$Eh))

ggplot(df, aes(x=t, y=Eh, color=factor(ds))) + geom_line() + facet_grid(h0 ~ nstar) + xlim(0,30)
ggplot(df[df$h0==0.5 & df$nstar == 40,], aes(x=t, y=En, color=factor(ds))) + geom_line() + facet_grid(h0 ~ nstar) + xlim(0,30)
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

g.ets = ggplot() +
  geom_line(data=df, aes(x=t, y=Eh, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=t, y=Eh, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) +
  theme_minimal()
g.ets
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

df.sub = df[df$ds %in% c(-0.25, 0, 0.25) & df$h0 %in% c(0.1, 0.5, 0.9),]
g.sd = ggplot() +
  geom_line(data=df.sub, aes(x=Eh, y=SDh, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df.sub[df.sub$t == max(df.sub$t),], size=3, aes(x=Eh, y=SDh, color=factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
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
png(paste0("set-illus-", expt, ".png", collapse=""), width=800*sf, height=800*sf, res=72*sf)
ggarrange(g.ets, g.vts, g.sd, g.vp1 + scale_y_log10(), nrow=2, ncol=2)
dev.off()

if(expt == "turnover") {
  dfh = read.csv("output-turnover-hist.csv")
  dfsub = dfh[dfh$nstar == 40 & dfh$ds %in% c(-0.25, 0, 0.25) & dfh$t %in% c(0, 9, 99, 999),]
  g.hist.1 = ggplot(dfsub, aes(xmin=h, ymin=log10(t+1), xmax=h+0.025, ymax=Ph/1000+log10(t+1), 
                  fill=factor(t+1))) + geom_rect() + theme_minimal() + facet_grid(ds ~ h0)  
  dfsub1 = dfh[dfh$nstar == 640 & dfh$ds %in% c(-0.25, 0, 0.25) & dfh$t %in% c(0, 9, 99, 999),]
  g.hist.2 = ggplot(dfsub1, aes(xmin=h, ymin=log10(t+1), xmax=h+0.025, ymax=Ph/1000+log10(t+1), 
                    fill=factor(t+1))) + geom_rect() + theme_minimal() + facet_grid(ds ~ h0)  
  sf = 2
  png(paste0("set-hist-", expt, ".png", collapse=""), width=800*sf, height=600*sf, res=72*sf)
  ggarrange(g.hist.1, g.hist.2, nrow=2)
  dev.off()
}

if(expt == "sample") {
  ngen = 100
  ndf = data.frame(neff = rep(c(40, 160, 640, 2560), each=ngen+1),
                   gen = rep(0:ngen, 4))
  ndf$simp = ndf$gen*(1/ndf$neff)
  # Wright formula (1942)
  ndf$wright = (1-(1-1/ndf$neff)**ndf$gen)
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright, color=factor(neff)))
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp*neff, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright*neff, color=factor(neff)))
  # if we can estimate an effective n for a short timescale, can we use this to generalise away from the approximation?
  dfsub = df[df$ds == 0 & df$h0 == 0.5 & df$nstar %in% c(40, 160, 640, 2560) & df$t < ngen,]
  ggplot() +
    geom_line(data=dfsub, aes(x = t, y = Vph, colour = factor(nstar))) + 
    geom_point(data=ndf, size = 0.2, aes(x=gen, y=wright, color=factor(neff)))
}

if(expt == "turnover") {
  ngen = 500
  ndf = data.frame(neff = rep(c(40, 160, 640, 2560)/(2*0.1), each=ngen+1),
                   gen = rep(0:ngen, 4))
  ndf$simp = ndf$gen*(1/ndf$neff)
  # Wright formula (1942)
  ndf$wright = (1-(1-1/ndf$neff)**ndf$gen)
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright, color=factor(neff)))
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp*neff, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright*neff, color=factor(neff)))
  # if we can estimate an effective n for a short timescale, can we use this to generalise away from the approximation?
  dfsub = df[df$ds == 0 & df$h0 == 0.1 & df$nstar %in% c(40, 160, 640, 2560) & df$t < ngen,]
  ggplot() +
    geom_line(data=dfsub, aes(x = t, y = Vph, colour = factor(nstar))) + 
    geom_point(data=ndf, size = 0.2, aes(x=gen, y=wright, color=factor(neff)))
  ## ^ yes we can! just consider effective N for one time unit, then stack results in Wright formula
}

if(expt == "moran") {
  ngen = 50
  ndf = data.frame(neff = rep(c(40, 160, 640, 2560)/2, each=ngen+1),
                   gen = rep(0:ngen, 4))
  ndf$simp = ndf$gen*(1/ndf$neff)
  # Wright formula (1942)
  ndf$wright = (1-(1-1/ndf$neff)**ndf$gen)
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright, color=factor(neff)))
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp*neff, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright*neff, color=factor(neff)))
  # if we can estimate an effective n for a short timescale, can we use this to generalise away from the approximation?
  dfsub = df[df$ds == 0 & df$h0 == 0.1 & df$nstar %in% c(40, 160, 640, 2560) & df$t < ngen,]
  ggplot() +
    geom_line(data=dfsub, aes(x = t, y = Vph, colour = factor(nstar))) + 
    geom_point(data=ndf, size = 0.2, aes(x=gen, y=wright, color=factor(neff)))
  ## ^ yes we can! just consider effective N for one time unit, then stack results in Wright formula
  # so Moran model normalised variance at generation t =(tau/n) is Wright(n/2, t) -- known result
}

if(expt == "general") {
  ngen = 100
  ndf = data.frame()
  for(expt in 0:4) {
  tmpdf = data.frame(neff = rep(c(40, 160, 640, 2560), each=ngen+1),
                   gen = rep(0:ngen, 4))
  tmpdf$simp = tmpdf$gen*(1/tmpdf$neff)
  tmpdf$nturn = tmpdf$neff/(2*0.1)
  # Wright formula (1942)
  
  ##### this is not correctly capturing the behaviour yet
  if(expt == 0) {
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen)
  } else if(expt == 1) {
    delta = 0.5* ( 1/(tmpdf$neff/2) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor(tmpdf$gen/10))
  } else if(expt == 2) {
    delta = 0.5* ( 1/(tmpdf$neff/5) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor(tmpdf$gen/10))
  } else if(expt == 3) {
    delta = 1* ( 1/(tmpdf$neff/2) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor(tmpdf$gen/10))
  } else if(expt == 4) {
    delta = 1* ( 1/(tmpdf$neff/5) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor(tmpdf$gen/10))
  }
  tmpdf$ds = expt
  ndf = rbind(ndf, tmpdf)
  }
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright, color=factor(neff)))
  ggplot(ndf) + 
    geom_line(aes(x=gen, y=simp*neff, color=factor(neff))) + 
    geom_point(aes(x=gen, y=wright*neff, color=factor(neff)))
  # if we can estimate an effective n for a short timescale, can we use this to generalise away from the approximation?
  dfsub = df[df$h0 == 0.5 & df$nstar %in% c(40, 160, 640, 2560) & df$t < ngen,]
  ggplot() +
    geom_line(data=ndf, aes(x=gen, y=wright, color=factor(neff))) + 
    geom_point(data=dfsub, size=0.2, aes(x = t, y = Vph, colour = factor(nstar))) + 
    facet_wrap( ~ ds) + xlim(0,40)
  ## ^ yes we can! just consider effective N for one time unit, then stack results in Wright formula
}
