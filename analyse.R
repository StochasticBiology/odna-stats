library(ggplot2)
library(ggpubr)

#expt = "sample"
expt = "turnover"
#expt = "moran"
#expt = "general"

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
  geom_line(data=df[df$nstar < 2000 & !is.na(df$Vph),], aes(x=t, y=Vph, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$nstar < 2000 & !is.na(df$Vph) & df$t %in% c(150,max(df$t)),], size=3, aes(x=t, y=Vph, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) + labs(x="t", y="Var'(h)",  shape="N") +guides(colour = "none") +
  theme_minimal()
g.v0ts

g.ets = ggplot() +
  geom_line(data=df[df$nstar < 2000 & !is.na(df$Vph),], aes(x=t, y=Eh, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$nstar < 2000 & !is.na(df$Vph) & df$t %in% c(150,max(df$t)),], size=3, aes(x=t, y=Eh, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) + labs(x="t", y="E(h)",  shape="N") +guides(colour = "none") +
  theme_minimal()
g.ets

g.ets.sub = ggplot() +
  geom_line(data=df[df$nstar < 2000 & !is.na(df$Vph) & df$ds %in% c(-0.125, 0),], aes(x=t, y=Eh, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$nstar < 2000 & !is.na(df$Vph) & df$ds %in% c(-0.125, 0) & df$t %in% c(150,max(df$t)),], size=3, aes(x=t, y=Eh, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) + labs(x="t", y="E(h)",  shape="N") +guides(colour = "none") +
  theme_minimal()
g.ets.sub

g.v0ts.sub = ggplot() +
  geom_line(data=df[df$nstar < 2000 & !is.na(df$Vph) & df$ds %in% c(-0.125, 0),], aes(x=t, y=Vph, color=factor(ds), group=paste(nstar, h0))) +
  geom_point(data=df[df$nstar < 2000 & !is.na(df$Vph) & df$ds %in% c(-0.125, 0) & df$t %in% c(150,max(df$t)),], size=3, aes(x=t, y=Vph, color=factor(ds), shape=factor(nstar), group=paste(nstar,h0))) +
  facet_wrap(~ ds) + labs(x="t", y="Var'(h)",  shape="N") +guides(colour = "none") +
  theme_minimal()
g.v0ts.sub

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

df.sub = df[df$ds %in% c(-0.25, 0, 0.25) & df$h0 %in% c(0.1, 0.5, 0.9) & df$nstar < 2000,]
g.sd = ggplot() +
  geom_line(data=df.sub, aes(x=Eh, y=SDh, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df.sub[df.sub$t == max(df.sub$t),], size=3, aes(x=Eh, y=SDh, color=factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal()
print(g.sd)

g.v0 = ggplot() +
  geom_line(data=df.sub[df.sub$ds != 0,], aes(x=Eh, y=SDh**2, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_line(data=df.sub[df.sub$ds == 0,], alpha=0.1, aes(x=Eh, y=SDh**2, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df.sub[df.sub$t %in% c(10,999),], size=3, aes(x=Eh, y=SDh**2, color=factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 0.25), limits=c(1e-5,0.26)) +
  theme_minimal() + labs(x = "E(h)", y = "Var(h)", color = "ds", shape = "N") +
  scale_colour_manual(
    values = c("#FF8888FF",  "#8888FFFF", "#AAAAAAFF"))

print(g.v0)

g.vp = ggplot() +
  geom_line(data=df, aes(x=Eh, y=Vph, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df[df$t == max(df$t),], size=3, aes(x=Eh, y=Vph, color=factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal()
print(g.vp)

g.vp1 = ggplot() +
  geom_line(data=df[df$nstar<2000,], alpha=0.5,aes(x=Eh, y=Vph*nstar, color=factor(ds), group=paste(nstar, ds, h0))) +
  geom_point(data=df[df$nstar<2000 & df$t %in% c(999),], size=3, aes(x=Eh, y=Vph*nstar, color = factor(ds), shape=factor(nstar), group=paste(ds,h0))) +
  theme_minimal() + ylim(0,100)
g.vp1p = g.vp1 + scale_y_log10() + labs(x = "E(h)", y = "N × Var'(h)", color = "ds", shape = "N")

sf = 2
png(paste0("set-illus-", expt, ".png", collapse=""), width=800*sf, height=800*sf, res=72*sf)
ggarrange(g.ets, g.vts, g.sd, g.vp1 + scale_y_log10(), nrow=2, ncol=2)
dev.off()

sf = 2
png(paste0("set-illus2-", expt, ".png", collapse=""), width=800*sf, height=600*sf, res=72*sf)
ggarrange(g.ets, g.v0ts, g.v0, g.vp1p, nrow=2, ncol=2,
          labels=c("A", "B", "C", "D"))
dev.off()

png(paste0("set-illus3-", expt, ".png", collapse=""), width=400*sf, height=450*sf, res=72*sf)
print(g.v0)
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
  
  dfsub = dfh[dfh$nstar == 40 & 
                dfh$ds %in% c(-0.25, 0, 0.25) & 
                dfh$t %in% c(0, 9, 99, 999) &
                dfh$h0 %in% c(0.5,0.9),]
  
  elem.plot = function(tdf) {
    return(
      ggplot(tdf, aes(xmin=h, ymin=0, xmax=h+0.03, ymax=Ph)) + 
        xlim(-0.05,1.05) +facet_wrap(~(t+1), ncol=1, scales = "free_y")  +
        geom_rect() + theme_classic() + labs(x="h") +
        theme(
          strip.background = element_blank(),     # remove facet label box
          strip.text = element_text(face = "bold"), # optional: tweak facet label text
          axis.ticks.y = element_blank(),         # remove y-axis ticks
          axis.text.y = element_blank(),          # remove y-axis text
          axis.line.y = element_blank()           # optionally remove y-axis line
        )
    )
  }
  
  g.illus = ggarrange(elem.plot(dfsub[dfsub$ds == 0 & dfsub$h0 == 0.5,]),
                      elem.plot(dfsub[dfsub$ds == 0 & dfsub$h0 == 0.9,]),
                      elem.plot(dfsub[dfsub$ds == -0.25 & dfsub$h0 == 0.5,]),
                      elem.plot(dfsub[dfsub$ds == -0.25 & dfsub$h0 == 0.9,]),
                      labels = c("i", "ii", "iii", "iv"), nrow=1)
  
  sf = 3
  png(paste0("set-hist2-", expt, ".png", collapse=""), width=600*sf, height=200*sf, res=72*sf)
  print(g.illus)
  dev.off()
  
  g.illus.alt = ggarrange(elem.plot(dfsub[dfsub$ds == 0 & dfsub$h0 == 0.5,]),
                      elem.plot(dfsub[dfsub$ds == 0 & dfsub$h0 == 0.9,]),
                      elem.plot(dfsub[dfsub$ds == -0.25 & dfsub$h0 == 0.5,]),
                      elem.plot(dfsub[dfsub$ds == -0.25 & dfsub$h0 == 0.9,]),
                      labels = c("i", "ii", "iii", "iv"), nrow=2, ncol=2)
  
  sf = 3
  png(paste0("set-hist2-alt-", expt, ".png", collapse=""), width=300*sf, height=350*sf, res=72*sf)
  print(g.illus.alt)
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
  
  ##### this is not QUITE correctly capturing the behaviour yet -- but getting there
  # some dt=1 issues perhaps -- turnover vs cell division timescales?
  if(expt == 0) {
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen)
  } else if(expt == 1) {
    delta = 1* ( 1/(tmpdf$neff/2) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor((tmpdf$gen-0)/10))
  } else if(expt == 2) {
    delta = 1* ( 1/(tmpdf$neff/5) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor((tmpdf$gen-0)/10))
  } else if(expt == 3) {
    delta = 2* ( 1/(tmpdf$neff/2) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor((tmpdf$gen-0)/10))
  } else if(expt == 4) {
    delta = 2* ( 1/(tmpdf$neff/5) - (1/tmpdf$neff) )
    tmpdf$wright = (1-(1-1/tmpdf$nturn)**tmpdf$gen*(1-delta)**floor((tmpdf$gen-0)/10))
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
    facet_wrap( ~ ds) + xlim(0,50) 
  ## ^ yes we can! just consider effective N for one time unit, then stack results in Wright formula
}
