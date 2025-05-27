library(readxl)
library(ggplot2)

# read datafile. this is https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-018-04797-2/MediaObjects/41467_2018_4797_MOESM4_ESM.xlsx
df = as.data.frame(read_excel("burgstaller-nc-data.xlsx"))
# populate a dataframe from the first sheet. 
res.df = data.frame()
for(i in 3*(1:(ncol(df)/3))) {
  tmp = df[,i]
  # original h measurement
  ref = as.numeric(tmp[2])/100
  # age at sampling
  age = as.numeric(tmp[3])+21
  # set of final sampled heteroplasmies
  otherset = tmp[4:length(tmp)]
  otherset = as.numeric(otherset[which(!is.na(otherset))])/100
  # add to dataframe
  res.df = rbind(res.df, data.frame(num=i/3, id = tmp[1], age=age, ref=ref, vals=otherset ))
}
# create plot
g1 = ggplot(res.df[res.df$num %% 2 == 0,], aes(x = vals, y = age, xend = ref, yend = 0, color=id)) + 
  geom_segment(size=0.15) + geom_point(size=0.4) + labs(x = "h", y = "Age / dpc") +
  xlim(0,1) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90), legend.position="none") 

# save
sf = 5
png("mouse-samples.png", width=200*sf, height=200*sf, res=72*sf)
print(g1)
dev.off()
