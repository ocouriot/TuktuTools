require(TuktuTools)

data(caribou)

# flag outliers
caribou.cleaned <- removeOutliers(caribou)

head(caribou.cleaned)

table(caribou.cleaned$outlier)

# visualize 
ggplot(data=caribou.cleaned, aes(x=x, y = y)) + geom_path() + 
  geom_point(data= caribou.cleaned %>% subset(outlier == "TRUE"), aes(colour=factor(outlier))) + 
  facet_wrap(~ID, scales = "free")


which(caribou.cleaned$outlier == "TRUE") # [1] 12263 12326 13394 13689

# check those locations
caribou.cleaned[12262:12264,] # one minute interval

caribou.cleaned[12325:12327,] # one minute interval

caribou.cleaned[13393:13395,] # one minute interval

caribou.cleaned[13687:13690,] # one minute interval
