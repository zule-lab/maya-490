#Species accumulation curve 
install.packages("vegan")
library(vegan)

View(site.sp.mat)

spec.acc <- specaccum(site.sp.mat, method=random, permutation=999, gamma=chao)
spec.acc
