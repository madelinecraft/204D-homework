dpath='~/Desktop/204D/lab2.csv'
mydata<-read.csv(dpath, header=TRUE, sep=',')
head(mydata)
mydata$group = as.factor(mydata$group)
grpmeans=tapply(mydata$attended, mydata$group, mean)
ngrp=tapply(mydata$attended, mydata$group, length)
sdgrp=tapply(mydata$attended, mydata$group, sd)

fit1 <- aov(attended ~ group, data = mydata)
summary(fit1, intercept=T)
#reject the null, there are differences between groups

pairwise.t.test(mydata$attended, mydata$group, p.adjust = "none", pool.sd = TRUE)
#group 1 compared to 2 is significant, p=0.033, group 1 compared to 3 is significant, p=0.005, group 1 compared to 4 is significant, p=0.00014, group 2 compared to group 4 is significant, p=0.039. 

pairwise.t.test(mydata$attended, mydata$group, p.adjust = "bonferroni", pool.sd = TRUE)
#group 1 compared to group 3 is significant, p=0.031, group 1 compared to group 4 is significant, p=0.00083.

pairwise.t.test(mydata$attended, mydat$group, p.adjust = "holm", pool.sd = TRUE)
#group 1 compared to group 3 is significant, p=0.026, group 1 compared to group 4, p=0.00083. 

TukeyHSD(fit1, conf.level=.95)
#group 3 compared to group 1 is significant, p=0.025, group 4 compared to grop 1 is significant, p=0.00076.
#Tukey's HSD gives the actual difference between the groups being compared and upper/lower bounds for confidence intervals whereas the other comparison methods just give p values.

plot(TukeyHSD(aov(mydata$attended~mydata$group), conf.level=.95))