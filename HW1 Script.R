load("/Users/madelinecraft/Desktop/22081-0001-Data.rda")
mydata<-load("/Users/madelinecraft/Desktop/22081-0001-Data.rda")
head(mydata)
dataframe = da22081.0001
head(dataframe)
female=dataframe$V1102
summary(female)
female=ifelse(female=="(2) FEMALE", 1, ifelse(female=="(1) MALE",0,2))
as.factor(female)
ID<-dataframe$CASE_ID
head(ID)
df=data.frame(female, ID, D1=dataframe$DEPRESS1, D2=dataframe$DEPRESS2, D3=dataframe$DEPRESS3, D4=dataframe$DEPRESS4)
head(df)

D1=ifelse(df$D1=="(1) ALWAYS", 1, ifelse(df$D1=="(2) MOST OF THE TIME", 2, ifelse(df$D1=="(3) SOME OF THE TIME", 3, ifelse(df$D1=="(4) NEVER", 4, 0))))
df=data.frame(female, ID, D1, D2=dataframe$DEPRESS2, D3=dataframe$DEPRESS3, D4=dataframe$DEPRESS4)
head(df)

D2=ifelse(df$D2=="(1) ALWAYS", 1, ifelse(df$D2=="(2) MOST OF THE TIME", 2, ifelse(df$D2=="(3) SOME OF THE TIME", 3, ifelse(df$D2=="(4) NEVER", 4, 0))))
df=data.frame(female, ID, D1, D2, D3=dataframe$DEPRESS3, D4=dataframe$DEPRESS4)
head(df)

D3=ifelse(df$D3=="(1) ALWAYS", 1, ifelse(df$D3=="(2) MOST OF THE TIME", 2, ifelse(df$D3=="(3) SOME OF THE TIME", 3, ifelse(df$D3=="(4) NEVER", 4, 0))))
df=data.frame(female, ID, D1, D2, D3, D4=dataframe$DEPRESS4)
head(df)

D4=ifelse(df$D4=="(1) ALWAYS", 1, ifelse(df$D4=="(2) MOST OF THE TIME", 2, ifelse(df$D4=="(3) SOME OF THE TIME", 3, ifelse(df$D4=="(4) NEVER", 4, 0))))
df=data.frame(female, ID, D1, D2, D3, D4)
head(df)

as.factor(df$D1)
as.factor(df$D2)
as.factor(df$D3)
as.factor(df$D4)

depression=(df$D1+df$D2+df$D3+df$D4)/4
head(depression)

by(df$D1, df[,"female"], summary)
by(df$D1, df[,"female"], sd)

by(df$D2, df[,"female"], summary)
by(df$D2, df[,"female"], sd, na.rm=TRUE)

by(df$D3, df[,"female"], summary)
by(df$D3, df[,"female"], sd, na.rm=TRUE)

by(df$D4, df[,"female"], summary)
by(df$D4, df[,"female"], sd, na.rm=TRUE)

by(df$depression, df[,"female"], summary)
by(df$depression, df[,"female"], sd, na.rm=TRUE)

anxiety=dataframe$ANX2
anxiety
summary(anxiety)
df=data.frame(female, ID, D1, D2, D3, D4, anxiety, depression)
head(df)

by(df$anxiety, df[,"female"], summary)
by(df$anxiety, df[,"female"], sd, na.rm=TRUE)

by(df, df$female, FUN=function(df) cor(df$depression, df$anxiety, method="spearman", use="complete.obs"))

by(df, df$female, FUN=function(df) boxplot(df$))

boxplot(df$anxiety~df$female, xlab="Gender", ylab="Anxiety", main="Plot of Anxiety for Males and Females")