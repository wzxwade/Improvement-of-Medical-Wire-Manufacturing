library(readxl)
library(base)
library(car)
library(FrF2)
options(scipen = 200)
setwd("C:/Users/wzxwa/Desktop")

#Fractional factorial design
df<-read.csv("Fractional_factorial_table.csv")
df$A[df$A==0] = -1
df$B[df$B==0] = -1
df$C[df$C==0] = -1
df$D[df$D==0] = -1
ID <- as.factor(df$`Spool ID`)
Block = as.factor(df$Block)
Machine <- as.factor(df$A)
Angle <- as.factor(df$B)
Length <- as.factor(df$C)
Diameter <- as.factor(df$D)
Ratio <- df$`YS/UTS (percent)`
df = data.frame(ID,Block,Machine,Angle,Length,Diameter,Ratio)
df

#Exploer significance
#Generator I=ABCD
fit = lm(Ratio~Machine+Angle+Length+Diameter+Machine*Angle+Machine*Length+Machine*Diameter,data=df)
summary(fit)      

#Adjust outliers
qqPlot(fit, main="QQ Plot") 
leveragePlots(fit)
plot(fit, which=4, cook.levels=cutoff)
cutoff <- 4/120
d <- cooks.distance(fit)>=cutoff
d[d==TRUE]
df$Ratio[df$ID==6]=93.22
df$Ratio[df$ID==8]=93.34
df$Ratio[df$ID==10]=93.34
df$Ratio[df$ID==28]=93.2
sort(df$ID)

#Using different type response variables
df_md <- data.frame(r1=df$Ratio[1:24],r2=df$Ratio[25:48],r3=df$Ratio[49:72],r4=df$Ratio[73:96],r5=df$Ratio[97:120])
df_md <- data.frame(r1=df$Ratio[1:24],r2=df$Ratio[25:48],r3=df$Ratio[49:72],r4=df$Ratio[73:96],r5=df$Ratio[97:120],mean=apply(df_md,1,mean),std=apply(df_md,1,sd))
df_r <- subset(df,select=ID:Diameter)
df_r<-rbind(df_r[1:8,],df_r[9:16,],df_r[17:24,])
df_r<-cbind(df_r,df_md)
df_r
fit_avg <- lm(mean~Machine+Angle+Length+Diameter+Machine*Angle+Machine*Length+Machine*Diameter,data=df_r)
plot(fit_log, which=4, cook.levels=cutoff)
fit_rec <- lm(1/mean~Machine+Angle+Length+Diameter+Machine*Angle+Machine*Length+Machine*Diameter,data=df_r)
fit_log <- lm(log(mean)~Machine+Angle+Length+Diameter+Machine*Angle+Machine*Length+Machine*Diameter,data=df_r)
fit_two <-lm(mean~Machine+Angle+Length+Diameter+Machine*Angle+Machine*Length+Machine*Diameter+Length*Angle+Diameter*Angle+Length*Diameter,data=df_r)
summary(fit_avg)
summary(fit_rec) 
summary(fit_log) 
summary(fit_two)
IAPlot(fit_avg)

#Using A,B,C to do a full factorial design
df_ff<-read.csv("Full_factorial_table.csv")
df_ff$A[df_ff$A==0] = -1
df_ff$B[df_ff$B==0] = -1
df_ff$C[df_ff$C==0] = -1
Machine <- as.factor(df_ff$A)
Angle <- as.factor(df_ff$B)
Length <- as.factor(df_ff$C)
mean <- df_ff$mean
df_ff = data.frame(Machine,Angle,Length,mean)
df_ff[1:8,]
fit <- lm(mean~(Machine+Angle+Length)^2,data=df_ff)
summary(fit)
IAPlot(fit)