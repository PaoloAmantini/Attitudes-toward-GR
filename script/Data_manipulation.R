library(rio)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)
library(dplyr)
library(ltm)
library(xtable)
library(foreign)
library(hrbrthemes)
library(viridis)
library(forcats)
library(patchwork)

setwd("/Users/paoloamantini/Desktop/dataset")
data <- import("ZA5900_v4-0-0.sav") 
dat <- vector(mode= "list")

dat <-filter(data, data$C_ALPHAN== "DE"|data$C_ALPHAN=="FI"|data$C_ALPHAN=="PT"
             |data$C_ALPHAN=="SK")

dat<- dat[,c("C_ALPHAN","V5","V6","V7","V8","V9","V10","V11","V12","V13","SEX",
             "AGE","DEGREE","ATTEND","PARTY_LR","HHCHILDR","HHTODD","MARITAL",
             "URBRURAL")]

#gender male = 1 

#recoding missing value

dat$V5<- ifelse (dat$V5==1,1,
                 ifelse(dat$V5==2,2,
                        ifelse(dat$V5 ==3,3,
                               ifelse(dat$V5==4,4,
                                      ifelse(dat$V5==5,5,NA)))))
dat$V6<- ifelse (dat$V6==1,1,
                 ifelse(dat$V6==2,2,
                        ifelse(dat$V6 ==3,3,
                               ifelse(dat$V6==4,4,
                                      ifelse(dat$V6==5,5,NA)))))


dat$V7<- ifelse (dat$V7==1,1,
                 ifelse(dat$V7==2,2,
                        ifelse(dat$V7 ==3,3,
                               ifelse(dat$V7==4,4,
                                      ifelse(dat$V7==5,5,NA)))))

dat$V8<- ifelse (dat$V8==1,1,
                 ifelse(dat$V8==2,2,
                        ifelse(dat$V8 ==3,3,
                               ifelse(dat$V8==4,4,
                                      ifelse(dat$V8==5,5,NA)))))
dat$V9<- ifelse (dat$V9==1,1,
                 ifelse(dat$V9==2,2,
                        ifelse(dat$V9 ==3,3,
                               ifelse(dat$V9==4,4,
                                      ifelse(dat$V9==5,5,NA)))))

#dat$V10 <- ifelse (dat$V10==1,1,
#ifelse(dat$V10==2,2,
#ifelse(dat$V10 ==3,3,
#ifelse(dat$V10==4,4,
#ifelse(dat$V10==5,5,NA)))))
dat$V11<- ifelse (dat$V11==1,1,
                  ifelse(dat$V11==2,2,
                         ifelse(dat$V11 ==3,3,
                                ifelse(dat$V11==4,4,
                                       ifelse(dat$V11==5,5,NA)))))

#dat$ATTEND<- ifelse (dat$ATTEND==1,1,
#ifelse(dat$ATTEND==2,2,
#ifelse(dat$ATTEND ==3,3,
#ifelse(dat$ATTEND==4,4,
#ifelse(dat$ATTEND==5,5,
#ifelse(dat$ATTEND== 6,6,
#ifelse(dat$ATTEND== 7,7,
#ifelse(dat$ATTEND==8,8,NA))))))))
#dat$PARTY_LR <- ifelse (dat$PARTY_LR==1,1,
#ifelse(dat$PARTY_LR==2,2,
#ifelse(dat$PARTY_LR ==3,3,
#ifelse(dat$PARTY_LR==4,4,
#ifelse(dat$PARTY_LR==5,5,NA)))))


#scale for V5 and V10,  1 s.agree 5= s.disagree so I reverse the scale to have
#people with hig  egalitarian attitude scoring high.

table(dat$ATTEND)
dat[ ,"V5"] = 6 - dat[ ,"V5"]
#dat[ ,"V10"] = 6 - dat[ ,"V10"]
#dat[ ,"ATTEND"] = 9 - dat[ ,"ATTEND"] # REVERSE
colSums(is.na(dat))



#now the scale are reverted

#MARITAL =1 = MARRIED

dat$g_egat <-  (as.numeric(dat$V5+dat$V6+dat$V7+dat$V8+ dat$V11, na.rm = TRUE)/5)


c_egat <- dat%>%
  group_by(C_ALPHAN)%>%
  summarise(
    m_g_egat= mean(g_egat, na.rm= T)
  )%>%
  ungroup()
c_egat

s_egat <- dat%>%
  group_by(C_ALPHAN)%>%
  summarise(
    sd_egat= sd(g_egat, na.rm= T)
  )%>%
  ungroup()
s_egat

n_obs <- dat%>%
  group_by(C_ALPHAN)%>%
  count()%>%
  ungroup()
n_obs

#mar_DE <- qt(0.975,df=1766-1)*0.9237590/sqrt(1766)
#mar_FI <- qt(0.975,df=1171-1)*0.8239660/sqrt(1171)
#mar_PT <- qt(0.975,df=1001-1)*0.7299401/sqrt(1001)
#mar_SK <- qt(0.975,df=1128-1)*0.7810620/sqrt(1128)


df <- data.frame(matrix(ncol = 4, nrow = 4))

#provide column names
colnames(df) <- c('country', 'mean', 'un_ci','up_ci')

df$country <- c("DE","FI","PT","SK")
df$mean <- c(3.754007, 3.800207, 3.186861, 3.034957)
df$un_ci <- c(3.710894, 3.752965,3.141587,2.989327)
df$up_ci <- c(3.79712, 3.847449,3.232135,3.080587)
df


dat$pub_egat <- (as.numeric(dat$V5+dat$V6+dat$V7+dat$V8+dat$V11, na.rm = TRUE)/5)

#cronbach alpha

dt <- dat[,c("V5","V6","V7","V8","V11")]
dt<- dt[rowSums(is.na(dt)) == 0, ]       # Apply rowSums & is.na


cronbach.alpha(dt, na.rm = T)
cronbach.alpha(dt, CI=TRUE, na.rm= T)


# create the table for LateX
prov <- dat%>%
  group_by(C_ALPHAN)%>%
  summarise(
    m_V5 = mean(V5, na.rm=T),
    m_V6 = mean(V6, na.rm=T),
    m_V7 = mean(V7, na.rm=T),
    m_V8 = mean(V8, na.rm=T),
    m_V11 = mean(V11, na.rm=T)
  )%>%
  ungroup()

prov$obs<- c(1766,1171,1001,1128) 

prov <- t(prov)

#to export the table 
#xtable(prov, type = "latex", file = "tt.tex")


#distribution

f_g <- dat[,-(13:20)]
f_g <-f_g[,-c(7,6,9,10)]
f_g<- f_g[rowSums(is.na(f_g)) == 0, ]   

gend<- f_g%>%
  group_by(C_ALPHAN,SEX)%>%
  count()%>%
  ungroup()
gend

#1 male 2 female


f_g1<- f_g[,-(7:8)]
f_g1<- f_g1[,-(7:8)]

data1 <- f_g1
data1<- data1 %>%
  gather(key="C_ALPHAN", value="value")

data1$color <-ifelse(data1$C_ALPHAN =="V5",1,
                     ifelse(data1$C_ALPHAN == "V6",2,
                            ifelse(data1$C_ALPHAN == "V7",3,
                                   ifelse(data1$C_ALPHAN == "V8",4,5))))


table(dat$AGE)
dat$age_gr <- ifelse(dat$AGE<=35,1,
                     ifelse(dat$AGE<=55,2,
                            ifelse(dat$AGE<=80,3,NA)))


gee<- dat%>%
  group_by(age_gr,C_ALPHAN)%>%
  count()%>%
  ungroup()
gee

cag_se<- dat%>%
  group_by(C_ALPHAN,age_gr,SEX)%>%
  summarise(
    m_pat=mean(pub_egat, na.rm=T)
  )%>%
  ungroup()
cag_se
cag_se<- cag_se[rowSums(is.na(cag_se)) == 0, ]


#selecting just married couple

mar<- dat%>%
  group_by(C_ALPHAN,SEX,MARITAL)%>%
  summarise(
    mar_at=mean(pub_egat, na.rm=T)
  )%>%
  ungroup()
mar
mar$MARITAL <- ifelse(mar$MARITAL == 1,1,NA)
mar<- mar[rowSums(is.na(mar)) == 0, ]



#t o count obs of married  by country 
martt<- dat%>%
  group_by(C_ALPHAN,MARITAL)%>%
  count()%>%
  ungroup()
martt

# mean by gender
me_sex<- dat%>%
  group_by(SEX)%>%
  summarise(
    mi=mean(pub_egat, na.rm=T)
  )%>%
  ungroup()
me_sex





