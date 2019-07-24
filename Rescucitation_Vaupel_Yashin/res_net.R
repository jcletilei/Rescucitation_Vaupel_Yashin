library(MortalityLaws)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(plyr)
# Downloading some countries. We have to use the HMD codes
cntr <- c('SWE', "ITA","BEL","NLD","FIN",
          "NOR", "CHE")

#reading in the life tables in single ages, by single-year periods
# females
LT_f<- ReadHMD(what = "LT_f",
               countries = cntr,
               interval = "1x1",
               username = "vdilego@gmail.com",
               password = "588672vitor",save = FALSE)


ls(LT_f)
LT_f

#males

LT_m<- ReadHMD(what = "LT_m",
               countries = cntr,
               interval = "1x1",
               username = "vdilego@gmail.com",
               password = "588672vitor",save = FALSE)

ls(LT_m)
LT_m


#############################################
#### Netherlands ############################
#############################################

# preparing data 

#females
lt_f<-LT_f$data

# retrieving the columns of interest
net_lt_f<-lt_f%>%
  filter(country %in% c("NLD"))%>%
  filter(Age<=100)%>%
  select(c("Year","Age","lx"))
View(net_lt_f) 

# tranforming to wide format for calculations
net_lt_f_surv <- net_lt_f %>%
  spread(key=Year, value=lx)

View(net_lt_f_surv)

#saving for later checking or performing simple tasks in excel
write.table(net_lt_f_surv, file="net_females_lx.csv", sep=",", row.names = F) #saving for using excel as well

#the same for life expectancy only
net_lt_f_ex<-lt_f%>%
  filter(country %in% c("NLD"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(net_lt_f_ex) 


net_lt_f_ex_surv <- net_lt_f_ex %>%
  spread(key=Year, value=ex)


View(net_lt_f_ex_surv)
write.table(net_lt_f_ex_surv, file="net_females_ex.csv", sep=",", row.names = F) #saving for using excel as well

# males

lt_m<-LT_m$data
net_lt_m<-lt_m%>%
  filter(country %in% c("NLD"))%>%
  select(c("Year","Age","lx"))
View(net_lt_m) 


net_lt_m_surv <- net_lt_m %>%
  spread(key=Year, value=lx)


View(net_lt_m_surv)
write.table(net_lt_m_surv, file="net_males_lx.csv", sep=",", row.names = F) #saving for using excel as well

#only ex
net_lt_m_ex<-lt_m%>%
  filter(country %in% c("NLD"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(net_lt_m_ex) 


net_lt_m_ex_surv <- net_lt_m_ex %>%
  spread(key=Year, value=ex)


View(net_lt_m_ex_surv)
write.table(net_lt_m_ex_surv, file="net_males_ex.csv", sep=",", row.names = F) #saving for using excel as well

###############################################
### Implementing Vaupel큦 Approach : FEMALES ##
###############################################

# selecting years for mortatliy regime comparison. IN the case of Swenet, we will perform 50-year analysis#
net_lt_f<-lt_f%>%
  filter(country %in% c("NLD"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1850","1851","1901","1951","2016"))%>%
  select(c("Year","Age","lx"))
View(net_lt_f) 

# ploting survival curves

library(ggplot2)
X11(width=7,height=7)
ggplot(net_lt_f, aes(x = Age, y = lx, group=factor(Year))) +
  geom_line(aes(linetype=factor(Year)))+ ylim(1,100000)+
  theme_bw()+scale_linetype_manual(name="Year",  values = c("1850"="longdash",
                                                            "1851"="solid",
                                                            "1901"="dotdash",
                                                            "1951"="dashed",
                                                            "2016"="dotted"), 
                                   labels=c("1850","1851","1901","1951","2016"))+
  ylab("lx")+annotate("text", x = c(50,60,68,70,80), y = c(40000,45000,52000,80000,90000), label = c("1850","1851","1901","1951","2016"))

dev.off()

#wide format for estimation

lx_net<- net_lt_f%>%
  spread(key=Year, value=lx)
View(lx_net)
#only ex
net_lt_f_ex<-lt_f%>%
  filter(country %in% c("NLD"))%>%
  filter(Year %in% c("1850","1851","1901","1951","2016"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(net_lt_f_ex) 




#wide format for estimation

net_lt_f_ex_surv <- net_lt_f_ex %>%
  spread(key=Year, value=ex)


View(net_lt_f_ex_surv)

# taking the first row out because we are not dealing with the radix of the lifetable
lx_net<-lx_net[-1,]


View(lx_net)
# creating the variables for estimating the resuscitated

lx_net_full<-lx_net %>%
  mutate(hazard_1=log(lx_net[,3]/lx_net[,2]),hazard_2=log(lx_net[,4]/lx_net[,3]),hazard_3=log(lx_net[,5]/lx_net[,4]),
         hazard_4=log(lx_net[,6]/lx_net[,5]),change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1)

View(lx_net_full)
# taking only the complete cases because since there are no survivors after age 104 na큦 are generated

lx_net_full<-lx_net_full[complete.cases(lx_net_full), ]

# creating a datanetme for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison 1850/1851

lx_net_res_1<-lx_net_full %>%
  mutate(res_1=lx_net_full[,2]*lx_net_full[,7], res_2=(lx_net_full[,2]*(lx_net_full[,7]^2))/factorial(2), res_3=(lx_net_full[,2]*(lx_net_full[,7]^3))/factorial(3),
         res_4=(lx_net_full[,2]*(lx_net_full[,7]^4))/factorial(4), res_5=(lx_net_full[,2]*(lx_net_full[,7]^5))/factorial(5),res_6=(lx_net_full[,2]*(lx_net_full[,7]^6))/factorial(6),
         res_7=(lx_net_full[,2]*(lx_net_full[,7]^7))/factorial(7), res_8=(lx_net_full[,2]*(lx_net_full[,7]^8))/factorial(8),res_9=(lx_net_full[,2]*(lx_net_full[,7]^9))/factorial(9),
         res_10=(lx_net_full[,2]*(lx_net_full[,7]^10))/factorial(10))


View(lx_net_res_1)
radix<-100000
write.table(lx_net_res_1, file="lx_net.csv", sep=",", row.names = F) #saving for using excel as well)

lx_net_res_years<-lx_net_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_res_years<-lx_net_res_years%>% mutate(total=sum(lx_net_res_years[1,25:34]))

View(lx_net_res_years)

#adding life expectancies, estimating differences 
lx_net_res_years$ex_old<-net_lt_f_ex_surv$`1850`
lx_net_res_years$ex_new<-net_lt_f_ex_surv$`1851`

#mortality gap between regimes
lx_net_res_years$ex_diff<-lx_net_res_years$ex_new-lx_net_res_years$ex_old

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_res_years$ex_gap_res1<-lx_net_res_years$ex_new-(lx_net_res_years$ex_old+lx_net_res_years$tau_1)
lx_net_res_years$ex_gap_res_percent<-100-((lx_net_res_years$ex_gap_res1/lx_net_res_years$ex_diff)*100)


# 2. estimating life years spent in each resuscitation state for the second mortality regime comparison 1851/1901

lx_net_res_2<-lx_net_full %>%
  mutate(res_1=lx_net_full[,3]*lx_net_full[,8], res_2=(lx_net_full[,3]*(lx_net_full[,8]^2))/factorial(2), res_3=(lx_net_full[,3]*(lx_net_full[,8]^3))/factorial(3),
         res_4=(lx_net_full[,3]*(lx_net_full[,8]^4))/factorial(4), res_5=(lx_net_full[,3]*(lx_net_full[,8]^5))/factorial(5),res_6=(lx_net_full[,3]*(lx_net_full[,8]^6))/factorial(6),
         res_7=(lx_net_full[,3]*(lx_net_full[,8]^7))/factorial(7), res_8=(lx_net_full[,3]*(lx_net_full[,8]^8))/factorial(8),res_9=(lx_net_full[,3]*(lx_net_full[,8]^9))/factorial(9),
         res_10=(lx_net_full[,3]*(lx_net_full[,8]^10))/factorial(10))


View(lx_net_res_2)

lx_net_res_years_2<-lx_net_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_res_years_2<-lx_net_res_years_2%>% mutate(total=sum(lx_net_res_years_2[1,25:34]))

View(lx_net_res_years_2)

#adding life expectancies, estimating differences 
lx_net_res_years_2$ex_old2<-net_lt_f_ex_surv$`1851`
lx_net_res_years_2$ex_new2<-net_lt_f_ex_surv$`1901`

#mortality gap between regimes
lx_net_res_years_2$ex_diff2<-lx_net_res_years_2$ex_new2-lx_net_res_years_2$ex_old2

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_res_years_2$ex_gap_res2<-lx_net_res_years_2$ex_new2-(lx_net_res_years_2$ex_old2+lx_net_res_years_2$tau_1)
lx_net_res_years_2$ex_gap_res_percent2<-100-((lx_net_res_years_2$ex_gap_res2/lx_net_res_years_2$ex_diff2)*100)


# 3. estimating life years spent in each resuscitation state for the third mortality regime comparison 1901/1951

lx_net_res_3<-lx_net_full %>%
  mutate(res_1=lx_net_full[,4]*lx_net_full[,9], res_2=(lx_net_full[,4]*(lx_net_full[,9]^2))/factorial(2), res_3=(lx_net_full[,4]*(lx_net_full[,9]^3))/factorial(3),
         res_4=(lx_net_full[,4]*(lx_net_full[,9]^4))/factorial(4), res_5=(lx_net_full[,4]*(lx_net_full[,9]^5))/factorial(5),res_6=(lx_net_full[,4]*(lx_net_full[,9]^6))/factorial(6),
         res_7=(lx_net_full[,4]*(lx_net_full[,9]^7))/factorial(7), res_8=(lx_net_full[,4]*(lx_net_full[,9]^8))/factorial(8),res_9=(lx_net_full[,4]*(lx_net_full[,9]^9))/factorial(9),
         res_10=(lx_net_full[,4]*(lx_net_full[,9]^10))/factorial(10))


View(lx_net_res_3)

lx_net_res_years_3<-lx_net_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_res_years_3<-lx_net_res_years_3%>% mutate(total=sum(lx_net_res_years_3[1,25:34]))

View(lx_net_res_years_3)

#adding life expectancies, estimating differences 
lx_net_res_years_3$ex_old3<-net_lt_f_ex_surv$`1901`
lx_net_res_years_3$ex_new3<-net_lt_f_ex_surv$`1951`

#mortality gap between regimes
lx_net_res_years_3$ex_diff3<-lx_net_res_years_3$ex_new3-lx_net_res_years_3$ex_old3

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_res_years_3$ex_gap_res3<-lx_net_res_years_3$ex_new3-(lx_net_res_years_3$ex_old3+lx_net_res_years_3$tau_1)
lx_net_res_years_3$ex_gap_res_percent3<-100-((lx_net_res_years_3$ex_gap_res3/lx_net_res_years_3$ex_diff3)*100)



# 4. estimating life years spent in each resuscitation state for the fourth mortality regime comparison - 1951/2016

lx_net_res_4<-lx_net_full %>%
  mutate(res_1=lx_net_full[,5]*lx_net_full[,10], res_2=(lx_net_full[,5]*(lx_net_full[,10]^2))/factorial(2), res_3=(lx_net_full[,5]*(lx_net_full[,10]^3))/factorial(3),
         res_4=(lx_net_full[,5]*(lx_net_full[,10]^4))/factorial(4), res_5=(lx_net_full[,5]*(lx_net_full[,10]^5))/factorial(5),res_6=(lx_net_full[,5]*(lx_net_full[,10]^6))/factorial(6),
         res_7=(lx_net_full[,5]*(lx_net_full[,10]^7))/factorial(7), res_8=(lx_net_full[,5]*(lx_net_full[,10]^8))/factorial(8),res_9=(lx_net_full[,5]*(lx_net_full[,10]^9))/factorial(9),
         res_10=(lx_net_full[,5]*(lx_net_full[,10]^10))/factorial(10))


View(lx_net_res_4)

lx_net_res_years_4<-lx_net_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_res_years_4<-lx_net_res_years_4%>% mutate(total=sum(lx_net_res_years_4[1,25:34]))

View(lx_net_res_years_4)

#adding life expectancies, estimating differences 
lx_net_res_years_4$ex_old4<-net_lt_f_ex_surv$`1951`
lx_net_res_years_4$ex_new4<-net_lt_f_ex_surv$`2016`

#mortality gap between regimes
lx_net_res_years_4$ex_diff4<-lx_net_res_years_4$ex_new4-lx_net_res_years_4$ex_old4

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_res_years_4$ex_gap_res4<-lx_net_res_years_4$ex_new4-(lx_net_res_years_4$ex_old4+lx_net_res_years_4$tau_1)
lx_net_res_years_4$ex_gap_res_percent4<-100-((lx_net_res_years_4$ex_gap_res4/lx_net_res_years_4$ex_diff4)*100)

### reshaping data

# 1. first only first regime
lx_net_res_years$Regime<-c("1850-1851")
View(lx_net_res_years)
lx_net_females_1<-lx_net_res_years%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_females_1)
lx_net_females_1_long<- gather (lx_net_females_1, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_females_1_long)

colnames(lx_net_females_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime
lx_net_res_years_2$Regime<-c("1851-1901")
View(lx_net_res_years_2)
lx_net_females_2<-lx_net_res_years_2%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_females_2)
lx_net_females_2_long<- gather (lx_net_females_2, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_females_2_long)

colnames(lx_net_females_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_net_res_years_3$Regime<-c("1901-1951")
View(lx_net_res_years_3)
lx_net_females_3<-lx_net_res_years_3%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_females_3)
lx_net_females_3_long<- gather (lx_net_females_3, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_females_3_long)
colnames(lx_net_females_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_net_res_years_4$Regime<-c("1951-2016")
View(lx_net_res_years_4)
lx_net_females_4<-lx_net_res_years_4%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_females_4)
lx_net_females_4_long<- gather (lx_net_females_4, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_females_4_long)
colnames(lx_net_females_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# combining everything
library(reshape)
library(reshape2)
resuscitated_females_netnet<- rbind(lx_net_females_1_long,lx_net_females_2_long,lx_net_females_3_long,lx_net_females_4_long)
View(resuscitated_females_netnet)

# ploting the hazards and tau큦
X11(width=15,height=5.5)
par(mfrow=c(1,2))
plot(x=c(1:length(lx_net_res_years_4$Age)), lx_net_res_years_4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab="Intensity of lifesaving ", xlab="Age", lwd=2,main="a) Females")
lines(x=c(1:length(lx_net_res_years_4$Age)), lx_net_res_years_4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_net_res_years_4$Age)), lx_net_res_years_4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_net_res_years_4$Age)), lx_net_res_years_4$hazard_4, lty=1,lwd=2, col="blue")
axis(1, seq(0,length(lx_net_res_years_4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)

legend("topleft", legend=c("1850-1851","1851-1901","1901-1951","1951-2016"),
       lty=c(1,5,3,1),col=c("black","black","black","blue"), bty="n")
abline(h=0, col="grey", lwd=2)

# force of mortality 
net_qx_f<-lt_f%>%
  filter(country %in% c("NLD"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1850","1851","1901","1951","2016"))%>%
  select(c("Year","Age","qx"))
View(net_qx_f) 

net_qx_f_wide<- net_qx_f %>% 
  spread(key=Year, value=qx)

pdf(file="prob_death_females_netnet.pdf",width=5.5,height=5.5)
plot(x=c(1:length(net_qx_f_wide$Age)),net_qx_f_wide$`1851`, ylim=c(0,1), type="l", axes=FALSE,
     ylab="Probability of death- qx", xlab="Age", lwd=1.5, main="a) Females")
lines(x=c(1:length(net_qx_f_wide$Age)), net_qx_f_wide$`1901`, lty=3,lwd=1.5)
lines(x=c(1:length(net_qx_f_wide$Age)), net_qx_f_wide$`1951`, lty=2,lwd=1.5)
lines(x=c(1:length(net_qx_f_wide$Age)), net_qx_f_wide$`2016`, lty=6,lwd=1.5)
axis(1, seq(0,length(net_qx_f_wide$Age),5), las=1,cex.axis=.5, lwd=1.5)
axis(2, seq(0, 1, 0.1),lwd=1.5,cex.axis=.5, las=1)

legend("topleft", legend=c("1851","1901","1951","2016"),
       lty=c(1,5,3,2,6), bty="n", cex = .5)
dev.off()


#ggplot for number of resuscitations and the number of resuscitated, females, netnet
library(forcats)
resuscitated_females_netnet$Resuscitations<-fct_collapse(resuscitated_females_netnet$Resuscitations,
                                                         res_1 = c("res_1"),
                                                         res_2 = c("res_2"),
                                                         res_3 = c("res_3"),
                                                         res_4 = c("res_4"),
                                                         res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_females_netnet$Resuscitations <- factor(resuscitated_females_netnet$Resuscitations, ordered = TRUE,
                                                     levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"))


pdf(file="res_females_netnet.pdf",width=15,height=5.5)
X11(width=15,height=5.5)
ggplot(resuscitated_females_netnet %>% filter(Regime != "1850-1851") , aes(x = Age, y = Res_number, group=Resuscitations)) +
  geom_line(aes(linetype=Resuscitations))+ facet_grid(.~Regime)+ 
  theme_bw()+scale_linetype_manual(name="Number of \nResuscitations",  values = c("res_1"="longdash",
                                                                                  "res_2"="solid",
                                                                                  "res_3"="dotdash",
                                                                                  "res_4"="dashed",
                                                                                  "res_5_plus"="dotted"), 
                                   labels=c("1", "2", "3","4","5+"))+
  ylab("Number of resuscitated persons")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()

# assesing differences in life expectancy and number of life years lived in each resuscitation state


View(lx_net_res_years)



tau_fem_1<-lx_net_res_years[1,25:41]
colnames(tau_fem_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_2<-lx_net_res_years_2[1,25:41]
colnames(tau_fem_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_3<-lx_net_res_years_3[1,25:41]
colnames(tau_fem_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_4<-lx_net_res_years_4[1,25:41]
colnames(tau_fem_4)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem<-rbind(tau_fem_1,tau_fem_2, tau_fem_3,tau_fem_4)
View(tau_fem)


tau_fem_long <- gather(tau_fem, tau, life_years_res, tau_1:tau_10)

View(tau_fem_long)
tau_fem_prop<-tau_fem_long %>%
  group_by(tau)%>%
  mutate(prop_res=(life_years_res/total)*100)

View(tau_fem_prop)

tau_fem_prop$tau<-fct_collapse(tau_fem_prop$tau,
                               tau_1 = c("tau_1"),
                               tau_2 = c("tau_2"),
                               tau_3 = c("tau_3"),
                               tau_4 = c("tau_4"),
                               tau_5_plus=c("tau_5","tau_6","tau_7","tau_8","tau_9","tau_10")
)
tau_fem_prop$tau <- factor(tau_fem_prop$tau, ordered = TRUE,
                           levels = c("tau_1", "tau_2", "tau_3","tau_4","tau_5_plus"))


library(latex2exp)
X11(width=10,height=10)

ggplot(data = tau_fem_prop %>% filter(Regime!="1850-1851"), aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(color=Regime), size=1)+theme_bw()
# add here the proportions

# if animation is interesting..

#p <- resuscitated_females_netnet %>%
# plot_ly(
#  x = ~Age, 
# y = ~Res_number, 
# color = ~Resuscitations, 
#  netme = ~Regime, 
# text = ~Resuscitations, 
# hoverinfo = "text",
# type = 'scatter',
#  mode = 'markers'
# ) 

#p %>% 
# animation_opts(1000, transition = 300,redraw = FALSE
# )
#p

###############################################
### Implementing Vaupel큦 Approach : MALES ##
###############################################

# selecting years for mortatliy regime comparison. IN the case of Swenet, we will perform 50-year analysis#
net_lt_m<-lt_m%>%
  filter(country %in% c("NLD"))%>%
  filter(Year %in% c("1850","1851","1901","1951","2016"))%>%
  filter(Age<=100) %>%
  select(c("Year","Age","lx"))
View(net_lt_m) 

X11(width=7,height=7)
ggplot(net_lt_m, aes(x = Age, y = lx, group=factor(Year))) +
  geom_line(aes(linetype=factor(Year)))+ ylim(1,100000)+
  theme_bw()+scale_linetype_manual(name="Year",  values = c("1850"="longdash",
                                                            "1851"="solid",
                                                            "1901"="dotdash",
                                                            "1951"="dashed",
                                                            "2016"="dotted"), 
                                   labels=c("1850","1851","1901","1951","2016"))+
  ylab("lx")+annotate("text", x = c(46,57,65,70,75), y = c(38000,45000,55000,75000,85000), label = c("1850","1851","1901","1951","2016"))

#if combining males and females in one plot
require(gridExtra)
grid.arrange(females, males, ncol=2)
dev.off

#wide format for estimation
net_lt_m_surv <- net_lt_m %>%
  spread(key=Year, value=lx)


View(net_lt_m_surv)


#only ex
net_lt_m_ex<-lt_m%>%
  filter(country %in% c("NLD"))%>%
  filter(Year %in% c("1850","1851","1901","1951","2016"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(net_lt_m_ex) 


net_lt_m_ex_surv <- net_lt_m_ex %>%
  spread(key=Year, value=ex)


View(net_lt_m_ex_surv)

# taking the first row out because we are not dealing with the radix of the lifetable
lx_net_m<-net_lt_m_surv[-1,]

View(lx_net_m)
# creating the variables for estimating the resuscitated

lx_net_m_full<-lx_net_m %>%
  mutate(hazard_1=log(lx_net_m[,3]/lx_net_m[,2]),hazard_2=log(lx_net_m[,4]/lx_net_m[,3]),hazard_3=log(lx_net_m[,5]/lx_net_m[,4]),
         hazard_4=log(lx_net_m[,6]/lx_net_m[,5]),change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1)

View(lx_net_m_full)
# taking only the complete cases because since there are no survivors after age 104 na큦 are generated

lx_net_m_full<-lx_net_m_full[complete.cases(lx_net_m_full), ]

# creating a datanetme for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison 1850/1851

lx_net_m_res_1<-lx_net_m_full %>%
  mutate(res_1=lx_net_m_full[,2]*lx_net_m_full[,7], res_2=(lx_net_m_full[,2]*(lx_net_m_full[,7]^2))/factorial(2), res_3=(lx_net_m_full[,2]*(lx_net_m_full[,7]^3))/factorial(3),
         res_4=(lx_net_m_full[,2]*(lx_net_m_full[,7]^4))/factorial(4), res_5=(lx_net_m_full[,2]*(lx_net_m_full[,7]^5))/factorial(5),res_6=(lx_net_m_full[,2]*(lx_net_m_full[,7]^6))/factorial(6),
         res_7=(lx_net_m_full[,2]*(lx_net_m_full[,7]^7))/factorial(7), res_8=(lx_net_m_full[,2]*(lx_net_m_full[,7]^8))/factorial(8),res_9=(lx_net_m_full[,2]*(lx_net_m_full[,7]^9))/factorial(9),
         res_10=(lx_net_m_full[,2]*(lx_net_m_full[,7]^10))/factorial(10))


View(lx_net_m_res_1)
radix<-100000

lx_net_m_res_years<-lx_net_m_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_net_m_res_years)
lx_net_m_res_years<-lx_net_m_res_years%>% mutate(total=sum(lx_net_m_res_years[1,25:34]))

View(lx_net_m_res_years)

#adding life expectancies, estimating differences 
lx_net_m_res_years$ex_old_m<-net_lt_m_ex_surv$`1850`
lx_net_m_res_years$ex_new_m<-net_lt_m_ex_surv$`1851`

#mortality gap between regimes
lx_net_m_res_years$ex_diff<-lx_net_m_res_years$ex_new_m-lx_net_m_res_years$ex_old_m

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_net_m_res_years$ex_gap_m_res1<-lx_net_m_res_years$ex_new_m-(lx_net_m_res_years$ex_old_m+lx_net_m_res_years$tau_1)
lx_net_m_res_years$ex_gap_m_res_percent<-100-((lx_net_m_res_years$ex_gap_m_res1/lx_net_m_res_years$ex_diff)*100)
View(lx_net_m_res_years)

# 2. estimating life years spent in each resuscitation state for the second mortality regime comparison 1851/1901

lx_net_m_res_2<-lx_net_m_full %>%
  mutate(res_1=lx_net_m_full[,3]*lx_net_m_full[,8], res_2=(lx_net_m_full[,3]*(lx_net_m_full[,8]^2))/factorial(2), res_3=(lx_net_m_full[,3]*(lx_net_m_full[,8]^3))/factorial(3),
         res_4=(lx_net_m_full[,3]*(lx_net_m_full[,8]^4))/factorial(4), res_5=(lx_net_m_full[,3]*(lx_net_m_full[,8]^5))/factorial(5),res_6=(lx_net_m_full[,3]*(lx_net_m_full[,8]^6))/factorial(6),
         res_7=(lx_net_m_full[,3]*(lx_net_m_full[,8]^7))/factorial(7), res_8=(lx_net_m_full[,3]*(lx_net_m_full[,8]^8))/factorial(8),res_9=(lx_net_m_full[,3]*(lx_net_m_full[,8]^9))/factorial(9),
         res_10=(lx_net_m_full[,3]*(lx_net_m_full[,8]^10))/factorial(10))


View(lx_net_m_res_2)

lx_net_m_res_years_2<-lx_net_m_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_m_res_years_2<-lx_net_m_res_years_2%>% mutate(total=sum(lx_net_m_res_years_2[1,25:34]))

View(lx_net_m_res_years_2)

#adding life expectancies, estimating differences 
lx_net_m_res_years_2$ex_old2_m<-net_lt_m_ex_surv$`1851`
lx_net_m_res_years_2$ex_new2_m<-net_lt_m_ex_surv$`1901`

#mortality gap between regimes
lx_net_m_res_years_2$ex_diff2<-lx_net_m_res_years_2$ex_new2_m-lx_net_m_res_years_2$ex_old2_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_m_res_years_2$ex_gap_m_res2<-lx_net_m_res_years_2$ex_new2_m-(lx_net_m_res_years_2$ex_old2_m+lx_net_m_res_years_2$tau_1)
lx_net_m_res_years_2$ex_gap_m_res_percent2<-100-((lx_net_m_res_years_2$ex_gap_m_res2/lx_net_m_res_years_2$ex_diff2)*100)
View(lx_net_m_res_years_2)

# 3. estimating life years spent in each resuscitation state for the third mortality regime comparison 1901/1951

lx_net_m_res_3<-lx_net_m_full %>%
  mutate(res_1=lx_net_m_full[,4]*lx_net_m_full[,9], res_2=(lx_net_m_full[,4]*(lx_net_m_full[,9]^2))/factorial(2), res_3=(lx_net_m_full[,4]*(lx_net_m_full[,9]^3))/factorial(3),
         res_4=(lx_net_m_full[,4]*(lx_net_m_full[,9]^4))/factorial(4), res_5=(lx_net_m_full[,4]*(lx_net_m_full[,9]^5))/factorial(5),res_6=(lx_net_m_full[,4]*(lx_net_m_full[,9]^6))/factorial(6),
         res_7=(lx_net_m_full[,4]*(lx_net_m_full[,9]^7))/factorial(7), res_8=(lx_net_m_full[,4]*(lx_net_m_full[,9]^8))/factorial(8),res_9=(lx_net_m_full[,4]*(lx_net_m_full[,9]^9))/factorial(9),
         res_10=(lx_net_m_full[,4]*(lx_net_m_full[,9]^10))/factorial(10))


View(lx_net_m_res_3) 

lx_net_m_res_years_3<-lx_net_m_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_m_res_years_3<-lx_net_m_res_years_3%>% mutate(total=sum(lx_net_m_res_years_3[1,25:34]))

View(lx_net_m_res_years_3)

#adding life expectancies, estimating differences 
lx_net_m_res_years_3$ex_old3_m<-net_lt_m_ex_surv$`1901`
lx_net_m_res_years_3$ex_new3_m<-net_lt_m_ex_surv$`1951`

#mortality gap between regimes
lx_net_m_res_years_3$ex_diff3<-lx_net_m_res_years_3$ex_new3_m-lx_net_m_res_years_3$ex_old3_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_m_res_years_3$ex_gap_m_res3<-lx_net_m_res_years_3$ex_new3_m-(lx_net_m_res_years_3$ex_old3_m+lx_net_m_res_years_3$tau_1)
lx_net_m_res_years_3$ex_gap_m_res_percent3<-100-((lx_net_m_res_years_3$ex_gap_m_res3/lx_net_m_res_years_3$ex_diff3)*100)


View(lx_net_m_res_years_3)

# 4. estimating life years spent in each resuscitation state for the fourth mortality regime comparison - 1951/2016

lx_net_m_res_4<-lx_net_m_full %>%
  mutate(res_1=lx_net_m_full[,5]*lx_net_m_full[,10], res_2=(lx_net_m_full[,5]*(lx_net_m_full[,10]^2))/factorial(2), res_3=(lx_net_m_full[,5]*(lx_net_m_full[,10]^3))/factorial(3),
         res_4=(lx_net_m_full[,5]*(lx_net_m_full[,10]^4))/factorial(4), res_5=(lx_net_m_full[,5]*(lx_net_m_full[,10]^5))/factorial(5),res_6=(lx_net_m_full[,5]*(lx_net_m_full[,10]^6))/factorial(6),
         res_7=(lx_net_m_full[,5]*(lx_net_m_full[,10]^7))/factorial(7), res_8=(lx_net_m_full[,5]*(lx_net_m_full[,10]^8))/factorial(8),res_9=(lx_net_m_full[,5]*(lx_net_m_full[,10]^9))/factorial(9),
         res_10=(lx_net_m_full[,5]*(lx_net_m_full[,10]^10))/factorial(10))


View(lx_net_m_res_4)

lx_net_m_res_years_4<-lx_net_m_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_net_m_res_years_4<-lx_net_m_res_years_4%>% mutate(total=sum(lx_net_m_res_years_4[1,25:34]))

View(lx_net_m_res_years_4)

#adding life expectancies, estimating differences 
lx_net_m_res_years_4$ex_old4_m<-net_lt_m_ex_surv$`1951`
lx_net_m_res_years_4$ex_new4_m<-net_lt_m_ex_surv$`2016`

#mortality gap between regimes
lx_net_m_res_years_4$ex_diff4<-lx_net_m_res_years_4$ex_new4_m-lx_net_m_res_years_4$ex_old4_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_net_m_res_years_4$ex_gap_m_res4<-lx_net_m_res_years_4$ex_new4_m-(lx_net_m_res_years_4$ex_old4_m+lx_net_m_res_years_4$tau_1)
lx_net_m_res_years_4$ex_gap_m_res_percent4<-100-((lx_net_m_res_years_4$ex_gap_m_res4/lx_net_m_res_years_4$ex_diff4)*100)
View(lx_net_m_res_years_4)


### reshaping data

# 1. first only first regime

lx_net_m_res_years$Regime<-c("1850-1851")
View(lx_net_m_res_years)
lx_net_males_1<-lx_net_m_res_years%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_males_1)
lx_net_males_1_long<- gather (lx_net_males_1, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_net_males_1_long)

colnames(lx_net_males_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime

lx_net_m_res_years_2$Regime<-c("1851-1901")
View(lx_net_m_res_years_2)
lx_net_males_2<-lx_net_m_res_years_2%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_males_2)
lx_net_males_2_long<- gather (lx_net_males_2, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_net_males_2_long)

colnames(lx_net_males_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_net_m_res_years_3$Regime<-c("1901-1951")
View(lx_net_m_res_years_3)
lx_net_males_3<-lx_net_m_res_years_3%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_males_3)
lx_net_males_3_long<- gather (lx_net_males_3, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_net_males_3_long)
colnames(lx_net_males_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_net_m_res_years_4$Regime<-c("1951-2016")
View(lx_net_m_res_years_4)
lx_net_males_4<-lx_net_m_res_years_4%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_net_males_4)
lx_net_males_4_long<- gather (lx_net_males_4, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_net_males_4_long)
colnames(lx_net_males_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# combining everything
library(reshape)
library(reshape2)
resuscitated_males_netnet<- rbind(lx_net_males_1_long,lx_net_males_2_long,lx_net_males_3_long,lx_net_males_4_long)
View(resuscitated_males_netnet)

# ploting the hazards and tau큦
X11()
par(mfrow=c(1,2))
plot(x=c(1:length(lx_net_m_res_years_4$Age)), lx_net_m_res_years_4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab="Intensity of lifesaving ", xlab="Age", lwd=2, main="b. Males")
lines(x=c(1:length(lx_net_m_res_years_4$Age)), lx_net_m_res_years_4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_net_m_res_years_4$Age)), lx_net_m_res_years_4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_net_m_res_years_4$Age)), lx_net_m_res_years_4$hazard_4, lty=1,lwd=2, col="blue")
axis(1, seq(0,length(lx_net_m_res_years_4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1850-1851","1851-1901","1901-1951","1951-2016"),
       lty=c(1,5,3,1),col=c("black","black","black","blue"), bty="n")
abline(h=0,col="grey", lwd=2)

# force of mortality 
net_qx_m<-lt_m%>%
  filter(country %in% c("NLD"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1850","1851","1901","1951","2016"))%>%
  select(c("Year","Age","qx"))
View(net_qx_m) 

net_qx_m_wide<- net_qx_m %>% 
  spread(key=Year, value=qx)

X11()
pdf(file="prob_death_males_netnet.pdf",width=5.5,height=5.5)
plot(x=c(1:length(net_qx_m_wide$Age)),net_qx_m_wide$`1851`, ylim=c(0,1), type="l", axes=FALSE,
     ylab="Probability of death- qx", xlab="Age", lwd=1.5, main="b. Males")
lines(x=c(1:length(net_qx_m_wide$Age)), net_qx_m_wide$`1901`, lty=3,lwd=1.5)
lines(x=c(1:length(net_qx_m_wide$Age)), net_qx_m_wide$`1951`, lty=2,lwd=1.5)
lines(x=c(1:length(net_qx_m_wide$Age)), net_qx_m_wide$`2016`, lty=6,lwd=1.5)
axis(1, seq(0,length(net_qx_m_wide$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(0, 1, 0.1),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1851","1901","1951","2016"),
       lty=c(1,5,3,2,6), bty="n", cex = .8)
dev.off()


#ggplot for number of resuscitations and the number of resuscitated, males, netnet
library(forcats)
resuscitated_males_netnet$Resuscitations<-fct_collapse(resuscitated_males_netnet$Resuscitations,
                                                       res_1 = c("res_1"),
                                                       res_2 = c("res_2"),
                                                       res_3 = c("res_3"),
                                                       res_4 = c("res_4"),
                                                       res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_males_netnet$Resuscitations <- factor(resuscitated_males_netnet$Resuscitations, ordered = TRUE,
                                                   levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"))


# doing it for both females and males
resuscitated_females_netnet$Sex<-factor("Females")
resuscitated_males_netnet$Sex<-factor("Males")

resuscitated_all<-rbind (resuscitated_females_netnet,resuscitated_males_netnet)
View(resuscitated_all)

pdf(file="res_all_net.pdf",width=15,height=8)
X11(width=15,height=8)

ggplot(resuscitated_all %>% filter(Regime!="1850-1851"), aes(x = Age, y = Res_number, group=Resuscitations)) +
  geom_line(aes(linetype=Resuscitations))+ facet_grid(Sex~ Regime)+ 

  theme_bw()+scale_linetype_manual(name="Number of \nResuscitations",  values = c("res_1"="longdash",
                                                                                  "res_2"="solid",
                                                                                  "res_3"="dotdash",
                                                                                  "res_4"="dashed",
                                                                                  "res_5_plus"="dotted"), 
                                   labels=c("1", "2", "3","4","5+"))+
  ylab("Number of resuscitated persons")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()


#################################################
#### comparing gender gaps ######################
#################################################

# 1. females versus male regime 1850
# creating the variables for estimating the resuscitated

colnames (lx_net )<- c( "Age","fem_1850","fem_1851","fem_1901","fem_1951","fem_2016")
lx_net_compare<-cbind(lx_net,lx_net_m)
View(lx_net_compare)
lx_net_compare<-lx_net_compare[,-c(7)]
lx_net_compare_full<- lx_net_compare %>%
  mutate(hazard_1=log(lx_net_compare[,2]/lx_net_compare[,7]),hazard_2=log(lx_net_compare[,3]/lx_net_compare[,8]),hazard_3=log(lx_net_compare[,4]/lx_net_compare[,9]),
         hazard_4=log(lx_net_compare[,5]/lx_net_compare[,10]),hazard_5=log(lx_net_compare[,6]/lx_net_compare[,11]), change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1,change_5=exp(hazard_5)-1)

View(lx_net_compare_full)
# taking only the complete cases because since there are no survivors after age 104 na큦 are generated

lx_net_compare_full<-lx_net_compare_full[complete.cases(lx_net_compare_full), ]

# creating a datanetme for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison between sex 1850/1850 

lx_net_compare_res_1<-lx_net_compare_full %>%
  mutate(res_1=lx_net_compare_full[,7]*lx_net_compare_full[,12], res_2=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^2))/factorial(2), res_3=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^3))/factorial(3),
         res_4=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^4))/factorial(4), res_5=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^5))/factorial(5),res_6=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^6))/factorial(6),
         res_7=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^7))/factorial(7), res_8=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^8))/factorial(8),res_9=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^9))/factorial(9),
         res_10=(lx_net_compare_full[,7]*(lx_net_compare_full[,12]^10))/factorial(10))


View(lx_net_compare_res_1)
radix<-100000

lx_net_compare_res_years<-lx_net_compare_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_net_compare_res_years)
lx_net_compare_res_years<-lx_net_compare_res_years%>% mutate(total=sum(lx_net_compare_res_years[1,32:41]))

View(lx_net_compare_res_years)

#adding life expectancies, estimating differences 
lx_net_compare_res_years$ex_male<-net_lt_m_ex_surv$`1850`
lx_net_compare_res_years$ex_female<-net_lt_f_ex_surv$`1850`

#mortality gap between regimes
lx_net_compare_res_years$ex_diff_compare<-lx_net_compare_res_years$ex_female-lx_net_compare_res_years$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_net_compare_res_years$ex_gap_compare_res1<-lx_net_compare_res_years$ex_female-(lx_net_compare_res_years$ex_male+lx_net_compare_res_years$tau_1)
lx_net_compare_res_years$ex_gap_compare_res_percent<-100-((lx_net_compare_res_years$ex_gap_compare_res1/lx_net_compare_res_years$ex_diff_compare)*100)
View(lx_net_compare_res_years)


# 2. females versus male regime 1851

lx_net_compare_res_2<-lx_net_compare_full %>%
  mutate(res_1=lx_net_compare_full[,8]*lx_net_compare_full[,13], res_2=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^2))/factorial(2), res_3=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^3))/factorial(3),
         res_4=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^4))/factorial(4), res_5=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^5))/factorial(5),res_6=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^6))/factorial(6),
         res_7=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^7))/factorial(7), res_8=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^8))/factorial(8),res_9=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^9))/factorial(9),
         res_10=(lx_net_compare_full[,8]*(lx_net_compare_full[,13]^10))/factorial(10))


View(lx_net_compare_res_2)
radix<-100000

lx_net_compare_res_years2<-lx_net_compare_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_net_compare_res_years2)
lx_net_compare_res_years2<-lx_net_compare_res_years2%>% mutate(total=sum(lx_net_compare_res_years2[1,32:41]))

View(lx_net_compare_res_years2)

#adding life expectancies, estimating differences 
lx_net_compare_res_years2$ex_male<-net_lt_m_ex_surv$`1851`
lx_net_compare_res_years2$ex_female<-net_lt_f_ex_surv$`1851`

#mortality gap between regimes
lx_net_compare_res_years2$ex_diff_compare<-lx_net_compare_res_years2$ex_female-lx_net_compare_res_years2$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_net_compare_res_years2$ex_gap_compare_res1<-lx_net_compare_res_years2$ex_female-(lx_net_compare_res_years2$ex_male+lx_net_compare_res_years2$tau_1)
lx_net_compare_res_years2$ex_gap_compare_res_percent<-100-((lx_net_compare_res_years2$ex_gap_compare_res1/lx_net_compare_res_years2$ex_diff_compare)*100)
View(lx_net_compare_res_years2)

# 3. females versus male regime 1901

lx_net_compare_res_3<-lx_net_compare_full %>%
  mutate(res_1=lx_net_compare_full[,9]*lx_net_compare_full[,14], res_2=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^2))/factorial(2), res_3=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^3))/factorial(3),
         res_4=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^4))/factorial(4), res_5=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^5))/factorial(5),res_6=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^6))/factorial(6),
         res_7=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^7))/factorial(7), res_8=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^8))/factorial(8),res_9=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^9))/factorial(9),
         res_10=(lx_net_compare_full[,9]*(lx_net_compare_full[,14]^10))/factorial(10))


View(lx_net_compare_res_3)
radix<-100000

lx_net_compare_res_years3<-lx_net_compare_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_net_compare_res_years3)
lx_net_compare_res_years3<-lx_net_compare_res_years3%>% mutate(total=sum(lx_net_compare_res_years3[1,32:41]))

View(lx_net_compare_res_years3)

#adding life expectancies, estimating differences 
lx_net_compare_res_years3$ex_male<-net_lt_m_ex_surv$`1901`
lx_net_compare_res_years3$ex_female<-net_lt_f_ex_surv$`1901`

#mortality gap between regimes
lx_net_compare_res_years3$ex_diff_compare<-lx_net_compare_res_years3$ex_female-lx_net_compare_res_years3$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_net_compare_res_years3$ex_gap_compare_res1<-lx_net_compare_res_years3$ex_female-(lx_net_compare_res_years3$ex_male+lx_net_compare_res_years3$tau_1)
lx_net_compare_res_years3$ex_gap_compare_res_percent<-100-((lx_net_compare_res_years3$ex_gap_compare_res1/lx_net_compare_res_years3$ex_diff_compare)*100)
View(lx_net_compare_res_years3)

# 3. females versus male regime 1951
lx_net_compare_res_4<-lx_net_compare_full %>%
  mutate(res_1=lx_net_compare_full[,10]*lx_net_compare_full[,15], res_2=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^2))/factorial(2), res_3=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^3))/factorial(3),
         res_4=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^4))/factorial(4), res_5=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^5))/factorial(5),res_6=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^6))/factorial(6),
         res_7=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^7))/factorial(7), res_8=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^8))/factorial(8),res_9=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^9))/factorial(9),
         res_10=(lx_net_compare_full[,10]*(lx_net_compare_full[,15]^10))/factorial(10))


View(lx_net_compare_res_4)
radix<-100000

lx_net_compare_res_years4<-lx_net_compare_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_net_compare_res_years4)
lx_net_compare_res_years4<-lx_net_compare_res_years4%>% mutate(total=sum(lx_net_compare_res_years4[1,32:41]))

View(lx_net_compare_res_years4)

#adding life expectancies, estimating differences 

lx_net_compare_res_years4$ex_male<-net_lt_m_ex_surv$`1951`
lx_net_compare_res_years4$ex_female<-net_lt_f_ex_surv$`1951`

#mortality gap between regimes
lx_net_compare_res_years4$ex_diff_compare<-lx_net_compare_res_years4$ex_female-lx_net_compare_res_years4$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_net_compare_res_years4$ex_gap_compare_res1<-lx_net_compare_res_years4$ex_female-(lx_net_compare_res_years4$ex_male+lx_net_compare_res_years4$tau_1)
lx_net_compare_res_years4$ex_gap_compare_res_percent<-100-((lx_net_compare_res_years4$ex_gap_compare_res1/lx_net_compare_res_years4$ex_diff_compare)*100)
View(lx_net_compare_res_years4)

# 4. females versus male regime 2016

lx_net_compare_res_5<-lx_net_compare_full %>%
  mutate(res_1=lx_net_compare_full[,11]*lx_net_compare_full[,16], res_2=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^2))/factorial(2), res_3=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^3))/factorial(3),
         res_4=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^4))/factorial(4), res_5=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^5))/factorial(5),res_6=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^6))/factorial(6),
         res_7=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^7))/factorial(7), res_8=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^8))/factorial(8),res_9=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^9))/factorial(9),
         res_10=(lx_net_compare_full[,11]*(lx_net_compare_full[,16]^10))/factorial(10))


View(lx_net_compare_res_5)
radix<-100000

lx_net_compare_res_years5<-lx_net_compare_res_5%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_net_compare_res_years5)
lx_net_compare_res_years5<-lx_net_compare_res_years5%>% mutate(total=sum(lx_net_compare_res_years5[1,32:41]))

View(lx_net_compare_res_years5)

#adding life expectancies, estimating differences 
lx_net_compare_res_years5$ex_male<-net_lt_m_ex_surv$`2016`
lx_net_compare_res_years5$ex_female<-net_lt_f_ex_surv$`2016`

#mortality gap between regimes
lx_net_compare_res_years5$ex_diff_compare<-lx_net_compare_res_years5$ex_female-lx_net_compare_res_years5$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_net_compare_res_years5$ex_gap_compare_res1<-lx_net_compare_res_years5$ex_female-(lx_net_compare_res_years5$ex_male+lx_net_compare_res_years5$tau_1)
lx_net_compare_res_years5$ex_gap_compare_res_percent<-100-((lx_net_compare_res_years5$ex_gap_compare_res1/lx_net_compare_res_years5$ex_diff_compare)*100)



### reshaping data

# 1. first only first regime
lx_net_compare_res_years$Regime<-c("1850")
View(lx_net_compare_res_years)
lx_net_compare_1<-lx_net_compare_res_years%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_net_compare_1)
lx_net_compare_1_long<- gather (lx_net_compare_1, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_compare_1_long)

colnames(lx_net_compare_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4","hazard_5"  ,        
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime

lx_net_compare_res_years2$Regime<-c("1851")
View(lx_net_compare_res_years2)
lx_net_compare_2<-lx_net_compare_res_years2%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_net_compare_2)
lx_net_compare_2_long<- gather (lx_net_compare_2, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_compare_2_long)

colnames(lx_net_compare_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,"hazard_5",         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_net_compare_res_years3$Regime<-c("1901")
lx_net_compare_3<-lx_net_compare_res_years3%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_net_compare_3)
lx_net_compare_3_long<- gather (lx_net_compare_3, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_compare_3_long)
colnames(lx_net_compare_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4","hazard_5",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_net_compare_res_years4$Regime<-c("1951")
View(lx_net_compare_res_years4)
lx_net_compare_4<-lx_net_compare_res_years4%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_net_compare_4)
lx_net_compare_4_long<- gather (lx_net_compare_4, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_compare_4_long)
colnames(lx_net_compare_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4", "hazard_5",         
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )



# 5. fifth regime
lx_net_compare_res_years5$Regime<-c("2016")
View(lx_net_compare_res_years5)
lx_net_compare_5<-lx_net_compare_res_years5%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_net_compare_5)
lx_net_compare_5_long<- gather (lx_net_compare_5, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_net_compare_5_long)
colnames(lx_net_compare_5_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4", "hazard_5",         
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )






# combining everything
library(reshape)
library(reshape2)
resuscitated_compare_netnet<- rbind(lx_net_compare_2_long,lx_net_compare_3_long,lx_net_compare_4_long,lx_net_compare_5_long)
View(resuscitated_compare_netnet)

# ploting the hazards and tau큦
X11()
par(mfrow=c(1,2))
plot(x=c(1:length(lx_net_compare_res_years4$Age)), lx_net_compare_res_years4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab="Intensity of lifesaving ", xlab="Age", lwd=2)
lines(x=c(1:length(lx_net_compare_res_years4$Age)), lx_net_compare_res_years4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_net_compare_res_years4$Age)), lx_net_compare_res_years4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_net_compare_res_years4$Age)), lx_net_compare_res_years4$hazard_4, lty=2,lwd=2)
lines(x=c(1:length(lx_net_compare_res_years4$Age)), lx_net_compare_res_years4$hazard_5, lty=1,lwd=2)
axis(1, seq(0,length(lx_net_compare_res_years4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1850","1851","1901","1951","2016"),
       lty=c(1,5,3,2,1), bty="n")
abline(h=0,col="grey", lwd=2)



#ggplot for number of resuscitations and the number of resuscitated, males, netnet
library(forcats)
resuscitated_compare_netnet$Resuscitations<-fct_collapse(resuscitated_compare_netnet$Resuscitations,
                                                         res_1 = c("res_1"),
                                                         res_2 = c("res_2"),
                                                         res_3 = c("res_3"),
                                                         res_4 = c("res_4"),
                                                         res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_compare_netnet$Resuscitations <- factor(resuscitated_compare_netnet$Resuscitations, ordered = TRUE,
                                                     levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"))




pdf(file="res_compare_netnet2.pdf",width=15,height=8)
X11(width=15,height=8)
ggplot(resuscitated_compare_netnet %>% filter(Regime!="1850"), aes(x = Age, y = Res_number, group=Resuscitations)) +
  geom_line(aes(linetype=Resuscitations))+ facet_grid(~ Regime)+
  theme_bw()+scale_linetype_manual(name="Number of \nResuscitations",  values = c("res_1"="longdash",
                                                                                  "res_2"="solid",
                                                                                  "res_3"="dotdash",
                                                                                  "res_4"="dashed",
                                                                                  "res_5_plus"="dotted"), 
                                   labels=c("1", "2", "3","4","5+"))+
  ylab("Number of resuscitated persons")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()


# assesing differences in life expectancy and number of life years lived in each resuscitation state

View(lx_net_compare_res_years)

tau_compare_1<-lx_net_compare_res_years[1,32:48]
colnames(tau_compare_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_2<-lx_net_compare_res_years2[1,32:48]
colnames(tau_compare_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_3<-lx_net_compare_res_years3[1,32:48]
colnames(tau_compare_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_4<-lx_net_compare_res_years4[1,32:48]
colnames(tau_compare_4)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_5<-lx_net_compare_res_years5[1,32:48]
colnames(tau_compare_5)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")




tau_compare<-rbind(tau_compare_1,tau_compare_2, tau_compare_3,tau_compare_4,tau_compare_5)
View(tau_compare)

write.table(tau_compare, file="tau_compare_net.csv", sep=",", row.names = F) #saving for using excel as well

tau_compare_long <- gather(tau_compare, tau, life_years_res, tau_1:tau_10)

View(tau_compare_long)
tau_compare_prop<-tau_compare_long %>%
  group_by(tau)%>%
  mutate(prop_res=(life_years_res/total)*100)

View(tau_compare_prop)

tau_compare_prop$tau<-fct_collapse(tau_compare_prop$tau,
                                   tau_1 = c("tau_1"),
                                   tau_2 = c("tau_2"),
                                   tau_3 = c("tau_3"),
                                   tau_4 = c("tau_4"),
                                   tau_5_plus=c("tau_5","tau_6","tau_7","tau_8","tau_9","tau_10")
)
tau_compare_prop$tau <- factor(tau_compare_prop$tau, ordered = TRUE,
                               levels = c("tau_1", "tau_2", "tau_3","tau_4","tau_5_plus"))


library(latex2exp)
X11(width=10,height=10)

ggplot(data = tau_compare_prop %>% filter(Regime!="1850"), aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(color=Regime), size=1)+theme_bw()

