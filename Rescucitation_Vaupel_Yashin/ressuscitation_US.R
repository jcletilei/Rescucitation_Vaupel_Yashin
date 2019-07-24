library(MortalityLaws)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(plyr)
# Downloading some countries. We have to use the HMD codes
cntr <- c('SWE', "ITA","GBR_NP","DEUTNP","FRATNP","BEL",
          "NOR", "USA")

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
#### USA #################################
#############################################

# preparing data 

#females
lt_f<-LT_f$data

# retrieving the columns of interest
usa_lt_f<-lt_f%>%
  filter(country %in% c("USA"))%>%
  filter(Age<=100)%>%
  select(c("Year","Age","lx"))
View(usa_lt_f) 

# tranforming to wide format for calculations
usa_lt_f_surv <- usa_lt_f %>%
  spread(key=Year, value=lx)

View(usa_lt_f_surv)

#saving for later checking or performing simple tasks in excel
write.table(usa_lt_f_surv, file="usa_females_lx.csv", sep=",", row.names = F) #saving for using excel as well

#the same for life expectancy only
usa_lt_f_ex<-lt_f%>%
  filter(country %in% c("USA"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(usa_lt_f_ex) 


usa_lt_f_ex_surv <- usa_lt_f_ex %>%
  spread(key=Year, value=ex)


View(usa_lt_f_ex_surv)
write.table(usa_lt_f_ex_surv, file="usa_females_ex.csv", sep=",", row.names = F) #saving for using excel as well

# males

lt_m<-LT_m$data
usa_lt_m<-lt_m%>%
  filter(country %in% c("USA"))%>%
  select(c("Year","Age","lx"))
View(usa_lt_m) 


usa_lt_m_surv <- usa_lt_m %>%
  spread(key=Year, value=lx)


View(usa_lt_m_surv)
write.table(usa_lt_m_surv, file="usa_males_lx.csv", sep=",", row.names = F) #saving for using excel as well

#only ex
usa_lt_m_ex<-lt_m%>%
  filter(country %in% c("USA"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(usa_lt_m_ex) 


usa_lt_m_ex_surv <- usa_lt_m_ex %>%
  spread(key=Year, value=ex)


View(usa_lt_m_ex_surv)
write.table(usa_lt_m_ex_surv, file="usa_males_ex.csv", sep=",", row.names = F) #saving for using excel as well

###############################################
### Implementing Vaupel큦 Approach : FEMALES ##
###############################################

# selecting years for mortatliy regime comparison. IN the case of USA, we will perform 20-year analysis#
usa_lt_f<-lt_f%>%
  filter(country %in% c("USA"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1933","1973","2016"))%>%
  select(c("Year","Age","lx"))
View(usa_lt_f) 

# ploting survival curves

library(ggplot2)
X11(width=7,height=7)
females_surv<-ggplot(usa_lt_f, aes(x = Age, y = lx, group=factor(Year))) +
  geom_line(aes(linetype=factor(Year)))+ylim(1,100000)+
  theme_bw()+scale_linetype_manual(name="Year",  values = c("1933"="longdash",
                                                            "1973"="solid",
                                                            "2016"="dotted"), 
                                   labels=c("1933","1973","2016"))+
  ylab("lx")+annotate("text", x = c(60,80), y = c(50000,75000), label = c("1933","2016"))+ 
  theme(legend.position="none")+ labs(title="a. Females")

dev.off()
females_surv

# wide format for estimation

usa_lt_f_surv <- usa_lt_f %>%
  spread(key=Year, value=lx)

write.table(usa_lt_f_surv, file="usa_lt_f.csv", sep=",", row.names = F) #saving for using excel as well)
View(usa_lt_f_surv)

#only ex
usa_lt_f_ex<-lt_f%>%
  filter(country %in% c("USA"))%>%
  filter(Year %in% c("1933","1973","2016"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(usa_lt_f_ex) 




#wide format for estimation

usa_lt_f_ex_surv <- usa_lt_f_ex %>%
  spread(key=Year, value=ex)


View(usa_lt_f_ex_surv)

# taking the first row out because we are not dealing with the radix of the lifetable
lx_usa<-usa_lt_f_surv[-1,]


View(lx_usa)
# creating the variables for estimating the resuscitated

lx_usa_full<-lx_usa %>%
  mutate(hazard_1=log(lx_usa[,3]/lx_usa[,2]),
         hazard_2=log(lx_usa[,4]/lx_usa[,3]),
         change_1=exp(hazard_1)-1,
         change_2=exp(hazard_2)-1)

View(lx_usa_full)
# taking only the complete cases because since there are no survivors after age 104 na큦 are generated

lx_usa_full<-lx_usa_full[complete.cases(lx_usa_full), ]

# creating a dataframe for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison 1933/1973

lx_usa_res_1<-lx_usa_full %>%
  mutate(res_1=lx_usa_full[,2]*lx_usa_full[,5], res_2=(lx_usa_full[,2]*(lx_usa_full[,5]^2))/factorial(2), res_3=(lx_usa_full[,2]*(lx_usa_full[,5]^3))/factorial(3),
         res_4=(lx_usa_full[,2]*(lx_usa_full[,5]^4))/factorial(4), res_5=(lx_usa_full[,2]*(lx_usa_full[,5]^5))/factorial(5),res_6=(lx_usa_full[,2]*(lx_usa_full[,5]^6))/factorial(6),
         res_7=(lx_usa_full[,2]*(lx_usa_full[,5]^7))/factorial(7), res_8=(lx_usa_full[,2]*(lx_usa_full[,5]^8))/factorial(8),res_9=(lx_usa_full[,2]*(lx_usa_full[,5]^9))/factorial(9),
         res_10=(lx_usa_full[,2]*(lx_usa_full[,5]^10))/factorial(10))


View(lx_usa_res_1)
radix<-100000
write.table(lx_usa_res_1, file="lx_usa.csv", sep=",", row.names = F) #saving for using excel as well)

lx_usa_res_years<-lx_usa_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_usa_res_years<-lx_usa_res_years%>% mutate(total=sum(lx_usa_res_years[1,19:28]))

View(lx_usa_res_years)

#adding life expectancies, estimating differences 
lx_usa_res_years$ex_old<-usa_lt_f_ex_surv$`1933`
lx_usa_res_years$ex_new<-usa_lt_f_ex_surv$`1973`

#mortality gap between regimes
lx_usa_res_years$ex_diff<-lx_usa_res_years$ex_new-lx_usa_res_years$ex_old

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_res_years$ex_gap_res1<-lx_usa_res_years$ex_new-(lx_usa_res_years$ex_old+lx_usa_res_years$tau_1)
lx_usa_res_years$ex_gap_res_percent<-100-((lx_usa_res_years$ex_gap_res1/lx_usa_res_years$ex_diff)*100)


# 2. estimating life years spent in each resuscitation state for the second mortality regime comparison 1955/1975

lx_usa_res_2<-lx_usa_full %>%
  mutate(res_1=lx_usa_full[,3]*lx_usa_full[,6], res_2=(lx_usa_full[,3]*(lx_usa_full[,6]^2))/factorial(2), res_3=(lx_usa_full[,3]*(lx_usa_full[,6]^3))/factorial(3),
         res_4=(lx_usa_full[,3]*(lx_usa_full[,6]^4))/factorial(4), res_5=(lx_usa_full[,3]*(lx_usa_full[,6]^5))/factorial(5),res_6=(lx_usa_full[,3]*(lx_usa_full[,6]^6))/factorial(6),
         res_7=(lx_usa_full[,3]*(lx_usa_full[,6]^7))/factorial(7), res_8=(lx_usa_full[,3]*(lx_usa_full[,6]^8))/factorial(8),res_9=(lx_usa_full[,3]*(lx_usa_full[,6]^9))/factorial(9),
         res_10=(lx_usa_full[,3]*(lx_usa_full[,6]^10))/factorial(10))


View(lx_usa_res_2)

lx_usa_res_years_2<-lx_usa_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_usa_res_years_2<-lx_usa_res_years_2%>% mutate(total=sum(lx_usa_res_years_2[1,19:28]))

View(lx_usa_res_years_2)

#adding life expectancies, estimating differences 
lx_usa_res_years_2$ex_old2<-usa_lt_f_ex_surv$`1973`
lx_usa_res_years_2$ex_new2<-usa_lt_f_ex_surv$`2016`

#mortality gap between regimes
lx_usa_res_years_2$ex_diff2<-lx_usa_res_years_2$ex_new2-lx_usa_res_years_2$ex_old2

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_res_years_2$ex_gap_res2<-lx_usa_res_years_2$ex_new2-(lx_usa_res_years_2$ex_old2+lx_usa_res_years_2$tau_1)
lx_usa_res_years_2$ex_gap_res_percent2<-100-((lx_usa_res_years_2$ex_gap_res2/lx_usa_res_years_2$ex_diff2)*100)


### reshaping data

# 1. first only first regime
lx_usa_res_years$Regime<-c("1933-1973")
View(lx_usa_res_years)
lx_usa_females_1<-lx_usa_res_years%>%
  select(c(Age,hazard_1:hazard_2,res_1:Regime))
View(lx_usa_females_1)
lx_usa_females_1_long<- gather (lx_usa_females_1, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_females_1_long)

colnames(lx_usa_females_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime
lx_usa_res_years_2$Regime<-c("1973-2016")
View(lx_usa_res_years_2)
lx_usa_females_2<-lx_usa_res_years_2%>%
  select(c(Age,hazard_1:hazard_2,res_1:Regime))
View(lx_usa_females_2)
lx_usa_females_2_long<- gather (lx_usa_females_2, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_females_2_long)

colnames(lx_usa_females_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"  ,          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# combining everything
library(reshape)
library(reshape2)
resuscitated_females_usa<- rbind(lx_usa_females_1_long,lx_usa_females_2_long)
View(resuscitated_females_usa)
resuscitated_females_usa$Resuscitations<-as.factor(resuscitated_females_usa$Resuscitations)


# ploting the hazards and tau큦
X11(width=15,height=5.5)
par(mfrow=c(1,2))
plot(x=c(1:length(lx_usa_res_years_4$Age)), lx_usa_res_years_4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab="Intensity of lifesaving ", xlab="Age", lwd=2,main="a. Intensity of livesaving")
lines(x=c(1:length(lx_usa_res_years_4$Age)), lx_usa_res_years_4$hazard_2, lty=5,lwd=2)
axis(1, seq(0,length(lx_usa_res_years_4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)

legend("topleft", legend=c("1933-1973","1973-2016"),
       lty=c(1,5), bty="n")
abline(h=0, col="grey", lwd=2)

# force of mortality 
usa_qx_f<-lt_f%>%
  filter(country %in% c("USA"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1933","1973","2016"))%>%
  select(c("Year","Age","qx"))
View(usa_qx_f) 


usa_qx_f_wide<- usa_qx_f %>% 
  spread(key=Year, value=qx)
View(usa_qx_f_wide)
write.table(usa_qx_f_wide, file="usa_qx_f_wide.csv", sep=",", row.names = F) #saving for using excel as well)

X11()
pdf(file="prob_death_females_usa.pdf",width=5.5,height=5.5)
plot(x=c(1:length(usa_qx_f_wide$Age)),usa_qx_f_wide$`1933`, ylim=c(0,1), type="l", axes=FALSE,
     ylab="Probability of death- qx", xlab="Age", lwd=1.5, main="b. Probability of death")
lines(x=c(1:length(usa_qx_f_wide$Age)), usa_qx_f_wide$`1973`, lty=5,lwd=1.5)
lines(x=c(1:length(usa_qx_f_wide$Age)), usa_qx_f_wide$`2016`, lty=6,lwd=1.5)
axis(1, seq(0,length(usa_qx_f_wide$Age),5), las=1,cex.axis=.5, lwd=1.5)
axis(2, seq(0, 1, 0.1),lwd=1.5,cex.axis=.5, las=1)

legend("topleft", legend=c("1933","1973","2016"),
       lty=c(1,5,6), bty="n", cex = .5)
dev.off()


#ggplot for number of resuscitations and the number of resuscitated, females, usaden
library(forcats)
resuscitated_females_usa$Resuscitations<-fct_collapse(resuscitated_females_usa$Resuscitations,
                                                         res_1 = c("res_1"),
                                                         res_2 = c("res_2"),
                                                         res_3 = c("res_3"),
                                                         res_4 = c("res_4"),
                                                         res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_females_usa$Resuscitations <- factor(resuscitated_females_usa$Resuscitations, ordered = TRUE,
                                                     levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"))

pdf(file="res_females_usa.pdf",width=15,height=5.5)
X11(width=15,height=5.5)
ggplot(resuscitated_females_usa, aes(x = Age, y = Res_number, group=Resuscitations)) +
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


View(lx_usa_res_years)

tau_fem_1<-lx_usa_res_years[1,25:41]
colnames(tau_fem_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_2<-lx_usa_res_years_2[1,25:41]
colnames(tau_fem_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_3<-lx_usa_res_years_3[1,25:41]
colnames(tau_fem_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_4<-lx_usa_res_years_4[1,25:41]
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

ggplot(data = tau_fem_prop, aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(linetype=Regime), size=1)+theme_bw()

# if animation is interesting..

#p <- resuscitated_females_usaden %>%
# plot_ly(
#  x = ~Age, 
# y = ~Res_number, 
# color = ~Resuscitations, 
#  frame = ~Regime, 
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

# selecting years for mortatliy regime comparison. IN the case of usaden, we will perform 50-year analysis#
usa_lt_m<-lt_m%>%
  filter(country %in% c("USA"))%>%
  filter(Year %in% c("1933","1955","1975","1995","2016"))%>%
  filter(Age<=100) %>%
  select(c("Year","Age","lx"))
View(usa_lt_m) 

X11(width=7,height=7)
males_surv<-ggplot(usa_lt_m, aes(x = Age, y = lx, group=factor(Year))) +
  geom_line(aes(linetype=factor(Year)))+ ylim(1,100000)+
  theme_bw()+scale_linetype_manual(name="Year",  values = c("1933"="longdash",
                                                            "1955"="solid",
                                                            "1975"="dotdash",
                                                            "1995"="dashed",
                                                            "2016"="dotted"), 
                                   labels=c("1933","1955","1975","1995","2016"))+
  ylab("lx")+annotate("text", x = c(58,70), y = c(58000,80000), label = c("1933","2016"))+ labs(title="b. Males")

#if combining males and females in one plot
require(gridExtra)
X11(width=15,height=8)
grid.arrange(females_surv, males_surv, ncol=2)
dev.off

#wide format for estimation
usa_lt_m_surv <- usa_lt_m %>%
  spread(key=Year, value=lx)


View(usa_lt_m_surv)


#only ex
usa_lt_m_ex<-lt_m%>%
  filter(country %in% c("USA"))%>%
  filter(Year %in% c("1933","1955","1975","1995","2016"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(usa_lt_m_ex) 


usa_lt_m_ex_surv <- usa_lt_m_ex %>%
  spread(key=Year, value=ex)


View(usa_lt_m_ex_surv)

# taking the first row out because we are not dealing with the radix of the lifetable
lx_usa_m<-usa_lt_m_surv[-1,]

View(lx_usa_m)
# creating the variables for estimating the resuscitated

lx_usa_m_full<-lx_usa_m %>%
  mutate(hazard_1=log(lx_usa_m[,3]/lx_usa_m[,2]),hazard_2=log(lx_usa_m[,4]/lx_usa_m[,3]),hazard_3=log(lx_usa_m[,5]/lx_usa_m[,4]),
         hazard_4=log(lx_usa_m[,6]/lx_usa_m[,5]),change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1)

View(lx_usa_m_full)
# taking only the complete cases because since there are no survivors after age 104 na큦 are generated

lx_usa_m_full<-lx_usa_m_full[complete.cases(lx_usa_m_full), ]

# creating a dataframe for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison 1801/1851

lx_usa_m_res_1<-lx_usa_m_full %>%
  mutate(res_1=lx_usa_m_full[,2]*lx_usa_m_full[,7], res_2=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^2))/factorial(2), res_3=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^3))/factorial(3),
         res_4=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^4))/factorial(4), res_5=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^5))/factorial(5),res_6=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^6))/factorial(6),
         res_7=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^7))/factorial(7), res_8=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^8))/factorial(8),res_9=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^9))/factorial(9),
         res_10=(lx_usa_m_full[,2]*(lx_usa_m_full[,7]^10))/factorial(10))


View(lx_usa_m_res_1)
radix<-100000

lx_usa_m_res_years<-lx_usa_m_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_usa_m_res_years)
lx_usa_m_res_years<-lx_usa_m_res_years%>% mutate(total=sum(lx_usa_m_res_years[1,25:34]))

View(lx_usa_m_res_years)

#adding life expectancies, estimating differences 
lx_usa_m_res_years$ex_old_m<-usa_lt_m_ex_surv$`1933`
lx_usa_m_res_years$ex_new_m<-usa_lt_m_ex_surv$`1955`

#mortality gap between regimes
lx_usa_m_res_years$ex_diff<-lx_usa_m_res_years$ex_new_m-lx_usa_m_res_years$ex_old_m

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_m_res_years$ex_gap_m_res1<-lx_usa_m_res_years$ex_new_m-(lx_usa_m_res_years$ex_old_m+lx_usa_m_res_years$tau_1)
lx_usa_m_res_years$ex_gap_m_res_percent<-100-((lx_usa_m_res_years$ex_gap_m_res1/lx_usa_m_res_years$ex_diff)*100)
View(lx_usa_m_res_years)

# 2. estimating life years spent in each resuscitation state for the second mortality regime comparison 1851/1901

lx_usa_m_res_2<-lx_usa_m_full %>%
  mutate(res_1=lx_usa_m_full[,3]*lx_usa_m_full[,8], res_2=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^2))/factorial(2), res_3=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^3))/factorial(3),
         res_4=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^4))/factorial(4), res_5=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^5))/factorial(5),res_6=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^6))/factorial(6),
         res_7=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^7))/factorial(7), res_8=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^8))/factorial(8),res_9=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^9))/factorial(9),
         res_10=(lx_usa_m_full[,3]*(lx_usa_m_full[,8]^10))/factorial(10))


View(lx_usa_m_res_2)

lx_usa_m_res_years_2<-lx_usa_m_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_usa_m_res_years_2<-lx_usa_m_res_years_2%>% mutate(total=sum(lx_usa_m_res_years_2[1,25:34]))

View(lx_usa_m_res_years_2)

#adding life expectancies, estimating differences 
lx_usa_m_res_years_2$ex_old2_m<-usa_lt_m_ex_surv$`1955`
lx_usa_m_res_years_2$ex_new2_m<-usa_lt_m_ex_surv$`1975`

#mortality gap between regimes
lx_usa_m_res_years_2$ex_diff2<-lx_usa_m_res_years_2$ex_new2_m-lx_usa_m_res_years_2$ex_old2_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_m_res_years_2$ex_gap_m_res2<-lx_usa_m_res_years_2$ex_new2_m-(lx_usa_m_res_years_2$ex_old2_m+lx_usa_m_res_years_2$tau_1)
lx_usa_m_res_years_2$ex_gap_m_res_percent2<-100-((lx_usa_m_res_years_2$ex_gap_m_res2/lx_usa_m_res_years_2$ex_diff2)*100)
View(lx_usa_m_res_years_2)

# 3. estimating life years spent in each resuscitation state for the third mortality regime comparison 1901/1951

lx_usa_m_res_3<-lx_usa_m_full %>%
  mutate(res_1=lx_usa_m_full[,4]*lx_usa_m_full[,9], res_2=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^2))/factorial(2), res_3=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^3))/factorial(3),
         res_4=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^4))/factorial(4), res_5=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^5))/factorial(5),res_6=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^6))/factorial(6),
         res_7=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^7))/factorial(7), res_8=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^8))/factorial(8),res_9=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^9))/factorial(9),
         res_10=(lx_usa_m_full[,4]*(lx_usa_m_full[,9]^10))/factorial(10))


View(lx_usa_m_res_3) 

lx_usa_m_res_years_3<-lx_usa_m_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_usa_m_res_years_3<-lx_usa_m_res_years_3%>% mutate(total=sum(lx_usa_m_res_years_3[1,25:34]))

View(lx_usa_m_res_years_3)

#adding life expectancies, estimating differences 
lx_usa_m_res_years_3$ex_old3_m<-usa_lt_m_ex_surv$`1975`
lx_usa_m_res_years_3$ex_new3_m<-usa_lt_m_ex_surv$`1995`

#mortality gap between regimes
lx_usa_m_res_years_3$ex_diff3<-lx_usa_m_res_years_3$ex_new3_m-lx_usa_m_res_years_3$ex_old3_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_m_res_years_3$ex_gap_m_res3<-lx_usa_m_res_years_3$ex_new3_m-(lx_usa_m_res_years_3$ex_old3_m+lx_usa_m_res_years_3$tau_1)
lx_usa_m_res_years_3$ex_gap_m_res_percent3<-100-((lx_usa_m_res_years_3$ex_gap_m_res3/lx_usa_m_res_years_3$ex_diff3)*100)


View(lx_usa_m_res_years_3)

# 4. estimating life years spent in each resuscitation state for the fourth mortality regime comparison - 1951/2016

lx_usa_m_res_4<-lx_usa_m_full %>%
  mutate(res_1=lx_usa_m_full[,5]*lx_usa_m_full[,10], res_2=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^2))/factorial(2), res_3=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^3))/factorial(3),
         res_4=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^4))/factorial(4), res_5=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^5))/factorial(5),res_6=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^6))/factorial(6),
         res_7=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^7))/factorial(7), res_8=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^8))/factorial(8),res_9=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^9))/factorial(9),
         res_10=(lx_usa_m_full[,5]*(lx_usa_m_full[,10]^10))/factorial(10))


View(lx_usa_m_res_4)

lx_usa_m_res_years_4<-lx_usa_m_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_usa_m_res_years_4<-lx_usa_m_res_years_4%>% mutate(total=sum(lx_usa_m_res_years_4[1,25:34]))

View(lx_usa_m_res_years_4)

#adding life expectancies, estimating differences 
lx_usa_m_res_years_4$ex_old4_m<-usa_lt_m_ex_surv$`1995`
lx_usa_m_res_years_4$ex_new4_m<-usa_lt_m_ex_surv$`2016`

#mortality gap between regimes
lx_usa_m_res_years_4$ex_diff4<-lx_usa_m_res_years_4$ex_new4_m-lx_usa_m_res_years_4$ex_old4_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_m_res_years_4$ex_gap_m_res4<-lx_usa_m_res_years_4$ex_new4_m-(lx_usa_m_res_years_4$ex_old4_m+lx_usa_m_res_years_4$tau_1)
lx_usa_m_res_years_4$ex_gap_m_res_percent4<-100-((lx_usa_m_res_years_4$ex_gap_m_res4/lx_usa_m_res_years_4$ex_diff4)*100)
View(lx_usa_m_res_years_4)


### reshaping data

# 1. first only first regime

lx_usa_m_res_years$Regime<-c("1933-1955")
View(lx_usa_m_res_years)
lx_usa_males_1<-lx_usa_m_res_years%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_usa_males_1)
lx_usa_males_1_long<- gather (lx_usa_males_1, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_usa_males_1_long)

colnames(lx_usa_males_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime

lx_usa_m_res_years_2$Regime<-c("1955-1975")
View(lx_usa_m_res_years_2)
lx_usa_males_2<-lx_usa_m_res_years_2%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_usa_males_2)
lx_usa_males_2_long<- gather (lx_usa_males_2, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_usa_males_2_long)

colnames(lx_usa_males_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_usa_m_res_years_3$Regime<-c("1975-1995")
View(lx_usa_m_res_years_3)
lx_usa_males_3<-lx_usa_m_res_years_3%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_usa_males_3)
lx_usa_males_3_long<- gather (lx_usa_males_3, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_usa_males_3_long)
colnames(lx_usa_males_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_usa_m_res_years_4$Regime<-c("1995-2016")
View(lx_usa_m_res_years_4)
lx_usa_males_4<-lx_usa_m_res_years_4%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_usa_males_4)
lx_usa_males_4_long<- gather (lx_usa_males_4, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_usa_males_4_long)
colnames(lx_usa_males_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# combining everything
library(reshape)
library(reshape2)
resuscitated_males_usa<- rbind(lx_usa_males_1_long,lx_usa_males_2_long,lx_usa_males_3_long,lx_usa_males_4_long)
View(resuscitated_males_usa)

# ploting the hazards and tau큦
X11()
par(mfrow=c(1,2))
plot(x=c(1:length(lx_usa_m_res_years_4$Age)), lx_usa_m_res_years_4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab="Intensity of lifesaving ", xlab="Age", lwd=2, main="b. Males")
lines(x=c(1:length(lx_usa_m_res_years_4$Age)), lx_usa_m_res_years_4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_usa_m_res_years_4$Age)), lx_usa_m_res_years_4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_usa_m_res_years_4$Age)), lx_usa_m_res_years_4$hazard_4, lty=2,lwd=2)
axis(1, seq(0,length(lx_usa_m_res_years_4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1933-1955","1955-1975","1975-1995","1995-2016"),
       lty=c(1,5,3,2), bty="n")
abline(h=0,col="grey", lwd=2)

# force of mortality 
usa_qx_m<-lt_m%>%
  filter(country %in% c("USA"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1933","1955","1975","1995","2016"))%>%
  select(c("Year","Age","qx"))
View(usa_qx_m) 

usa_qx_m_wide<- usa_qx_m %>% 
  spread(key=Year, value=qx)

X11()
pdf(file="prob_death_males_usaden.pdf",width=5.5,height=5.5)
plot(x=c(1:length(usa_qx_m_wide$Age)),usa_qx_m_wide$`1933`, ylim=c(0,1), type="l", axes=FALSE,
     ylab="Probability of death- qx", xlab="Age", lwd=1.5, main="b. Males")
lines(x=c(1:length(usa_qx_m_wide$Age)), usa_qx_m_wide$`1955`, lty=5,lwd=1.5)
lines(x=c(1:length(usa_qx_m_wide$Age)), usa_qx_m_wide$`1975`, lty=3,lwd=1.5)
lines(x=c(1:length(usa_qx_m_wide$Age)), usa_qx_m_wide$`1995`, lty=2,lwd=1.5)
lines(x=c(1:length(usa_qx_m_wide$Age)), usa_qx_m_wide$`2016`, lty=6,lwd=1.5)
axis(1, seq(0,length(usa_qx_m_wide$Age),5), las=1,cex.axis=.5, lwd=1.5)
axis(2, seq(0, 1, 0.1),lwd=1.5,cex.axis=.5, las=1)
legend("topleft", legend=c("1933","1955","1975","1995","2016"),
       lty=c(1,5,3,2,6), bty="n", cex = .5)
dev.off()


#ggplot for number of resuscitations and the number of resuscitated, males, usaden
library(forcats)
resuscitated_males_usa$Resuscitations<-fct_collapse(resuscitated_males_usa$Resuscitations,
                                                       res_1 = c("res_1"),
                                                       res_2 = c("res_2"),
                                                       res_3 = c("res_3"),
                                                       res_4 = c("res_4"),
                                                       res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_males_usa$Resuscitations <- factor(resuscitated_males_usa$Resuscitations, ordered = TRUE,
                                                   levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"))


# doing it for both females and males
resuscitated_females_usa$Sex<-factor("Females")
resuscitated_males_usa$Sex<-factor("Males")

resuscitated_all<-rbind (resuscitated_females_usa,resuscitated_males_usa)
View(resuscitated_all)

pdf(file="res_all_usaden.pdf",width=15,height=8)
X11(width=15,height=8)
ggplot(resuscitated_all, aes(x = Age, y = Res_number, group=Resuscitations)) +
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

# 1. females versus male regime 1801
# creating the variables for estimating the resuscitated

colnames (lx_usa )<- c( "Age","fem_1933","fem_1955","fem_1975","fem_1995","fem_2016")
lx_usa_compare<-cbind(lx_usa,lx_usa_m)
View(lx_usa_compare)
lx_usa_compare<-lx_usa_compare[,-c(7)]
lx_usa_compare_full<- lx_usa_compare %>%
  mutate(hazard_1=log(lx_usa_compare[,2]/lx_usa_compare[,7]),hazard_2=log(lx_usa_compare[,3]/lx_usa_compare[,8]),hazard_3=log(lx_usa_compare[,4]/lx_usa_compare[,9]),
         hazard_4=log(lx_usa_compare[,5]/lx_usa_compare[,10]),hazard_5=log(lx_usa_compare[,6]/lx_usa_compare[,11]), change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1,change_5=exp(hazard_5)-1)

View(lx_usa_compare_full)
# taking only the complete cases because since there are no survivors after age 104 na큦 are generated

lx_usa_compare_full<-lx_usa_compare_full[complete.cases(lx_usa_compare_full), ]

# creating a dataframe for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison between sex 1801/1801 

lx_usa_compare_res_1<-lx_usa_compare_full %>%
  mutate(res_1=lx_usa_compare_full[,7]*lx_usa_compare_full[,12], res_2=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^2))/factorial(2), res_3=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^3))/factorial(3),
         res_4=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^4))/factorial(4), res_5=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^5))/factorial(5),res_6=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^6))/factorial(6),
         res_7=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^7))/factorial(7), res_8=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^8))/factorial(8),res_9=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^9))/factorial(9),
         res_10=(lx_usa_compare_full[,7]*(lx_usa_compare_full[,12]^10))/factorial(10))


View(lx_usa_compare_res_1)
radix<-100000

lx_usa_compare_res_years<-lx_usa_compare_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_usa_compare_res_years)
lx_usa_compare_res_years<-lx_usa_compare_res_years%>% mutate(total=sum(lx_usa_compare_res_years[1,32:41]))

View(lx_usa_compare_res_years)

#adding life expectancies, estimating differences 
lx_usa_compare_res_years$ex_male<-usa_lt_m_ex_surv$`1933`
lx_usa_compare_res_years$ex_female<-usa_lt_f_ex_surv$`1933`

#mortality gap between regimes
lx_usa_compare_res_years$ex_diff_compare<-lx_usa_compare_res_years$ex_female-lx_usa_compare_res_years$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_compare_res_years$ex_gap_compare_res1<-lx_usa_compare_res_years$ex_female-(lx_usa_compare_res_years$ex_male+lx_usa_compare_res_years$tau_1)
lx_usa_compare_res_years$ex_gap_compare_res_percent<-100-((lx_usa_compare_res_years$ex_gap_compare_res1/lx_usa_compare_res_years$ex_diff_compare)*100)
View(lx_usa_compare_res_years)


# 2. females versus male regime 1851

lx_usa_compare_res_2<-lx_usa_compare_full %>%
  mutate(res_1=lx_usa_compare_full[,8]*lx_usa_compare_full[,13], res_2=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^2))/factorial(2), res_3=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^3))/factorial(3),
         res_4=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^4))/factorial(4), res_5=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^5))/factorial(5),res_6=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^6))/factorial(6),
         res_7=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^7))/factorial(7), res_8=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^8))/factorial(8),res_9=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^9))/factorial(9),
         res_10=(lx_usa_compare_full[,8]*(lx_usa_compare_full[,13]^10))/factorial(10))


View(lx_usa_compare_res_2)
radix<-100000

lx_usa_compare_res_years2<-lx_usa_compare_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_usa_compare_res_years2)
lx_usa_compare_res_years2<-lx_usa_compare_res_years2%>% mutate(total=sum(lx_usa_compare_res_years2[1,32:41]))

View(lx_usa_compare_res_years2)

#adding life expectancies, estimating differences 
lx_usa_compare_res_years2$ex_male<-usa_lt_m_ex_surv$`1955`
lx_usa_compare_res_years2$ex_female<-usa_lt_f_ex_surv$`1955`

#mortality gap between regimes
lx_usa_compare_res_years2$ex_diff_compare<-lx_usa_compare_res_years2$ex_female-lx_usa_compare_res_years2$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_compare_res_years2$ex_gap_compare_res1<-lx_usa_compare_res_years2$ex_female-(lx_usa_compare_res_years2$ex_male+lx_usa_compare_res_years2$tau_1)
lx_usa_compare_res_years2$ex_gap_compare_res_percent<-100-((lx_usa_compare_res_years2$ex_gap_compare_res1/lx_usa_compare_res_years2$ex_diff_compare)*100)
View(lx_usa_compare_res_years2)

# 3. females versus male regime 1901

lx_usa_compare_res_3<-lx_usa_compare_full %>%
  mutate(res_1=lx_usa_compare_full[,9]*lx_usa_compare_full[,14], res_2=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^2))/factorial(2), res_3=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^3))/factorial(3),
         res_4=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^4))/factorial(4), res_5=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^5))/factorial(5),res_6=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^6))/factorial(6),
         res_7=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^7))/factorial(7), res_8=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^8))/factorial(8),res_9=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^9))/factorial(9),
         res_10=(lx_usa_compare_full[,9]*(lx_usa_compare_full[,14]^10))/factorial(10))


View(lx_usa_compare_res_3)
radix<-100000

lx_usa_compare_res_years3<-lx_usa_compare_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_usa_compare_res_years3)
lx_usa_compare_res_years3<-lx_usa_compare_res_years3%>% mutate(total=sum(lx_usa_compare_res_years3[1,32:41]))

View(lx_usa_compare_res_years3)

#adding life expectancies, estimating differences 
lx_usa_compare_res_years3$ex_male<-usa_lt_m_ex_surv$`1975`
lx_usa_compare_res_years3$ex_female<-usa_lt_f_ex_surv$`1975`

#mortality gap between regimes
lx_usa_compare_res_years3$ex_diff_compare<-lx_usa_compare_res_years3$ex_female-lx_usa_compare_res_years3$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_compare_res_years3$ex_gap_compare_res1<-lx_usa_compare_res_years3$ex_female-(lx_usa_compare_res_years3$ex_male+lx_usa_compare_res_years3$tau_1)
lx_usa_compare_res_years3$ex_gap_compare_res_percent<-100-((lx_usa_compare_res_years3$ex_gap_compare_res1/lx_usa_compare_res_years3$ex_diff_compare)*100)
View(lx_usa_compare_res_years3)

# 3. females versus male regime 1951
lx_usa_compare_res_4<-lx_usa_compare_full %>%
  mutate(res_1=lx_usa_compare_full[,10]*lx_usa_compare_full[,15], res_2=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^2))/factorial(2), res_3=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^3))/factorial(3),
         res_4=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^4))/factorial(4), res_5=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^5))/factorial(5),res_6=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^6))/factorial(6),
         res_7=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^7))/factorial(7), res_8=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^8))/factorial(8),res_9=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^9))/factorial(9),
         res_10=(lx_usa_compare_full[,10]*(lx_usa_compare_full[,15]^10))/factorial(10))


View(lx_usa_compare_res_4)
radix<-100000

lx_usa_compare_res_years4<-lx_usa_compare_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_usa_compare_res_years4)
lx_usa_compare_res_years4<-lx_usa_compare_res_years4%>% mutate(total=sum(lx_usa_compare_res_years4[1,32:41]))

View(lx_usa_compare_res_years4)

#adding life expectancies, estimating differences 

lx_usa_compare_res_years4$ex_male<-usa_lt_m_ex_surv$`1995`
lx_usa_compare_res_years4$ex_female<-usa_lt_f_ex_surv$`1995`

#mortality gap between regimes
lx_usa_compare_res_years4$ex_diff_compare<-lx_usa_compare_res_years4$ex_female-lx_usa_compare_res_years4$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_compare_res_years4$ex_gap_compare_res1<-lx_usa_compare_res_years4$ex_female-(lx_usa_compare_res_years4$ex_male+lx_usa_compare_res_years4$tau_1)
lx_usa_compare_res_years4$ex_gap_compare_res_percent<-100-((lx_usa_compare_res_years4$ex_gap_compare_res1/lx_usa_compare_res_years4$ex_diff_compare)*100)
View(lx_usa_compare_res_years4)

# 4. females versus male regime 2016

lx_usa_compare_res_5<-lx_usa_compare_full %>%
  mutate(res_1=lx_usa_compare_full[,11]*lx_usa_compare_full[,16], res_2=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^2))/factorial(2), res_3=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^3))/factorial(3),
         res_4=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^4))/factorial(4), res_5=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^5))/factorial(5),res_6=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^6))/factorial(6),
         res_7=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^7))/factorial(7), res_8=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^8))/factorial(8),res_9=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^9))/factorial(9),
         res_10=(lx_usa_compare_full[,11]*(lx_usa_compare_full[,16]^10))/factorial(10))


View(lx_usa_compare_res_5)
radix<-100000

lx_usa_compare_res_years5<-lx_usa_compare_res_5%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_usa_compare_res_years5)
lx_usa_compare_res_years5<-lx_usa_compare_res_years5%>% mutate(total=sum(lx_usa_compare_res_years5[1,32:41]))

View(lx_usa_compare_res_years5)

#adding life expectancies, estimating differences 
lx_usa_compare_res_years5$ex_male<-usa_lt_m_ex_surv$`2016`
lx_usa_compare_res_years5$ex_female<-usa_lt_f_ex_surv$`2016`

#mortality gap between regimes
lx_usa_compare_res_years5$ex_diff_compare<-lx_usa_compare_res_years5$ex_female-lx_usa_compare_res_years5$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_usa_compare_res_years5$ex_gap_compare_res1<-lx_usa_compare_res_years5$ex_female-(lx_usa_compare_res_years5$ex_male+lx_usa_compare_res_years5$tau_1)
lx_usa_compare_res_years5$ex_gap_compare_res_percent<-100-((lx_usa_compare_res_years5$ex_gap_compare_res1/lx_usa_compare_res_years5$ex_diff_compare)*100)



### reshaping data

# 1. first only first regime
lx_usa_compare_res_years$Regime<-c("1933")
View(lx_usa_compare_res_years)
lx_usa_compare_1<-lx_usa_compare_res_years%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_usa_compare_1)
lx_usa_compare_1_long<- gather (lx_usa_compare_1, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_compare_1_long)

colnames(lx_usa_compare_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4","hazard_5"  ,        
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime

lx_usa_compare_res_years2$Regime<-c("1955")
View(lx_usa_compare_res_years2)
lx_usa_compare_2<-lx_usa_compare_res_years2%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_usa_compare_2)
lx_usa_compare_2_long<- gather (lx_usa_compare_2, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_compare_2_long)

colnames(lx_usa_compare_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,"hazard_5",         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_usa_compare_res_years3$Regime<-c("1975")
lx_usa_compare_3<-lx_usa_compare_res_years3%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_usa_compare_3)
lx_usa_compare_3_long<- gather (lx_usa_compare_3, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_compare_3_long)
colnames(lx_usa_compare_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4","hazard_5",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_usa_compare_res_years4$Regime<-c("1995")
View(lx_usa_compare_res_years4)
lx_usa_compare_4<-lx_usa_compare_res_years4%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_usa_compare_4)
lx_usa_compare_4_long<- gather (lx_usa_compare_4, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_compare_4_long)
colnames(lx_usa_compare_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4", "hazard_5",         
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )



# 5. fifth regime
lx_usa_compare_res_years5$Regime<-c("2016")
View(lx_usa_compare_res_years5)
lx_usa_compare_5<-lx_usa_compare_res_years5%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_usa_compare_5)
lx_usa_compare_5_long<- gather (lx_usa_compare_5, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_usa_compare_5_long)
colnames(lx_usa_compare_5_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4", "hazard_5",         
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )






# combining everything
library(reshape)
library(reshape2)
resuscitated_compare_usa<- rbind(lx_usa_compare_2_long,lx_usa_compare_3_long,lx_usa_compare_4_long,lx_usa_compare_5_long)
View(resuscitated_compare_usa)

# ploting the hazards and tau큦
X11()
par(mfrow=c(1,2))
plot(x=c(1:length(lx_usa_compare_res_years4$Age)), lx_usa_compare_res_years4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab="Intensity of lifesaving ", xlab="Age", lwd=2)
lines(x=c(1:length(lx_usa_compare_res_years4$Age)), lx_usa_compare_res_years4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_usa_compare_res_years4$Age)), lx_usa_compare_res_years4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_usa_compare_res_years4$Age)), lx_usa_compare_res_years4$hazard_4, lty=2,lwd=2)
lines(x=c(1:length(lx_usa_compare_res_years4$Age)), lx_usa_compare_res_years4$hazard_5, lty=1,lwd=2)
axis(1, seq(0,length(lx_usa_compare_res_years4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1933","1955","1975","1995","2016"),
       lty=c(1,5,3,2,1), bty="n")
abline(h=0,col="grey", lwd=2)



#ggplot for number of resuscitations and the number of resuscitated, males, usaden
library(forcats)
resuscitated_compare_usa$Resuscitations<-fct_collapse(resuscitated_compare_usa$Resuscitations,
                                                         res_1 = c("res_1"),
                                                         res_2 = c("res_2"),
                                                         res_3 = c("res_3"),
                                                         res_4 = c("res_4"),
                                                         res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_compare_usa$Resuscitations <- factor(resuscitated_compare_usa$Resuscitations, ordered = TRUE,
                                                     levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"))



pdf(file="res_compare_usaden2.pdf",width=15,height=8)
X11(width=15,height=8)
ggplot(resuscitated_compare_usa, aes(x = Age, y = Res_number, group=Resuscitations)) +
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

View(lx_usa_compare_res_years)

tau_compare_1<-lx_usa_compare_res_years[1,32:48]
colnames(tau_compare_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_2<-lx_usa_compare_res_years2[1,32:48]
colnames(tau_compare_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_3<-lx_usa_compare_res_years3[1,32:48]
colnames(tau_compare_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_4<-lx_usa_compare_res_years4[1,32:48]
colnames(tau_compare_4)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_5<-lx_usa_compare_res_years5[1,32:48]
colnames(tau_compare_5)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")




tau_compare<-rbind(tau_compare_1,tau_compare_2, tau_compare_3,tau_compare_4,tau_compare_5)
View(tau_compare)

write.table(tau_compare, file="tau_compare_usa.csv", sep=",", row.names = F) #saving for using excel as well

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

ggplot(data = tau_compare_prop, aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(linetype=Regime), size=1)+theme_bw()

#assessing the differences in life expectancy






# differences between 









