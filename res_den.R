library(MortalityLaws)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(plyr)
library(data.table)
library(purrr)
#reading in the life tables in single ages, by single-year periods
# females
cntr <- c("SWE", "DNK","FRATNP","NOR", "NLD")

LT_f<- ReadHMD(what = "LT_f",
               countries = "DNK",
               interval = "1x1",
               username = "vdilego@gmail.com",
               password = "588672vitor",save = FALSE)


ls(LT_f)
LT_f

#males

LT_m<- ReadHMD(what = "LT_m",
               countries = "DNK",
               interval = "1x1",
               username = "vdilego@gmail.com",
               password = "588672vitor",save = FALSE)

ls(LT_m)
LT_m



#############################################
#### Denmark #################################
#############################################

# preparing data 

#females
lt_f<-LT_f$data
setDT(lt_f)
# retrieving the columns of interest
View(lt_f)
den_lt_f <- lt_f[, .(Year, Age,ax, lx, dx, ex)]
View(den_lt_f) 


#Denmark is from 1835 to 2016. Separating into 50-year regimes

den_lt_f<-den_lt_f%>%
  filter(Year %in% c("1835","1851","1901","1951","2016")) %>% 
  filter(Age<= 100 )
View(den_lt_f)  
setDT(den_lt_f)

# estimate the hazard or intensity of lifesaving
den_lt_f<-den_lt_f[, hazard := log((lx/shift(lx))), by = .(Age)]  #using shift to calculate one year-the first

#assigning zero to na´s. The na´s here are due to the shifting columns. They are non-informative
den_lt_f[is.na(den_lt_f)] <- 0

#set key to perform operations correctly
setkey(den_lt_f, Year,Age)
View(den_lt_f)


#estimating the resuscitations times- function for averting deaths up to 10 times

res_all<-function (x,y,z) {
  fact_res<-(lag(x)*(y^z))/factorial(z)
  return(fact_res)
  
}

# check out the function

den2

# applying formula of states of rescusitation
den_lt_f2<-den_lt_f[ , res_1:= res_all(lx,hazard,1), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_2:= res_all(lx,hazard,2), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_3:= res_all(lx,hazard,3), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_4:= res_all(lx,hazard,4), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_5:= res_all(lx,hazard,5), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_6:= res_all(lx,hazard,6), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_7:= res_all(lx,hazard,7), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_8:= res_all(lx,hazard,8), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_9:= res_all(lx,hazard,9), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_10:= res_all(lx,hazard,10), by=.(Age)]

View(den_lt_f2)
#taking the old lx out, no worries, the sum will put it back in
#den_lt_f2 <- den_lt_f2[complete.cases(den_lt_f2), ] 

# summing everything and checking that the sum is equal to the difference in survivorship
den_lt_f2<-den_lt_f2[, sum_res := rowSums(.SD), .SDcols = 8:17, by=.(Age,Year)]
write.table(den_lt_f2, file="den_fem_res.csv", sep=",", row.names = F)

# putting everything in long format
den_lt_f2_long = melt(den_lt_f2, id.vars = c("Year", "Age","ax","lx","dx","ex","hazard"),
             measure.vars = c("res_1","res_2","res_3","res_4","res_5",
                              "res_6","res_7","res_8","res_9","res_10"), 
             variable.name = "times", value.name = "res")
View(den_lt_f2_long)
#den_lt_f2_long$times<-as.character(as.numeric(den_lt_f2_long$times))

# now estimating the tau´s
tau_all<-function (x) {
  radix<-100000
  tau<-sum(x/radix)
  return(tau)
  
}

# setting taus
den_lt_f2_long<-den_lt_f2_long[ , tau:= tau_all(res), by=.(Year, times)]
View(den_lt_f2_long)
den_lt_f2_long<-den_lt_f2_long[ , ex0:= .SD[1,ex], by=.(Year)]

den_lt_f2_long[is.na(den_lt_f2_long)] <- 0
# summing old survivorship plus res states
# now estimating the tau´s
lx_all<-function (x,y) {
  lx_prop<-lag(x)+(y)
  return(lx_prop)
  
}

den_lt_f2_long<-den_lt_f2_long[ , lx_res:= lx_all(lx,res), by=.(times,Age)]
den_lt_f2_long[is.na(den_lt_f2_long)] <- 0
View(den_lt_f2_long)

#dx res
dx_res<-function (x,y,z) {
  dx_res<-(lag(x)*(y^z))/factorial(z)
  return(fact_res)
  
}

# applying formula of states of rescusitation
den_lt_f2<-den_lt_f[ , res_1:= res_all(lx,hazard,1), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_2:= res_all(lx,hazard,2), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_3:= res_all(lx,hazard,3), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_4:= res_all(lx,hazard,4), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_5:= res_all(lx,hazard,5), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_6:= res_all(lx,hazard,6), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_7:= res_all(lx,hazard,7), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_8:= res_all(lx,hazard,8), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_9:= res_all(lx,hazard,9), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_10:= res_all(lx,hazard,10), by=.(Age)]

View(den_lt_f2)




# applying formula of states of rescusitation
den_lt_f2_long<-den_lt_f2_long[ , eddager_1:= ed(ex,dx,ax), by=.(Year,times)]

den_lt_f2<-den_lt_f[ , res_2:= res_all(lx,hazard,2), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_3:= res_all(lx,hazard,3), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_4:= res_all(lx,hazard,4), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_5:= res_all(lx,hazard,5), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_6:= res_all(lx,hazard,6), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_7:= res_all(lx,hazard,7), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_8:= res_all(lx,hazard,8), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_9:= res_all(lx,hazard,9), by=.(Age)]
den_lt_f2<-den_lt_f[ , res_10:= res_all(lx,hazard,10), by=.(Age)]






#the same for life expectancy only
den_lt_f_ex<-lt_f%>%
  filter(country %in% c("DNK"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(den_lt_f_ex) 


den_lt_f_ex_surv <- den_lt_f_ex %>%
  spread(key=Year, value=ex)


View(den_lt_f_ex_surv)
write.table(den_lt_f_ex_surv, file="den_females_ex.csv", sep=",", row.names = F) #saving for using excel as well

# males

lt_m<-LT_m$data
den_lt_m<-lt_m%>%
  filter(country %in% c("DNK"))%>%
  select(c("Year","Age","lx"))
View(den_lt_m) 


den_lt_m_surv <- den_lt_m %>%
  spread(key=Year, value=lx)


View(den_lt_m_surv)
write.table(den_lt_m_surv, file="den_males_lx.csv", sep=",", row.names = F) #saving for using excel as well

#only ex
den_lt_m_ex<-lt_m%>%
  filter(country %in% c("DNK"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(den_lt_m_ex) 


den_lt_m_ex_surv <- den_lt_m_ex %>%
  spread(key=Year, value=ex)


View(den_lt_m_ex_surv)
write.table(den_lt_m_ex_surv, file="den_males_ex.csv", sep=",", row.names = F) #saving for using excel as well

###############################################
### Implementing Vaupel?s Approach : FEMALES ##
###############################################

# selecting years for mortatliy regime comparison. IN the case of Sweden, we will perform 50-year analysis#
den_lt_f<-lt_f%>%
  filter(country %in% c("DNK"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1835","1851","1901","1951","2016"))%>%
  select(c("Year","Age","lx"))
View(den_lt_f) 

# ploting survival curves

library(ggplot2)
X11(width=7,height=7)
surv_fem_den<-ggplot(den_lt_f %>% filter(Year!="1835"), aes(x = Age, y = lx, group=factor(Year))) +
  geom_line(aes(linetype=factor(Year)))+ ylim(1,100000)+ggtitle("a.Females")+
  theme_bw()+scale_linetype_manual(name="Year",  values = c(
                                                            "1851"="solid",
                                                            "1901"="dotdash",
                                                            "1951"="dashed",
                                                            "2016"="dotted"), 
                                   labels=c("1851","1901","1951","2016"))+
  ylab("lx")+annotate("text", x = c(57,70,70,80), y = c(45000,52000,80000,90000), label = c("1851","1901","1951","2016"))

dev.off()


# wide format for estimation

den_lt_f_surv <- den_lt_f %>%
  spread(key=Year, value=lx)


View(den_lt_f_surv)

#only ex
den_lt_f_ex<-lt_f%>%
  filter(country %in% c("DNK"))%>%
  filter(Year %in% c("1835","1851","1901","1951","2016"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(den_lt_f_ex) 




#wide format for estimation

den_lt_f_ex_surv <- den_lt_f_ex %>%
  spread(key=Year, value=ex)


View(den_lt_f_ex_surv)

# taking the first row out because we are not dealing with the radix of the lifetable
lx_den<-den_lt_f_surv[-1,]


View(lx_den)
# creating the variables for estimating the resuscitated

lx_den_full<-lx_den %>%
  mutate(hazard_1=log(lx_den[,3]/lx_den[,2]),hazard_2=log(lx_den[,4]/lx_den[,3]),hazard_3=log(lx_den[,5]/lx_den[,4]),
         hazard_4=log(lx_den[,6]/lx_den[,5]),change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1)

View(lx_den_full)
# taking only the complete cases because since there are no survivors after age 104 na?s are generated

lx_den_full<-lx_den_full[complete.cases(lx_den_full), ]

# creating a datadenme for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison 1835/1851

lx_den_res_1<-lx_den_full %>%
  mutate(res_1=lx_den_full[,2]*lx_den_full[,7], res_2=(lx_den_full[,2]*(lx_den_full[,7]^2))/factorial(2), res_3=(lx_den_full[,2]*(lx_den_full[,7]^3))/factorial(3),
         res_4=(lx_den_full[,2]*(lx_den_full[,7]^4))/factorial(4), res_5=(lx_den_full[,2]*(lx_den_full[,7]^5))/factorial(5),res_6=(lx_den_full[,2]*(lx_den_full[,7]^6))/factorial(6),
         res_7=(lx_den_full[,2]*(lx_den_full[,7]^7))/factorial(7), res_8=(lx_den_full[,2]*(lx_den_full[,7]^8))/factorial(8),res_9=(lx_den_full[,2]*(lx_den_full[,7]^9))/factorial(9),
         res_10=(lx_den_full[,2]*(lx_den_full[,7]^10))/factorial(10))


View(lx_den_res_1)
radix<-100000
write.table(lx_den_res_1, file="lx_den.csv", sep=",", row.names = F) #saving for using excel as well)

lx_den_res_years<-lx_den_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_res_years<-lx_den_res_years%>% mutate(total=sum(lx_den_res_years[1,25:34]))

View(lx_den_res_years)

#adding life expectancies, estimating differences 
lx_den_res_years$ex_old<-den_lt_f_ex_surv$`1835`
lx_den_res_years$ex_new<-den_lt_f_ex_surv$`1851`

#mortality gap between regimes
lx_den_res_years$ex_diff<-lx_den_res_years$ex_new-lx_den_res_years$ex_old

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_res_years$ex_gap_res1<-lx_den_res_years$ex_new-(lx_den_res_years$ex_old+lx_den_res_years$tau_1)
lx_den_res_years$ex_gap_res_percent<-100-((lx_den_res_years$ex_gap_res1/lx_den_res_years$ex_diff)*100)


# 2. estimating life years spent in each resuscitation state for the second mortality regime comparison 1851/1901

lx_den_res_2<-lx_den_full %>%
  mutate(res_1=lx_den_full[,3]*lx_den_full[,8], res_2=(lx_den_full[,3]*(lx_den_full[,8]^2))/factorial(2), res_3=(lx_den_full[,3]*(lx_den_full[,8]^3))/factorial(3),
         res_4=(lx_den_full[,3]*(lx_den_full[,8]^4))/factorial(4), res_5=(lx_den_full[,3]*(lx_den_full[,8]^5))/factorial(5),res_6=(lx_den_full[,3]*(lx_den_full[,8]^6))/factorial(6),
         res_7=(lx_den_full[,3]*(lx_den_full[,8]^7))/factorial(7), res_8=(lx_den_full[,3]*(lx_den_full[,8]^8))/factorial(8),res_9=(lx_den_full[,3]*(lx_den_full[,8]^9))/factorial(9),
         res_10=(lx_den_full[,3]*(lx_den_full[,8]^10))/factorial(10))


View(lx_den_res_2)

lx_den_res_years_2<-lx_den_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_res_years_2<-lx_den_res_years_2%>% mutate(total=sum(lx_den_res_years_2[1,25:34]))

View(lx_den_res_years_2)

#adding life expectancies, estimating differences 
lx_den_res_years_2$ex_old2<-den_lt_f_ex_surv$`1851`
lx_den_res_years_2$ex_new2<-den_lt_f_ex_surv$`1901`

#mortality gap between regimes
lx_den_res_years_2$ex_diff2<-lx_den_res_years_2$ex_new2-lx_den_res_years_2$ex_old2

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_res_years_2$ex_gap_res2<-lx_den_res_years_2$ex_new2-(lx_den_res_years_2$ex_old2+lx_den_res_years_2$tau_1)
lx_den_res_years_2$ex_gap_res_percent2<-100-((lx_den_res_years_2$ex_gap_res2/lx_den_res_years_2$ex_diff2)*100)


# 3. estimating life years spent in each resuscitation state for the third mortality regime comparison 1901/1951

lx_den_res_3<-lx_den_full %>%
  mutate(res_1=lx_den_full[,4]*lx_den_full[,9], res_2=(lx_den_full[,4]*(lx_den_full[,9]^2))/factorial(2), res_3=(lx_den_full[,4]*(lx_den_full[,9]^3))/factorial(3),
         res_4=(lx_den_full[,4]*(lx_den_full[,9]^4))/factorial(4), res_5=(lx_den_full[,4]*(lx_den_full[,9]^5))/factorial(5),res_6=(lx_den_full[,4]*(lx_den_full[,9]^6))/factorial(6),
         res_7=(lx_den_full[,4]*(lx_den_full[,9]^7))/factorial(7), res_8=(lx_den_full[,4]*(lx_den_full[,9]^8))/factorial(8),res_9=(lx_den_full[,4]*(lx_den_full[,9]^9))/factorial(9),
         res_10=(lx_den_full[,4]*(lx_den_full[,9]^10))/factorial(10))


View(lx_den_res_3)

lx_den_res_years_3<-lx_den_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_res_years_3<-lx_den_res_years_3%>% mutate(total=sum(lx_den_res_years_3[1,25:34]))

View(lx_den_res_years_3)

#adding life expectancies, estimating differences 
lx_den_res_years_3$ex_old3<-den_lt_f_ex_surv$`1901`
lx_den_res_years_3$ex_new3<-den_lt_f_ex_surv$`1951`

#mortality gap between regimes
lx_den_res_years_3$ex_diff3<-lx_den_res_years_3$ex_new3-lx_den_res_years_3$ex_old3

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_res_years_3$ex_gap_res3<-lx_den_res_years_3$ex_new3-(lx_den_res_years_3$ex_old3+lx_den_res_years_3$tau_1)
lx_den_res_years_3$ex_gap_res_percent3<-100-((lx_den_res_years_3$ex_gap_res3/lx_den_res_years_3$ex_diff3)*100)



# 4. estimating life years spent in each resuscitation state for the fourth mortality regime comparison - 1951/2016

lx_den_res_4<-lx_den_full %>%
  mutate(res_1=lx_den_full[,5]*lx_den_full[,10], res_2=(lx_den_full[,5]*(lx_den_full[,10]^2))/factorial(2), res_3=(lx_den_full[,5]*(lx_den_full[,10]^3))/factorial(3),
         res_4=(lx_den_full[,5]*(lx_den_full[,10]^4))/factorial(4), res_5=(lx_den_full[,5]*(lx_den_full[,10]^5))/factorial(5),res_6=(lx_den_full[,5]*(lx_den_full[,10]^6))/factorial(6),
         res_7=(lx_den_full[,5]*(lx_den_full[,10]^7))/factorial(7), res_8=(lx_den_full[,5]*(lx_den_full[,10]^8))/factorial(8),res_9=(lx_den_full[,5]*(lx_den_full[,10]^9))/factorial(9),
         res_10=(lx_den_full[,5]*(lx_den_full[,10]^10))/factorial(10))


View(lx_den_res_4)

lx_den_res_years_4<-lx_den_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_res_years_4<-lx_den_res_years_4%>% mutate(total=sum(lx_den_res_years_4[1,25:34]))

View(lx_den_res_years_4)

#adding life expectancies, estimating differences 
lx_den_res_years_4$ex_old4<-den_lt_f_ex_surv$`1951`
lx_den_res_years_4$ex_new4<-den_lt_f_ex_surv$`2016`

#mortality gap between regimes
lx_den_res_years_4$ex_diff4<-lx_den_res_years_4$ex_new4-lx_den_res_years_4$ex_old4

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_res_years_4$ex_gap_res4<-lx_den_res_years_4$ex_new4-(lx_den_res_years_4$ex_old4+lx_den_res_years_4$tau_1)
lx_den_res_years_4$ex_gap_res_percent4<-100-((lx_den_res_years_4$ex_gap_res4/lx_den_res_years_4$ex_diff4)*100)

### reshaping data

# 1. first only first regime
lx_den_res_years$Regime<-c("1835-1851")
View(lx_den_res_years)
lx_den_females_1<-lx_den_res_years%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_females_1)
lx_den_females_1_long<- gather (lx_den_females_1, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_females_1_long)

colnames(lx_den_females_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime
lx_den_res_years_2$Regime<-c("1851-1901")
View(lx_den_res_years_2)
lx_den_females_2<-lx_den_res_years_2%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_females_2)
lx_den_females_2_long<- gather (lx_den_females_2, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_females_2_long)

colnames(lx_den_females_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_den_res_years_3$Regime<-c("1901-1951")
View(lx_den_res_years_3)
lx_den_females_3<-lx_den_res_years_3%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_females_3)
lx_den_females_3_long<- gather (lx_den_females_3, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_females_3_long)
colnames(lx_den_females_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_den_res_years_4$Regime<-c("1951-2016")
View(lx_den_res_years_4)
lx_den_females_4<-lx_den_res_years_4%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_females_4)
lx_den_females_4_long<- gather (lx_den_females_4, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_females_4_long)
colnames(lx_den_females_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# combining everything
library(reshape)
library(reshape2)
resuscitated_females_denden<- rbind(lx_den_females_1_long,lx_den_females_2_long,lx_den_females_3_long,lx_den_females_4_long)
View(resuscitated_females_denden)

# ploting the hazards and tau?s
X11(width=15,height=5.5)
par(mfrow=c(1,2))
plot(x=c(1:length(lx_den_res_years_4$Age)), lx_den_res_years_4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))),  xlab="Age", lwd=2,main="a. Females")
lines(x=c(1:length(lx_den_res_years_4$Age)), lx_den_res_years_4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_den_res_years_4$Age)), lx_den_res_years_4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_den_res_years_4$Age)), lx_den_res_years_4$hazard_4, lty=1,lwd=2, col="blue")
axis(1, seq(0,length(lx_den_res_years_4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)

legend("topleft", legend=c("1835-1851","1851-1901","1901-1951","1951-2016"),
       lty=c(1,5,3,1),col=c("black","black","black","blue"), bty="n")
abline(h=0, col="grey", lwd=2)

# force of mortality 
den_qx_f<-lt_f%>%
  filter(country %in% c("DNK"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1835","1851","1901","1951","2016"))%>%
  select(c("Year","Age","qx"))
View(den_qx_f) 

den_qx_f_wide<- den_qx_f %>% 
  spread(key=Year, value=qx)

pdf(file="prob_death_females_denden.pdf",width=5.5,height=5.5)
plot(x=c(1:length(den_qx_f_wide$Age)),den_qx_f_wide$`1851`, ylim=c(0,1), type="l", axes=FALSE,
     ylab="Probability of death- qx", xlab="Age", lwd=1.5, main="a) Females")
lines(x=c(1:length(den_qx_f_wide$Age)), den_qx_f_wide$`1901`, lty=3,lwd=1.5)
lines(x=c(1:length(den_qx_f_wide$Age)), den_qx_f_wide$`1951`, lty=2,lwd=1.5)
lines(x=c(1:length(den_qx_f_wide$Age)), den_qx_f_wide$`2016`, lty=6,lwd=1.5)
axis(1, seq(0,length(den_qx_f_wide$Age),5), las=1,cex.axis=.5, lwd=1.5)
axis(2, seq(0, 1, 0.1),lwd=1.5,cex.axis=.5, las=1)

legend("topleft", legend=c("1851","1901","1951","2016"),
       lty=c(1,5,3,2,6), bty="n", cex = .5)
dev.off()


#ggplot for number of resuscitations and the number of resuscitated, females, denden
library(forcats)
resuscitated_females_denden$Resuscitations<-fct_collapse(resuscitated_females_denden$Resuscitations,
                                                         res_1 = c("res_1"),
                                                         res_2 = c("res_2"),
                                                         res_3 = c("res_3"),
                                                         res_4 = c("res_4"),
                                                         res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_females_denden$Resuscitations <- factor(resuscitated_females_denden$Resuscitations, ordered = TRUE,
                                                     levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"),
                                                     labels = c("1", "2", "3","4","5+"))

pdf(file="res_females_denden.pdf",width=15,height=5.5)
X11(width=15,height=5.5)
library(viridis)
ggplot(resuscitated_females_denden %>% filter(Regime!="1835-1851"), aes(x = Age, y = Res_number, group=Resuscitations, color=Resuscitations)) +
  geom_line(aes(color=Resuscitations))+ facet_grid(~ Regime)+ geom_point(aes(color=Resuscitations), size=1)+ 
  scale_color_viridis(discrete=T)+ 
  ylab("Number of resuscitated persons")+theme_bw()
dev.off()

# assesing differences in life expectancy and number of life years lived in each resuscitation state


View(lx_den_res_years)

tau_fem_1<-lx_den_res_years[1,25:41]
colnames(tau_fem_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_2<-lx_den_res_years_2[1,25:41]
colnames(tau_fem_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_3<-lx_den_res_years_3[1,25:41]
colnames(tau_fem_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem_4<-lx_den_res_years_4[1,25:41]
colnames(tau_fem_4)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_fem<-rbind(tau_fem_1,tau_fem_2, tau_fem_3,tau_fem_4)
View(tau_fem)

write.table(tau_fem, file="tau_fem_den.csv", sep=",", row.names = F) #saving for using excel as well
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
                           levels = c("tau_1", "tau_2", "tau_3","tau_4","tau_5_plus"),labels=c("1","2","3","4","5+"))


library(latex2exp)
X11(width=10,height=10)
tau_fem_den<-ggplot(data = tau_fem_prop %>% filter(Regime!="1835-1851"), aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(linetype=Regime), size=1)+theme_bw()+
  labs(x = expression(tau[i]), y="Proportion life years lived in each resuscitation state")+
  ggtitle("a.Females")

# add here the proportions

# if animation is interesting..

#p <- resuscitated_females_denden %>%
# plot_ly(
#  x = ~Age, 
# y = ~Res_number, 
# color = ~Resuscitations, 
#  denme = ~Regime, 
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
### Implementing Vaupel?s Approach : MALES ##
###############################################

# selecting years for mortatliy regime comparison. IN the case of Sweden, we will perform 50-year analysis#
den_lt_m<-lt_m%>%
  filter(country %in% c("DNK"))%>%
  filter(Year %in% c("1835","1851","1901","1951","2016"))%>%
  filter(Age<=100) %>%
  select(c("Year","Age","lx"))
View(den_lt_m) 

X11(width=7,height=7)
male_surv_den<-ggplot(den_lt_m %>% filter(Year!="1835"), aes(x = Age, y = lx, group=factor(Year))) +
  geom_line(aes(linetype=factor(Year)))+ ylim(1,100000)+ggtitle("b.Males")+
  theme_bw()+scale_linetype_manual(name="Year",  values = c(
                                                            "1851"="solid",
                                                            "1901"="dotdash",
                                                            "1951"="dashed",
                                                            "2016"="dotted"), 
                                   labels=c("1851","1901","1951","2016"))+
  ylab("lx")+annotate("text", x = c(57,65,70,75), y = c(40000,55000,75000,85000), label = c("1851","1901","1951","2016"))

#if combining males and females in one plot
require(gridExtra)
X11(width=15,height=7)
grid.arrange( surv_fem_den, male_surv_den, ncol=2)

dev.off

#wide format for estimation
den_lt_m_surv <- den_lt_m %>%
  spread(key=Year, value=lx)


View(den_lt_m_surv)


#only ex
den_lt_m_ex<-lt_m%>%
  filter(country %in% c("DNK"))%>%
  filter(Year %in% c("1835","1851","1901","1951","2016"))%>%
  filter(Age %in% c("0"))%>%
  select(c("Year","Age","ex"))
View(den_lt_m_ex) 


den_lt_m_ex_surv <- den_lt_m_ex %>%
  spread(key=Year, value=ex)


View(den_lt_m_ex_surv)

# taking the first row out because we are not dealing with the radix of the lifetable
lx_den_m<-den_lt_m_surv[-1,]

View(lx_den_m)
# creating the variables for estimating the resuscitated

lx_den_m_full<-lx_den_m %>%
  mutate(hazard_1=log(lx_den_m[,3]/lx_den_m[,2]),hazard_2=log(lx_den_m[,4]/lx_den_m[,3]),hazard_3=log(lx_den_m[,5]/lx_den_m[,4]),
         hazard_4=log(lx_den_m[,6]/lx_den_m[,5]),change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1)

View(lx_den_m_full)
# taking only the complete cases because since there are no survivors after age 104 na?s are generated

lx_den_m_full<-lx_den_m_full[complete.cases(lx_den_m_full), ]

# creating a datadenme for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison 1835/1851

lx_den_m_res_1<-lx_den_m_full %>%
  mutate(res_1=lx_den_m_full[,2]*lx_den_m_full[,7], res_2=(lx_den_m_full[,2]*(lx_den_m_full[,7]^2))/factorial(2), res_3=(lx_den_m_full[,2]*(lx_den_m_full[,7]^3))/factorial(3),
         res_4=(lx_den_m_full[,2]*(lx_den_m_full[,7]^4))/factorial(4), res_5=(lx_den_m_full[,2]*(lx_den_m_full[,7]^5))/factorial(5),res_6=(lx_den_m_full[,2]*(lx_den_m_full[,7]^6))/factorial(6),
         res_7=(lx_den_m_full[,2]*(lx_den_m_full[,7]^7))/factorial(7), res_8=(lx_den_m_full[,2]*(lx_den_m_full[,7]^8))/factorial(8),res_9=(lx_den_m_full[,2]*(lx_den_m_full[,7]^9))/factorial(9),
         res_10=(lx_den_m_full[,2]*(lx_den_m_full[,7]^10))/factorial(10))


View(lx_den_m_res_1)
radix<-100000

lx_den_m_res_years<-lx_den_m_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_den_m_res_years)
lx_den_m_res_years<-lx_den_m_res_years%>% mutate(total=sum(lx_den_m_res_years[1,25:34]))

View(lx_den_m_res_years)

#adding life expectancies, estimating differences 
lx_den_m_res_years$ex_old_m<-den_lt_m_ex_surv$`1835`
lx_den_m_res_years$ex_new_m<-den_lt_m_ex_surv$`1851`

#mortality gap between regimes
lx_den_m_res_years$ex_diff<-lx_den_m_res_years$ex_new_m-lx_den_m_res_years$ex_old_m

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_den_m_res_years$ex_gap_m_res1<-lx_den_m_res_years$ex_new_m-(lx_den_m_res_years$ex_old_m+lx_den_m_res_years$tau_1)
lx_den_m_res_years$ex_gap_m_res_percent<-100-((lx_den_m_res_years$ex_gap_m_res1/lx_den_m_res_years$ex_diff)*100)
View(lx_den_m_res_years)

# 2. estimating life years spent in each resuscitation state for the second mortality regime comparison 1851/1901

lx_den_m_res_2<-lx_den_m_full %>%
  mutate(res_1=lx_den_m_full[,3]*lx_den_m_full[,8], res_2=(lx_den_m_full[,3]*(lx_den_m_full[,8]^2))/factorial(2), res_3=(lx_den_m_full[,3]*(lx_den_m_full[,8]^3))/factorial(3),
         res_4=(lx_den_m_full[,3]*(lx_den_m_full[,8]^4))/factorial(4), res_5=(lx_den_m_full[,3]*(lx_den_m_full[,8]^5))/factorial(5),res_6=(lx_den_m_full[,3]*(lx_den_m_full[,8]^6))/factorial(6),
         res_7=(lx_den_m_full[,3]*(lx_den_m_full[,8]^7))/factorial(7), res_8=(lx_den_m_full[,3]*(lx_den_m_full[,8]^8))/factorial(8),res_9=(lx_den_m_full[,3]*(lx_den_m_full[,8]^9))/factorial(9),
         res_10=(lx_den_m_full[,3]*(lx_den_m_full[,8]^10))/factorial(10))


View(lx_den_m_res_2)

lx_den_m_res_years_2<-lx_den_m_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_m_res_years_2<-lx_den_m_res_years_2%>% mutate(total=sum(lx_den_m_res_years_2[1,25:34]))

View(lx_den_m_res_years_2)

#adding life expectancies, estimating differences 
lx_den_m_res_years_2$ex_old2_m<-den_lt_m_ex_surv$`1851`
lx_den_m_res_years_2$ex_new2_m<-den_lt_m_ex_surv$`1901`

#mortality gap between regimes
lx_den_m_res_years_2$ex_diff2<-lx_den_m_res_years_2$ex_new2_m-lx_den_m_res_years_2$ex_old2_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_m_res_years_2$ex_gap_m_res2<-lx_den_m_res_years_2$ex_new2_m-(lx_den_m_res_years_2$ex_old2_m+lx_den_m_res_years_2$tau_1)
lx_den_m_res_years_2$ex_gap_m_res_percent2<-100-((lx_den_m_res_years_2$ex_gap_m_res2/lx_den_m_res_years_2$ex_diff2)*100)
View(lx_den_m_res_years_2)

# 3. estimating life years spent in each resuscitation state for the third mortality regime comparison 1901/1951

lx_den_m_res_3<-lx_den_m_full %>%
  mutate(res_1=lx_den_m_full[,4]*lx_den_m_full[,9], res_2=(lx_den_m_full[,4]*(lx_den_m_full[,9]^2))/factorial(2), res_3=(lx_den_m_full[,4]*(lx_den_m_full[,9]^3))/factorial(3),
         res_4=(lx_den_m_full[,4]*(lx_den_m_full[,9]^4))/factorial(4), res_5=(lx_den_m_full[,4]*(lx_den_m_full[,9]^5))/factorial(5),res_6=(lx_den_m_full[,4]*(lx_den_m_full[,9]^6))/factorial(6),
         res_7=(lx_den_m_full[,4]*(lx_den_m_full[,9]^7))/factorial(7), res_8=(lx_den_m_full[,4]*(lx_den_m_full[,9]^8))/factorial(8),res_9=(lx_den_m_full[,4]*(lx_den_m_full[,9]^9))/factorial(9),
         res_10=(lx_den_m_full[,4]*(lx_den_m_full[,9]^10))/factorial(10))


View(lx_den_m_res_3) 

lx_den_m_res_years_3<-lx_den_m_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_m_res_years_3<-lx_den_m_res_years_3%>% mutate(total=sum(lx_den_m_res_years_3[1,25:34]))

View(lx_den_m_res_years_3)

#adding life expectancies, estimating differences 
lx_den_m_res_years_3$ex_old3_m<-den_lt_m_ex_surv$`1901`
lx_den_m_res_years_3$ex_new3_m<-den_lt_m_ex_surv$`1951`

#mortality gap between regimes
lx_den_m_res_years_3$ex_diff3<-lx_den_m_res_years_3$ex_new3_m-lx_den_m_res_years_3$ex_old3_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_m_res_years_3$ex_gap_m_res3<-lx_den_m_res_years_3$ex_new3_m-(lx_den_m_res_years_3$ex_old3_m+lx_den_m_res_years_3$tau_1)
lx_den_m_res_years_3$ex_gap_m_res_percent3<-100-((lx_den_m_res_years_3$ex_gap_m_res3/lx_den_m_res_years_3$ex_diff3)*100)


View(lx_den_m_res_years_3)

# 4. estimating life years spent in each resuscitation state for the fourth mortality regime comparison - 1951/2016

lx_den_m_res_4<-lx_den_m_full %>%
  mutate(res_1=lx_den_m_full[,5]*lx_den_m_full[,10], res_2=(lx_den_m_full[,5]*(lx_den_m_full[,10]^2))/factorial(2), res_3=(lx_den_m_full[,5]*(lx_den_m_full[,10]^3))/factorial(3),
         res_4=(lx_den_m_full[,5]*(lx_den_m_full[,10]^4))/factorial(4), res_5=(lx_den_m_full[,5]*(lx_den_m_full[,10]^5))/factorial(5),res_6=(lx_den_m_full[,5]*(lx_den_m_full[,10]^6))/factorial(6),
         res_7=(lx_den_m_full[,5]*(lx_den_m_full[,10]^7))/factorial(7), res_8=(lx_den_m_full[,5]*(lx_den_m_full[,10]^8))/factorial(8),res_9=(lx_den_m_full[,5]*(lx_den_m_full[,10]^9))/factorial(9),
         res_10=(lx_den_m_full[,5]*(lx_den_m_full[,10]^10))/factorial(10))


View(lx_den_m_res_4)

lx_den_m_res_years_4<-lx_den_m_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

lx_den_m_res_years_4<-lx_den_m_res_years_4%>% mutate(total=sum(lx_den_m_res_years_4[1,25:34]))

View(lx_den_m_res_years_4)

#adding life expectancies, estimating differences 
lx_den_m_res_years_4$ex_old4_m<-den_lt_m_ex_surv$`1951`
lx_den_m_res_years_4$ex_new4_m<-den_lt_m_ex_surv$`2016`

#mortality gap between regimes
lx_den_m_res_years_4$ex_diff4<-lx_den_m_res_years_4$ex_new4_m-lx_den_m_res_years_4$ex_old4_m

#if all females from the higher mortality regime had their lives saved once, the gap would be:
lx_den_m_res_years_4$ex_gap_m_res4<-lx_den_m_res_years_4$ex_new4_m-(lx_den_m_res_years_4$ex_old4_m+lx_den_m_res_years_4$tau_1)
lx_den_m_res_years_4$ex_gap_m_res_percent4<-100-((lx_den_m_res_years_4$ex_gap_m_res4/lx_den_m_res_years_4$ex_diff4)*100)
View(lx_den_m_res_years_4)


### reshaping data

# 1. first only first regime

lx_den_m_res_years$Regime<-c("1835-1851")
View(lx_den_m_res_years)
lx_den_males_1<-lx_den_m_res_years%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_males_1)
lx_den_males_1_long<- gather (lx_den_males_1, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_den_males_1_long)

colnames(lx_den_males_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime

lx_den_m_res_years_2$Regime<-c("1851-1901")
View(lx_den_m_res_years_2)
lx_den_males_2<-lx_den_m_res_years_2%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_males_2)
lx_den_males_2_long<- gather (lx_den_males_2, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_den_males_2_long)

colnames(lx_den_males_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_den_m_res_years_3$Regime<-c("1901-1951")
View(lx_den_m_res_years_3)
lx_den_males_3<-lx_den_m_res_years_3%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_males_3)
lx_den_males_3_long<- gather (lx_den_males_3, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_den_males_3_long)
colnames(lx_den_males_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_den_m_res_years_4$Regime<-c("1951-2016")
View(lx_den_m_res_years_4)
lx_den_males_4<-lx_den_m_res_years_4%>%
  select(c(Age,hazard_1:hazard_4,res_1:Regime))
View(lx_den_males_4)
lx_den_males_4_long<- gather (lx_den_males_4, key=Resuscitations, 
                              value=Res_number, res_1:res_10)
View(lx_den_males_4_long)
colnames(lx_den_males_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4",          
                                    "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                    "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                    "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                    "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# combining everything
library(reshape)
library(reshape2)
resuscitated_males_denden<- rbind(lx_den_males_1_long,lx_den_males_2_long,lx_den_males_3_long,lx_den_males_4_long)
View(resuscitated_males_denden)

# ploting the hazards and tau?s
X11()
par(mfrow=c(1,2))
plot(x=c(1:length(lx_den_m_res_years_4$Age)), lx_den_m_res_years_4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))),  xlab="Age", lwd=2, main="b. Males")
lines(x=c(1:length(lx_den_m_res_years_4$Age)), lx_den_m_res_years_4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_den_m_res_years_4$Age)), lx_den_m_res_years_4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_den_m_res_years_4$Age)), lx_den_m_res_years_4$hazard_4, lty=1,lwd=2, col="blue")
axis(1, seq(0,length(lx_den_m_res_years_4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1835-1851","1851-1901","1901-1951","1951-2016"),
       lty=c(1,5,3,1),col=c("black","black","black","blue"), bty="n")
abline(h=0,col="grey", lwd=2)

# force of mortality 
den_qx_m<-lt_m%>%
  filter(country %in% c("DNK"))%>%
  filter(Age<=100)%>%
  filter(Year %in% c("1835","1851","1901","1951","2016"))%>%
  select(c("Year","Age","qx"))
View(den_qx_m) 

den_qx_m_wide<- den_qx_m %>% 
  spread(key=Year, value=qx)

X11()
pdf(file="prob_death_males_denden.pdf",width=5.5,height=5.5)
plot(x=c(1:length(den_qx_m_wide$Age)),den_qx_m_wide$`1851`, ylim=c(0,1), type="l", axes=FALSE,
     ylab="Probability of death- qx", xlab="Age", lwd=1.5, main="b. Males")
lines(x=c(1:length(den_qx_m_wide$Age)), den_qx_m_wide$`1901`, lty=3,lwd=1.5)
lines(x=c(1:length(den_qx_m_wide$Age)), den_qx_m_wide$`1951`, lty=2,lwd=1.5)
lines(x=c(1:length(den_qx_m_wide$Age)), den_qx_m_wide$`2016`, lty=6,lwd=1.5)
axis(1, seq(0,length(den_qx_m_wide$Age),5), las=1,cex.axis=.5, lwd=1.5)
axis(2, seq(0, 1, 0.1),lwd=1.5,cex.axis=.5, las=1)
legend("topleft", legend=c("1851","1901","1951","2016"),
       lty=c(1,5,3,2,6), bty="n", cex = .5)
dev.off()


#ggplot for number of resuscitations and the number of resuscitated, males, denden
library(forcats)
resuscitated_males_denden$Resuscitations<-fct_collapse(resuscitated_males_denden$Resuscitations,
                                                       res_1 = c("res_1"),
                                                       res_2 = c("res_2"),
                                                       res_3 = c("res_3"),
                                                       res_4 = c("res_4"),
                                                       res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_males_denden$Resuscitations <- factor(resuscitated_males_denden$Resuscitations, ordered = TRUE,
                                                   levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"),
                                                   labels=c("1", "2", "3","4","5+"))


# doing it for both females and males
resuscitated_females_denden$Sex<-factor("Females")
resuscitated_males_denden$Sex<-factor("Males")

resuscitated_all<-rbind (resuscitated_females_denden,resuscitated_males_denden)
View(resuscitated_all)

pdf(file="res_all_den.pdf",width=15,height=8)
X11(width=15,height=8)
library(viridis)
all_res_den<-ggplot(resuscitated_all %>% filter(Regime!="1835-1851"), 
                    aes(x = Age, y = Res_number, group=Resuscitations, color=Resuscitations)) +
  geom_line(aes(color=Resuscitations))+ facet_grid(Sex~ Regime)+ geom_point(aes(color=Resuscitations), size=1)+ 
  scale_color_viridis(discrete=T)+ ylab("Number of resuscitated persons")+theme_bw()

dev.off()

# assesing differences in life expectancy and number of life years lived in each resuscitation state


View(lx_den_m_res_years)

tau_male_1<-lx_den_m_res_years[1,25:41]
colnames(tau_male_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_male_2<-lx_den_m_res_years_2[1,25:41]
colnames(tau_male_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_male_3<-lx_den_m_res_years_3[1,25:41]
colnames(tau_male_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_male_4<-lx_den_m_res_years_4[1,25:41]
colnames(tau_male_4)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                       "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_male<-rbind(tau_male_1,tau_male_2, tau_male_3,tau_male_4)
View(tau_male)

write.table(tau_male, file="tau_male_den.csv", sep=",", row.names = F) #saving for using excel as well

tau_male_long <- gather(tau_male, tau, life_years_res, tau_1:tau_10)

View(tau_male_long)
tau_male_prop<-tau_male_long %>%
  group_by(tau)%>%
  mutate(prop_res=(life_years_res/total)*100)

View(tau_male_prop)

tau_male_prop$tau<-fct_collapse(tau_male_prop$tau,
                               tau_1 = c("tau_1"),
                               tau_2 = c("tau_2"),
                               tau_3 = c("tau_3"),
                               tau_4 = c("tau_4"),
                               tau_5_plus=c("tau_5","tau_6","tau_7","tau_8","tau_9","tau_10")
)
tau_male_prop$tau <- factor(tau_male_prop$tau, ordered = TRUE,
                           levels = c("tau_1", "tau_2", "tau_3","tau_4","tau_5_plus"),labels=c("1","2","3","4","5+"))


library(latex2exp)
X11(width=10,height=10)
tau_male_den<-ggplot(data = tau_male_prop %>% filter(Regime!="1835-1851"), aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(linetype=Regime), size=1)+theme_bw()+
  labs(x = expression(tau[i]), y="Proportion life years lived in each resuscitation state")+
  ggtitle("b.Males")

#together
X11(width=15,height=7)
grid.arrange(tau_fem_den,tau_male_den, ncol=2)

#################################################
#### comparing gender gaps ######################
#################################################

# 1. females versus male regime 1835
# creating the variables for estimating the resuscitated

colnames (lx_den )<- c( "Age","fem_1835","fem_1851","fem_1901","fem_1951","fem_2016")
lx_den_compare<-cbind(lx_den,lx_den_m)
View(lx_den_compare)
lx_den_compare<-lx_den_compare[,-c(7)]
lx_den_compare_full<- lx_den_compare %>%
  mutate(hazard_1=log(lx_den_compare[,2]/lx_den_compare[,7]),hazard_2=log(lx_den_compare[,3]/lx_den_compare[,8]),hazard_3=log(lx_den_compare[,4]/lx_den_compare[,9]),
         hazard_4=log(lx_den_compare[,5]/lx_den_compare[,10]),hazard_5=log(lx_den_compare[,6]/lx_den_compare[,11]), change_1=exp(hazard_1)-1,change_2=exp(hazard_2)-1,change_3=exp(hazard_3)-1,
         change_4=exp(hazard_4)-1,change_5=exp(hazard_5)-1)

View(lx_den_compare_full)
# taking only the complete cases because since there are no survivors after age 104 na?s are generated

lx_den_compare_full<-lx_den_compare_full[complete.cases(lx_den_compare_full), ]

# creating a datadenme for the number of resuscitations - we go until ten because that is what Vaupel does
#two mortality regimes at a time. Here the first two years.

# 1. estimating life years spent in each resuscitation state for the first mortality regime comparison between sex 1835/1835 

lx_den_compare_res_1<-lx_den_compare_full %>%
  mutate(res_1=lx_den_compare_full[,7]*lx_den_compare_full[,12], res_2=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^2))/factorial(2), res_3=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^3))/factorial(3),
         res_4=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^4))/factorial(4), res_5=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^5))/factorial(5),res_6=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^6))/factorial(6),
         res_7=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^7))/factorial(7), res_8=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^8))/factorial(8),res_9=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^9))/factorial(9),
         res_10=(lx_den_compare_full[,7]*(lx_den_compare_full[,12]^10))/factorial(10))


View(lx_den_compare_res_1)
radix<-100000

lx_den_compare_res_years<-lx_den_compare_res_1%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_den_compare_res_years)
lx_den_compare_res_years<-lx_den_compare_res_years%>% mutate(total=sum(lx_den_compare_res_years[1,32:41]))

View(lx_den_compare_res_years)

#adding life expectancies, estimating differences 
lx_den_compare_res_years$ex_male<-den_lt_m_ex_surv$`1835`
lx_den_compare_res_years$ex_female<-den_lt_f_ex_surv$`1835`

#mortality gap between regimes
lx_den_compare_res_years$ex_diff_compare<-lx_den_compare_res_years$ex_female-lx_den_compare_res_years$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_den_compare_res_years$ex_gap_compare_res1<-lx_den_compare_res_years$ex_female-(lx_den_compare_res_years$ex_male+lx_den_compare_res_years$tau_1)
lx_den_compare_res_years$ex_gap_compare_res_percent<-100-((lx_den_compare_res_years$ex_gap_compare_res1/lx_den_compare_res_years$ex_diff_compare)*100)
View(lx_den_compare_res_years)


# 2. females versus male regime 1851

lx_den_compare_res_2<-lx_den_compare_full %>%
  mutate(res_1=lx_den_compare_full[,8]*lx_den_compare_full[,13], res_2=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^2))/factorial(2), res_3=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^3))/factorial(3),
         res_4=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^4))/factorial(4), res_5=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^5))/factorial(5),res_6=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^6))/factorial(6),
         res_7=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^7))/factorial(7), res_8=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^8))/factorial(8),res_9=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^9))/factorial(9),
         res_10=(lx_den_compare_full[,8]*(lx_den_compare_full[,13]^10))/factorial(10))


View(lx_den_compare_res_2)
radix<-100000

lx_den_compare_res_years2<-lx_den_compare_res_2%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_den_compare_res_years2)
lx_den_compare_res_years2<-lx_den_compare_res_years2%>% mutate(total=sum(lx_den_compare_res_years2[1,32:41]))

View(lx_den_compare_res_years2)

#adding life expectancies, estimating differences 
lx_den_compare_res_years2$ex_male<-den_lt_m_ex_surv$`1851`
lx_den_compare_res_years2$ex_female<-den_lt_f_ex_surv$`1851`

#mortality gap between regimes
lx_den_compare_res_years2$ex_diff_compare<-lx_den_compare_res_years2$ex_female-lx_den_compare_res_years2$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_den_compare_res_years2$ex_gap_compare_res1<-lx_den_compare_res_years2$ex_female-(lx_den_compare_res_years2$ex_male+lx_den_compare_res_years2$tau_1)
lx_den_compare_res_years2$ex_gap_compare_res_percent<-100-((lx_den_compare_res_years2$ex_gap_compare_res1/lx_den_compare_res_years2$ex_diff_compare)*100)
View(lx_den_compare_res_years2)

# 3. females versus male regime 1901

lx_den_compare_res_3<-lx_den_compare_full %>%
  mutate(res_1=lx_den_compare_full[,9]*lx_den_compare_full[,14], res_2=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^2))/factorial(2), res_3=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^3))/factorial(3),
         res_4=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^4))/factorial(4), res_5=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^5))/factorial(5),res_6=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^6))/factorial(6),
         res_7=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^7))/factorial(7), res_8=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^8))/factorial(8),res_9=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^9))/factorial(9),
         res_10=(lx_den_compare_full[,9]*(lx_den_compare_full[,14]^10))/factorial(10))


View(lx_den_compare_res_3)
radix<-100000

lx_den_compare_res_years3<-lx_den_compare_res_3%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_den_compare_res_years3)
lx_den_compare_res_years3<-lx_den_compare_res_years3%>% mutate(total=sum(lx_den_compare_res_years3[1,32:41]))

View(lx_den_compare_res_years3)

#adding life expectancies, estimating differences 
lx_den_compare_res_years3$ex_male<-den_lt_m_ex_surv$`1901`
lx_den_compare_res_years3$ex_female<-den_lt_f_ex_surv$`1901`

#mortality gap between regimes
lx_den_compare_res_years3$ex_diff_compare<-lx_den_compare_res_years3$ex_female-lx_den_compare_res_years3$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_den_compare_res_years3$ex_gap_compare_res1<-lx_den_compare_res_years3$ex_female-(lx_den_compare_res_years3$ex_male+lx_den_compare_res_years3$tau_1)
lx_den_compare_res_years3$ex_gap_compare_res_percent<-100-((lx_den_compare_res_years3$ex_gap_compare_res1/lx_den_compare_res_years3$ex_diff_compare)*100)
View(lx_den_compare_res_years3)

# 3. females versus male regime 1951
lx_den_compare_res_4<-lx_den_compare_full %>%
  mutate(res_1=lx_den_compare_full[,10]*lx_den_compare_full[,15], res_2=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^2))/factorial(2), res_3=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^3))/factorial(3),
         res_4=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^4))/factorial(4), res_5=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^5))/factorial(5),res_6=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^6))/factorial(6),
         res_7=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^7))/factorial(7), res_8=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^8))/factorial(8),res_9=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^9))/factorial(9),
         res_10=(lx_den_compare_full[,10]*(lx_den_compare_full[,15]^10))/factorial(10))


View(lx_den_compare_res_4)
radix<-100000

lx_den_compare_res_years4<-lx_den_compare_res_4%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_den_compare_res_years4)
lx_den_compare_res_years4<-lx_den_compare_res_years4%>% mutate(total=sum(lx_den_compare_res_years4[1,32:41]))

View(lx_den_compare_res_years4)

#adding life expectancies, estimating differences 

lx_den_compare_res_years4$ex_male<-den_lt_m_ex_surv$`1951`
lx_den_compare_res_years4$ex_female<-den_lt_f_ex_surv$`1951`

#mortality gap between regimes
lx_den_compare_res_years4$ex_diff_compare<-lx_den_compare_res_years4$ex_female-lx_den_compare_res_years4$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_den_compare_res_years4$ex_gap_compare_res1<-lx_den_compare_res_years4$ex_female-(lx_den_compare_res_years4$ex_male+lx_den_compare_res_years4$tau_1)
lx_den_compare_res_years4$ex_gap_compare_res_percent<-100-((lx_den_compare_res_years4$ex_gap_compare_res1/lx_den_compare_res_years4$ex_diff_compare)*100)
View(lx_den_compare_res_years4)

# 4. females versus male regime 2016

lx_den_compare_res_5<-lx_den_compare_full %>%
  mutate(res_1=lx_den_compare_full[,11]*lx_den_compare_full[,16], res_2=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^2))/factorial(2), res_3=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^3))/factorial(3),
         res_4=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^4))/factorial(4), res_5=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^5))/factorial(5),res_6=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^6))/factorial(6),
         res_7=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^7))/factorial(7), res_8=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^8))/factorial(8),res_9=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^9))/factorial(9),
         res_10=(lx_den_compare_full[,11]*(lx_den_compare_full[,16]^10))/factorial(10))


View(lx_den_compare_res_5)
radix<-100000

lx_den_compare_res_years5<-lx_den_compare_res_5%>%
  mutate(tau_1=sum(res_1/radix),tau_2=sum(res_2/radix),tau_3=sum(res_3/radix),tau_4=sum(res_4/radix),tau_5=sum(res_5/radix),tau_6=sum(res_6/radix),
         tau_7=sum(res_7/radix),tau_8=sum(res_8/radix),tau_9=sum(res_9/radix),tau_10=sum(res_10/radix))

View(lx_den_compare_res_years5)
lx_den_compare_res_years5<-lx_den_compare_res_years5%>% mutate(total=sum(lx_den_compare_res_years5[1,32:41]))

View(lx_den_compare_res_years5)

#adding life expectancies, estimating differences 
lx_den_compare_res_years5$ex_male<-den_lt_m_ex_surv$`2016`
lx_den_compare_res_years5$ex_female<-den_lt_f_ex_surv$`2016`

#mortality gap between regimes
lx_den_compare_res_years5$ex_diff_compare<-lx_den_compare_res_years5$ex_female-lx_den_compare_res_years5$ex_male

#if all males from the higher mortality regime had their lives saved once, the gap would be:
lx_den_compare_res_years5$ex_gap_compare_res1<-lx_den_compare_res_years5$ex_female-(lx_den_compare_res_years5$ex_male+lx_den_compare_res_years5$tau_1)
lx_den_compare_res_years5$ex_gap_compare_res_percent<-100-((lx_den_compare_res_years5$ex_gap_compare_res1/lx_den_compare_res_years5$ex_diff_compare)*100)



### reshaping data

# 1. first only first regime
lx_den_compare_res_years$Regime<-c("1835")
View(lx_den_compare_res_years)
lx_den_compare_1<-lx_den_compare_res_years%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_den_compare_1)
lx_den_compare_1_long<- gather (lx_den_compare_1, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_compare_1_long)

colnames(lx_den_compare_1_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4","hazard_5"  ,        
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 2. second regime

lx_den_compare_res_years2$Regime<-c("1851")
View(lx_den_compare_res_years2)
lx_den_compare_2<-lx_den_compare_res_years2%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_den_compare_2)
lx_den_compare_2_long<- gather (lx_den_compare_2, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_compare_2_long)

colnames(lx_den_compare_2_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,"hazard_5",         "hazard_4",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )
# 3. third regime
lx_den_compare_res_years3$Regime<-c("1901")
lx_den_compare_3<-lx_den_compare_res_years3%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_den_compare_3)
lx_den_compare_3_long<- gather (lx_den_compare_3, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_compare_3_long)
colnames(lx_den_compare_3_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4","hazard_5",          
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )

# 4. fourth regime
lx_den_compare_res_years4$Regime<-c("1951")
View(lx_den_compare_res_years4)
lx_den_compare_4<-lx_den_compare_res_years4%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_den_compare_4)
lx_den_compare_4_long<- gather (lx_den_compare_4, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_compare_4_long)
colnames(lx_den_compare_4_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4", "hazard_5",         
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )



# 5. fifth regime
lx_den_compare_res_years5$Regime<-c("2016")
View(lx_den_compare_res_years5)
lx_den_compare_5<-lx_den_compare_res_years5%>%
  select(c(Age,hazard_1:hazard_5,res_1:Regime))
View(lx_den_compare_5)
lx_den_compare_5_long<- gather (lx_den_compare_5, key=Resuscitations, 
                                value=Res_number, res_1:res_10)
View(lx_den_compare_5_long)
colnames(lx_den_compare_5_long) <- c( "Age"       ,         "hazard_1"     ,      "hazard_2"    ,       "hazard_3"  ,         "hazard_4", "hazard_5",         
                                      "tau_1"   ,           "tau_2"      ,        "tau_3"  ,            "tau_4" ,             "tau_5"  ,           
                                      "tau_6"   ,           "tau_7"    ,          "tau_8"  ,            "tau_9"   ,           "tau_10" ,           
                                      "total"     ,         "ex_old",             "ex_new"    ,         "ex_diff"    ,        "ex_gap_res1"   ,    
                                      "ex_gap_res_percent", "Regime"      ,       "Resuscitations"   ,  "Res_number" )






# combining everything
library(reshape)
library(reshape2)
resuscitated_compare_denden<- rbind(lx_den_compare_2_long,lx_den_compare_3_long,lx_den_compare_4_long,lx_den_compare_5_long)
View(resuscitated_compare_denden)

# ploting the hazards and tau?s
X11()
par(mfrow=c(1,2))
plot(x=c(1:length(lx_den_compare_res_years4$Age)), lx_den_compare_res_years4$hazard_1, ylim=c(-1,3.38), type="l", axes=FALSE,
     ylab=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))), xlab="Age", lwd=2)
lines(x=c(1:length(lx_den_compare_res_years4$Age)), lx_den_compare_res_years4$hazard_2, lty=5,lwd=2)
lines(x=c(1:length(lx_den_compare_res_years4$Age)), lx_den_compare_res_years4$hazard_3, lty=3,lwd=2)
lines(x=c(1:length(lx_den_compare_res_years4$Age)), lx_den_compare_res_years4$hazard_4, lty=2,lwd=2)
lines(x=c(1:length(lx_den_compare_res_years4$Age)), lx_den_compare_res_years4$hazard_5, lty=1,lwd=2, col="blue")
axis(1, seq(0,length(lx_den_compare_res_years4$Age),5), las=1,cex.axis=.8, lwd=1.5)
axis(2, seq(-1, 4, 0.5),lwd=1.5,cex.axis=.8, las=1)
legend("topleft", legend=c("1835","1851","1901","1951","2016"),
       lty=c(1,5,3,2,1),col=c("black","black","black","black","blue"), bty="n")
abline(h=0,col="grey", lwd=2)



#ggplot for number of resuscitations and the number of resuscitated, males, denden
library(forcats)
resuscitated_compare_denden$Resuscitations<-fct_collapse(resuscitated_compare_denden$Resuscitations,
                                                         res_1 = c("res_1"),
                                                         res_2 = c("res_2"),
                                                         res_3 = c("res_3"),
                                                         res_4 = c("res_4"),
                                                         res_5_plus=c("res_5","res_6","res_7","res_8","res_9","res_10")
)
resuscitated_compare_denden$Resuscitations <- factor(resuscitated_compare_denden$Resuscitations, ordered = TRUE,
                                                     levels = c("res_1", "res_2", "res_3","res_4","res_5_plus"),labels=c("1", "2", "3","4","5+"))




pdf(file="res_compare_denden2.pdf",width=15,height=8)
X11(width=15,height=8)
ggplot(resuscitated_compare_denden%>% filter(Regime!="1835"), aes(x = Age, y = Res_number, group=Resuscitations, color=Resuscitations)) +
  geom_line(aes(color=Resuscitations))+ facet_grid(.~ Regime)+ geom_point(aes(color=Resuscitations), size=1)+ 
  scale_color_viridis(discrete=T)+ 
  ylab("Number of resuscitated persons")+theme_bw()

dev.off()
fem_res_fra<-ggplot(resuscitated_females_fraden %>% filter(Regime!="1816-1851"), aes(x = Age, y = Res_number, group=Resuscitations, color=Resuscitations)) +
  geom_line(aes(color=Resuscitations))+ facet_grid(.~ Regime)+ geom_point(aes(color=Resuscitations), size=1)+ 
  scale_color_viridis(discrete=T)+ 
  ylab("Number of resuscitated persons")+theme_bw()


# assesing differences in life expectancy and number of life years lived in each resuscitation state

View(lx_den_compare_res_years)

tau_compare_1<-lx_den_compare_res_years[1,32:48]
colnames(tau_compare_1)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_2<-lx_den_compare_res_years2[1,32:48]
colnames(tau_compare_2)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_3<-lx_den_compare_res_years3[1,32:48]
colnames(tau_compare_3)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_4<-lx_den_compare_res_years4[1,32:48]
colnames(tau_compare_4)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")

tau_compare_5<-lx_den_compare_res_years5[1,32:48]
colnames(tau_compare_5)<-c("tau_1" , "tau_2","tau_3","tau_4","tau_5" , "tau_6","tau_7",
                           "tau_8","tau_9","tau_10", "total","ex_old","ex_new","ex_diff","ex_gap_res1","ex_gap_res_percent", "Regime")




tau_compare<-rbind(tau_compare_1,tau_compare_2, tau_compare_3,tau_compare_4,tau_compare_5)
View(tau_compare)

write.table(tau_compare, file="tau_compare_den.csv", sep=",", row.names = F) #saving for using excel as well

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
                               levels = c("tau_1", "tau_2", "tau_3","tau_4","tau_5_plus"),
                               labels=c("1", "2", "3","4","5+"))


library(latex2exp)
X11(width=10,height=10)

ggplot(data = tau_compare_prop%>% filter(Regime!="1835"), aes(x = factor(tau),y = prop_res, group = factor(Regime) )) + 
  geom_line(aes(linetype=Regime),size=1)+theme_bw()+labs(x = expression(tau[i]),y="Proportion life years lived in each resuscitation state")

