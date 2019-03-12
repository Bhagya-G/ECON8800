# this code is for the problem set of topic in macro and Labor course
# code is beased on the following two papers
# 1. Acemoglu, D. and Autor, D. (2011). Handbook of Labor Economics, 4b:1043–1171
# 2. Katz, L. F. and Murphy, K. M. (1992). The Quarterly Journal of Economics
# clear environment
rm(list = ls())
# laad libaries
install.packages("lubridate")
library(tidyverse)
library(lubridate)

setwd("C:/Users/s4521039/Dropbox/ECON8800/week2")
# read the downloaded
data_00 <- read_fwf(file="data_00.dat",                     
                    fwf_cols(year      = c(1, 4),
                             serial    = c(5,9),
                             month     = c(10,11),
                            # hwtfinl   = c(12,21),
                             cpsid     = c(12,25),
                             asecflag  = c(26,26),
                             hflag     = c(27,27),
                             asecwth   = c(28,37),
                             pernum    = c(38,39),
                            # wtfinl    = c(50,63),
                             cpsidp    = c(40,53),
                             asecwt    = c(54,63),
                             age       = c(64,65),
                             sex       = c(66,66),
                             race      = c(67,69),
                             educ      = c(70,72),
                             schlcoll  = c(73,73),
                             indly     = c(74,77),
                             classwly  = c(78,79),
                             wkswork1  = c(80,81),
                             wkswork2  = c(82,82),
                             fullpart  = c(83,83),
                             incwage   = c(84,90)),
                    col_types = cols(year       = "i",
                                     serial     = "n",
                                     month      = "i",
                                   #  hwtfinl    = "d",
                                     cpsid      = "d",
                                     asecflag   = "i",
                                     hflag      = "i",
                                     asecwth    = "d",
                                     pernum     = "i",
                                    # wtfinl     = "d",
                                     cpsidp     = "d",
                                     asecwt     = "d",                    
                                     age        = "i",
                                     sex        = "i",
                                     race       = "i",
                                     educ       = "i",
                                     schlcoll   = "i",
                                     indly      = "i",
                                     classwly   = "i",
                                     wkswork1   = "i",
                                     wkswork2   = "i",
                                     fullpart   = "i",
                                     incwage    = "n"))
#data_00$hwtfinl = data_00$hwtfinl/10000
#data_00$wtfinl = data_00$wtfinl/10000
data_00$asecwt = data_00$asecwt/10000
# merge cpi data (see Acemoglu and Autor's Data Appendix)
data_cpi <- read_csv(file = "data_cpi.csv", col_names = c("year","cpi"), col_types=cols(year = "D", cpi = "d"), skip = 1)
data_cpi$year <- year(data_cpi$year)
data_cpi <- data_cpi %>%
  mutate(price_1982 = ifelse(year == 1982, cpi, 0)) %>% # the base year is 1982 (see Acemoglu and Autor's Data Appendix)
  mutate(price_1982 = max(price_1982)) %>%
  mutate(cpi = cpi/price_1982) %>%
  select(year, cpi)
data_00 <- data_00 %>%
  left_join(data_cpi, by = "year")
# replace missing values
data_00 <- data_00 %>%
  mutate(educ = ifelse(educ == 999, NA, educ)) %>%
  mutate(classwly = ifelse(classwly == 99, NA, classwly)) %>%  
  mutate(wkswork2 = ifelse(wkswork2 == 999, NA, wkswork2)) %>%  
  mutate(incwage = ifelse(incwage == 9999999 | incwage == 9999998, NA, incwage)) %>%
  mutate(race = ifelse(race == 999, NA, race))
# create wrkswork variable: worked weeks are in brackets before 1976 see Katz and Murphy (1992)
data_00 <- data_00 %>%
  mutate(wkswork = ifelse(year >= 1976, wkswork1, NA)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 1, 7, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 2, 20, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 3, 33, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 4, 43.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 5, 48.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 6, 51, wkswork))
# handle the top coding issue for income see Katz and Murphy (1992)'s Data section
data_00 <- data_00 %>%
  group_by(year) %>%
  mutate(top_incwage = max(incwage, na.rm = TRUE)) %>%
  mutate(incwage = ifelse(incwage == top_incwage, 1.45*incwage, incwage)) %>%
  ungroup()
# calculate log real wages
data_00 <- data_00 %>%
  mutate(rwage = incwage/cpi/wkswork) %>%
  mutate(lrwage = log(rwage))
# create education duammies
data_00 <- data_00 %>%
  mutate(dfemale = (sex == 2)) # female
data_00 <- data_00 %>%      
  mutate(deduc_1 = ifelse(educ < 70, 1, 0)) %>%                # highshool dropout
  mutate(deduc_2 = ifelse(educ >= 80 & educ < 110, 1, 0)) %>%  # some college
  mutate(deduc_3 = ifelse(educ >= 110 & educ < 123, 1, 0)) %>% # 4 years college 
  mutate(deduc_4 = ifelse(educ >= 123, 1, 0))                  # more than college
# data_00 <- data_00 %>%
#   mutate(drace_1 = (race == 200)) %>% # black
#   mutate(drace_2 = (race > 200)) # nonwhite other
data_00 <- data_00 %>%
  mutate(drace_1 = ifelse(race == 200,1,0)) %>% # black
  mutate(drace_2 = ifelse(race > 200,1,0)) # nonwhite other

# create experience variable: check the IPUMS website for variable definition
data_00 <- data_00 %>%
  mutate(exp = ifelse(educ == 10, age - 8.5, NA)) %>%
  mutate(exp = ifelse(educ == 11, age - 7, exp)) %>%
  mutate(exp = ifelse(educ == 12, age - 8, exp)) %>%
  mutate(exp = ifelse(educ == 13, age - 9, exp)) %>%
  mutate(exp = ifelse(educ == 14, age - 10, exp)) %>%
  mutate(exp = ifelse(educ == 20, age - 11.5, exp)) %>%
  mutate(exp = ifelse(educ == 21, age - 11, exp)) %>%
  mutate(exp = ifelse(educ == 22, age - 12, exp)) %>%
  mutate(exp = ifelse(educ == 30, age - 13.5, exp)) %>%
  mutate(exp = ifelse(educ == 31, age - 13, exp)) %>%
  mutate(exp = ifelse(educ == 32, age - 14, exp)) %>%
  mutate(exp = ifelse(educ == 40, age - 15, exp)) %>%
  mutate(exp = ifelse(educ == 50, age - 16, exp)) %>%
  mutate(exp = ifelse(educ == 60, age - 17, exp)) %>%
  mutate(exp = ifelse(educ == 70, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 71, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 72, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 73, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 80, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 81, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 90, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 91, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 92, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 100, age - 21, exp)) %>%
  mutate(exp = ifelse(educ == 110, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 111, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 120, age - 23.5, exp)) %>%
  mutate(exp = ifelse(educ == 121, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 122, age - 24, exp)) %>%
  mutate(exp = ifelse(educ == 123, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 124, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 125, age - 27, exp))
# sample selection (see Katz and Murphy (1992) and Acemoglu and Autor (2011)'s Data Appendix)
data_00 <- data_00 %>%
  filter(rwage >= 67) %>%                                                       # real wage more than 67 dollars in the 1987 dollar
  filter(age >= 16 & age <= 64) %>%                                             # age equal or above 16 and equal or less than 64
  filter(fullpart == 1) %>%                                                     # work more than 35 hours
  filter(wkswork >= 40) %>%                                                     # work more than 40 weeks
  filter(classwly != 10 | classwly != 13 | classwly != 14) %>%                  # not self-employed
  filter(!((year >= 1992 & year <= 2002) & (indly >= 940 & indly <= 960))) %>%  # not in military
  filter(!(year >= 2003 & indly == 9890)) %>%
  filter(schlcoll == 5 | year < 1986) %>%                                       # no school attendance
  filter(exp >= 0)                                                              # get rid of negative experience

write.csv(data_00,'data_01.csv')

# graphing figures from HERE #######################


attach(data_00)


#the regression
# lm(lrwage~deduc_1+deduc_2+deduc_3+deduc_4+exp+I(exp^2)+(deduc_1+deduc_2+deduc_3+deduc_4)*I(exp^2)+drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4)*exp*(drace_1+drace_2),subset = (dfemale=="TRUE"&year==1962))
# lm(lrwage~deduc_1+deduc_2+deduc_3+deduc_4+exp+I(exp^2)+(deduc_1+deduc_2+deduc_3+deduc_4)*I(exp^2)+drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4)*exp*(drace_1+drace_2),subset = (dfemale=="FALSE"))

#lm(lrwage~deduc_1+deduc_2+deduc_3+deduc_4+poly(exp,4,raw=TRUE)+(deduc_1+deduc_2+deduc_3+deduc_4)*poly(exp,4,raw = TRUE)+drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4)*exp*(drace_1+drace_2),subset = (dfemale=="FALSE"&year==1962))
needexp <-c(5,15,25,35,45)

#defining the dataframes
ed0 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(0),
                   deduc_3 = c(0), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
ed1 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(1), deduc_2 = c(0),
                 deduc_3 = c(0), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
ed2 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(1),
                 deduc_3 = c(0), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
ed3 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(0),
                 deduc_3 = c(1), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
ed4 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(0),
                 deduc_3 = c(0), deduc_4 = c(1), drace_1 = 0, drace_2 = 0)

exed<-list(ed0,ed1,ed2,ed3,ed4)
#exed[2]
#length(exed)
y = 2009-1964+1 #NUmber of years
#y=3

mmale=array(0,dim=c(length(exed),5,y))
mfmale=array(0,dim=c(length(exed),5,y))

wm=array(0,dim = c(5,y)) #male weight
wf=array(0,dim = c(5,y)) #female weight

wam = array(0,dim = c(5,y))#weighted average
waf = array(0,dim = c(5,y))

colldata = array(0,dim = c(y,1))
hsdata = array(0,dim = c(y,1))
fig1data = array(0,dim = c(y,1))

#in columns:ed1,ed2,..., in rows:exp
for (i in 1:y) {
  regmale=lm(lrwage~deduc_1+deduc_2+deduc_3+deduc_4+poly(exp,4,raw=TRUE)+
               (deduc_1+deduc_2+deduc_3+deduc_4)*poly(exp,4,raw = TRUE)+drace_1+drace_2+
               (deduc_1+deduc_2+deduc_3+deduc_4)*exp*(drace_1+drace_2),subset = (dfemale=="FALSE"&year==1964+i-1))
  regfmale=lm(lrwage~deduc_1+deduc_2+deduc_3+deduc_4+poly(exp,4,raw=TRUE)+
               (deduc_1+deduc_2+deduc_3+deduc_4)*poly(exp,4,raw = TRUE)+drace_1+drace_2+
               (deduc_1+deduc_2+deduc_3+deduc_4)*exp*(drace_1+drace_2),subset = (dfemale=="TRUE"&year==1964+i-1))

  
  for (j in 1:length(exed)) {
    mmale[,j,i] = predict(regmale,exed[[j]])
    mfmale[,j,i] = predict(regfmale,exed[[j]])
    
    #Calculate weights
    wm[j,i]=sum(year==1964+i-1 & exp==needexp[j] & dfemale=="FALSE")/sum(year==1964+i-1 & exp %in% needexp & dfemale=="FALSE")
    wf[j,i]=sum(year==1964+i-1 & exp==needexp[j] & dfemale=="TRUE")/sum(year==1964+i-1  & exp %in% needexp & dfemale=="TRUE")
    
    #Calculate weighted averages
     wam[,i] = t(mmale[,,i])%*%wm[,i]
     waf[,i] = t(mfmale[,,i])%*%wf[,i]
     # wwam[i] = matrix(mmale[,,i]%*%wm[,i],nrow=5,ncol=1)
     # wwaf[i] = matrix(mfmale[,,i]%*%wf[,i],nrow=5,ncol=1)
    #colldata[i] = (wam)
  }
  hsdata[i]=(sum(wam[1:2,i])+sum(waf[1:2,i]))
  colldata[i]=(sum(wam[3:5,i])+sum(waf[3:5,i]))
  fig1data[i]=hsdata[i]/colldata[i]

}

#plottting the graph
dyears = seq.int(1964,2009,1)
fig1 = data.frame(log_wage_gap = fig1data, year = dyears)


ggplot(data = fig1) + 
  geom_point(mapping = aes(x = year, y = log_wage_gap)) +
  geom_line(mapping = aes(x = year, y = log_wage_gap))+
  ggtitle('Compositiion adjusted college/high-school log weekly wage ratio, 1963-2008')


#####################Figure 2 #####################################################
data_00 <- data_00 %>%
  mutate(edlvl = ifelse(deduc_1==1,1,ifelse(deduc_2==1,3,ifelse(deduc_3==1,4,ifelse(deduc_4==1,5,2)))))
attach(data_00)

#Creating the groups
# y1=2


#labour supply based on group
lsup<-data_00 %>%
      group_by(year,sex,exp,edlvl) %>%
      summarise(labsup = log(mean(wkswork)))
  
avgwage <-data_00%>%
  group_by(year,sex,exp,edlvl) %>%
  summarise(wwage = mean(rwage))

relw <-avgwage%>%
  group_by(year)%>%
  summarise(ywage = mean(wwage))

avgwage <- merge(x= avgwage, y = relw, by = c("year"))

avgwage <- avgwage%>%
  mutate(relwage = wwage/ywage)

lsupavgwage <- merge(x=lsup,y=avgwage,by=c("year","sex","exp","edlvl"))

lsupavgwage <-lsupavgwage%>%
  mutate(sindex = lsupavgwage$labsup * lsupavgwage$relwage)

supindex<- lsupavgwage%>%
  group_by(year)%>%
  summarise(index = log(mean(sindex)))



ggplot(data = supindex)+ 
  geom_point(mapping = aes(x = year, y = log(index))) +
  geom_line(mapping = aes(x = year, y = log(index)))+
  ggtitle('College/high-school log relative supply')+
  ylab('Log relative supplu index')

#####
# lsupavgwage <-lsupavgwage%>%
#   mutate(tocompare = ifelse(edlvl==1|edlvl==2,1,2))
# 
# lsupavgwage1<-lsupavgwage%>%
#   group_by(year,tocompare)%>%
#   summarise(b=mean(sindex))
# 
# hss<-filter(lsupavgwage1,tocompare==1)
# collg<-filter(lsupavgwage1,tocompare==2)
# forfig2<-merge(x=hss,y=collg,by = c("year"))
# forfig2<-forfig2%>%
#   mutate(rat=b.x/b.y)
# ggplot(data = forfig2)+ 
#   geom_point(mapping = aes(x = year, y = log(rat))) +
#   geom_line(mapping = aes(x = year, y = log(rat)))+
#   ggtitle('College/high-school log relative supply')+
#   ylab('Log relative supplu index')
######


#Only considered full year full time workers :)
