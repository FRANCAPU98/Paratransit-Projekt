library(corrplot)
library(faraway)
library(tidyverse)
library(foreign)
library(readr)
library(utils)
library(RColorBrewer)
library(sf)
library(xtable)
library(stargazer)
library(ggspatial)
library(rnaturalearth)
library(tidygeocoder)
library(maps)
library(ggrepel)

##Simulation with Limited Capacity ####
data.ohne.begrenzte.kap <- read.csv2(file.choose())

# Data Manipulation
# Time Convertion
data.ohne.begrenzte.kap <- data.ohne.begrenzte.kap %>% 
  mutate(trav_time = c(as.matrix(read.table(text = as.character(trav_time), 
                                            sep = ":")) %*% c(60, 1, 1/60)),
         wait_time =  c(as.matrix(read.table(text = as.character(wait_time),
                                             sep= ":"))%*% c(60,1,1/60)))

# Selection of the important variables to analyze their outliers
data.ohne.begrenzte.kap_pt <- 
  data.ohne.begrenzte.kap[which(data.ohne.begrenzte.kap$longest_distance_mode == 
                                  c("pt","walk")),] %>% 
  st_as_sf(coords = c("start_x", "start_y")) %>% 
  st_set_crs(32721)

summary(data.ohne.begrenzte.kap_pt[,c("trav_time", "wait_time")])

##Legs Data Set ####
legs.ohne.begrenzte.kap <- read.csv(file.choose())

# Data Manipulation 
#Transformation der Zeit in Minuten
legs.ohne.begrenzte.kap <- legs.ohne.begrenzte.kap %>% 
  mutate(trav_time = c(as.matrix(read.table(text = as.character(trav_time),
                                  sep = ":")) %*% c(60, 1, 1/60)),
         wait_time = c(as.matrix(read.table(text = as.character(wait_time), 
                                            sep = ":")) %*% c(60, 1, 1/60)))

summary(legs.ohne.begrenzte.kap[,c("trav_time","wait_time")])

# Conection with the trip data set above

legs.ohne.begrenzte.kap_pt <- data.ohne.begrenzte.kap_pt %>% 
  st_drop_geometry() %>% 
  inner_join(., legs.ohne.begrenzte.kap, by = "trip_id") %>% 
  select(-c(1,2,4:23, 30:39)) %>% na.omit() %>% unique() %>% 
  mutate(mode1 = if_else(mode == "walk", 0, 1))

View(legs.ohne.begrenzte.kap_pt)
names(legs.ohne.begrenzte.kap_pt)

legs.total.time<- legs.ohne.begrenzte.kap_pt %>% group_by(trip_id) %>% 
  summarize(no_legs=n(), 
            sum_walk = sum(trav_time.y[mode1==0]),
            sum_pt = sum(trav_time.y[mode1==1]), 
            sum_trav_dist=sum(distance), sum_w_pt = sum(wait_time.y[mode1==1]), 
            sum_t_pt = (sum_pt - sum_w_pt), sum_total = (sum_walk+sum_pt))

nrow(legs.total.time)
nrow(data.ohne.begrenzte.kap_pt)
summary(legs.total.time[,-c(1)])

# Presentation in LATEX presentation

print(xtable(data.frame("Variable" = 
                          c("Travel Time Total", "Travel Time Pt", 
                            "Nr. of Legs", "Waiting Time"),
                        "Mean" = c(63.9432, 39.50, 4.196, 7.649),
                        "Median" = c(53.4167, 31.70, 3.000, 5.017))))

## Excluding the random distrubution of the homes
legs_ohne_cuadras_ohne_begr.kap_pt <- 
  legs.ohne.begrenzte.kap_pt[which(legs.ohne.begrenzte.kap_pt$mode != "car" &
                                     legs.ohne.begrenzte.kap_pt$mode != 
                                     "pt_info"),]  %>% 
  group_by(trip_id) %>% mutate(leg_id=1:n()) %>% filter(leg_id != 1 & 
                                                          leg_id != n())


legs.total.time_un_ohne_cuadras<- legs_ohne_cuadras_ohne_begr.kap_pt %>% 
  group_by(trip_id) %>% 
  summarize(no_legs=n(), 
            sum_walk = sum(trav_time.y[mode1==0]),
            sum_pt = sum(trav_time.y[mode1==1]), 
            sum_trav_dist=sum(distance), sum_w_pt =
              sum(wait_time.y[mode1==1]), sum_t_pt = 
              (sum_pt - sum_w_pt), sum_total = (sum_walk+sum_pt))

summary(legs.total.time_un_ohne_cuadras)
View(legs.total.time_un_ohne_cuadras)
nrow(legs.total.time_un_ohne_cuadras)

################################################################################
#Descriptive Statistics
# Modal Split Distribution

#Longest Distance Mode
table(data.ohne.begrenzte.kap$longest_distance_mode)/
  sum(table(data.ohne.begrenzte.kap$longest_distance_mode))*100

# Each Mode
table(legs.ohne.begrenzte.kap$mode)/sum(table(legs.ohne.begrenzte.kap$mode))*100

# Modal Split after Filtering the car and walking Mode
table(legs.ohne.begrenzte.kap[which(legs.ohne.begrenzte.kap$mode != "car" & 
                                      legs.ohne.begrenzte.kap$mode != 
                                      "walk"), ]$mode)/
  sum(table(legs.ohne.begrenzte.kap[which(legs.ohne.begrenzte.kap$mode != 
                                            "car" & 
                                            legs.ohne.begrenzte.kap$mode != 
                                            "walk"),]$mode))*100
#Travel time Distributaion 
#Histogramm
hist(log(data.ohne.begrenzte.kap$trav_time))
# Percentage
nrow(data.ohne.begrenzte.kap[which(data.ohne.begrenzte.kap$trav_time < 10), ])/
  nrow(data.ohne.begrenzte.kap)
nrow(data.ohne.begrenzte.kap[which(data.ohne.begrenzte.kap$trav_time >= 10 & 
                                     data.ohne.begrenzte.kap$trav_time < 20),])/
  nrow(data.ohne.begrenzte.kap)
nrow(data.ohne.begrenzte.kap[which(data.ohne.begrenzte.kap$trav_time >= 20 & 
                                     data.ohne.begrenzte.kap$trav_time < 30), ])/
  nrow(data.ohne.begrenzte.kap)
nrow(data.ohne.begrenzte.kap[which(data.ohne.begrenzte.kap$trav_time >= 30 &
                                     data.ohne.begrenzte.kap$trav_time < 60), ])/
  nrow(data.ohne.begrenzte.kap)
nrow(data.ohne.begrenzte.kap[which(data.ohne.begrenzte.kap$trav_time >= 60), ])/
  nrow(data.ohne.begrenzte.kap)

###############################################################################

#Regression
#1- Dependent Variable for the execution of the LOGISTIC REGRESION All the 
#dataset

y_regression_unbegrenzt <- data.ohne.begrenzte.kap_pt %>% 
  mutate(informal = if_else(data.ohne.begrenzte.kap_pt$wait_time >= 30 |
                          data.ohne.begrenzte.kap_pt$trav_time >= 120 | 
                              (data.ohne.begrenzte.kap_pt$longest_distance_mode
                               =="walk" & 
                                 data.ohne.begrenzte.kap_pt$trav_time >= 
                                 30), 1, 0)) %>% 
  select(c(3,24))

summary(y_regression_unbegrenzt)
table(y_regression_unbegrenzt$informal)/sum(table(y_regression_unbegrenzt$informal))

library(xtable)
print(xtable(data.frame("10% Capacity" = c(0.7855147,0.2144853),
                        "Infinite Capacity" = c(0.8993947,0.1006053),
                        row.names = c("Formal Public Transport",
                                      "Informal Public Transport"))))

#Dependet Variable with exception of the assumptions

y_regression_unbegrenzt_n <- legs.total.time_un_ohne_cuadras %>% 
  group_by(trip_id) %>% 
  summarise(trav_time = sum_total, wait_time = sum_w_pt) %>%
  full_join(.,data.ohne.begrenzte.kap_pt[,c("trip_id","longest_distance_mode")], 
                                         by = "trip_id") %>% na.omit() %>% 
  mutate(informal = if_else(y_new_un$wait_time >= 30 |
                              y_new_un$trav_time >= 120 | 
                              (y_new_un$longest_distance_mode =="walk" & 
                                 y_new_un$trav_time >= 
                                 30), 1, 0)) %>% select(c(1,5))

nrow(y_new_un)
View(y_new_un)
summary(y_regression_unbegrenzt_n)
table(y_regression_unbegrenzt_n$informal)/
  sum(table(y_regression_unbegrenzt_n$informal))

library(xtable)
xtable(table(y_regression_unbegrenzt_n$informal)/
         sum(table(y_regression_unbegrenzt_n$informal)),auto = TRUE)

################################################################################
#Independent Variables  
# External Variables should be uploaded
# Income
xt_einkommen.average <- data.ohne.begrenzte.kap_pt %>% st_join(quant_final1) %>% 
  distinct(trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,25)) %>%
  mutate(average_income.qua = (xt_einkommen.average$average_income - 
           mean(as.numeric(unlist(xt_einkommen.average$average_income)),
                na.rm = T)^2))

summary(xt_einkommen.average)

##Travel and Waiting Time 
zeitlegs<- data.ohne.begrenzte.kap_pt[,c(3,5,6)] %>% 
  full_join(., legs.total.time, by = "trip_id") %>% 
  distinct(trip_id , .keep_all= T ) %>% select(-c(2,3))

names(zeitlegs)
summary(zeitlegs)

#Excluding the random starting points 
zeitlegs_no <- data.ohne.begrenzte.kap_pt[,c(3,5,6)] %>% 
  full_join(., legs.total.time_un_ohne_cuadras, by = "trip_id") %>% 
  distinct(trip_id, .keep_all = TRUE) %>%  na.omit() %>%  select(-c(2,3))

names(zeitlegs_no)
summary(zeitlegs_no)

# Population Density in absolute numbers and squared Values
xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad <-  
  data.ohne.begrenzte.kap_pt %>% st_join(pop_density_localidades_sf) %>% 
  distinct(trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,25)) %>% 
  mutate(quadriert = 
           (xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad$density_hab_pro_km2_by_localidad - 
              mean(as.numeric(unlist(xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad$density_hab_pro_km2_by_localidad)), 
                   na.rm = T))^2)

summary(xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad)

#Public Station Density
xt_haltestelledichte <-  data.ohne.begrenzte.kap_pt %>% 
  st_join(Paradas_Transporte_Publico_density) %>% 
  distinct(trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,26)) %>% 
  mutate(qua = (density_stations_in_km_2-
                  mean(as.numeric(unlist(density_stations_in_km_2)), 
                       na.rm = T))^2)

summary(xt_haltestelledichte)

#Cordonas
xt_nachcordones <- st_join(data.ohne.begrenzte.kap_pt, localidades_sf) %>% 
  distinct(.,trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,23))


#Rush Hour defined by a Dummy Variable
#transformation of the depart time in min. 
data.ohne.begrenzte.kap_pt <- data.ohne.begrenzte.kap %>%
  mutate(trans = c(as.matrix(read.table(text = as.character(data.ohne.begrenzte.kap_pt$dep_time), 
                                         sep = ":")) %*% c(60, 1, 1/60))) 
  

#get choose as rush hours  7:00:00 to 9:30:00 and 16.30:00 to 19:00:00
#1ra Rush Hour
#7*60 = 420 / 9*60+30*1 = 570

x1.rush_h1 <- data.ohne.begrenzte.kap_pt %>% 
  mutate(x1.rush_h = if_else(trans >= 420 & trans < 570 , 1, 0)) %>% 
  st_drop_geometry() %>% 
  select(c(3,26))

table(x1.rush_h1$x1.rush_h)/sum(table(x1.rush_h1$x1.rush_h))

#2da Rush Hour
#16*60 + 30*1 = 990 / 19*60 = 1140

x2.rush_h2 <- data.ohne.begrenzte.kap_pt %>%
  mutate(x2.rush_h = if_else(trans >= 990 & trans < 1140 , 1, 0)) %>% 
  select(c(3,27))

table(x2.rush_h2$x2.rush_h)/sum(table(x2.rush_h2$x2.rush_h))

#Rush Hour general

x.rush_h <-  data.ohne.begrenzte.kap_pt %>% 
  mutate(x.rush_h = 
           if_else(trans >= 420 & trans < 570)| (trans >= 990 & trans < 1140), 
         1, 0) %>% select(c(3,28))

summary(x.rush_h)
table(x.rush_h$x.rush_h)/sum(table(x.rush_h$x.rush_h))


#####Regression durchführen
##### Korregiertes Modell
#Start
X1.k1 <-full_join(xt_einkommen.average, 
                  xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad, 
                  by = "trip_id")
X1.k11 <- full_join(X1.k1, xt_haltestelledichte,  by = "trip_id")
X1.k111 <-full_join(X1.k11, zeitlegs_no,  by = "trip_id")         #zeitlegs_no for     #zeitlegs
X1.k1111 <- full_join(X1.k111, xt_nachcordones.clasificaciones,  by = "trip_id")
X1.k11111 <- full_join(X1.k1111, x1.rush_h1,  by = "trip_id")
X1.k111111 <- full_join(X1.k11111, x2.rush_h2,  by = "trip_id")
X1.k1111111 <- full_join(X1.k111111, x.rush_h,  by = "trip_id")
X1.k <- full_join(X1.k1111111, y_regression_unbegrenzt_n,  by = "trip_id") %>% 
  na.omit() ##y_regression_unbegrenzt_n   #y_regression_unbegrenzt




summary(X1.k)
names(X1.k)

names(X1.k)[1] <- "trip_id"       ## trip_id
names(X1.k)[2] <- "income"            ## income
names(X1.k)[3] <- "income.qua"        ## income quadriert
names(X1.k)[4] <- "pop_den"            ## Bevölkerungsdichte
names(X1.k)[5] <- "pop_den.qua"        ## Bevölkerungsdichte quadriert
names(X1.k)[6] <- "pt_st_den"            ## Bushaltestelledichte
names(X1.k)[7] <- "pt_st_den.qua"        ## Bushaltestelledichte quadiert
names(X1.k)[8] <- "no_legs"            ## no_legs
names(X1.k)[9] <- "sum_total"            ## sum_total travel time
names(X1.k)[10]<- "sum_walk"            ## sum_walk
names(X1.k)[11]<- "sum_pt"            ## sum_pt
names(X1.k)[12]<- "s_t_dist"            ## sum_trav_dist
names(X1.k)[13] <- "w_pt"                ## Waiting time during pt
names(X1.k)[14] <- "t_pt"               ##Time in pt
names(X1.k)[15] <-"dis_to_cnt"           ## Distance to city center
names(X1.k)[16] <-"x1.rush_h"           ## Rush Hour Dummy Value 7- 7:00:00 to 9:30:00
names(X1.k)[17] <-"x2.rush_h"           ## Rush Hour Dummy Value 16.30:00 to 19:00:00
names(X1.k)[18] <-"x.rush_h"           ## Rush Hour Dummy Value 7- 7:00:00 to 9:30:00 or 16.30:00 to 19:00:00
names(X1.k)[19] <-"y"             ## Independe Value Value 0 or 1 



###Analyse of the interdepence of the independe variables
cor(X1.k[,c("income", "pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", 
            "dis_to_cnt")], use = "complete.obs") 

corrplot(cor(X1.k[,c("income", "pop_den", "pt_st_den","sum_walk","w_pt", 
                     "x.rush_h", "dis_to_cnt")],
             use = "complete.obs"), method = "number", type = "lower", 
         title = "Correlation between the variables")

vif(X1.k[,c("income", "income.qua" , "pop_den", "pt_st_den","sum_walk","w_pt", 
            "x.rush_h", "dis_to_cnt")])

summary(lm(sum_walk ~ w_pt, data = X1.k))

print(xtable(data.frame("Variable" = c("income","income.qua","pop_den",
                                       "pt_st_den","sum_walk","w_pt", 
                                       "x.rush_h", "dis_to_cnt"),
                        "VIF" = c(4.346999,2.254281,2.536328,3.195971,
                                  11.866572,11.462686,1.005584,3.909396),
                        row.names = c("x2","x2^2","x3","x4","x5","x6","x7","x8"))))



## High Multicollineality between sum_walk and w_pt  R^2 Value 0.9003 VERY HIGH
# EXCLUSION

### Lineare Regression 
##OLS Regresion
#starting points
olsreg1.k <- lm(y ~ log(income)+ log(income.qua) +log(pop_den)+
                  log(pt_st_den)
                +sum_walk + w_pt + x.rush_h + dis_to_cnt, 
                data = X1.k)

summary(olsreg1.k)
coef(olsreg1.k)

polsreg1.k <- predict(olsreg1.k)
summary(polsreg1.k)

plot(olsreg1.k$residuals)

### Logit
#starting points
logit1.k <- glm(y ~ log(income)+ log(income.qua) +log(pop_den)+
                  log(pt_st_den)
                +sum_walk + w_pt + x.rush_h + dis_to_cnt, 
                family = binomial (link = "logit"), data = X1.k)

summary(logit1.k)
names(logit1.k)
plot(logit1.k$residuals)

#Predicted Probability
plogit1.k <- predict(logit1.k, type="response")
summary(plogit1.k)

#Odd Ratio
exp(logit1.k$coefficients)    

# Logit model average marginal effects
LogitScalar <- mean(dlogis(predict(logit1.k, type = "link")))
LogitScalar * coef(logit1.k)


# Table with the assumption
print(xtable(data.frame("Variable" = c("income","income.qua","pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", "dis_to_cnt"),
                        "VIF" = c(4.346999,2.254281,2.536328,3.195971,11.866572,11.462686,1.005584,3.909396),
                        "Average Marginal Effect" = c(1.186768e-02,1.970754e-03,-1.297048e-02,-2.237451e-02,5.056606e-03,-2.844338e-03,-5.271750e-03,9.582618e-08),
                        row.names = c("x2","x2^2","x3","x4","x5","x6","x7","x8"))))

# Table without the assumption
print(xtable(data.frame("Variable" = c("income","income.qua","pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", "dis_to_cnt"),
                        "VIF" = c(4.229364,2.197453,2.480360,3.168201,11.321595,10.906410,1.005549,3.893830),
                        "Average Marginal Effect" = c(-1.447116e-03,-7.970655e-05,1.453310e-04,-1.345957e-03,3.085824e-03,-2.305184e-03,-1.280491e-03,1.215019e-07),
                        row.names = c("x2","x2^2","x3","x4","x5","x6","x7","x8"))))

# McFadden's Pseudo R-squared
logit1.k0<-update(logit1.k, formula = X1.k$y ~ 1)
McFadden<- 1-as.vector(logLik(logit1.k)/logLik(logit1.k0))
McFadden


### Probit
probit1.k <- glm(y ~ log(income)+ log(income.qua) +log(pop_den)+
                   log(pt_st_den)
                 +sum_walk + w_pt + x.rush_h + dis_to_cnt, 
                             family = binomial (link = "probit"), data = X1.k)
summary(probit1.k)

#Predicted Probability
pprobit1.k <- predict(probit1.k, type="response")
summary(pprobit1.k)

#Odd Ratio
exp(probit1.k$coefficients)

# Probit model average marginal effects
ProbitScalar <- mean(dlogis(predict(probit1.k, type = "link")))
ProbitScalar * coef(probit1.k)

# Percent correctly predicted values
table(true = X1.k$y, pred = round(fitted(probit1.k)))
table(true = X1.k$y, pred = round(fitted(logit1.k)))

# McFadden's Pseudo R-squared
probit1.k0<-update(probit1.k, formula= X.k$y ~ 1)
McFadden<- 1-as.vector(logLik(probit1.k)/logLik(probit1.k0))
McFadden

##Transform in LateX format
stargazer(logit1.k, probit1.k, olsreg1.k, 
          title="Results of the Regression -Infinity Capacity- excluding the first and last leg",
          align=TRUE)

stargazer(linear.1, linear.2, probit.model, title="Regression Results", align=TRUE, 
          dep.var.labels=c("Overall Rating","High Rating"), 
          covariate.labels=c("Handling of Complaints","No Special Privileges", 
                             "Opportunity to Learn","Performance-Based Raises",
                             "Too Critical","Advancement"), omit.stat=c("LL","ser","f"), 
          no.space=TRUE)