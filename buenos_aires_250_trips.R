library(tidyverse)
library(foreign)
library(readr)
library(utils)
library(RColorBrewer)
library(sf)
library(xtable)
library(ggspatial)
library(rnaturalearth)
library(tidygeocoder)
library(maps)
library(ggrepel)

###Simulation with 10% Capability
data.begrenzt <- read.csv(file.choose(), sep = ";") %>% 
  st_as_sf(coords = c("start_x", "start_y"))%>% 
  st_set_crs(32721) %>% 
  mutate(trav_time = 
           c(as.matrix(read.table(text = as.character(trav_time), 
                                            sep = ":")) %*% c(60, 1, 1/60)),
         wait_time = c(as.matrix(read.table(text =as.character(wait_time),
                                  sep = ":")) %*% c(60,1,1/60)))

summary(data.begrenzt[,c("trav_time", "wait_time")])

#legs
legs.mit.begrenzte.Kapazität <- read.csv(file.choose(), sep = ";") %>% 
  mutate(trav_time =  c(as.matrix(read.table(text = as.character(trav_time),
                                            sep = ":")) %*% c(60, 1, 1/60)),
         wait_time = c(as.matrix(read.table(text = as.character(wait_time), 
                                             sep = ":")) %*% c(60, 1, 1/60)))%>% 
  mutate(mode1 = if_else(mode == "walk", 0,1))

summary(legs.mit.begrenzte.Kapazität[,c("trav_time","wait_time")])

legs.total.time10<- legs.mit.begrenzte.Kapazität %>% group_by(trip_id) %>% 
  summarize(no_legs=n(), 
            sum_walk = sum(trav_time[mode1==0]),
            sum_pt = sum(trav_time[mode1==1]), 
            sum_trav_dist=sum(distance), sum_w_pt = sum(wait_time[mode1==1]), 
            sum_t_pt = (sum_pt - sum_w_pt), sum_total = (sum_walk+sum_pt))


nrow(legs.total.time10)
nrow(data.begrenzt)
summary(legs.total.time10[,-c(1)])
View(legs.total.time10)
names(legs.total.time10)

print(xtable(data.frame("Travel Time Total" = c(59.37,85.81),
                        "Waiting Time" = c(6.933,22.284),
                        "Travel Time Pt" = c(25.87,34.79),
                        row.names = c("Median","Mean"))))

print(xtable(data.frame("Variable" = c("Travel Time Total", "Travel Time Pt", 
                                       "Nr. of Legs", "Waiting Time"),
                        "Mean" = c(85.81, 34.79, 4.111, 22.284),
                        "Median" = c(59.37, 25.87, 3.000, 6.933))))

print(xtable(data.frame("10% Capacity" = c(20.83,28.74),
                        "Infinite Capacity" = c(20.05,24.44),
                        row.names = c("Median","Mean"))),
      main = "Median and Mean of Travel Time")


## Excluding the random distrubution of the homes
legs_ohne_cuadras_begr.kap <- legs.mit.begrenzte.Kapazität %>% 
  filter(mode != "car" & mode != "pt_info") %>% 
  group_by(trip_id, person) %>% mutate(leg_id=1:n()) %>% 
  filter(leg_id != 1 & leg_id != n())

legs.total.time_ohne_cuadras<- legs_ohne_cuadras_begr.kap %>% 
  group_by(trip_id) %>% 
  summarize(no_legs=n(), 
            sum_walk = sum(trav_time[mode1==0]),
            sum_pt = sum(trav_time[mode1==1]), 
            sum_trav_dist=sum(distance), sum_w_pt = sum(wait_time[mode1==1]), 
            sum_t_pt = (sum_pt - sum_w_pt), sum_total = (sum_walk+sum_pt))

summary(legs.total.time_ohne_cuadras)
View(legs.total.time_ohne_cuadras)
nrow(legs.total.time_ohne_cuadras)


print(xtable(data.frame("10% Capacity" = c(7.5833,23.5426),
                        "Infinite Capacity" = c(5.017,7.649),
                        row.names = c("Median","Mean"))),
      main = "Median and Mean of Waiting Times Excluding the first assumption")

################################################################################
################################################################################
#Descriptive Statistics
#Anwendung von Transportmittel 

table(data.begrenzt$longest_distance_mode)/sum(table(data.begrenzt$longest_distance_mode))

#Regression durchführen

### Erste Alternativ <-  Start Points
#1- Erklärte Variablen (yt) Für Formatierung füe Probit/Logit Modell

yt_regression.begrenzt <- data.begrenzt %>% 
  mutate(informal = if_else((wait_time >= 30 | 
                               trav_time >= 120 | 
                               longest_distance_mode =="walk" & 
                               trav_time >= 30), 1, 0)) %>% select(c(3,24))
         

table(yt_regression.begrenzt$informal)/sum(table(yt_regression.begrenzt$informal))

#Regresion mit dem Ausschluss der Annahmen 

y_regression_begrenzt_n <- legs.total.time_ohne_cuadras %>%
  group_by(trip_id) %>% 
  summarise(trav_time = sum_total, wait_time = sum_w_pt) %>% 
  full_join(.,data.begrenzt[,c("trip_id","longest_distance_mode")],
            by = "trip_id") %>% na.omit() %>%
  mutate(informal = if_else(wait_time >= 30 | 
                              trav_time >= 120 | longest_distance_mode =="walk" & 
                              trav_time >= 30), 1, 0) %>%  select(c(1,5))

table(y_regression_begrenzt_n$informal)/sum(table(y_regression_begrenzt_n$informal))


print(xtable(data.frame("10% Capacity" = c(0.8443288,0.1556712),
                        "Infinite Capacity" = c(0.95844349,0.04155651 ),
                        row.names = c("Formal Public Transport Usage",
                                      "Paratransit Usage"))), 
      main = "Percetage of Paratransit usage after excluding the first assumption")


y_regression_unbegrenzt_n <- legs.total.time_un_ohne_cuadras %>% 
  group_by(trip_id) %>% 
  summarise(trav_time = sum_total, wait_time = sum_w_pt) %>%
  full_join(.,data.begrenzt[,c("trip_id","longest_distance_mode")], 
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
xt_einkommen.average <- data.begrenzt %>% st_join(quant_final1) %>% 
  distinct(trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,25)) %>%
  mutate(average_income.qua = (xt_einkommen.average$average_income - 
                                 mean(as.numeric(unlist(xt_einkommen.average$average_income)),
                                      na.rm = T)^2))

summary(xt_einkommen.average)

##Travel and Waiting Time 
zeitlegs<- data.begrenzt[,c(3,5,6)] %>% 
  full_join(., legs.total.time10, by = "trip_id") %>% 
  distinct(trip_id , .keep_all= T ) %>% select(c(1, 4:10))

names(zeitlegs)
summary(zeitlegs)

#Excluding the random starting points 
zeitlegs_no <- data.begrenzt[,c(3,5,6)] %>% 
  full_join(., legs.total.time_un_ohne_cuadras, by = "trip_id") %>% 
  distinct(trip_id, .keep_all = TRUE) %>%  na.omit() %>%  select(c(1, 4:10))

names(zeitlegs_no)
summary(zeitlegs_no)

# Population Density in absolute numbers and squared Values
xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad <-  
  data.begrenzt %>% st_join(pop_density_localidades_sf) %>% 
  distinct(trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,25)) %>% 
  mutate(quadriert = 
           (xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad$density_hab_pro_km2_by_localidad - 
              mean(as.numeric(unlist(xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad$density_hab_pro_km2_by_localidad)), 
                   na.rm = T))^2)

summary(xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad)

#Public Station Density
xt_haltestelledichte <-  data.begrenzt %>% 
  st_join(Paradas_Transporte_Publico_density) %>% 
  distinct(trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,26)) %>% 
  mutate(qua = (density_stations_in_km_2-
                  mean(as.numeric(unlist(density_stations_in_km_2)), 
                       na.rm = T))^2)

summary(xt_haltestelledichte)

#Cordonas
xt_nachcordones <- st_join(data.begrenzt, localidades_sf) %>% 
  distinct(.,trip_id, .keep_all = TRUE) %>% st_drop_geometry() %>% 
  select(c(3,23))


#Rush Hour defined by a Dummy Variable
#transformation of the depart time in min. 
data.begrenzt <- data.ohne.begrenzte.kap %>%
  mutate(trans = c(as.matrix(read.table(text = as.character(data.begrenzt$dep_time), 
                                        sep = ":")) %*% c(60, 1, 1/60))) 

#get choose as rush hours  7:00:00 to 9:30:00 and 16.30:00 to 19:00:00
#1ra Rush Hour
#7*60 = 420 / 9*60+30*1 = 570

x1.rush_h1 <- data.begrenzt %>% 
  mutate(x1.rush_h = if_else(trans >= 420 & trans < 570 , 1, 0)) %>% 
  st_drop_geometry() %>% 
  select(c(3,26))

table(x1.rush_h1$x1.rush_h)/sum(table(x1.rush_h1$x1.rush_h))

#2da Rush Hour
#16*60 + 30*1 = 990 / 19*60 = 1140

x2.rush_h2 <- data.begrenzt %>%
  mutate(x2.rush_h = if_else(trans >= 990 & trans < 1140 , 1, 0)) %>% 
  select(c(3,27))

table(x2.rush_h2$x2.rush_h)/sum(table(x2.rush_h2$x2.rush_h))

#Rush Hour general

x.rush_h <-  data.begrenzt %>% 
  mutate(x.rush_h = 
           if_else(trans >= 420 & trans < 570)|(trans >= 990 & trans < 1140), 
         1, 0) %>% select(c(3,28))

summary(x.rush_h)
table(x.rush_h$x.rush_h)/sum(table(x.rush_h$x.rush_h))

##### Korregiertes Modell
#Start
X1.k1 <-full_join(xt_einkommen.begrenzt, 
                  xt_bevölkerungsdichte.density_hab_pro_km2_by_localidad, 
                  by = "trip_id")
X1.k11 <- full_join(X1.k1, xt_haltestelledichte,  by = "trip_id")
X1.k111 <-full_join(X1.k11, zeitlegs_no_start,  by = "trip_id")                   # zeitlegs_no_start without starting and ending points zeitlegs with thous
X1.k1111 <- full_join(X1.k111, xt_nachcordones.clasificaciones, by = "trip_id")
X1.k11111 <- full_join(X1.k1111, x1.rush_h1, by = "trip_id")
X1.k111111 <- full_join(X1.k11111, x2.rush_h2,  by = "trip_id")
X1.k1111111 <- full_join(X1.k111111, x.rush_h,  by = "trip_id")
X1.k1 <- full_join(X1.k1111111, y_regression_begrenzt_n,  by = "trip_id") %>% 
  na.omit() #y_regression_begrenzt_n  sont yt_regression.begrenzt




summary(X1.k1)
names(X1.k1)

names(X1.k1)[1] <- "trip_id"       ## trip_id
names(X1.k1)[2] <- "income"            ## income
names(X1.k1)[3] <- "income.qua"        ## income quadriert
names(X1.k1)[4] <- "pop_den"            ## Bevölkerungsdichte
names(X1.k1)[5] <- "pop_den.qua"        ## Bevölkerungsdichte quadriert
names(X1.k1)[6] <- "pt_st_den"            ## Bushaltestelledichte
names(X1.k1)[7] <- "pt_st_den.qua"        ## Bushaltestelledichte quadiert
names(X1.k1)[8] <- "no_legs"            ## no_legs
names(X1.k1)[9] <- "sum_total"            ## sum_total travel time
names(X1.k1)[10]<- "sum_walk"            ## sum_walk
names(X1.k1)[11]<- "sum_pt"            ## sum_pt
names(X1.k1)[12]<- "s_t_dist"            ## sum_trav_dist
names(X1.k1)[13] <- "w_pt"                ## Waiting time during pt
names(X1.k1)[14] <- "t_pt"               ##Time in pt
names(X1.k1)[15] <-"dis_to_cnt"           ## Distance to city center
names(X1.k1)[16] <-"x1.rush_h"           ## Rush Hour Dummy Value 7- 7:00:00 to 9:30:00
names(X1.k1)[17] <-"x2.rush_h"           ## Rush Hour Dummy Value 16.30:00 to 19:00:00
names(X1.k1)[18] <-"x.rush_h"           ## Rush Hour Dummy Value 7- 7:00:00 to 9:30:00 or 16.30:00 to 19:00:00
names(X1.k1)[19] <-"y"             ## Independe Value Value 0 or 1


###Analyse of the interdepence of the independe variables
cor(X1.k1, use = "complete.obs") 

library("corrplot")
install.packages("corrplot")
corrplot(cor(X1.k1[,c("income", "pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", "dis_to_cnt")],
             use = "complete.obs"), method = "number", type = "lower", title = "Correlation between the variables")

vif(X1.k1[,c("income", "income.qua" ,"pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", "dis_to_cnt")])



print(xtable(data.frame("Variable" = c("income","income.qua","pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", "dis_to_cnt"),
                        "VIF" = c(4.390740,2.276702,2.536318,3.207140,2.006293,1.926066,1.004343,3.782086),
                        "Average Marginal Effect" = c(5.150943e-02, 3.449771e-03, -1.270382e-02, -5.138442e-02, 7.076686e-03, -5.460399e-03, 7.475280e-03, 1.040088e-06),
                        row.names = c("x2","x2^2","x3","x4","x5","x6","x7","x8"))))

#Excluding the assumption
print(xtable(data.frame("Variable" = c("income","income.qua","pop_den", "pt_st_den","sum_walk","w_pt", "x.rush_h", "dis_to_cnt"),
                        "VIF" = c(4.408950,2.277257,2.440026,3.118981,1.958415,1.873745,1.003778,3.810162),
                        "Average Marginal Effect" = c(-1.362241e-03, -1.504600e-04, -5.389523e-04, -8.330631e-04, 5.655907e-03, -4.685352e-03, 6.952420e-06, 3.402324e-07),
                        row.names = c("x2","x2^2","x3","x4","x5","x6","x7","x8"))))

### Lineare Regression 

##OLS Regresion
#starting points
olsreg1.k <- lm(y ~ log(income) + log(income.qua) + log(pop_den)+
                  log(pt_st_den)
                +sum_walk + w_pt + x.rush_h + dis_to_cnt, 
                data = X1.k1)

summary(olsreg1.k)
coef(olsreg1.k)

polsreg1.k <- predict(olsreg1.k)
summary(polsreg1.k)

plot(olsreg1.k)



### Logit
#starting points
logit1.k1 <- glm(y ~ log(income) + log(income.qua) + log(pop_den)+
                  log(pt_st_den)
                +sum_walk + w_pt + x.rush_h + log(dis_to_cnt), 
                family = binomial (link = "logit"), data = X1.k1)

summary(logit1.k1)

#Predicted Probability
plogit1.k1 <- predict(logit1.k1, type="response")
summary(plogit1.k1)

#Odd Ratio
exp(logit1.k1$coefficients)               

# Logit model average marginal effects
LogitScalar <- mean(dlogis(predict(logit1.k1, type = "link")))
LogitScalar * coef(logit1.k1)

# McFadden's Pseudo R-squared
logit1.k0<-update(logit1.k, formula = X1.k$y ~ 1)
McFadden<- 1-as.vector(logLik(logit1.k)/logLik(logit1.k0))
McFadden


### Probit
probit1.k <-  glm(y ~ log(income) + log(income.qua) + log(pop_den)+
                    log(pt_st_den)
                  +sum_walk + w_pt + x.rush_h + dis_to_cnt, 
                  family = binomial (link = "probit"),
                  data = X1.k1)
summary(probit1.k)

#Predicted Probability
pprobit1.k <- predict(probit1.k, type="response")
summary(pprobit1.k)

#Odd Ratio
exp(probit1.k$coefficients)

# Logit model average marginal effects
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
          title="Results of the Regression -10 % Capacity- excluding the frist und last leg", align=TRUE)

stargazer(linear.1, linear.2, probit.model, title="Regression Results", align=TRUE, 
          dep.var.labels=c("Overall Rating","High Rating"), 
          covariate.labels=c("Handling of Complaints","No Special Privileges", 
                             "Opportunity to Learn","Performance-Based Raises",
                             "Too Critical","Advancement"), omit.stat=c("LL","ser","f"), 
          no.space=TRUE)