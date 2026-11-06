### Load packages ##############################################################

  library(dtms)
  library(tidyverse)

  
### Load data ##################################################################

  load("Data/long_hrs50.Rdata")

  
### Edit data ##################################################################    

  # Rename some of the variables
  hrs <- hrs %>% rename('state' = 'worksimple',
                        'id'    = 'hhidpn')

  # Select key variables 
  hrs <- hrs |> select(id,age,gender,state,race,wave,education)
  
  # Add dummies for threshold age 
  hrs$dum65<-ifelse(hrs$age==65,1,0)
  hrs$dum66<-ifelse(hrs$age==66,1,0)
  hrs$dum67<-ifelse(hrs$age>=67,1,0)
  
  # For cohort
  hrs <- hrs |> mutate(year=NA,
                       year=ifelse(wave==11,2012,year),
                       year=ifelse(wave==12,2014,year),
                       year=ifelse(wave==13,2016,year),
                       year=ifelse(wave==14,2018,year),
                       year=ifelse(wave==15,2020,year),
                       year=ifelse(wave==16,2022,year),
                       cohort=year-age)
  
  
  
### Define dtms ################################################################  

  # dtms
  hrsdtms <- dtms(transient=c("working","retired","not working"),
                  absorbing="dead",
                  timescale=seq(50,98,1),
                  timestep=1:3)
  

### Reshape ####################################################################
  
  # Drop 3 obs with missing age
  hrs <- hrs |> filter(!is.na(age))
  
  # Reshape 
  estdata <- dtms_format(data=hrs,
                         dtms=hrsdtms,
                         idvar="id",
                         timevar="age",
                         statevar="state",
                         steplength=TRUE)

  
### First editing and cleaning #################################################
  
  # Restrict to waves including and after 2012 (2000 is wave 5, 2010 is wave 10)
  estdata <- estdata |> filter(wave>=11)
  dim(estdata)  
  
  # Clean
  estdata <- dtms_clean(data=estdata,
                        dtms=hrsdtms)
  
  
### Drop observations ##########################################################

  # Starting point
  dim(estdata)
  
  # Drop if education and/or race is missing
  estdata <- estdata |> filter(!is.na(race) & !is.na(education))
  dim(estdata)
  
  # Drop if state is missing 
  estdata <- estdata |> filter( !is.na(from) & !is.na(to))
  dim(estdata)

  # Drop if race/ethnicity = "Other" (few person years, heterogeneous group)
  estdata <- estdata |> filter(race!="Other")
  dim(estdata)
  
  
### Edit variables #############################################################
  
  estdata$time2 <- estdata$time^2
  estdata$education <- as.factor(estdata$education)
  
  
### Sample splits ##############################################################
  
  # Gender
  estdata_m <- estdata |> filter(gender==0)
  estdata_w <- estdata |> filter(gender==1)
  

### Fit models #################################################################  
  
  # Controls (at the moment without education)
  convar <- c("time","time2",
              "dum65","dum66","dum67",
              "steplength")
  
  # Basic models without education and race/ethnicity
  fit_m <- dtms_fit(data=estdata_m,controls=convar)
  fit_w <- dtms_fit(data=estdata_w,controls=convar)

  
### Values for prediction ######################################################
  
  # Timescale
  timescale <- seq(50,98,2)

  # dtms
  hrspredict <- dtms(transient=c("working","retired","not working"),
                     absorbing="dead",
                     timescale=timescale)
  
  # Timescale, steplength
  reused <- list(time=timescale,
                 time2=timescale^2,
                 dum65=ifelse(timescale==65,1,0),
                 dum66=ifelse(timescale==66,1,0),
                 dum67=ifelse(timescale>=67,1,0),
                 steplength=2)
  

### Predict probabilities ######################################################
  
  probs_m <- dtms_transitions(model=fit_m,dtms=hrspredict,controls=reused)
  probs_w <- dtms_transitions(model=fit_w,dtms=hrspredict,controls=reused)


### Starting distribution ######################################################
  
  start_m <- dtms_start(data=estdata_m,dtms=hrspredict,start_time=50:55)
  start_w <- dtms_start(data=estdata_w,dtms=hrspredict,start_time=50:55)

  
### Expectancies ###############################################################  
  
  wle_m <- dtms_expectancy(probs=probs_m,start_distr=start_m,dtms=hrspredict)
  wle_w <- dtms_expectancy(probs=probs_w,start_distr=start_w,dtms=hrspredict)

  
### Distribution of visits #####################################################
  
  distr_m <- dtms_visits(risk="working",probs=probs_m,start_distr=start_m,dtms=hrspredict)
  distr_w <- dtms_visits(risk="working",probs=probs_w,start_distr=start_w,dtms=hrspredict)