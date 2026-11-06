### Load Packages ##############################################################

  library(readstata13)
  require(tidyverse)


### Load data ##################################################################

  # rda file, makes reloading a lot faster
  rdafile <- "Data/hrs.Rda"
  
  if(!file.exists(rdafile)) { 
    
    # Data; can be obtained from https://hrs.isr.umich.edu
    dtafile <- "Data/randhrs1992_2022v1.dta"
    
    # Load (slow!)
    hrs <- read.dta13(file=dtafile,
                      convert.factors=FALSE) 
    
    # Save
    save(hrs,file=rdafile)
    
  } else load(rdafile)


  
### Select variables ###########################################################

  hrs <- hrs %>% select(hhidpn,ragender,raddate,radyear,raeduc,raedyrs,rahispan,raracem,rabdate,rabyear,
                       # Wave status: Response indicator (1= in wave)
                       inw1,inw2,inw3,inw4,inw5,inw6,inw7,inw8,inw9,inw10,inw11,inw12,inw13,inw14,inw15,inw16,
                       # Interview status (5 & 6 = dead)
                       r1iwstat,r2iwstat,r3iwstat,r4iwstat,r5iwstat,r6iwstat,r7iwstat,r8iwstat,r9iwstat,r10iwstat,r11iwstat,r12iwstat,r13iwstat,r14iwstat,r15iwstat,r16iwstat,
                       # Interview midpoint date
                       r1iwmid,r2iwmid,r3iwmid,r4iwmid,r5iwmid,r6iwmid,r7iwmid,r8iwmid,r9iwmid,r10iwmid,r11iwmid,r12iwmid,r13iwmid,r14iwmid,r15iwmid,r16iwmid,
                       # Age in years at interview month
                       r1agey_e,r2agey_e,r3agey_e,r4agey_e,r5agey_e,r6agey_e,r7agey_e,r8agey_e,r9agey_e,r10agey_e,r11agey_e,r12agey_e,r13agey_e,r14agey_e,r15agey_e,r16agey_e,
                       # Labor force status
                       r1lbrf,r2lbrf,r3lbrf,r4lbrf,r5lbrf,r6lbrf,r7lbrf,r8lbrf,r9lbrf,r10lbrf,r11lbrf,r12lbrf,r13lbrf,r14lbrf,r15lbrf,r16lbrf,
                       )

  
### Education, race, gender ####################################################

  # Education
  hrs <- hrs |> mutate(education=case_match(raeduc,
                                            5~2, # College or more
                                            c(3,4)~1, # High-school, some college
                                            c(1,2)~0)) # GED or less than high-school
  
  # Race recode
  hrs <- hrs |> mutate(race=NA) |> 
    mutate(race=ifelse(raracem%in%1,"White",race),
           race=ifelse(raracem%in%2,"Black",race),
           race=ifelse(rahispan%in%1,"Hispan",race),
           race=ifelse(raracem%in%3 & rahispan%in%0,"Other",race),
           race=ifelse(raracem%in%3 & is.na(rahispan),"Other",race))
  
  # Gender
  hrs <- hrs |> mutate(gender=case_match(ragender,
                                         2~1, # Women 
                                         1~0))# Men

  
### Rename vars for easier reshaping below #####################################
  
  # Get names of time varying variables consistent
  hrs <- hrs |> rename_with(~paste0("r",1:16,"inw"),starts_with("inw"))
  hrs <- hrs |> rename_with(~paste0("r",1:16,"age"),ends_with("agey_e"))

  # Change name format of time varying variables (not a good solution, but works)
  hrsnames <- str_split_fixed(names(hrs),"r[[:digit:]]{1,2}",2)
  hrsnames <- apply(hrsnames,1,function(x) {paste0(x,collapse="")})
  hrsnumbers <- parse_number(names(hrs))
  hrswhich <- !is.na(hrsnumbers)
  hrsnames[hrswhich] <- paste(hrsnames[hrswhich],hrsnumbers[hrswhich],sep="_")
  names(hrs) <- hrsnames
  
 
### Reshape ####################################################################

  # Get names of longitudinal vars and their ordering right 
  repvars <- grepl("_",names(hrs))   
  repvars <- names(hrs)[repvars]
  repvars <- unique(unlist(lapply(strsplit(repvars,split="_"),function(x)x[1])))
  repvars <- paste(rep(repvars, each = length(1:16)), 1:16, sep = "_")

  # Reshape (pivot_longer is just not intuitive to me, sorry)
  hrs <- reshape(data=as.data.frame(hrs),
                 direction="long",
                 varying=repvars,
                 sep="_",
                 idvar="hhidpn",
                 #times=1:15,
                 timevar="wave")
  
  # Sort 
  hrs <- hrs |> arrange(hhidpn,wave)

  
### Restrict to observed person years ##########################################
  
  # Drop people after death, and when not (yet) in wave
  hrs <- hrs |> filter(iwstat%in%c(1,5))
  
  
### Variables: age/birth cohort/state pension age ##############################

  # Age is missing in the year of death, add
  hrs <- hrs |> mutate(age=ifelse(iwstat==5,radyear-rabyear,age))
  
  # If year of death is not known: year of wave
  hrs <- hrs |> mutate(waveyear=case_match(wave,
                                           1~1992,
                                           2~1994,
                                           3~1996,
                                           4~1998,
                                           5~2000,
                                           6~2002,
                                           7~2004,
                                           8~2006,
                                           9~2008,
                                          10~2010, 
                                          11~2012, 
                                          12~2014,
                                          13~2016, 
                                          14~2018,
                                          15~2020, 
                                          16~2022))
  
  hrs <- hrs |> mutate(age=ifelse(iwstat==5 & is.na(age),waveyear-rabyear,age))
  
  # Cohort & pension age
  hrs <- hrs |> mutate(birth_coh=ifelse(rabyear<=1942,"1942",
                                        ifelse(rabyear>1942 & rabyear<=1959,"1943-1959",
                                               ifelse(rabyear>=1960,"1960",NA))),
                       # Pension age (in years)                       
                       state_pension=ifelse(rabyear<=1942,65,
                                            ifelse(rabyear>1942 & rabyear<=1959,66,
                                                   ifelse(rabyear>=1960,67,NA))))
  

### Variable: employment #######################################################
  
  # Employment (slightly more detailed)
  hrs <- hrs |> mutate(workstatus=case_match(lbrf,
                                             1:2~"working",
                                             3  ~"unemployed",
                                             4:5~"retired",
                                             6:7~"inactive"),
                       # Not working and above statutory retirement age = retired
                       workstatus=ifelse(lbrf%in%6:7&age>=state_pension,
                                         "retired",
                                         workstatus),
                       # Above age 79 no one works anymore
                       workstatus=ifelse(lbrf%in%1:3&age>=79,
                                         "retired",
                                         workstatus),
                       # Simplified employment status
                       worksimple=case_match(workstatus,
                                             c("unemployed","inactive")~"not working",
                                              .default=workstatus)) 
  
  
### State variables including death ############################################

  # Add dead
  hrs <- hrs |> mutate(workstatus=ifelse(iwstat==5,"dead",workstatus),
                       worksimple=ifelse(iwstat==5,"dead",worksimple))
  

### Saving #####################################################################

  save(hrs,file="Data/long_hrs50.Rdata")
  