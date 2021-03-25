library(tidyverse)
library(anytime)
library(maps)
library(data.table)

## This script will perform preprocessing steps from the raw datasets
## Be sure to update the working directory and, if necessary, the
## PATH_TO_DATASETS_DIRECTORY and PATH_TO_DATASETS_DIRECTORY_orig variables


setwd("~/")
options(scipen=999)


##Load facebook data.
################# 0. READ DATA

## UPDATE THIS VARIABLE TO THE FOLDER WHERE THE POST-DECEMEBER (2nd period)
## DATASETS ARE STORED
PATH_TO_DATASETS_DIRECTORY = "/datasets_2021/"

# list of all daily survey files:
files <- paste0(paste(getwd(), PATH_TO_DATASETS_DIRECTORY, sep=""),
                list.files(paste(getwd(), PATH_TO_DATASETS_DIRECTORY, sep="")))


##Load facebook data.
################# 0. READ DATA

## UPDATE THIS VARIABLE TO THE FOLDER WHERE THE NOVEMBER-DECEMEBER (1st period)
## DATASETS ARE STORED
PATH_TO_DATASETS_DIRECTORY_orig = "/datasets/"  # this is the folder where you have you survey files


# list of all daily survey files:
files_orig <- paste0(paste(getwd(), PATH_TO_DATASETS_DIRECTORY_orig, sep=""),
                list.files(paste(getwd(), PATH_TO_DATASETS_DIRECTORY_orig, sep="")))

############################## I. PROCESS DATA
# Function to process raw dataset:
process_Dailydf <- function(df, min_date="2020-11-24", max_date="2021-02-10") {
  
  
  df$Date <- as.Date(df$StartDatetime)
  
  df$State <- as.factor(df$A3b)
  df <- filter(df, Date >= min_date & Date <= max_date)
  df <- filter(df, State != 40) # Exclude Puerto Rico
  df <- filter(df, State != 53) # Exclude "Rest of the world"
  
  
  # 
  df$Pre_K <- as.factor(ifelse(df$E1_1==1, "Yes", "No"))
  df$Grades_15 <- as.factor(ifelse(df$E1_2==1, "Yes", "No"))
  df$Grades_68 <- as.factor(ifelse(df$E1_3==1, "Yes", "No"))
  df$Grades_912 <- as.factor(ifelse(df$E1_4==1, "Yes", "No"))
  df$Child_in_K12 <- as.factor(ifelse(df$Pre_K=="Yes" | df$Grades_15 =="Yes" | df$Grades_68 =="Yes" | df$Grades_912=="Yes", 1, 0))
  
  # Filter the sample to only K12 households:
  df <- filter(df, Child_in_K12==1)

  df$feverHH <- as.factor(ifelse(df$A1_1==1, "Yes", "No"))
  df$throatHH <- as.factor(ifelse(df$A1_2==1, "Yes", "No"))
  df$coughHH <- as.factor(ifelse(df$A1_3==1, "Yes", "No"))
  df$shrtbreathHH <- as.factor(ifelse(df$A1_4==1, "Yes", "No"))
  df$diffbreathHH <- as.factor(ifelse(df$A1_5==1, "Yes", "No"))
  
  df$hospER <- as.factor(ifelse(str_detect(df$B7,"5") | str_detect(df$B7,"6") , "Yes", "No"))
  
  df$COVID_tested <- as.factor(ifelse(df$B10==1 , "Yes", "No"))
  df$COVID_positive <- as.factor(ifelse(df$B10a==1 , "Yes", ifelse(df$B10a==2, "No", "Don't know")))
  df$COVID_ever_positive <- as.factor(ifelse(df$B11==1 , "Yes", ifelse(df$B11==2, "No", "Don't know")))
  
  #COVID test reasons:
  df$testd_felt_sick <- as.factor(ifelse(str_detect(df$B10b,"1"), "Yes", "No"))
  df$testd_cntct_someone_sick <- as.factor(ifelse(str_detect(df$B10b,"2"), "Yes", "No"))
  df$testd_other_medcare <- as.factor(ifelse(str_detect(df$B10b,"3"), "Yes", "No"))
  df$testd_work_req <- as.factor(ifelse(str_detect(df$B10b,"4"), "Yes", "No"))
  df$testd_attd_pub_event <- as.factor(ifelse(str_detect(df$B10b,"5"), "Yes", "No"))
  df$testd_crowded_indoor <- as.factor(ifelse(str_detect(df$B10b,"6"), "Yes", "No"))
  df$testd_visit_frfam <- as.factor(ifelse(str_detect(df$B10b,"7"), "Yes", "No"))
  
  df$had_wanted_testd14d <- as.factor(ifelse(df$B12==TRUE, "Yes", "No"))
  
  # Vaccination:
  df$had_vaccine <- as.factor(ifelse(df$V1==1, "Yes", "No"))
  df$vaccine_doses <- as.factor(ifelse(df$V2==1, "1", ifelse(df$V2==2, "2", "Don't know")))
  
  #Social activities 24h
  df$work_outside_activ <- as.factor(ifelse(str_detect(df$C13,"1"), "Yes", "No"))
  df$mkt_grocery_pharm_activ <- as.factor(ifelse(str_detect(df$C13,"2"), "Yes", "No"))
  df$bar_rest_cafe_activ <- as.factor(ifelse(str_detect(df$C13,"3"), "Yes", "No"))
  df$ext_person_activ <- as.factor(ifelse(str_detect(df$C13,"4"), "Yes", "No"))
  df$large_event_activ <- as.factor(ifelse(str_detect(df$C13,"5"), "Yes", "No"))
  df$pub_transit_activ <- as.factor(ifelse(str_detect(df$C13,"6"), "Yes", "No"))
  
  #Masked act 24h
  df$work_outside_msk <- as.factor(ifelse(str_detect(df$C13a,"1"), "Yes", "No"))
  df$mkt_grocery_pharm_msk <- as.factor(ifelse(str_detect(df$C13a,"2"), "Yes", "No"))
  df$bar_rest_cafe_msk <- as.factor(ifelse(str_detect(df$C13a,"3"), "Yes", "No"))
  df$ext_person_msk <- as.factor(ifelse(str_detect(df$C13a,"4"), "Yes", "No"))
  df$large_event_msk <- as.factor(ifelse(str_detect(df$C13a,"5"), "Yes", "No"))
  df$pub_transit_msk <- as.factor(ifelse(str_detect(df$C13a,"6"), "Yes", "No"))
  
  # B11, V1, V2, C13, C13a, C10 (all outcomes), C14, C7, C6, C11, C12, E1, E2, E3, D1, D2, D6, D7, D8, D9, D10, Q64, Q66.
  
  
  df$C10_1_1<- as.numeric(df$C10_2_1)
  df$C10_2_1<- as.numeric(df$C10_2_1)
  df$C10_3_1<- as.numeric(df$C10_3_1)
  df$C10_4_1<- as.numeric(df$C10_4_1)
  df$A4 <- as.numeric(df$A4)
  df$A5_1 <- as.numeric(df$A5_1) # No. of children under 18 years old in household 
  df$A5_2 <- as.numeric(df$A5_2) 
  df$A5_3 <- as.numeric(df$A5_3) 

  
  
  df$Gender <- as.factor(ifelse(df$D1==1, "Male", ifelse(df$D1==2, "Female", ifelse(df$D1==3, "Non-binary",
                                                                                    "Other/Prefer not to answer"))))
  
  #Social contact 24hr (no. people) Replacing *extreme* outliers with NA:
  df$soc_cnct_work <- ifelse(df$C10_1_1 > 1000, NA, df$C10_1_1)
  df$soc_cnct_shop <- ifelse(df$C10_2_1 > 1000, NA, df$C10_2_1)
  df$soc_cnct_gath <-  ifelse(df$C10_3_1 > 1000, NA, df$C10_3_1)
  df$soc_cnct_other <-  ifelse(df$C10_4_1 > 1000, NA, df$C10_4_1)
  
  df$known_sick <-  ifelse(df$A4 > 1000, NA, df$A4)
  df$num_kids <-  ifelse(df$A5_1 > 100, NA, df$A5_1)
  df$num_ppl1 <-  ifelse(df$A5_2 > 100, NA, df$A5_2)
  df$num_ppl2 <-  ifelse(df$A5_2 > 100, NA, df$A5_3)
  df$total_ppl <-  df$num_kids + df$num_ppl1 + df$num_ppl2
  
  # Traveled outside state
  df$trav_out_state <- as.factor(ifelse(df$C6==1 , "Yes", "No"))
  
  # Avoiding social contact
  df$avoidsoc_always <- as.factor(ifelse(df$C7==1 , "Yes", "No"))
  df$avoidsoc_most <- as.factor(ifelse(df$C7==2 , "Yes", "No"))
  df$avoidsoc_some <- as.factor(ifelse(df$C7==3 , "Yes", "No"))
  df$avoidsoc_none <- as.factor(ifelse(df$C7==4 , "Yes", "No"))

  # Contact
  df$cntct_covidpos <- as.factor(ifelse(df$C11==1 , "Yes", "No"))
  # Contact was from family (linked to C11)
  df$cntct_covidpos_fam <- as.factor(ifelse(df$C12==1 , "Yes", "No"))
  
  # Mask usage frq:
  df$mask_always <- as.factor(ifelse(df$C14==1 , "Yes", "No"))
  df$mask_most <- as.factor(ifelse(df$C14==2 , "Yes", "No"))
  df$mask_some <- as.factor(ifelse(df$C14==3 , "Yes", "No"))
  df$mask_little <- as.factor(ifelse(df$C14==4 , "Yes", "No"))
  df$mask_never <- as.factor(ifelse(df$C14==5 , "Yes", "No"))
  df$never_in_public <- as.factor(ifelse(df$C14==6 , "Yes", "No"))

 
  
  df$Age <- as.factor(ifelse(df$D2==1, "18.24", ifelse(df$D2==2, "25.34",
                                                       ifelse(df$D2==3, "35.44", ifelse(df$D2==4, "45.54",
                                                                                        ifelse(df$D2==5, "55.64", ifelse(df$D2==6 | df$D2==7, "65plus", NA)))))))
  
  df$Educational_Level <- as.factor(ifelse(df$D8==1, "Less than HS", ifelse(df$D8==2, "HS", ifelse(df$D8==3, "Some college",
                                                                                                   ifelse(df$D8==4 | df$D8==5 | df$D8==6, "College/Professional Degree",
                                                                                                          ifelse(df$D8==7 | df$D8==8 , "Graduate", NA))))))
  
  df$Occupation <- as.factor(ifelse(df$Q64==1, "Comm/social service", ifelse(df$Q64==2, "Education", ifelse(df$Q64==3, "Arts/entertainment",
                                                                                                            ifelse(df$Q64==4 | df$Q64==5, "Healthcare",  ifelse(df$Q64==6, "Protective service",
                                                                                                                                                                ifelse(df$Q64==7, "Food service", ifelse(df$Q64==8,"Cleaning and maintenance",
                                                                                                                                                                                   ifelse(df$Q64==9, "Personal care and service", ifelse(df$Q64==10, "Sales",
                                                                                                                                                                                            ifelse(df$Q64==11,"Office and admin support", ifelse(df$Q64==12, "Construction and extraction",
                                                                                                                                                                                                                    ifelse(df$Q64==13, "Installation, maintenance, and repair", ifelse(df$Q64==14, "Production",
                                                                                                                                                                                                               ifelse(df$Q64==15, "Transportation and material moving/delivery", "Other")))))))))))))))
  df$educationjob_type <- as.factor(ifelse(df$Q66==1, "preKteacher", ifelse(df$Q66==2, "elemid_teacher", ifelse(df$Q66==3, "second_teacher",
                       ifelse(df$Q66==4, "postsecond_teacher",  ifelse(df$Q66==5, "other", ifelse(df$Q66==6, "teach_assist", "libr/museum tech")))))))
  
  df$Educational_Level<- factor(df$Educational_Level, levels=c("Less than HS", "HS", "Some college", "College/Professional Degree", "Graduate"))
  
  
  
  
  df$zipcode <- as.factor(df$A3)
  df$Employed <- as.factor(ifelse(df$D9==1, "Yes", "No")) # past month
  df$work_payed <- as.factor(ifelse(df$D10==1, "Yes", "No")) # linked to employed
  df$B2 <- ifelse(!is.na(df$B2), paste(" ",df$B2, " "), NA)
  df$fever <- as.factor(ifelse(str_detect(df$B2, "  1,"),1,0))
  df$cough <- as.factor(ifelse(str_detect(df$B2, ",2  ") | str_detect(df$B2, "  2,") | str_detect(df$B2, ",2,"),1,0))
  df$short_brth <- as.factor(ifelse(str_detect(df$B2, ",3  ") | str_detect(df$B2, "  3,") | str_detect(df$B2, ",3,"),1,0))
  df$diffic_brth <- as.factor(ifelse(str_detect(df$B2, ",4  ") | str_detect(df$B2, "  4,") | str_detect(df$B2, ",4,"),1,0))
  df$CLIv1 <- as.factor(ifelse(df$fever==1 & (df$cough ==1 | df$short_brth==1 | df$diffic_brth ==1), "Yes", "No"))
  df$CLIv2 <- as.factor(ifelse(str_detect(df$B2, "13"), "Yes", "No"))
  
  df$Child_IP_full <- ifelse(df$E2_1==2, "Yes", ifelse(df$E2_1==3, "No", "Don't know")) 
  df$Child_IP_part <- ifelse(df$E2_2==2, "Yes", ifelse(df$E2_2==3, "No", "Don't know")) 
  df$Child_IP_any <- ifelse(df$Child_IP_full=="Yes" | df$Child_IP_part=="Yes", "Yes", 
                            ifelse((df$Child_IP_full=="Don't know" & (df$Child_IP_part=="Don't know" | is.na(df$Child_IP_part))) |
                                     (df$Child_IP_part=="Don't know" & (df$Child_IP_full=="Don't know" | is.na(df$Child_IP_full))) , "Don't know",
                                   ifelse((df$Child_IP_full=="No" & (df$Child_IP_part=="No" | is.na(df$Child_IP_part))) |
                                            (df$Child_IP_part=="No" & (df$Child_IP_full=="No" | is.na(df$Child_IP_full))) , 
                                          "No", NA)))
  
  df$Child_IP_type <- ifelse(!is.na(df$Child_IP_full) & df$Child_IP_full=="Yes", "FT",
                             ifelse(!is.na(df$Child_IP_part) & df$Child_IP_part=="Yes", "PT",
                                    ifelse(df$Child_IP_any=="Don't know", "Don't know", "None")))
  
  df$Child_IP_any <- as.factor(df$Child_IP_any)
  df$Child_IP_full <- as.factor(df$Child_IP_full)
  df$Child_IP_part <- as.factor(df$Child_IP_part)
  df$Child_IP_type <- as.factor(df$Child_IP_type)
  
  df$policy_num <- unlist(lapply(df$E3, function(x) {ifelse(!is.na(x) & x!="", length(str_split(x, ",")[[1]]), 0)})) # adjusted for the "" issue
  df$policy_numG <- as.factor(ifelse(df$policy_num >=5, "5+", df$policy_num))
  
  df$E3 <- ifelse(!is.na(df$E3), paste(" ",df$E3, " "), NA)
  df$sch_maskMand_st <- as.factor(ifelse(str_detect(df$E3, "  1"), "Yes", "No"))
  df$sch_maskMand_tch <- as.factor(ifelse(str_detect(df$E3, ",2  ") | str_detect(df$E3, "  2,") |
                                            str_detect(df$E3, ",2,"),"Yes", "No"))
  df$sch_sameTch <- as.factor(ifelse(str_detect(df$E3, ",3  ") | str_detect(df$E3, "  3,") |
                                       str_detect(df$E3, ",3,"), "Yes", "No"))
  df$sch_sameSt <- as.factor(ifelse(str_detect(df$E3, ",4  ") | str_detect(df$E3, "  4,") |
                                      str_detect(df$E3, ",4,"), "Yes", "No"))
  df$sch_outdoorInstr<- as.factor(ifelse(str_detect(df$E3, ",5  ") | str_detect(df$E3, "  5,") |
                                           str_detect(df$E3, ",5,"), "Yes", "No"))
  df$sch_restEntry <- as.factor(ifelse(str_detect(df$E3, ",6  ") | str_detect(df$E3, "  6,") |
                                         str_detect(df$E3, ",6,"), "Yes", "No"))
  df$sch_redClassSize <- as.factor(ifelse(str_detect(df$E3, ",7  ") | str_detect(df$E3, "  7,") |
                                            str_detect(df$E3, ",7,"), "Yes", "No"))
  df$sch_closedCafe <- as.factor(ifelse(str_detect(df$E3, ",8  ") | str_detect(df$E3, "  8,") |
                                          str_detect(df$E3, ",8,"), "Yes", "No"))
  df$sch_closedPlay <- as.factor(ifelse(str_detect(df$E3, ",9  ") | str_detect(df$E3, "  9,") |
                                          str_detect(df$E3, ",9,"), "Yes", "No"))
  df$sch_deskShields <- as.factor(ifelse(str_detect(df$E3, ",10  ") | str_detect(df$E3, "  10,") |
                                           str_detect(df$E3, ",10,"), "Yes", "No"))
  df$sch_xdeskSpace <- as.factor(ifelse(str_detect(df$E3, ",11  ") | str_detect(df$E3, "  11,") |
                                          str_detect(df$E3, ",11,"), "Yes", "No"))
  df$sch_noExtraCurr <- as.factor(ifelse(str_detect(df$E3, ",12  ") | str_detect(df$E3, "  12,") |
                                           str_detect(df$E3, ",12,"), "Yes", "No"))
  df$sch_noShareSupp <- as.factor(ifelse(str_detect(df$E3, ",14  ") | str_detect(df$E3, "  14,") |
                                           str_detect(df$E3, ",14,"), "Yes", "No"))
  df$sch_dailySymptScr <- as.factor(ifelse(str_detect(df$E3, ",15  ") | str_detect(df$E3, "  15,") |
                                             str_detect(df$E3, ",15,"), "Yes", "No"))
  df$sch_Unk <- as.factor(ifelse(str_detect(df$E3, ",16  ") | str_detect(df$E3, "  16,") |
                                   str_detect(df$E3, ",16,"), "Yes", "No"))
  
  householdQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "HH")) {x}}))
  maskQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "mask")) {x}}))
  schoolingQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "sch_")) {x}}))
  kidsQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "Child")) {x}}))
  soc_cnctQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "soc_cnct")) {x}}))
  avoidsocQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "avoidsoc")) {x}}))
  event_mskQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "_msk")) {x}}))
  soc_activQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "_activ")) {x}}))
  testdQ <- unlist(lapply(names(df), function(x) {if(str_detect(x, "testd")) {x}}))
  
  df$anxious <- ifelse(df$C8_1==3 | df$C8_1==4, 1, 0)
  df$depressed <- ifelse(df$C8_2==3 | df$C8_2==4, 1, 0)
  df$isolated  <- ifelse(df$C8_3==3 | df$C8_3==4, 1, 0)
  
  df$covid_worry <- ifelse(df$C9==1, 1, 0)
  df$finance_worry <- ifelse(df$C15==1, 1, 0)
  
  st_names <- c("AL","AK", "AZ","AR","CA","CO" ,
                "CT", "DE", "DC","FL", "GA","HI",
                "ID" ,"IL" , "IN" , "IA","KS", "KY" ,
                "LA" ,"ME" ,"MD" , "MA" ,"MI" ,  "MN",
                "MS","MO" ,"MT" , "NE" , "NV" ,"NH" ,
                "NJ" ,"NM" ,  "NY",  "NC", "ND",  "OH", 
                "OK", "OR" ,  "PA","RI" , "SC","SD" ,
                "TN" ,"TX" ,"UT","VT","VA", "WA",
                "WV" , "WI","WY" )
  
  st_nums <- c(1,2,3,4, 5, 6,
               7,8, 9,10,11,12,
               13,14,15,16,17,18,
               19,20,21, 22, 23, 24, 
               25, 26, 27, 28, 29, 30, 
               31, 32, 33, 34, 35, 36,
               37,38, 39, 41,42, 43,
               44, 45,46, 47,48,49,
               50, 51, 52)
  
  df$State <- as.factor(plyr::mapvalues(df$State, st_nums, st_names))     
  
  df$fips <- as.factor(df$fips)
  df$Date <- as.factor(df$Date)
  df$COVID_positive <- factor(df$COVID_positive, levels = c("Yes", "No"))
  df$COVID_tested <- factor(df$COVID_tested, levels = c("Yes", "No"))
  df$Child_IP_full <- factor(df$Child_IP_full, levels = c("Yes", "No", "Don't know")) # 
  df$Child_IP_part<- factor(df$Child_IP_part, levels = c("Yes", "No", "Don't know"))
  df$Child_IP_any<- factor(df$Child_IP_any, levels = c("Yes", "No", "Don't know")) 
  #df$Child_IP_type<- factor(df$Child_IP_type, levels = c("FT", "PT", "No", "Don't know"))
  df$Employed <- factor(df$Employed, levels = c("Yes", "No"))
  df$CLIv1 <- factor(df$CLIv1, levels = c("Yes", "No"))
  df$CLIv2 <- factor(df$CLIv2, levels = c("Yes", "No"))
  
  
  df <- select(df, c("Date", "period", "State", "weight", "fips", "zipcode", "Gender", "Age","Employed",
                     "Educational_Level", "Occupation", "educationjob_type", "work_payed", "num_kids", "total_ppl",
                     householdQ, kidsQ, schoolingQ, maskQ, event_mskQ, soc_cnctQ,  avoidsocQ,  testdQ,
                     "cntct_covidpos", "cntct_covidpos_fam",
                     "CLIv1", "CLIv2", "COVID_tested", "COVID_positive", "known_sick", "hospER",
                     "trav_out_state", "had_vaccine", "vaccine_doses",
                     "anxious", "depressed", "isolated", "covid_worry", "finance_worry", "soc_cnct_shop", "soc_cnct_gath",
                     "Pre_K", "Grades_15", "Grades_68", "Grades_912", 
                     "policy_numG", "policy_num"))
  return(df)
}


# Read and concatenate each raw daily file into a single dataset:
dfr <- data.table::fread(files[length(files)])
for (i in 1:(length(files)-1)) {
  print(i)
  dfi <- fread(files[i])
  dfi$A4 <- as.character(dfi$A4)
  dfi$A5_1 <- as.numeric(dfi$A5_1)
  dfi$C10_4_1 <- as.character(dfi$C10_4_1)
  dfr <- rbind(dfr, dfi, fill=TRUE)
}

dfr$period <- "post_dec"

dfr_orig <- data.table::fread(files[length(files_orig)])
for (i in 1:(length(files_orig)-1)) {
  print(i)
  dfi <- fread(files_orig[i])
  dfi$A4 <- as.character(dfi$A4)
  dfi$A5_1 <- as.numeric(dfi$A5_1)
  dfi$C10_4_1 <- as.character(dfi$C10_4_1)
  dfr_orig <- rbind(dfr_orig, dfi, fill=TRUE)
}

dfr_orig$period <- "nov-dec"

dfr <- rbind(dfr_orig, dfr)

# Process the full raw dataset:
df <- process_Dailydf(dfr)

df$Pre_K <- factor(df$Pre_K, levels=c("Yes", "No"))
df$Grades1_5 <- factor(df$Grades_15, levels=c("Yes", "No"))
df$Grades6_8 <- factor(df$Grades_68, levels=c("Yes", "No"))
df$Grades9_12 <- factor(df$Grades_912, levels=c("Yes", "No"))
df$Child_IP_any <- factor(df$Child_IP_any , levels=c("Yes", "No", "Don't know"))

# Provisionally turning "NA" into a factor category so that we can summarize data without R "complaining" about
#NAs or excluding them when presenting descriptives (not for the models, where we want actual NA):
df$Child_IP_full <- forcats::fct_explicit_na(df$Child_IP_full)
df$Child_IP_part  <- forcats::fct_explicit_na(df$Child_IP_part)
df$Child_IP_any <- forcats::fct_explicit_na(df$Child_IP_any)
df$Child_IP_type <- forcats::fct_explicit_na(df$Child_IP_type)
df$Employed <- forcats::fct_explicit_na(df$Employed)
df$Gender <- forcats::fct_explicit_na(df$Gender)
df$Age <- forcats::fct_explicit_na(df$Age)
df$COVID_positive <- forcats::fct_explicit_na(df$COVID_positive)
df$COVID_tested <- forcats::fct_explicit_na(df$COVID_tested)
df$CLIv1 <- forcats::fct_explicit_na(df$CLIv1)
df$CLIv2 <- forcats::fct_explicit_na(df$CLIv2)
df$Educational_Level <- forcats::fct_explicit_na(df$Educational_Level)
df$Pre_K <- forcats::fct_explicit_na(df$Pre_K )
df$Grades1_5 <- forcats::fct_explicit_na(df$Grades1_5)
df$Grades6_8 <- forcats::fct_explicit_na(df$Grades6_8)
df$Grades9_12 <- forcats::fct_explicit_na(df$Grades9_12)
df$sch_maskMand_st <- forcats::fct_explicit_na(df$sch_maskMand_st)
df$sch_maskMand_tch <- forcats::fct_explicit_na(df$sch_maskMand_tch)
df$sch_sameTch <- forcats::fct_explicit_na(df$sch_sameTch)
df$sch_sameSt <- forcats::fct_explicit_na(df$sch_sameSt)
df$sch_outdoorInstr <- forcats::fct_explicit_na(df$sch_outdoorInstr)
df$sch_restEntry <- forcats::fct_explicit_na(df$sch_restEntry)
df$sch_redClassSize <- forcats::fct_explicit_na(df$sch_redClassSize)
df$sch_closedCafe <- forcats::fct_explicit_na(df$sch_closedCafe)
df$sch_closedPlay <- forcats::fct_explicit_na(df$sch_closedPlay)
df$sch_deskShields <- forcats::fct_explicit_na(df$sch_deskShields)
df$sch_xdeskSpace <- forcats::fct_explicit_na(df$sch_xdeskSpace)
df$sch_noExtraCurr <- forcats::fct_explicit_na(df$sch_noExtraCurr)
df$sch_noShareSupp <- forcats::fct_explicit_na(df$sch_noShareSupp)
df$sch_dailySymptScr <- forcats::fct_explicit_na(df$sch_dailySymptScr)
saveRDS(df, "data/df_full_ext.rds")

