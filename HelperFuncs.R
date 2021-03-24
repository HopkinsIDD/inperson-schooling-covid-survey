##' Function for formatting epi tables.
##' @param tab a tibble from the epi format object
##'
##' @return a gt object formatted pretty
format_epitab <- function(etab, exp_nm ="Test +", unexp_nm ="Test -") {
  rc <- etab%>%select(-p.value)%>%
    gt::gt(rowname_col = "Gender")%>%
    gt::fmt_percent(
      columns=vars(p0,p1),
      decimals=1
    )%>%
    gt::fmt_number(
      columns = vars(riskratio, lower, upper),
      decimals= 2
    )%>% 
    gt::fmt_number(
      columns = vars(`TRUE`,`FALSE`),
      decimals=0
    )%>%
    gt::cols_merge_range(
      col_begin = vars(lower),
      col_end =vars (upper)
    )%>%
    gt::cols_label(
      riskratio="RR",
      `FALSE`=unexp_nm,
      `TRUE`=exp_nm,
      p0="%",
      p1="%",
      lower="95% CI"
    )%>%gt::fmt_missing(
      columns=everything(),
      missing_text = "---")
  
  return(rc)
}


##'Function that does a survey weighted regression on data.
##' doing this so we calculate the subsets correctly
##'
##' @param data
##' @param formula
##' @param ... other arguments to surveyglm
##'
##' @return results of survey glm
survey_reg <- function(data, formula, wts=data$weight, ...) {
  options(survey.lonely.psu="remove")
  
  svy_d <- data%>%
    mutate(weight=wts)%>%
    srvyr::as_survey_design(weights=weight, strata=State)
  
  rc <- svyglm(formula, design=svy_d, family=quasibinomial,...)
  
  return(rc)
}



##' Function that does a fully adjusted analysis and unadjusted analysis foe regression on
##' postive cases and returns a data frame of the combined results.
##' 
##' @param df the data frame to use for the regression
##' @param outcome the outcome of the regression to use
##' @param as_df should the outcome be a data frame.
##' @param cnty_cov include county level covariates other than AR
##' @param n_mit include number of mitigatoin measures
##' 
##' @return a series for regrsion summary tables or no
pos_regs <- function(data, outcome="tst", as_df=FALSE,
                     cnty_cov = TRUE, n_mit=FALSE) {
  
  rc <- list()
  
  
  if(outcome=="tst") {
    data <- data%>%filter(COVID_tested=="Yes")%>%
      mutate(pos=tst_pos)
    outcome_nm <-"Test+"
  } else if(outcome=="tst2") {
    data <- data%>%mutate(pos=tst_pos)
    outcome_nm <- "Overall Test+"
  } else if (outcome=="cli") {
    data<- data%>%mutate(pos=cli_pos)
    outcome_nm <- "COVID like illness"
  } else if (outcome=="cli2") {
    data<- data%>%mutate(pos=cli2_pos)
    outcome_nm <- "Loss of taste/smell"
  } else if (outcome=="hh_cli") {
    data <- data%>%mutate(pos=cliHH_pos)
    outcome_nm <- "COVID like illness in HH"
  } else if (outcome=="hh_pos") {
    data <- data%>%mutate(pos=cntct_HH_test_pos)
    outcome_nm <- "HH member Test+"
  } else if (outcome=="tst_ind"){
    data <- data%>%mutate(pos=tst_pos_ind)
    outcome_nm <- "Indicated Test+"
  } else if(outcome=="tst_rout"){
    data <- data%>%mutate(pos=tst_pos_routine)
    outcome_nm <- "Non-indicated Test+"
  } else if(outcome=="bp") {
    data <- data%>%mutate(pos=prior_bloodprss)
    outcome_nm <- "High Blood Pressure"
  } else {
    stop("Unknown outcome")
  }
  
  ##First look at the raw weighted relationship
  reg_ip_unadj <- data%>%
    survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin)
  
  rc$rtbl_ip_unadj <- 
      gtsummary::tbl_regression(reg_ip_unadj, exponentiate = TRUE)
  

  ##Full adjustment
  if (cnty_cov) {
    if (n_mit) {
      reg_ip_adjfull<- data%>%
        survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                     rel_avg_biwk_AR+
                     IsMale+Age+OccupationRed+
                     Educational_Level+
                     HH_sz+num_kid_recode+
                     mask_level+trav_out_state+
                     bar_rest_cafe_activ +
                     large_event_activ+
                     pub_transit_activ +
                     Population+
                     White+
                     GINI+
                     Poverty+
                     Description+
                     n_interventions
                   )   
    } else {
      reg_ip_adjfull<- data%>%
        survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                     rel_avg_biwk_AR+
                     IsMale+Age+OccupationRed+
                     Educational_Level+
                     HH_sz+num_kid_recode+
                     mask_level+trav_out_state+
                     bar_rest_cafe_activ +
                     large_event_activ+
                     pub_transit_activ +
                     Population+
                     White+
                     GINI+
                     Poverty+
                     Description
                   )   
    }
  } else {
    if (n_mit) {
      reg_ip_adjfull<- data%>%
        survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                     rel_avg_biwk_AR+
                     IsMale+Age+OccupationRed+
                     Educational_Level+
                     HH_sz+num_kid_recode+
                     mask_level+trav_out_state+
                     bar_rest_cafe_activ +
                     large_event_activ+
                     pub_transit_activ +
                     n_interventions
                   )
    } else {
      reg_ip_adjfull<- data%>%
        survey_reg(pos~Child_IP_full_bin+Child_IP_part_bin+
                     rel_avg_biwk_AR+
                     IsMale+Age+OccupationRed+
                     Educational_Level+
                     HH_sz+num_kid_recode+
                     mask_level+trav_out_state+
                     bar_rest_cafe_activ +
                     large_event_activ+
                     pub_transit_activ)
    }
  }
  
  rc$rtbl_ip_adjfull <- 
    gtsummary::tbl_regression(reg_ip_adjfull, exponentiate = TRUE)
  
  
  if (as_df) {
    tmp <- rc$rtbl_ip_unadj$table_body%>%
        mutate(adjusted=FALSE,
               outcome=outcome_nm)
    
    tmp <-  rc$rtbl_ip_adjfull$table_body%>%
                mutate(adjusted=TRUE,
                outcome=outcome_nm)%>%
      bind_rows(tmp)
    
    rc<-tmp
  }
  
  return(rc)
}


##` Calculate the propensity of an individual to have no in person schooling.
##' Split into function to modify df, run model, make predictions
##' 
##' @param df the data frame
##' 
##' @return a data frame with the proppensity scorees added,
##'
prop_to_inhome_df <- function (df) {
  
  df <- df%>%mutate(no_ip=Child_IP_any=="No")%>%
    drop_na(State, OccupationRed, IsMale,
            Age, Employed, num_kid_recode, HH_sz,
            mask_level,
            trav_out_state, 
            bar_rest_cafe_activ,
            large_event_activ,
            pub_transit_activ,
            ext_person_activ,
            mkt_grocery_pharm_activ,
            work_outside_activ,
            Pre_K, Grades_15,
            Grades_68, Grades_912,
            rel_avg_biwk_AR, population,
            White, Black, GINI, BBInternet.HH,
            PopPerSqMile, Poverty, NoHealthIns,
            Employed.Essential, Computer.HH,
            Description, UrbanRural
    )
  
  return(df)
}

prop_to_inhome_model <- function (df) {
  
  df <- prop_to_inhome_df(df)
  
  mdl <- ranger::ranger(no_ip~State+ OccupationRed+ IsMale+
                          Age+ Employed+ num_kid_recode+ HH_sz +
                          mask_level+
                          trav_out_state+ 
                          bar_rest_cafe_activ+
                          large_event_activ+
                          pub_transit_activ+
                          ext_person_activ+
                          mkt_grocery_pharm_activ+
                          work_outside_activ+
                          Pre_K+ Grades_15+
                          Grades_68+ Grades_912+
                          rel_avg_biwk_AR+ population+
                          White+ Black+ GINI+ BBInternet.HH+
                          PopPerSqMile+ Poverty+ NoHealthIns+
                          Employed.Essential+ Computer.HH+
                          Description+ UrbanRural,
                        data=df,
                        probability=TRUE,
                        importance = "impurity")
  return(mdl)
}

prop_to_inhome <- function (df) {
    df <- prop_to_inhome_df(df)
    mdl <- prop_to_inhome_model(df)
    rc<- predict(mdl,df)
    rc<- df%>%mutate(prop_no_ip=rc$predictions[,2])
    return(rc)
}



##' Function to do a standard recode of all of the data.
##' 
##' @param df the data frame to recode
##' 
##' @return a data frame with all of the standard recodes done.
##'
standard_recode <- function(df, min_HH=2) {
  ##Recode COVID Tested
  df <- df%>%mutate(COVID_tested = recode(COVID_tested,  `(Missing)`="No"),
                    tst_pos = COVID_positive=="Yes",
                    COVID_tst_status=if_else(COVID_tested=="No", "Not Tested",
                                             ifelse(tst_pos, "Tested +", "Tested -")))
  
  
  ##CLI 1 recode
  df<- df%>%mutate(cli_pos=ifelse(CLIv1=="(Missing)", NA, CLIv1=="Yes"))
  
  
  ##CLI 2 recode
  df<- df%>%mutate(cli2_pos=ifelse(CLIv2=="(Missing)", NA, CLIv2=="Yes"))
  
  ## HH CLI recode
  ## NOTE: CLIv1_HH is just whether reported symptoms in HH
  ##       cliHH_pos is whether HH or individual CLI symptoms responded
  df <- df %>% mutate(CLIv1_HH = as.factor(ifelse(df$feverHH=="Yes" & (df$coughHH =="Yes" | df$shrtbreathHH=="Yes" | df$diffbreathHH =="Yes"), 
                                                  "Yes", 
                                                  "No")))
  df <- df%>%mutate(cliHH_pos=ifelse(is.na(CLIv1_HH), NA, CLIv1_HH=="Yes" | cli_pos))
  
  ## HH test pos recode
  df <- df %>% mutate(cntct_HH_test_pos = cntct_covidpos=="Yes" & cntct_covidpos_fam=="Yes")
  
  ## Test indicators
  ## indicated = sick/in contact with sick person
  ## 'routine' = for work or medical procedure + not indicated
  ## then make covid outcomes: 
  ## test_pos_ind - positive if indicated, neg/not tested, NA otherwise
  ## test_pos_routine - positive/negative if routine, NA otherwise
  df <- df %>%
    mutate(tested_indic = ifelse(COVID_tested=="Yes",
                                 testd_felt_sick=="Yes" | testd_cntct_someone_sick=="Yes",
                                 NA),
           tested_routine = ifelse(COVID_tested=="Yes",
                                   !tested_indic & (testd_work_req=="Yes" | testd_other_medcare=="Yes"),
                                   NA),
           tst_pos_ind = case_when(tested_indic & tst_pos ~ TRUE,
                                   tested_indic ~ FALSE,
                                   tst_pos ~ NA,
                                   TRUE ~ FALSE),
           tst_pos_routine = case_when(tested_routine & tst_pos ~ TRUE,
                                       tested_routine ~ FALSE,
                                       TRUE ~ NA)
    )
  
  ##Add is make variable
  df <- df%>%mutate(IsMale=Gender=="Male")
  
  ##Recode occupatoin
  df <- df%>%
    mutate(Occupation=ifelse(!is.na(Occupation),as.character(Occupation),
                             ifelse(Employed=="No", "Not Employed", NA)))
  
  df <- df%>%
        mutate(Employed = ifelse(Employed=="(Missing)", NA, Employed))
  
  ##Group all occupations with <10000 respondents into other
  df <- df%>%
    group_by(Occupation)%>%
    mutate(OccupationRed = ifelse(n()<10000, "Other",Occupation))%>%
    ungroup()
  
  ##HH Sizer recode
  # first recalculate to consider negatives, NAs, drop non-whole numbers, etc. then categorize
  df<-df%>%mutate(num_kids = case_when(num_kids %/% 1 != num_kids ~ NA_real_,
                                       num_kids<0 ~ NA_real_,
                                       num_kids>=100 ~ NA_real_,
                                       TRUE ~ num_kids),
                  num_adults = case_when(num_adults %/% 1 != num_adults ~ NA_real_,
                                         num_adults<0 ~ NA_real_,
                                         num_adults>=100 ~ NA_real_,
                                         TRUE ~ num_adults),
                  num_65plus = case_when(num_65plus %/% 1 != num_65plus ~ NA_real_,
                                         num_65plus<0 ~ NA_real_,
                                         num_65plus>=100 ~ NA_real_,
                                         TRUE ~ num_65plus)) %>%
            mutate(total_ppl_recoded = ifelse(is.na(num_kids) & is.na(num_adults) & is.na(num_65plus),
                                             NA,
                                             rowSums(select(.,num_kids, num_adults, num_65plus), na.rm=TRUE)),
                  HH_sz = ifelse(total_ppl_recoded<10, total_ppl_recoded, "10+"),
                  HH_sz=factor(HH_sz, c(as.character(min_HH:9),"10+")))
  
  ##Number of kids/adults/65+ recode
  df <- df%>%mutate(num_kid_recode=ifelse(num_kids>=7,"7+",num_kids),
                    num_adult_recode=ifelse(num_adults>=10,"10+",num_adults),
                    num_65plus_recode=ifelse(num_65plus>=10,"10+",num_65plus))
  
  ##Sickk contacts recode
  df <- df%>%mutate(known_sick_cat=ifelse(known_sick>=10,"10+", floor(known_sick)))
  
  
  df<-df%>%mutate(mask_level=ifelse(mask_always=="Yes","Always",
                                    ifelse(mask_most=="Yes", "Mostly",
                                           ifelse(mask_some=="Yes","Sometimes",
                                                  ifelse(mask_little=="Yes","Rarely",
                                                         ifelse(mask_never=="Yes","Never",
                                                                ifelse(never_in_public=="Yes", "Never in Public",
                                                                NA)))))))
  
  ## Recode of the social gathering and shopping data
  df <- df%>%mutate(`Social Gathering`=cut(soc_cnct_gath, c(0,1,5,Inf), 
                                           right = F, labels=c("0","1-4","5+")),
                    `Shopping`=cut(soc_cnct_shop, c(0,1,5,Inf), 
                                   right = F, labels=c("0","1-4","5+")))
  
  ## Recode avoiding social contact variable
  df <- df %>% mutate(avoidsoc = case_when(avoidsoc_always=="Yes" ~ "Always",
                                           avoidsoc_most=="Yes" ~ "Mostly",
                                           avoidsoc_some=="Yes" ~ "Sometimes",
                                           avoidsoc_none=="Yes" ~ "Never",
                                           TRUE ~ NA_character_))
  
  ## Recode outside activity variables (categorical masked/unmasked/none)
  df <- df %>%
    mutate(work_outside_activ_msk = factor(case_when(work_outside_activ=="Yes" & work_outside_msk=="Yes" ~ "Masked",
                                                     work_outside_activ=="Yes" & work_outside_msk=="No" ~ "Unmasked",
                                                     work_outside_activ=="No" ~ "No activity"),
                                           levels=c("No activity", "Masked", "Unmasked")),
           mkt_grocery_pharm_activ_msk = factor(case_when(mkt_grocery_pharm_activ=="Yes" & mkt_grocery_pharm_msk=="Yes" ~ "Masked",
                                                          mkt_grocery_pharm_activ=="Yes" & mkt_grocery_pharm_msk=="No" ~ "Unmasked",
                                                          mkt_grocery_pharm_activ=="No" ~ "No activity"),
                                                levels=c("No activity", "Masked", "Unmasked")),
           bar_rest_cafe_activ_msk = factor(case_when(bar_rest_cafe_activ=="Yes" & bar_rest_cafe_msk=="Yes" ~ "Masked",
                                                      bar_rest_cafe_activ=="Yes" & bar_rest_cafe_msk=="No" ~ "Unmasked",
                                                      bar_rest_cafe_activ=="No" ~ "No activity"),
                                            levels=c("No activity", "Masked", "Unmasked")),
           ext_person_activ_msk = factor(case_when(ext_person_activ=="Yes" & ext_person_msk=="Yes" ~ "Masked",
                                                   ext_person_activ=="Yes" & ext_person_msk=="No" ~ "Unmasked",
                                                   ext_person_activ=="No" ~ "No activity"),
                                         levels=c("No activity", "Masked", "Unmasked")),
           large_event_activ_msk = factor(case_when(large_event_activ=="Yes" & large_event_msk=="Yes" ~ "Masked",
                                                    large_event_activ=="Yes" & large_event_msk=="No" ~ "Unmasked",
                                                    large_event_activ=="No" ~ "No activity"),
                                          levels=c("No activity", "Masked", "Unmasked")),
           pub_transit_activ_msk = factor(case_when(pub_transit_activ=="Yes" & pub_transit_msk=="Yes" ~ "Masked",
                                                    pub_transit_activ=="Yes" & pub_transit_msk=="No" ~ "Unmasked",
                                                    pub_transit_activ=="No" ~ "No activity"),
                                          levels=c("No activity", "Masked", "Unmasked")))
  
  ##Recode missing and NAs to Nos
  df <- df%>%mutate(Pre_K=replace_na(Pre_K, "No"),
                    Grades_15= replace_na(Grades_15,"No"),
                    Grades_68=replace_na(Grades_68, "No"),
                    Grades_912=replace_na(Grades_912,"No"))
  
  
  
  ##Recode school mitigation variales as binart
  df<- df%>%mutate_at(vars(matches("sch_")), 
                      function(x){ifelse(x=="Yes",TRUE,ifelse(x=="(Missing)",NA,FALSE))})
  
  
  
  ##recode Missing and don'g know for Child IP levels to NAs
  df <- df%>%
    mutate(Child_IP_full_bin=ifelse(Child_IP_full=="(Missing)"|Child_IP_full=="Don't know", NA, Child_IP_full=="Yes"),
           Child_IP_part_bin=ifelse(Child_IP_part=="(Missing)"|Child_IP_part=="Don't know", NA, Child_IP_part=="Yes"),
           Age=na_if(Age,"(Missing)"),
           Age=relevel(Age, ref="35.44"),
           Educational_Level=na_if(Educational_Level,"(Missing)"),
           Educational_Level=relevel(Educational_Level, ref="HS"),
           OccupationRed=relevel(as.factor(OccupationRed), 
                                 ref="Office and admin support"))
  
  
  ##give make a variable that is specifically for folks with paid work
  ##outside th ehome vs. everyone else.
  df <- df%>%mutate(work_outside_paid = ifelse(Employed=="No", FALSE,
                                               work_payed=="Yes"))
  
  
  ## make travel variable (from trav_out_state)
  df <- df%>%mutate(travel = trav_out_state=="Yes")
  
  return(df)
}


##' Function to do the standard merge with the CSSE data
##' 
##' @param df the data frame
##' @param csse the csse data
##' 
##' @return a data frame merged with this data.
##' 
merge_csse <- function(df, csse) {
  
  ##Get the relevant JHU CSSE data.
  ##Let's subset to after Nov 15 and ending the week
  ##of Christmas then aggregate to epiweeks.
  obs_cases <- csse%>%
    filter(Update>="2020-11-15", Update<="2021-1-31")%>%
    mutate(year=lubridate::epiyear(Update),
           wk=lubridate::epiweek(Update))%>%
    mutate(period=ifelse(Update<="2020-12-27","nov-dec","post_dec"))%>%
    group_by(year, wk, FIPS,period)%>%
    summarize(cases=sum(incidI))%>%
    ungroup()%>%
    rename(fips=FIPS)
  
  
  ##Note that the following calls require your census API key be installed
  pops <- tidycensus::get_estimates(geography="county", product="population")%>%
    filter(variable=="POP")%>%
    rename(population=value, fips=GEOID)%>%
    select(fips,population)
  
  
  cmb_cases<-obs_cases%>%
    inner_join(pops)%>%
    group_by(fips, population, period)%>%
    summarize(cs=sum(cases),
              len = max(wk+52*(year-2020))-min(wk+52*(year-2020)),
              avg_biwk_case=cs/(len/2),
              avg_biwk_AR = avg_biwk_case/mean(population))%>%
    ungroup()%>%
    select(-cs, -len)
  
  
  df <- df%>%mutate(fips=str_pad(fips,5,"left",0))%>%
    left_join(cmb_cases)%>%
    mutate(rel_avg_biwk_AR = log2(avg_biwk_AR*1000+1))
  
  return(df)
  
}


##' Functoin expands table by adjusted and not and
##' then plots during GT
##' 

reg_tbl_wrangle <- function(dat, inc_unadj=T) {
  if (inc_unadj) {
    rc<- dat%>%
      gt::gt()%>%
      gt::fmt_number(
        columns=starts_with("estimate"),
        decimals=2
      )%>%
      gt::fmt_number(
        columns=starts_with("conf"),
        decimals=2
      )%>%
      gt::tab_spanner(
        label=c("Test+"),
        columns = ends_with("Test+")
      )%>% 
      gt::tab_spanner(
        label=c("COVID like illness"),
        columns = ends_with("COVID like illness")
      )%>%
      gt::tab_spanner(
        label=c("Loss of taste/smell"),
        columns = ends_with("taste/smell")
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_TRUE_Loss of taste/smell`),
        col_end=vars(`conf.high_TRUE_Loss of taste/smell`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_TRUE_Overall Test+`),
        col_end=vars(`conf.high_TRUE_Overall Test+`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_TRUE_COVID like illness`),
        col_end=vars(`conf.high_TRUE_COVID like illness`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_FALSE_Loss of taste/smell`),
        col_end=vars(`conf.high_FALSE_Loss of taste/smell`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_FALSE_Overall Test+`),
        col_end=vars(`conf.high_FALSE_Overall Test+`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_FALSE_COVID like illness`),
        col_end=vars(`conf.high_FALSE_COVID like illness`)
      )%>%
      gt::cols_label(
        `conf.low_FALSE_Loss of taste/smell`="95% CI",
        `conf.low_TRUE_Loss of taste/smell`="95% CI",
        `conf.low_FALSE_Overall Test+`="95% CI",
        `conf.low_TRUE_Overall Test+`="95% CI",
        `conf.low_FALSE_COVID like illness`="95% CI",
        `conf.low_TRUE_COVID like illness`="95% CI",
        `estimate_FALSE_Loss of taste/smell`="OR",
        `estimate_TRUE_Loss of taste/smell`="adj. OR",
        `estimate_FALSE_Overall Test+`="OR",
        `estimate_TRUE_Overall Test+`="adj OR",
        `estimate_FALSE_COVID like illness`="OR",
        `estimate_TRUE_COVID like illness`="adj OR",
        label="Schooling"
      )%>%
      gt::tab_options(
        row_group.font.weight   = "lighter",
        column_labels.font.weight = "bold"
      )%>%
      gt::row_group_order(
        groups=c(
          "Overall",
          "Grades K or under",
          "Grades 1 to 5",
          "Grades 6 to 8",
          "Grades 9 to 12"
        )
      )%>%
      gt::cols_move(
        columns=vars(`conf.low_TRUE_COVID like illness`),
        after=vars(`estimate_TRUE_COVID like illness`)
      )%>%
      gt::cols_move(
        columns=vars(`conf.low_TRUE_Overall Test+`),
        after=vars(`estimate_TRUE_Overall Test+`)
      )%>%
      gt::cols_move(
        columns=vars(`conf.low_TRUE_Loss of taste/smell`),
        after=vars(`estimate_TRUE_Loss of taste/smell`)
      )
  } else {
    rc<- dat%>%
      gt::gt()%>%
      gt::fmt_number(
        columns=starts_with("estimate"),
        decimals=2
      )%>%
      gt::fmt_number(
        columns=starts_with("conf"),
        decimals=2
      )%>%
      gt::tab_spanner(
        label=c("Test+"),
        columns = ends_with("Test+")
      )%>% 
      gt::tab_spanner(
        label=c("COVID like illness"),
        columns = ends_with("COVID like illness")
      )%>%
      gt::tab_spanner(
        label=c("Loss of taste/smell"),
        columns = ends_with("taste/smell")
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_TRUE_Loss of taste/smell`),
        col_end=vars(`conf.high_TRUE_Loss of taste/smell`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_TRUE_Overall Test+`),
        col_end=vars(`conf.high_TRUE_Overall Test+`)
      )%>%
      gt::cols_merge_range(
        col_begin=vars(`conf.low_TRUE_COVID like illness`),
        col_end=vars(`conf.high_TRUE_COVID like illness`)
      )%>%
      gt::cols_label(
        `conf.low_TRUE_Loss of taste/smell`="95% CI",
        `conf.low_TRUE_Overall Test+`="95% CI",
        `conf.low_TRUE_COVID like illness`="95% CI",
        `estimate_TRUE_Loss of taste/smell`="adj. OR",
        `estimate_TRUE_Overall Test+`="adj OR",
        `estimate_TRUE_COVID like illness`="adj OR",
        label="Schooling"
      )%>%
      gt::tab_options(
        row_group.font.weight   = "lighter",
        column_labels.font.weight = "bold"
      )%>%
      gt::row_group_order(
        groups=c(
          "Overall",
          "Grades K or under",
          "Grades 1 to 5",
          "Grades 6 to 8",
          "Grades 9 to 12"
        )
      )%>%
      gt::cols_move(
        columns=vars(`conf.low_TRUE_COVID like illness`),
        after=vars(`estimate_TRUE_COVID like illness`)
      )%>%
      gt::cols_move(
        columns=vars(`conf.low_TRUE_Overall Test+`),
        after=vars(`estimate_TRUE_Overall Test+`)
      )%>%
      gt::cols_move(
        columns=vars(`conf.low_TRUE_Loss of taste/smell`),
        after=vars(`estimate_TRUE_Loss of taste/smell`)
      )
  }
  return(rc)
}