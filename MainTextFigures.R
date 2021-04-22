## Helper Functions/variables
sch_order <- c("maskMand_st","maskMand_tch" , "restEntry",
               "xdeskSpace" ,   "noShareSupp", "sameSt",
               "redClassSize",  "dailySymptScr", "sameTch",
               "noExtraCurr","closedCafe",   "deskShields" ,
               "closedPlay","outdoorInstr" )

sch_names <-c("student masking",
              "teacher masking",
              "restricted entry",
              "extra space",
              "no supply sharing",
              "same students",
              "reduced class size",
              "daily symptom screen",
              "same teacher",
              "no extracurricular",
              "closed cafe",
              "desk shields",
              "closed play",
              "outdoor instruction")

names(sch_names)<-sch_order

###put data in right form for donut plots
mk_donut_dat <- function(data, strata) {
   data%>%
      summarize_at(vars(matches("sch_")),mean, na.rm=T)%>%
      select(-sch_Unk)%>%
      pivot_longer(everything(), values_to = "yes")%>%
      mutate(no=yes)%>%
      pivot_longer(-name, names_to="cat")%>%
      mutate(ymin=ifelse(cat=="yes",0,value),
             ymax=ifelse(cat=="yes",value,1),
             name=factor(str_remove(name,"sch_"),
                         levels=sch_order),
             strata=strata)
}

##' Get a list of data used by fig 1. Convience function
##' 
fig_1_fig_dat<- function(df) {
   rc <- list()
   
   ##responses by county
   rc$resp_by_cnty <- df%>%
      group_by(fips)%>%
      summarize(n=n())%>%
      ungroup
   
   ##in person scholing by county
   rc$inper_school_by_county <- df %>%
      filter(Child_IP_any%in%c("Yes","No"))%>%
      group_by(fips)%>%
      summarize(n=n(),
                n_ip=sum(Child_IP_any=="Yes"))%>%
      ungroup()%>%
      mutate(pct_ip = n_ip/n*100)
   
   ##peent full time by county
   rc$pct_ft_by_cnty <- df%>% filter(Child_IP_any=="Yes")%>%
      group_by(fips) %>%
      summarize(n=n(),
                pct_ft = mean(Child_IP_full_bin, na.rm=T)*100)%>%
      ungroup()%>%
      filter(n>=10)
   
   
   ##number of mitigations by county
   rc$n_mit_by_county <- df%>% filter(Child_IP_any=="Yes")%>%
      mutate(n_interventions=#Child_IP_part_bin+
                sch_maskMand_st+
                sch_maskMand_tch+
                sch_sameTch+
                sch_sameSt+
                sch_outdoorInstr+
                sch_restEntry+
                sch_redClassSize+
                sch_closedCafe+
                sch_closedPlay+
                sch_deskShields+
                sch_xdeskSpace+
                sch_noExtraCurr+
                sch_noShareSupp+ 
                sch_dailySymptScr)%>%
      filter(n_interventions<=13)%>%
      group_by(fips)%>%
      summarize(n=n(),
                avg_int=mean(n_interventions, na.rm=T))%>%
      ungroup()%>%
      filter(n>=10)
   
   return(rc)
}

##' Figure 1 produces maps of the distribution of respondents and in person schooling
##' nationally.
fig_1 <- function(df) {
  fig_dat <- fig_1_fig_dat(df)
   
  ##Panel A: Respondents with children in HH
   resp_kids <- usmap::plot_usmap(regions="counties", 
                    data=fig_dat$resp_by_cnty,
                    values="n", 
                    color=NA)+
    theme(legend.position = "bottom")+
    scale_fill_continuous(type="viridis", trans="log10")+
    #ggtitle("Number of responsents reporting children in HH.")+
      labs(fill="N respondents")
   
   
   ##Panel B: Proportion reporting in person schooling by county with
   ##>10 responondants
   
   ip_pct <- usmap::plot_usmap(regions="counties", 
                     data=filter(fig_dat$inper_school_by_county, n>=10),
                     values="pct_ip",
                     color=NA)+
     #ggtitle("In person schooling by County (>=10 responses)")+
     colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                  limits=c(0,100), name="percent in person") +
     theme(legend.position = "bottom")
   
   

   
   
  ##The pct reporting FT in counties where at least 10 people are reporting schooling
   
   ft_pct <- usmap::plot_usmap(regions="counties", 
                               data=fig_dat$pct_ft_by_cnty,
                               values="pct_ft",
                               color=NA)+
     #ggtitle("In person schooling by County (>=10 responses)")+
     colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                  limits=c(0,100), name="percent full time") +
     theme(legend.position = "bottom")
   
   
   
   
   ##Now the average number of interventions in those counties where at least 10 
   ##people ace in person schooling
   
   
   cnty_mit <- usmap::plot_usmap(regions="counties", 
                                  data=fig_dat$n_mit_by_county,
                                  values="avg_int", 
                                  color=NA)+
     theme(legend.position = "bottom")+
     scale_fill_distiller(palette="BrBG", direction=1, limits=c(0,11))+
     labs(fill="avg. # mitigations")
     #colorspace::scale_fill_continuous_sequential(palette = "Hawaii", 
      #                                            limits=c(0,14), name="# mitigations")
   
   rc <- cowplot::plot_grid(resp_kids, ip_pct, ft_pct, cnty_mit,
                            labels=c("A","B","C", "D"), ncol=2)
   
   return(rc)
}


##' Figure S1: Both periods and delta between them.
##' 
fig_S1 <- function(df) {
   fig_dat_p1 <- df%>%
      filter(period=="nov-dec")%>%
      fig_1_fig_dat
   
   
   fig_dat_p2 <- df%>%
      filter(period=="post_dec")%>%
      fig_1_fig_dat
 
   
   ##Panel A: Respondents with children in HH
   resp_kids1 <- usmap::plot_usmap(regions="counties", 
                                  data=fig_dat_p1$resp_by_cnty,
                                  values="n", 
                                  color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_continuous(type="viridis", trans="log10")+
      #ggtitle("Number of responsents reporting children in HH.")+
      labs(fill="N respondents")
   
   
   resp_kids2 <- usmap::plot_usmap(regions="counties", 
                                   data=fig_dat_p2$resp_by_cnty,
                                   values="n", 
                                   color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_continuous(type="viridis", trans="log10")+
      #ggtitle("Number of responsents reporting children in HH.")+
      labs(fill="N respondents")
   
   
   resp_by_cnty_diff <- fig_dat_p2$resp_by_cnty%>%
      select(fips, n) %>%
      rename(n2=n)%>%
      inner_join(select(fig_dat_p1$resp_by_cnty, fips, n))%>%
      mutate(n_diff=n2/n)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   resp_kids_diff <- usmap::plot_usmap(regions="counties", 
                                   data=resp_by_cnty_diff,
                                   values="n_diff", 
                                   color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Vik",mid=0, 
                                                   #limits=log10(c(1/12,12)), 
                                                   name="Relative Amount",
                                                   trans="log10") #+
      #scale_fill_continuous(type="viridis", trans="log10")
   
   
   
   ##Panel B: Proportion reporting in person schooling by county with
   ##>10 responondants
   
   ip_pct1 <- usmap::plot_usmap(regions="counties", 
                               data=filter(fig_dat_p1$inper_school_by_county, n>=10),
                               values="pct_ip",
                               color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent in person") +
      theme(legend.position = "bottom")
   
   
   ip_pct2 <- usmap::plot_usmap(regions="counties", 
                                data=filter(fig_dat_p2$inper_school_by_county, n>=10),
                                values="pct_ip",
                                color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent in person") +
      theme(legend.position = "bottom")
   
   
   
    inper_school_by_cnty_diff <- fig_dat_p2$inper_school_by_county%>%
      select(fips, pct_ip) %>%
      rename(pct_ip2=pct_ip)%>%
      inner_join(select(fig_dat_p1$inper_school_by_county, fips, pct_ip))%>%
      mutate(pct_ip_diff=pct_ip2/pct_ip)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   ip_diff <- usmap::plot_usmap(regions="counties", 
                                       data= inper_school_by_cnty_diff,
                                       values="pct_ip_diff", 
                                       color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Vik",mid=0, 
                                                  #limits=log10(c(1/12,12)), 
                                                  name="Relative Amount",
                                                  trans="log10") #+
   #scale_fill_continuous(type="viridis", trans="log10")
   
   
      
   ##The pct reporting FT in counties where at least 10 people are reporting schooling
   
   ft_pct1 <- usmap::plot_usmap(regions="counties", 
                               data=fig_dat_p1$pct_ft_by_cnty,
                               values="pct_ft",
                               color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent full time") +
      theme(legend.position = "bottom")
   
   
   ft_pct2 <- usmap::plot_usmap(regions="counties", 
                                data=fig_dat_p2$pct_ft_by_cnty,
                                values="pct_ft",
                                color=NA)+
      #ggtitle("In person schooling by County (>=10 responses)")+
      colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                   limits=c(0,100), name="percent full time") +
      theme(legend.position = "bottom")
   
   
   
   pct_ft_by_cnty_diff <- fig_dat_p2$pct_ft_by_cnty%>%
      select(fips, pct_ft) %>%
      rename(pct_ft2=pct_ft)%>%
      inner_join(select(fig_dat_p1$pct_ft_by_cnty, fips, pct_ft))%>%
      mutate(pct_ft_diff=pct_ft2/pct_ft)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   ft_diff <- usmap::plot_usmap(regions="counties", 
                                data= pct_ft_by_cnty_diff,
                                values="pct_ft_diff", 
                                color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Vik",mid=0, 
                                                  #limits=log10(c(1/12,12)), 
                                                  name="Relative Amount",
                                                  trans="log10") 
   
   ##Now the average number of interventions in those counties where at least 10 
   ##people ace in person schooling
   
   
   cnty_mit1 <- usmap::plot_usmap(regions="counties", 
                                 data=fig_dat_p1$n_mit_by_county,
                                 values="avg_int", 
                                 color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_distiller(palette="BrBG", direction=1, limits=c(0,11))+
      labs(fill="avg. # mitigations")
   
   
   cnty_mit2 <- usmap::plot_usmap(regions="counties", 
                                  data=fig_dat_p2$n_mit_by_county,
                                  values="avg_int", 
                                  color=NA)+
      theme(legend.position = "bottom")+
      scale_fill_distiller(palette="BrBG", direction=1, limits=c(0,11))+
      labs(fill="avg. # mitigations")
   
     
   
   n_mit_diff <- fig_dat_p2$n_mit_by_county%>%
      select(fips,avg_int) %>%
      rename(avg_int2=avg_int)%>%
      inner_join(select(fig_dat_p1$n_mit_by_county, fips, avg_int))%>%
      mutate(avg_int_diff=avg_int2-avg_int)
   
   
   
   #print(quantile(resp_by_cnty_diff$n_diff, probs=(0:10)/10))
   
   cnty_mit_diff <- usmap::plot_usmap(regions="counties", 
                                data= n_mit_diff,
                                values="avg_int_diff", 
                                color=NA)+
      theme(legend.position = "bottom")+
      colorspace::scale_fill_continuous_diverging(palette = "Red-Green",mid=0, 
                                                  #limits=log10(c(1/12,12)), 
                                                  name="Difference") 
   
   
   ##Return the layed out figure
   rc <- cowplot::plot_grid(resp_kids1, resp_kids2, resp_kids_diff,
                            ip_pct1, ip_pct2, ip_diff,
                            ft_pct1, ft_pct2, ft_diff,
                            cnty_mit1, cnty_mit2, cnty_mit_diff,
                            labels=c("A","","","B","","","C","","","D","",""), ncol=3)
   
   return(rc)
}


###Figure 2: Relation between grade and incidence.
fig_2<-function(single_grade_df, all_ip_reg_stats) {
      effect_plt <-
         all_ip_reg_stats%>%
         filter(adjusted,
                variable%in%c("Child_IP_full_bin",
                             "Child_IP_part_bin"))%>%
         mutate(variable=recode(variable, 
                Child_IP_full_bin="Full Time In-person Schooling",
                Child_IP_part_bin="Part Time In-person Schooling"))%>%
         ggplot(aes(x=factor(Grade, c("K or under", "1 to 5", "6 to 8", 
                                      "9 to 12","overall")), 
                    y=estimate, 
                    ymin=conf.low, ymax=conf.high,
                    color=outcome))+
         geom_pointrange(position=position_dodge(width=.5))+
         geom_hline(yintercept=1)+
         facet_grid(cols=vars(variable))+
         xlab("grade")+
         ylab("odds ratio")+
         #scale_y_log10()+
         scale_color_brewer(name="", type="qual", palette = "Dark2")+
         theme_bw()+
         theme(legend.position="bottom")
         
      
   
      ##reduce df to folks with only one grade
      single_grade_df<- df%>% mutate(Pre_K=Pre_K=="Yes",
                                     Grades_15=Grades_15=="Yes",
                                     Grades_68=Grades_68=="Yes",
                                     Grades_912=Grades_912=="Yes")%>%
      mutate(n_grades = rowSums(select(.,Pre_K,Grades_15,Grades_68,Grades_912)))%>%
      filter(n_grades==1)%>%
      pivot_longer(c(Pre_K,Grades_15,Grades_68,Grades_912),
                   values_to = "InGrade",
                   names_to = "Grade")%>%
      filter(InGrade)%>%
      filter(Child_IP_any=="Yes")
      
      
      only_ftpt_df <- df%>%
         mutate(Child_IP_full=Child_IP_full=="Yes",
                Child_IP_part=Child_IP_part=="Yes")%>%
         filter(Child_IP_full+Child_IP_part==1)
   
   
   to_plt <- single_grade_df%>%
      filter(Grade=="Pre_K")%>%
      mk_donut_dat("Pre K")
   to_plt <- single_grade_df%>%
      filter(Grade=="Grades_15")%>%
      mk_donut_dat("Grades 1-5")%>%
      bind_rows(to_plt)
   to_plt <- single_grade_df%>%
      filter(Grade=="Grades_68")%>%
      mk_donut_dat("Grades 6-8")%>%
      bind_rows(to_plt)
   to_plt <- single_grade_df%>%
      filter(Grade=="Grades_912")%>%
      mk_donut_dat("Grades 9-12")%>%
      bind_rows(to_plt)
   to_plt <- only_ftpt_df%>%
      filter(Child_IP_full)%>%
      mk_donut_dat("All full time")%>%
      bind_rows(to_plt)
   to_plt <- only_ftpt_df%>%
      filter(Child_IP_part)%>%
      mk_donut_dat("All part time")%>%
      bind_rows(to_plt)
   
   donut_plt<- to_plt%>%
      mutate(strata=factor(strata,levels=c("All full time",
                                           "All part time",
                                           "Pre K","Grades 1-5", 
                                           "Grades 6-8","Grades 9-12"
                                           )))%>%
      ggplot(aes(ymin=ymin, ymax=ymax, xmax=4, xmin=3, fill=cat))+
      geom_rect()+
      coord_polar(theta="y") + 
      xlim(c(0, 4))+
      theme_void() +
      scale_fill_manual(values=c("#EEEEEE", "#24803C"))+
      geom_text(aes(label=sprintf("%1.0f%%",value*100)), x=0, y=0,
                size=2.5)+
      facet_grid(rows=vars(strata), cols=vars(name),
                 labeller=labeller(name=sch_names),
                 switch="x") +
      theme(legend.position = "none",
            strip.text.x = element_text(angle=90, size=10, hjust=1),
            strip.text.y = element_text(face="bold",size=10, hjust=0))
   
   rc <- cowplot::plot_grid(effect_plt, donut_plt, ncol = 1,
                            labels = c("A","B"), 
                            rel_heights=c(1,1.5))
   return(rc)
}




###Figure 2: Relation between grade and incidence.
alt_fig_2<-function(single_grade_df, all_ip_reg_stats, mit_ip_reg_stats) {
   effect_plt <-
      all_ip_reg_stats%>%
      filter(adjusted,
             variable%in%c("Child_IP_full_bin",
                           "Child_IP_part_bin"))%>%
      mutate(variable=recode(variable, 
                             Child_IP_full_bin="Full Time In-person Schooling",
                             Child_IP_part_bin="Part Time In-person Schooling"))%>%
      ggplot(aes(x=factor(Grade, c("K or under", "1 to 5", "6 to 8", 
                                   "9 to 12","overall")), 
                 y=estimate, 
                 ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5))+
      geom_hline(yintercept=1)+
      facet_grid(cols=vars(variable))+
      xlab("grade")+
      ylab("odds ratio")+
      #scale_y_log10()+
      scale_color_brewer(name="", type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="none")
   
   
  
   mit_effect_plt <-
      mit_ip_reg_stats%>%
      filter(adjusted,
             variable%in%c("Child_IP_full_bin",
                           "Child_IP_part_bin"))%>%
      mutate(variable=recode(variable, 
                             Child_IP_full_bin="Full Time In-person Schooling",
                             Child_IP_part_bin="Part Time In-person Schooling"))%>%
      ggplot(aes(x=factor(Grade, c("K or under", "1 to 5", "6 to 8", 
                                   "9 to 12","overall")), 
                 y=estimate, 
                 ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5))+
      geom_hline(yintercept=1)+
      facet_grid(cols=vars(variable))+
      xlab("grade")+
      ylab("odds ratio")+
      #scale_y_log10()+
      scale_color_brewer(name="",type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="none")
   
   
    
   ##reduce df to folks with only one grade
   single_grade_df<- df%>% mutate(Pre_K=Pre_K=="Yes",
                                  Grades_15=Grades_15=="Yes",
                                  Grades_68=Grades_68=="Yes",
                                  Grades_912=Grades_912=="Yes")%>%
      mutate(n_grades = rowSums(select(.,Pre_K,Grades_15,Grades_68,Grades_912)))%>%
      filter(n_grades==1)%>%
      pivot_longer(c(Pre_K,Grades_15,Grades_68,Grades_912),
                   values_to = "InGrade",
                   names_to = "Grade")%>%
      filter(InGrade)%>%
      filter(Child_IP_any=="Yes")
   
   
   only_ftpt_df <- df%>%
      mutate(Child_IP_full=Child_IP_full=="Yes",
             Child_IP_part=Child_IP_part=="Yes")%>%
      filter(Child_IP_full+Child_IP_part==1)
   
   
   to_plt <- single_grade_df%>%
      filter(Grade=="Pre_K")%>%
      mk_donut_dat("Pre K")
   to_plt <- single_grade_df%>%
      filter(Grade=="Grades_15")%>%
      mk_donut_dat("Grades 1-5")%>%
      bind_rows(to_plt)
   to_plt <- single_grade_df%>%
      filter(Grade=="Grades_68")%>%
      mk_donut_dat("Grades 6-8")%>%
      bind_rows(to_plt)
   to_plt <- single_grade_df%>%
      filter(Grade=="Grades_912")%>%
      mk_donut_dat("Grades 9-12")%>%
      bind_rows(to_plt)
   to_plt <- only_ftpt_df%>%
      filter(Child_IP_full)%>%
      mk_donut_dat("All full time")%>%
      bind_rows(to_plt)
   to_plt <- only_ftpt_df%>%
      filter(Child_IP_part)%>%
      mk_donut_dat("All part time")%>%
      bind_rows(to_plt)
   
   donut_plt<- to_plt%>%
      mutate(strata=factor(strata,levels=c("All full time",
                                           "All part time",
                                           "Pre K","Grades 1-5", 
                                           "Grades 6-8","Grades 9-12"
      )))%>%
      ggplot(aes(ymin=ymin, ymax=ymax, xmax=4, xmin=3, fill=cat))+
      geom_rect()+
      coord_polar(theta="y") + 
      xlim(c(0, 4))+
      theme_void() +
      scale_fill_manual(values=c("#EEEEEE", "#24803C"))+
      geom_text(aes(label=sprintf("%1.0f%%",value*100)), x=0, y=0,
                size=2.5)+
      facet_grid(rows=vars(strata), cols=vars(name),
                 labeller=labeller(name=sch_names),
                 switch="x") +
      theme(legend.position = "none",
            strip.text.x = element_text(angle=90, size=10, hjust=1),
            strip.text.y = element_text(face="bold",size=10, hjust=0))
   
   legend_l<- cowplot::get_legend(
      effect_plt+ 
         theme(legend.position = "bottom")
   )
   
   
   plts <- cowplot::align_plots(effect_plt, donut_plt, align = 'h', axis="t")
   left <- cowplot::plot_grid(plts[[1]], mit_effect_plt, legend_l,ncol=1,
                              labels=c("A","B"), rel_heights = c(1,1,.1))
   rc <- cowplot::plot_grid(left, plts[[2]], nrow = 1,
                            labels = c("","C"))
   return(rc)
}



##' Figure 3 individual interventions and impact of number
##' of interventios. 
fig_3 <- function(inperson_df, reg_tbl, line_at = NA) {
   
   fig_stps <- inperson_df%>%
      tidyr::drop_na(cli2_pos, n_interventions)%>%
      mutate(cli2_pos=as.numeric(cli2_pos),
             tst_pos=as.numeric(tst_pos),
             cli_pos=as.numeric(cli_pos))%>%
      ggplot(aes(x=n_interventions, y=cli2_pos))+
      geom_smooth(method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#d95f02")+
      stat_smooth(method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE, color="#d95f02")+
      geom_smooth(aes(y=tst_pos), method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#7570b3")+
      stat_smooth(aes(y=tst_pos),
                  method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE,  color="#7570b3")+
      geom_smooth(aes(y=cli_pos), method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#1b9e77")+
      stat_smooth(aes(y=cli_pos),
                  method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE,  color="#1b9e77")+
      theme_bw()+
      xlab("num. interventions")+
      ylab("proportion with outcome")
   
   
   
   mit_effs <- reg_tbl%>%
      filter(str_detect(variable, "sch_")|str_detect(variable, "Child_"))%>%
      filter(str_detect(adjustment,"adjusted"))%>%
      mutate(variable=factor(str_remove(variable,"sch_"),
             levels=c(sch_order, "Child_IP_part_bin")))%>%
      ggplot(aes(x=variable, y=estimate, ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5)) +
      geom_hline(yintercept=1)+
      scale_y_log10()+
      scale_color_brewer(name="",type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="bottom")+
      ylab("odds ratio")+
      xlab(NULL)+
      scale_x_discrete(labels=c(sch_names,
                                Child_IP_part_bin="part time"))+
      theme(axis.text.x = element_text( angle = 45, hjust=1))
   
   if(!is.na(line_at)) {
      mit_effs <- mit_effs +
         geom_hline(yintercept=line_at, linetype="dashed")
   }
   
   rc <- cowplot::plot_grid(fig_stps, mit_effs, ncol = 1,
                            labels = c("A","B"), 
                            rel_heights=c(1,1.3))
   
   return(rc)
   
}


fig_4 <- function(inperson_df, mit_reg_tbl){
   
   mit_reg_plt <- mit_reg_tbl%>%
      filter(variable%in%c("ft_cat","pt_cat"),
             label!="ft_cat",
             label!="pt_cat",
             label!="None")%>%
      mutate(
         label=str_sub(label,start=6),
         label=factor(label, levels=c("[0,1)",
                                      "[1,4)",
                                      "[4,7)",
                                      "[7,10)",
                                      "[10,14)"),
                      labels = c("0","1-3","4-6","7-9","10+")))%>%
      ggplot(aes(x=label, y=estimate, 
                 ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5))+
      facet_grid(cols=vars(variable))+
      geom_hline(yintercept=1)+
      xlab("N mitigation ,easures")+
      ylab("odds ratio")+
      scale_y_log10()+
      scale_color_brewer(name="",type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="top")
   
   
   mk_donut_dat <- function(data, strata) {
      data%>%
         summarize_at(vars(matches("sch_")),mean, na.rm=T)%>%
         select(-sch_Unk)%>%
         pivot_longer(everything(), values_to = "yes")%>%
         mutate(no=yes)%>%
         pivot_longer(-name, names_to="cat")%>%
         mutate(ymin=ifelse(cat=="yes",0,value),
                ymax=ifelse(cat=="yes",value,1),
                name=factor(str_remove(name,"sch_"),
                            levels=sch_order),
                strata=strata)
   }
   to_plt <- inperson_df%>%
      filter(n_interventions>=1 & n_interventions<=3)%>%
      mk_donut_dat("1-3")
   to_plt <- inperson_df%>%
      filter(n_interventions>=4 & n_interventions<=6)%>%
      mk_donut_dat("4-6")%>%
      bind_rows(to_plt)
   to_plt <- inperson_df%>%
      filter(n_interventions>=7 & n_interventions<=9)%>%
      mk_donut_dat("7-9")%>%
      bind_rows(to_plt)
   to_plt <- inperson_df%>%
      filter(n_interventions>=10)%>%
      mk_donut_dat("10+")%>%
      bind_rows(to_plt)
   
   donut_fig <- to_plt%>%
      mutate(strata=factor(strata,levels=c("1-3","4-6","7-9","10+")))%>%
      ggplot(aes(ymin=ymin, ymax=ymax, xmax=4, xmin=3, fill=cat))+
      geom_rect()+
      coord_polar(theta="y") + 
      xlim(c(0, 4))+
      theme_void() +
      scale_fill_manual(values=c("#EEEEEE", "#24803C"))+
      geom_text(aes(label=sprintf("%1.0f%%",value*100)), x=0, y=0,
                size=2.5)+
      facet_grid(rows=vars(strata), cols=vars(name),
                 labeller=labeller(name=sch_names),
                 switch="both") +
      theme(legend.position = "none",
            strip.text.x = element_text(angle=90, size=10, hjust=1),
            strip.text.y = element_text(face="bold",size=10, angle=45))
   
   rc <- cowplot::plot_grid(mit_reg_plt, donut_fig, ncol = 1,
                            labels = c("A","B"), 
                            rel_heights=c(1,1.3))
   
   return(rc)
   
}



##' Figure  4. Looking at the impact of IP schooling stratified by
##' number of interventions, and the individual impact of interventions
##' 
fig_4_old <- function(inperson_df, mit_strata){

   mit_strata_fig <-mit_strata%>%
      filter(str_detect(variable, "Child_"))%>%
      mutate(variable=recode(variable, 
                             Child_IP_full_bin="Full Time In-person Schooling",
                             Child_IP_part_bin="Part Time In-person Schooling"))%>%
      filter(adjusted)%>%
      mutate(n_interventions=factor(n_interventions, 
                                    levels=c("0","1-3","4-6","7-9","10+")))%>%
      ggplot(aes(x=n_interventions, y=estimate, 
                 ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5))+
      facet_grid(cols=vars(variable))+
      geom_hline(yintercept=1)+
      xlab("num. interventions")+
      ylab("odds ratio")+
      scale_y_log10()+
      scale_color_brewer(name="",type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="top")
     
   
   mk_donut_dat <- function(data, strata) {
      data%>%
         summarize_at(vars(matches("sch_")),mean, na.rm=T)%>%
         select(-sch_Unk)%>%
         pivot_longer(everything(), values_to = "yes")%>%
         mutate(no=yes)%>%
         pivot_longer(-name, names_to="cat")%>%
         mutate(ymin=ifelse(cat=="yes",0,value),
                ymax=ifelse(cat=="yes",value,1),
                name=factor(str_remove(name,"sch_"),
                            levels=sch_order),
                strata=strata)
   }
   to_plt <- inperson_df%>%
      filter(n_interventions>=1 & n_interventions<=3)%>%
      mk_donut_dat("1-3")
   to_plt <- inperson_df%>%
      filter(n_interventions>=4 & n_interventions<=6)%>%
      mk_donut_dat("4-6")%>%
      bind_rows(to_plt)
   to_plt <- inperson_df%>%
      filter(n_interventions>=7 & n_interventions<=9)%>%
      mk_donut_dat("7-9")%>%
      bind_rows(to_plt)
   to_plt <- inperson_df%>%
      filter(n_interventions>=10)%>%
      mk_donut_dat("10+")%>%
      bind_rows(to_plt)
   
   donut_fig <- to_plt%>%
      mutate(strata=factor(strata,levels=c("1-3","4-6","7-9","10+")))%>%
      ggplot(aes(ymin=ymin, ymax=ymax, xmax=4, xmin=3, fill=cat))+
      geom_rect()+
      coord_polar(theta="y") + 
      xlim(c(0, 4))+
      theme_void() +
      scale_fill_manual(values=c("#EEEEEE", "#24803C"))+
      geom_text(aes(label=sprintf("%1.0f%%",value*100)), x=0, y=0,
                size=2.5)+
      facet_grid(rows=vars(strata), cols=vars(name),
                 labeller=labeller(name=sch_names),
                 switch="both") +
      theme(legend.position = "none",
            strip.text.x = element_text(angle=90, size=10, hjust=1),
            strip.text.y = element_text(face="bold",size=10, angle=45))
   
   rc <- cowplot::plot_grid(mit_strata_fig, donut_fig, ncol = 1,
                            labels = c("A","B"), 
                            rel_heights=c(1,1.3))
   
   return(rc)
      
}


##' Figure S2. Map pct of all mitigation measures in counties with 10 or more
##' in school respondents
##' 
##' 
fig_S2 <- function(df) {
   

 
   mits <- c("sch_maskMand_st","sch_maskMand_tch" , "sch_restEntry",
                  "sch_xdeskSpace" ,   "sch_noShareSupp", "sch_sameSt",
                  "sch_redClassSize",  "sch_dailySymptScr", "sch_sameTch",
                  "sch_noExtraCurr","sch_closedCafe",   "sch_deskShields" ,
                  "sch_closedPlay","sch_outdoorInstr" )
   
   sch_names <-c("student masking",
                 "teacher masking",
                 "restricted entry",
                 "extra space",
                 "no supply sharing",
                 "same students",
                 "reduced class size",
                 "daily symptom screen",
                 "same teacher",
                 "no extracurricular",
                 "closed cafe",
                 "desk shields",
                 "closed play",
                 "outdoor instruction")
   
       
   df <- df%>%filter(Child_IP_any=="Yes")
   
   maps <- list()
   
   for (i in 1:length(mits)) {
      map_dat <- df%>%
         group_by(fips)%>%
         summarize(n=n(),
                   ints=sum(!!sym(mits[i]),na.rm=T),
                   pct=ints/n*100)%>%
         ungroup()%>%
         filter(n>=10)
      
      maps[[i]] <- usmap::plot_usmap(regions="counties", 
                                     data=map_dat,
                                     values="pct",
                                     color=NA)+
         #ggtitle("In person schooling by County (>=10 responses)")+
         colorspace::scale_fill_continuous_divergingx(palette = "Tropic",mid=50, 
                                                      limits=c(0,100), name="pct") +
         ggtitle(sch_names[i])+
         theme(legend.position = "none")
   }
   
   rc <- cowplot::plot_grid(plotlist=maps,
                            labels=LETTERS[1:14])
 
   legend_l<- cowplot::get_legend(
      maps[[1]]+ 
         theme(legend.position = "bottom")
   )
   
   rc <- cowplot::plot_grid(rc, legend_l, ncol=1,rel_heights = c(4,.1))
   
   return(rc)
}


##' Figure S3(?). Secondary outcomes + in person full/part effect plot only
##' 
fig_S3<-function(all_ip_reg_stats) {
   rc <-
      all_ip_reg_stats%>%
      filter(adjusted,
             variable%in%c("Child_IP_full_bin",
                           "Child_IP_part_bin"))%>%
      mutate(variable=recode(variable, 
                             Child_IP_full_bin="Full Time In-person Schooling",
                             Child_IP_part_bin="Part Time In-person Schooling"))%>%
      ggplot(aes(x=factor(Grade, c("K or under", "1 to 5", "6 to 8", 
                                   "9 to 12","Overall")), 
                 y=estimate, 
                 ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5))+
      geom_hline(yintercept=1)+
      facet_grid(cols=vars(variable))+
      xlab("grade")+
      ylab("odds ratio")+
      #scale_y_log10()+
      scale_color_brewer(name="", type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="bottom")

   return(rc)
}

##' Fig S4: secondary outcomes + individual interventions and impact 
##' of number of interventions
##'  
fig_S4 <- function(inperson_df, reg_tbl, outcome_names=NULL) {
   
   if(!is.null(outcome_names)){
      reg_tbl <- reg_tbl %>%
                 mutate(outcome = factor(outcome, labels=outcome_names))
   }
   
   fig_stps <- inperson_df%>%
      tidyr::drop_na(n_interventions)%>%
      mutate(cliHH_pos=as.numeric(cliHH_pos),
             cntct_HH_test_pos=as.numeric(cntct_HH_test_pos),
             tst_pos_ind=as.numeric(tst_pos_ind),
             tst_pos_routine=as.numeric(tst_pos_routine))%>%
      ggplot(aes(x=n_interventions, y=cliHH_pos))+
      geom_smooth(method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#d95f02")+
      stat_smooth(method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE, color="#d95f02")+
      geom_smooth(aes(y=cntct_HH_test_pos), method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#7570b3")+
      stat_smooth(aes(y=cntct_HH_test_pos),
                  method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE,  color="#7570b3")+
      geom_smooth(aes(y=tst_pos_ind), method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#1b9e77")+
      stat_smooth(aes(y=tst_pos_ind),
                  method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE,  color="#1b9e77")+
      geom_smooth(aes(y=tst_pos_routine), method="glm",
                  method.args = list(family = "binomial"), 
                  se = FALSE, color="#E7298A")+
      stat_smooth(aes(y=tst_pos_routine),
                  method="gam",
                  formula=y~s(x),
                  method.args = list(family="binomial"),
                  linetype="dashed",
                  se = FALSE,  color="#E7298A")+
      theme_bw()+
      xlab("num. interventions")+
      ylab("proportion with outcome")
   
   
   
   mit_effs <- reg_tbl%>%
      filter(str_detect(variable, "sch_")|str_detect(variable, "Child_"))%>%
      filter(str_detect(adjustment,"adjusted"))%>%
      mutate(variable=factor(str_remove(variable,"sch_"),
                             levels=c(sch_order, "Child_IP_part_bin")))%>%
      ggplot(aes(x=variable, y=estimate, ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5)) +
      geom_hline(yintercept=1)+
      scale_y_log10()+
      scale_color_brewer(name="", type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="bottom")+
      ylab("odds ratio")+
      xlab(NULL)+
      scale_x_discrete(labels=c(sch_names,
                                Child_IP_part_bin="part time"))+
      theme(axis.text.x = element_text( angle = 45, hjust=1))
   
   rc <- cowplot::plot_grid(fig_stps, mit_effs, ncol = 1,
                            labels = c("A","B"), 
                            rel_heights=c(1,1.3))
   
   return(rc)
   
}

##' Figure S5. Looking at the impact of IP schooling stratified by
##' number of interventions, and the individual impact of interventions
##' on secondary outcomes
##' 
fig_S5 <- function(mit_strata, outcome_names=NULL){
   
   if(!is.null(outcome_names)){
      mit_strata <- mit_strata %>%
         mutate(outcome = factor(outcome, labels=outcome_names))
   }
   
   rc <-mit_strata%>%
      filter(str_detect(variable, "Child_"))%>%
      mutate(variable=recode(variable, 
                             Child_IP_full_bin="Full Time In-person Schooling",
                             Child_IP_part_bin="Part Time In-person Schooling"))%>%
      filter(adjusted)%>%
      mutate(n_interventions=factor(n_interventions, 
                                    levels=c("0","1-3","4-6","7-9","10+")))%>%
      ggplot(aes(x=n_interventions, y=estimate, 
                 ymin=conf.low, ymax=conf.high,
                 color=outcome))+
      geom_pointrange(position=position_dodge(width=.5))+
      facet_grid(cols=vars(variable))+
      geom_hline(yintercept=1)+
      xlab("num. interventions")+
      ylab("odds ratio")+
      scale_y_log10()+
      scale_color_brewer(name="", type="qual", palette = "Dark2")+
      theme_bw()+
      theme(legend.position="bottom")
   
   return(rc)
   
}



supp_fig_outcome_compare<- function(df) {
   
   
   cnty_inc <- df %>%
      group_by(fips, period, avg_biwk_AR)%>%
      summarize(n=n(),
                n_tst_pos=sum(tst_pos, na.rm=T),
                n_cli_pos=sum(cli_pos, na.rm=T),
                n_cli2_pos=sum(cli2_pos, na.rm=T),
                n_cliHH=sum(cliHH_pos, na.rm=T),
                n_HHtst=sum(cntct_HH_test_pos, na.rm=T),
                n_tst_ind=sum(tst_pos_ind, na.rm=T),
                n_tst_noind=sum(tst_pos_routine,na.rm=T),
                tst_AR=n_tst_pos/n,
                cli_AR=n_cli_pos/n,
                cli2_AR=n_cli2_pos/n,
                cliHH_AR=n_cliHH/n,
                HHtst_AR=n_HHtst/n)%>%
      ungroup() %>%
      select(-c(n_tst_pos, n_cli_pos, n_cli2_pos))%>%
      pivot_longer(c(tst_AR, cli_AR, cli2_AR, cliHH_AR, HHtst_AR),
                   names_to = "outcome",
                   values_to = "AR")%>%
      mutate(outcome=recode(outcome,tst_AR="Test+",
                            cli_AR="COVID-like illness",
                            cli2_AR="Loss taste/smell",
                            cliHH_AR="CLI in HH",
                            HHtst_AR="HH member test+"))
   
   
   
   
   assc_mes <- cnty_inc%>%filter(n>=50)%>%group_by(outcome)%>%
      group_modify(~{
         tmp <- broom::tidy(lm(AR~avg_biwk_AR, data=.x),conf.int=T)
         tmp$cor <- cor(.x$avg_biwk_AR,.x$AR, use="complete.obs")
         return(tmp)
      })%>% filter(term=="avg_biwk_AR")
   
   ar_plt <-cnty_inc%>%
      filter(n>=50)%>%
      ggplot(aes(x=avg_biwk_AR, y=AR))+
      geom_point(alpha=.1)+
      geom_smooth()+
      facet_wrap(~outcome)+
      ylab("outcome attack rate")+
      xlab("reported attack rate (confirmed cases)")+
      geom_text(data=assc_mes, 
                aes(label=sprintf("cor=%1.2f\nslope=%1.1f (95%% CI %1.1f,%1.1f)",
                                  cor,estimate,conf.low, conf.high)),
                x=.0, y=.15, color="blue",
                hjust=0, vjust=1,size=2.5)
   
   return(ar_plt)
}




