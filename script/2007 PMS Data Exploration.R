#### 2007 PMS Exploration ####
# weighting into tables, complex surevey design, ols, 
#limitations, bias, census type self report mh questions, 
#cmdgrp compared to cisr score, subject measures

rm(list = ls())

################################


################################

#rmarkdown::render("2007 PMS Data Exploration.R")
#citation()
#citation(skimr)
# Load libraries 


pacman::p_load(skimr,
               gtsummary,
               finalfit,
               knitr, 
               tidyverse, 
               haven,
               ggplot2,
               forcats,
               janitor)

# Read in 2007
source_pms07 <- haven::read_sav("pms07.sav", 
                                user_na = TRUE) %>%
  as_factor()



# Variables of interest subset 2007
subset_pms07 <- source_pms07 %>% 
  dplyr::select("pserial","trtment","eqvinc5","diag","CISRFOUR","ResSex","Health6","Language","Age10yr",
                "Age20yr","ETHNIC5","Origin","EDQUAL5","AnyQuals",
                "HiQuals","ResMarDF",
                "DVILO4a","DVPrac","Care1","Care2","Care3","Care4",
                "Care5","Everwk","yearjbl","JbReas","Looked",
                "YInAct","LookStop","LookSto2","Stat","Solo",
                "HrsWeek","PTWkHour","EmpStY","LookNot1","LookNot2",
                "LookNot3","DVLastWk","NotWk","WkShel1","HrsWork",
                "LookSto3","DiffJob","WkShel2","WkShel3","SEmpStY",
                "LookNow","LookAtAl","JobstM","EmpNo","Manage",
                "FtPtWk","NotWk","SEG","SC","SrcInc1","SrcInc2",
                "SrcInc3","SrcInc4","SrcInc5","SrcInc6","Gross4",
                "Gross4a","Ten1","LLord","SF6","SF7","SF9","SF11",
                "SF12","CONHOMD","IMon","FollUpGrp",
                "eligible","Stage2","Origin","DVPrac","Care1","Care2",
                "Care3","Care4","Care5","Medic","CnslHav","CnslTak",
                "CnslLng","DocYear","DocPsyc","PMatNum","DocTalk",
                "DocWeeks","InWhy","OutWhy","DayY","DayWht1",
                "DayWht2","DayWht3","DayWht4","CC2aY",
                "CC2Y1","CC2Y2","CC2Y3","CC2Y5","CC2Y6","CC2Y7",
                "CC2Y8","CC2Y9","MentHos","Ten1","Gross4a","qimd",
                "gor06","newsha","SF1","Happy","tenure","newten",
                "hhdtype","DISsex","DISeth","DISrel","DISAge",
                "DISmen","DISphy",
                "DISsori","CISRFOUR","panic","GAD","madd","OCD",
                "phob","dep","neurotic","nosymp","numdis",
                "PTSDever","MajorT16","PTSDcom","PTSDPos",
                "suicthwk","suicthyr","suicthlf","DSHlife",
                "suicatwk","suicatyr","suicatlf","DSHtry","suihfri",
                "suihhos","suihcom","suihhel","suihoth","DSH5",
                "DSHharm","DSH9","DSH10","PsycProb","Psycdis",
                "Psycdis_wt","BPDPH2","bdpd_wt","ASPDPH2","aspd_wt",
                "DVADHD1","DVADHD2","DVADHD4","scoff2","EDImpact",
                "bmigp4","DVAudit1","AUDITgp","DRNKPROB","SADQCSC",
                "SADQGP","AUDSAD2","Cannever","Cannyear","AMPHEVER",
                "AMPHYEAR","COCAEVER","CRACEVER","CRACYEAR",
                "ECSTEVER","ECSTYEAR","HEROEVER","HEROYEAR",
                "ACIDEVER","ACIDYEAR","MUSHEVER","MUSHYEAR",
                "METHEVER","METHYEAR","TRANEVER","TRANYEAR",
                "AMYLEVER","AMYLYEAR","ANABEVER",
                "ANABYEAR","GLUEEVER","GLUEYEAR","DRUGEVER",
                "DRUGYEAR","DRUGDEP",
                "DRUGDEP2","GamYR","dsmscgr","dsmprob","dsmpath",
                "numdiag","numdiag4","LCA","cluster1","cluster2",
                "cluster3","cluster4","cluster5","cluster6",
                "MedPsyc","MedDep","MedHyp","MedAnx","MedADHD",
                "anymed","Doc2Wks","DocPsyc","INQTRMEN","OUTQTRME",
                "PSYCTHER","COGTHER","ARTTHER","SOCTRAIN","MARITHER"
                ,"SEXTHER","COUNSEL","OTHTHER","OTHTHER","anyther",
                "trtment","DAYCOMYR","PSYTRTYR","PSYLGTYR",
                "CPNYR","CLDNYR","OTHNSEYR","SOCWRKYR","CPNYR",
                "ERI3","SFQA","SFQB","SFQC","SFQD",
                "SFQE","SFQF","SFQG","SFQH","DLSS4","DLSS5",
                "PrimGrp","ChldInst","LACare","BothMaPa",
                "YNotBoth","MaOrPa","AnyChild","NoChild",
                "InDebt1","InDebt6",
                "SFHELPYR","HMHELPYR","OREACHYR","anyccar","anyhlca"
                ,"anydaca","wt_ints1","ipsu_0714","istrata_0714","eqvinc5") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  janitor::remove_empty("rows") 

# Arrange alphabetically and make 'colnames' lowercase

subset_pms07 <-subset_pms07[,order(colnames(subset_pms07))] %>% rename_all(tolower)

# Check names
head(subset_pms07)

#skim(subset_pms07)


# Variable Cleanup
# diag var collapse  - works
cmdgroup <- fct_collapse(subset_pms07$diag,
                         "Mixed A&D" = c("MAD"),
                         GAD = c("GAD"),
                         Depression = c("sev dep ep", "mod dep ep", "mild dep ep"),
                         Phobia = c("social phob", "Agoraphob", "Spec iso phob"),
                         OCD = c("OCD"),
                         Panic = c("panic"),
                         NULL = c("No disord"))


# Age10yrs
age10yrs <- fct_collapse(subset_pms07$age10yr,
                         "16 to 24" = c("16 - 24"),
                         "25 to 34" = c("25 - 34"),
                         "35 to 44" = c("35 - 44"),
                         "45 to 54" = c("45 - 54"),
                         "55 to 64" = c("55 - 64"),
                         "65 to 74" = c("65 - 74"),
                         "75+" = c("75+"),
                         NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))



# Res_sex
sex <- fct_collapse(subset_pms07$res_sex,
                    Male = c("Male"),
                    Female = c("Female"),
                    NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))




eqhouse <- fct_collapse(subset_pms07$eqvinc5,
                        Highest = c("highest quintile (>=?40,384)"),
                        Second = c("2nd quintile (>=?24, 700 <?40,384)"),
                        Third = c("3rd quintile (>=16,195 <?24,700)"),
                        Fourth = c("4th quintile (>=?10,575 <?16,195)"),
                        Lowest = c("Lowest quintile (<?10,575)"),
                        NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))
table(eqhouse)


treatment <- fct_collapse(subset_pms07$trtment,
                          NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"),
                          Medication = c("medication only"),
                          Counselling = c("counselling only"),
                          None = c("no treatment"),
                          Both = c("both medication and counselling"))


employstat <- fct_collapse(subset_pms07$dvilo4a,
                           "Employed" = c("In employment not unpaid family worker"),
                           "Unpaid Family Work" = c("Unpaid family worker"),
                           "Unemployed" = c("Unemployed"),
                           "Economically inactive" = c("Economically inactive"),
                           NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))

hiqual <- fct_collapse(subset_pms07$edqual5,
                       "Degree" = c("Degree"),
                       "Teaching, HND, Nursing" = c("Teaching, HND, nursing"),
                       "A Level" = c("A Level"),
                       "GCSE or equivalent" = c("GCSE or equivalent"),
                       "Foreign or other" = c("Foreign/other"),
                       "None" = c("No qualifications"),
                       NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))

ethnicgrp <- fct_collapse(subset_pms07$ethnic5,
                          "White British" = c("White British"),
                          "White non British" = c("White non British"),
                          "Black" = c("Black"),
                          "South Asian" = c("South Asian (Indian, Pakistani or Bangladeshi)"),
                          "Mixed or other" = c("Mixed or other"),
                          NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))

maritalstat <- fct_collapse(subset_pms07$res_mar_df,
                            "Married" = c("Married"),
                            "Cohabiting" = c("Cohabiting"),
                            "Single" = c("Single"),
                            "Widowed" = c("Widowed"),
                            "Divorced" = c("Divorced"),
                            "Separated" = c("Separated"),
                            NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))


eqhouseinc <- fct_collapse(subset_pms07$eqvinc5,
                           "Highest" = c("highest quintile (>=?40,384)"),
                           "2nd" = c("2nd quintile (>=?24, 700 <?40,384)"),
                           "3rd" = c("3rd quintile (>=16,195 <?24,700)"),
                           "4th" = c("4th quintile (>=?10,575 <?16,195)"),
                           "Lowest" = c("Lowest quintile (<?10,575)"),
                           NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))

region <- fct_collapse(subset_pms07$gor06,
                       "North East" = c("North East"),
                       "North West" = c("North West"),
                       "Yorkshire & The Humber" = c("Yorkshire & The Humber"),
                       "East Midlands" = c("East Midlands"),
                       "West Midlands" = c("West Midlands"),
                       "East of England" = c("East of England"),
                       "London" = c("London"),
                       "South West" = c("South West"),
                       "South East" = c("South East"),
                       NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))

numcmd <- fct_collapse(subset_pms07$numdis,
                       "None" = c("None"),
                       "One" = c("One"),
                       "Two or more" = c("Two or more"),
                       NULL = c("No answer/refused","Don't know","Missing data","Proxy","Item not applicable"))

commcareyr <- fct_collapse(subset_pms07$cc2a_y,
                           "Yes" = c("Yes"),
                           "No" = c("No"),
                           NULL = c("No answer/refused","Don't know","Missing data","Schedule not applicable","Proxy","Item not applicable"))

table(commcareyr)


#all meds into 1 variable
#psychmed <- unite(subset_pms07, med_dep, med_hyp, med_anx, med_adhd, med_psyc, sep = "_", remove = TRUE, na.rm = FALSE)

#join new vars to subset
subset_pms07 <- cbind(subset_pms07, age10yrs, cmdgroup, commcareyr, employstat, eqhouse, eqhouseinc, ethnicgrp, hiqual, maritalstat, numcmd, region, sex, treatment)


#order col names
subset_pms07 <-subset_pms07[,order(colnames(subset_pms07))] %>% rename_all(tolower)


         


# Plot - Works! descriptive age by sex
subset_pms07 %>%
  filter(!is.na(age10yr)) %>%
ggplot(aes(x = factor(age10yr),
           y = prop.table(stat(count)),
           fill = fct_infreq(sex),
           label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Survey Makeup \n by Age & Sex (2007)", 
       x = 'Age Group', 
       y = 'Percentage', fill = 'Sex')


# CIS-R Score by Sex
subset_pms07 %>%
  filter(!is.na(cisrfour)) %>%
ggplot(aes(x = factor(cisrfour),
           y = prop.table(stat(count)),
           fill = fct_infreq(sex),
           label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Survey Makeup \n by Sex & CIS-R Score (2007)", 
       x = 'CIS-R Score Group', 
       y = 'Percentage', fill = 'Sex')


# CMD condition by sex - NAs dropped and sorted by frequency 
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
ggplot(aes(x = factor(cmdgroup),
           y = prop.table(stat(count)),
           fill = fct_infreq(sex),
           label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Prevalance of 'Common Mental Disorders' (CMD) \n by Sex (2007)", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'Sex')


# C<D by sex and household quintile 
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
ggplot(aes(x = factor(eqhouse),
           y = prop.table(stat(count)),
           fill = fct_infreq(cmdgroup),
           label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Prevalence of CMD \n by Household Quintile", 
       x = 'Household Quintile', 
       y = 'Percentage', fill = 'CMD Group')

# treatment recieved by CMD
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
ggplot(aes(x = factor(cmdgroup),
           y = prop.table(stat(count)),
           fill = fct_infreq(treatment),
           label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='stack') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Type of Treatment Recieved \n by CMD", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'Treatment')

# cmd by eq hhold income 
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(!is.na(eqhouseinc),
             y = prop.table(stat(count)),
             fill = fct_infreq(cmdgroup),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Prevalance of CMD \n by Equivilised Household Income", 
       x = 'Eqvivilised Houshold Income', 
       y = 'Percentage', fill = 'CMD Group')
  
# cmd by employment 
# treatment recieved by CMD
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(cmdgroup),
             y = prop.table(stat(count)),
             fill = fct_infreq(employstat),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Employment Status \n by CMD", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'Status')

# cmd by quals
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(hiqual),
             y = prop.table(stat(count)),
             fill = fct_infreq(cmdgroup),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Educational Status \n by CMD", 
       x = 'Education', 
       y = 'Percentage', fill = 'CMD Group')


#### tables cmd ####
#Table 1 - demographics by variable of interest ----
# cmd by vars of interest
subset_pms07 %>%
  select(sex, age10yrs, cmdgroup, treatment, employstat) %>%
  tbl_summary(by = cmdgroup,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# Table CISR Score x age and sex
subset_pms07 %>%
  select(sex, age10yrs, cisrfour) %>%
  tbl_summary(by = age10yrs,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by age and sex 
subset_pms07 %>%
  select(sex, age10yrs, cmdgroup) %>%
  tbl_summary(by = age10yrs,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

#cmd by ethnicity and sex
subset_pms07 %>%
  select(sex, ethnicgrp, cmdgroup) %>%
  tbl_summary(by = ethnicgrp,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

#cmd by marital status and sex
subset_pms07 %>%
  select(sex, maritalstat, cmdgroup) %>%
  tbl_summary(by = maritalstat,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by hhold and sex 
subset_pms07 %>%
  select(sex, cmdgroup, eqhouseinc) %>%
  tbl_summary(by = eqhouseinc,
              missing = "no") %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by region and sex 
subset_pms07 %>%
  select(sex, region, cmdgroup) %>%
  tbl_summary(by = region,
              missing = "no") %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by sex and employment 
subset_pms07 %>%
  select(sex, employstat, cmdgroup) %>%
  tbl_summary(by = employstat,
              missing = "no") %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by sex and qual
subset_pms07 %>%
  select(sex, hiqual, cmdgroup) %>%
  tbl_summary(by = hiqual,
              missing = "no") %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by commcare
subset_pms07 %>%
  select(sex, commcareyr, cmdgroup) %>%
  tbl_summary(by = cmdgroup,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()
