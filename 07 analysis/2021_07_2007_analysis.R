## 2021-06-28 ##
## 2007 apms analysis ##

# clear enviroment 
rm(list = ls())

#install.packages if not already
# load libraries
pacman::p_load(skimr,
       naniar,
       gtsummary,
       finalfit,
       knitr,
       tidyverse,
       haven,
       ggplot2,
       forcats,
       janitor,
       survey,
       srvyr)

# check packages
pacman::p_loaded()


# Read in 2007
source_pms07 <- haven::read_sav("2007 docs/apms07arch.sav", 
                                user_na = TRUE) %>%
  as_factor() 

# Variables of interest subset 2007
subset_pms07 <- source_pms07 %>% 
  dplyr::select("pserial","ResAge","trtment","eqvinc5","diag","CISRFOUR","ResSex","Health6","Language","Age10yr",
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
                ,"anydaca","wt_ints1","eqvinc5",
                "ptsdlong","PTSDarms","MajorT16","Psycdis_wt",
                "bdpd_wt","aspd_wt","ASDwt") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  janitor::remove_empty("rows") %>%
  mutate(across(where(is.factor), as.character))

head(subset_pms07, 5)


# Arrange alphabetically and make 'colnames' lowercase

subset_pms07 <-subset_pms07[,order(colnames(subset_pms07))] %>% rename_all(tolower)

# Check names
head(subset_pms07, 5)
table(subset_pms07$diag)
class(subset_pms07$diag)

# remove source df
remove(source_pms07)

table(subset_pms07$diag)

# Convert unwanted levels to NA
# write out all the offending strings of different NAs used
na_strings <- c("No answer/refused",
                "Don't know",
                "Missing data",
                "Proxy",
                "Item not applicable",
                "No disord",
                "Don't know",
                "Item not applicable")

#Then you write ~.x %in% na_strings - which reads as “does this 
#value occur in the list of NA strings”.
subset_pms07 %>%
  replace_with_na_all(condition = ~.x %in% na_strings)


# convert df back to factor 
subset_pms07 %>% mutate_if(is.character, as.factor)


# Find total of variable 
tabsum <- table(subset_pms07$age10yr)

addmargins(tabsum)

#### Univariate Plots ####

# Age
# change y to counts + add counts to bar + add n = 
subset_pms07 %>%
  drop_na() %>% # get rid of na from plot
  ggplot(aes(x = (age10yr),
      y = prop.table(stat(count)),
      fill = fct_infreq(age10yr),
      label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + # make y % and no decimal
  labs(title = "Survey Makeup (2007) \n by Age Group", 
       x = 'Respondant Age', 
       y = 'Percentage',
       fill = 'Age Group',
       caption = "(Test Footnote 2021)") + # footnote
  annotate("text", x = 7, y = .2, size = 3, label = "n = 7,403") + # add total n = manually
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) # 0 = left, 0.5 = middle, 1 = right

# Find total of variable 
tabsum <- table(subset_pms07$diag)
addmargins(tabsum)

7403 - 6126

# Renaming factor levels dplyr
subset_pms07$diag <- recode_factor(subset_pms07$diag, 
                                   "sev dep ep" = "Sev Dep",
                                   "mild dep ep" = "Mild Dep",
                                   "mod dep ep" = "Mod Dep",
                                   "panic" = "Panic",
                                   "social phob" = "Soc Phob",
                                   "Spec iso phob" = "Specific Phob")

# Common Mental Disorder Groups
subset_pms07 %>%
filter(diag != 'No disord') %>% # drop fct levels you dont want shown
  drop_na() %>%
  ggplot(aes(x = fct_infreq(diag),
             y = prop.table(stat(count)),
             fill = fct_infreq(diag),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  #scale_x_discrete(guide = guide_axis(n.dodge=2))+ # dodge labels from each other
  #scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+ # drop some overlapping text
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Common Mental Disorder Group", 
       x = 'Respondant CMD Group', 
       y = 'Percentage',
       fill = 'Condition',
       caption = "(Test Footnote 2021)") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        #axis.text.x = element_text(size = 7), # change text size
        axis.text.x = element_text(angle = 45, hjust=1)) + # angle text
  annotate("text", x = 9.3, y = .55, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 9.3, y = .50, size = 3, label = "n = 1,277")


# Find total of variable 
tabsum <- table(subset_pms07$cc2a_y)

addmargins(tabsum)
# Recieved Community Care in Last Year
subset_pms07 %>%
  filter(cc2a_y != "Don't know") %>% 
  drop_na() %>%
  ggplot(aes(x = fct_infreq(cc2a_y),
             y = prop.table(stat(count)),
             fill = fct_infreq(cc2a_y),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Reciept of Community Care", 
       x = 'Recieved Care', 
       y = 'Percentage',
       fill = 'Care Group',
       caption = "(Test Footnote 2021)") +
  annotate("text", x = 2.3, y = .9, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 2.3, y = .8, size = 3, label = "n = 7,402") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5))


# Renaming factor levels dplyr
subset_pms07$dvilo4a <- recode_factor(subset_pms07$dvilo4a, 
                                   "In employment not unpaid family worker" = "Employed")
# Employement Status
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(dvilo4a),
             y = prop.table(stat(count)),
             fill = fct_infreq(dvilo4a),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Employment Status", 
       x = 'Respondant Status', 
       y = 'Percentage',
       fill = 'Employment Type',
       caption = "(Test Footnote 2021)") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  annotate("text", x = 4, y = .6, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 4, y = .5, size = 3, label = "n = 7,402")
  

# Renaming factor levels dplyr
subset_pms07$eqvinc5 <- recode_factor(subset_pms07$eqvinc5, 
                                      "3rd quintile (>=16,195 <?24,700)" = "Third (16,195-24,700)",
                                      "highest quintile (>=?40,384)" = "Highest (>40,384)",
                                      "Lowest quintile (<?10,575)" = "Lowest (<10,575)",
                                      "2nd quintile (>=?24, 700 <?40,384)" = "Second (24,700-40,384)",
                                      "4th quintile (>=?10,575 <?16,195)" = "Fourth (10,575-16,195)")

# Equivalised income quintiles
subset_pms07 %>%
  filter(eqvinc5 != "Item not applicable") %>% 
  drop_na() %>%
  ggplot(aes(x = (eqvinc5),
             y = prop.table(stat(count)),
             fill = fct_infreq(eqvinc5),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Equivalised income quintiles",
       x = 'Equivalised income quintiles', 
       y = 'Percentage',
       fill = 'Income Quintile',
       caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  annotate("text", x = 5, y = .3, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 5, y = .25, size = 3, label = "n = 5,872")

# Renaming factor levels dplyr
subset_pms07$ethnic5 <- recode_factor(subset_pms07$ethnic5, 
                                      "South Asian (Indian, Pakistani or Bangladeshi)" = "South Asian")
# Ethnic Group
subset_pms07 %>%
  filter(!ethnic5 %in% c("Don't know", "No answer/refused")) %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(ethnic5),
             y = prop.table(stat(count)),
             fill = fct_infreq(ethnic5),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
  labs(title="Survey Makeup \n by Ethnic Group (2007)", 
       x = 'Respondant Ethnicity', 
       y = 'Percentage',
       fill = 'Ethnic Group',
       caption = "(Test Footnote 2021)") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  annotate("text", x = 5, y = .9, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 5, y = .8, size = 3, label = "n = 5,353")


# Renaming factor levels dplyr
subset_pms07$edqual5 <- recode_factor(subset_pms07$edqual5, 
                                      "No qualifications" = "None",
                                      "Teaching, HND, nursing" = "Vocaational*")
# Highest Qualification
subset_pms07 %>%
  filter(!edqual5 %in% c("Don't know", "No answer/refused")) %>% # drop more than 2 levels
  drop_na() %>%
  ggplot(aes(x = fct_infreq(edqual5),
             y = prop.table(stat(count)),
             fill = fct_infreq(edqual5),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup \n by Qualification (2007)", 
       x = 'Respondant Highest Qualification', 
       y = 'Percentage', fill = 'Qualification',
       caption = "* Includes: Nursing, Teaching, HND") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  annotate("text", x = 6, y = .4, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 6, y = .35, size = 3, label = "n = 7,235") 

# Marital Status
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(res_mar_df),
             y = prop.table(stat(count)),
             fill = fct_infreq(res_mar_df),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Marital Status", 
       x = 'Respondant Status', 
       y = 'Percentage', fill = 'Marital Status',
       caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 6, y = .5, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 6, y = .45, size = 3, label = "n = 7,235")



# Number of Common Mental Disorders
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(numdis),
             y = prop.table(stat(count)),
             fill = fct_infreq(numdis),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Survey Makeup (2007) \n by Number of CMDs Diagnosed", 
       x = 'Respondant Diagnosis', 
       y = 'Percentage',
       fill = 'Number of CMD',
       caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = .9, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 3, y = .80, size = 3, label = "n = 7,403")
  

# Find total of variable 

# Region
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(gor06),
             y = prop.table(stat(count)),
             fill = fct_infreq(gor06),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
 labs(title="Survey Makeup (2007) \n by Region", 
       x = 'Respondants by Region', 
       y = 'Percentage',
      fill = 'Region',
      caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 9.5, y = .15, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 9, y = .15, size = 3, label = "n = 7,403")
  

# Renaming factor levels dplyr
subset_pms07$trtment <- recode_factor(subset_pms07$trtment, 
                                      "no treatment" = "None",
                                      "medication only" = "Medication only",
                                      "counselling only" = "Counselling only",
                                      "both medication and counselling" = "Both")
# Treatment for CMDs
subset_pms07 %>%
  filter(trtment != "Don't know") %>% 
  drop_na() %>%
  ggplot(aes(x = fct_infreq(trtment),
             y = prop.table(stat(count)),
             fill = fct_infreq(trtment),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by CMD Treatment (2007)", 
       x = 'Treatment Group', 
       y = 'Percentage',
       fill = 'Treatment',
       caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 4, y = 1, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 4, y = .95, size = 3, label = "n = 7,382") 
  


# Ever Took Drugs
subset_pms07 %>%
  filter(!drugever %in% c("Missing data", "No answer/refused", "Don't know")) %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(drugever),
             y = prop.table(stat(count)),
             fill = fct_infreq(drugever),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Drug Use", 
       x = 'Ever Taken Drugs', 
       y = 'Percentage',
       fill = 'Drug Use',
       caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 2.3, y = .90, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 2.3, y = .85, size = 3, label = "n = 7,357")
  

# Find total of variable 

# Sex
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(res_sex),
             y = prop.table(stat(count)),
             fill = fct_infreq(res_sex),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))), # show counts & %
            stat='count',
            position = position_dodge(.9),
            vjust = -1,
            size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Survey Makeup (2007)\n by Sex", 
       x = 'Respondant Sex', 
       y = 'Percentage',
       fill = 'Sex',
       caption = "(Test Footnote 2021)") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 2.3, y = .70, size = 3, label = "(total n = 7,403)") +
  annotate("text", x = 2.3, y = .65, size = 3, label = "n = 7,403")
   

#### Bivariate ####

# plot cisr score age and sex
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(cisrfour),
             y = prop.table(stat(count)),
             fill = fct_infreq(sex),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  #scale_fill_viridis_d() +
  labs(title="Survey Makeup (2007) \n by Sex & CIS-R Score", 
       x = 'CIS-R Score Group', 
       y = 'Percentage',
       fill = 'Sex',
       caption = "(Test Footnote 2021)") + 
  annotate("text", x = 4, y = .4, size = 3, label = "n = 7,403") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) 

# Big desc table
subset_pms07 %>%
  select(age10yr, res_sex, diag, dvilo4a) %>%
  tbl_summary(by = age10yr) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()


#table cisr score age and sex
subset_pms07 %>%
  select(sex, age10yrs) %>%
  tbl_summary(by = sex) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

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

### CMD and Employment 
subset_pms07 %>%
  #filter(drugever != "No answer/refused") %>%
  drop_na() %>%
  ggplot(aes(x = fct_infreq(diag),
             y = prop.table(stat(count)),
             fill = fct_infreq(dvilo4a),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar() + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title="Survey Makeup (2007) \n by Drug Use", 
       x = 'Ever Taken Drugs', 
       y = 'Percentage',
       fill = 'Drug Use',
       caption = "(Test Footnote 2021)") + 
  annotate("text", x = 2.3, y = .7, size = 3, label = "n = 7,357") +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5))

# plot cmd group x cisr score
subset_pms07 %>%
  filter(!is.na(dvilo4a)) %>%
  ggplot(aes(x = factor(diag),
             y = prop.table(stat(count)),
             fill = fct_infreq(dvilo4a),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Survey Makeup \n by CMD & CIS-R Score (2007)", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'CIS-R Score')

# cmd and ethnicity
subset_pms07 %>%
  filter(!is.na(diag)) %>%
  ggplot(aes(x = factor(ethnic5),
             y = prop.table(stat(count)),
             fill = fct_infreq(diag),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Survey Makeup \n by CMD & Ethnicity (2007)", 
       x = 'Ethnicity', 
       y = 'Percentage', fill = 'CMD Group')

#cmd by ethnicity and sex
subset_pms07 %>%
  select(sex, ethnicgrp, cmdgroup) %>%
  tbl_summary(by = ethnicgrp,
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
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# C<D by sex and household quintile 
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(eqhouseinc),
             y = prop.table(stat(count)),
             fill = fct_infreq(cmdgroup),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Prevalence of CMD \n by Household Quintile", 
       x = 'Household Quintile', 
       y = 'Percentage', fill = 'CMD Group')

# CMD by age
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(age10yrs),
             y = prop.table(stat(count)),
             fill = fct_infreq(cmdgroup),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Prevalence of CMD \n by Age", 
       x = 'Age', 
       y = 'Percentage', fill = 'CMD Group')

# cmd by age 
subset_pms07 %>%
  select(cmdgroup, age10yrs) %>%
  tbl_summary(by = age10yrs,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

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

# age, cmd, treatment, employment
subset_pms07 %>%
  select(sex, age10yrs, cmdgroup, treatment, employstat) %>%
  tbl_summary(by = cmdgroup,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# treatment recieved by CMD
subset_pms07 %>%
  drop_na() %>%
  ggplot(aes(x = factor(cmdgroup),
             y = prop.table(stat(count)),
             fill = fct_infreq(evdrug),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='stack') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Drug Use \n by CMD", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'Treatment')

subset_pms07 %>%
  select(cmdgroup, evdrug) %>%
  tbl_summary(by = evdrug,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# CMD by community care use
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(cmdgroup),
             y = prop.table(stat(count)),
             fill = fct_infreq(commcareyr),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='stack') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="CMD \n by Community Care", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'Care Use')

# cmd by commcare
subset_pms07 %>%
  select(sex, commcareyr, cmdgroup) %>%
  tbl_summary(by = cmdgroup,
              missing = "no") %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# cmd by eq hhold income 
subset_pms07 %>%
  filter(!is.na(eqhouseinc)) %>%
  ggplot(aes(x = factor(cmdgroup),
             y = prop.table(stat(count)),
             fill = fct_infreq(eqhouseinc),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Prevalance of CMD \n by Equivilised Household Income", 
       x = 'CMD Group', 
       y = 'Percentage', fill = 'Income Quintile')

# sex by employ status 
subset_pms07 %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = factor(employstat),
             y = prop.table(stat(count)),
             fill = fct_infreq(sex),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Sex \n by Employment Status", 
       x = 'Status', 
       y = 'Percentage', fill = 'Sex')

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

# CMD by education 
subset_pms07 %>%
  filter(!is.na(cmdgroup)) %>%
  ggplot(aes(x = factor(hiqual),
             y = prop.table(stat(count)),
             fill = fct_infreq(cmdgroup),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position='dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Education \n by CMD", 
       x = 'Education Level', 
       y = 'Percentage', fill = 'CMD Group')



#### SMI Exploration ####
## Post Traumatic Stress Disorder (PTSD) ##


## ex model


## define custom test
fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = TRUE)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result
}

## add p-values to your gtsummary table, using custom test defined above

table(subset_pms07$res_age)
# cross tab ex
tbl_cross_ex2 <-
  subset_pms07 %>%
  tbl_cross(row = res_age, col = diag, percent = "cell")


## Survey weights test ##

# change weights to numeric 
subset_pms07$wt_ints1 <- as.numeric(as.character(subset_pms07$wt_ints1)) # phase 1 weight
class(subset_pms07$wt_ints1) 
subset_pms07$psycdis_wt <- as.numeric(as.character(subset_pms07$psycdis_wt)) # weight to use with psycdis var
class(subset_pms07$psycdis_wt)
subset_pms07$bdpd_wt <- as.numeric(as.character(subset_pms07$bdpd_wt)) # Borderline personality disorder weight - use with bpdph2
class(subset_pms07$bdpd_wt)
subset_pms07$aspd_wt <- as.numeric(as.character(subset_pms07$aspd_wt))# Antisocial personality disorder weight - use with aspdph2
class(subset_pms07$aspd_wt)
subset_pms07$res_age <- as.numeric(as.character(subset_pms07$res_age))

# test 
# The survey_total() function
#Let us see what happens to frequencies when we apply the phase 1 weights.
#Whereas in dplyr we use the n() function, here to function that does the
#same is called survey_total(). Note that it will produce a standard error as well.
#This is a function of the size of the weight: the further is their distance from 1
#(that is, the more we have to weight each observations to reach the population frequency)
#the more is our uncertainty, and the larger is the error.
subset_pms07 %>%
  as_survey(weights = c(wt_ints1)) %>%
  group_by(res_sex) %>%
  summarize(n = survey_total())


# without weighting 
mean(subset_pms07$res_age, na.rm = T)

# With population weights
subset_pms07 %>%
  as_survey(weights = c(wt_ints1)) %>%
  summarize(lr_m = survey_mean(res_age, na.rm = T))

