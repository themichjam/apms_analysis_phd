#### Code That Actually Works ####
## Last Updated: 2021-07-23 ##

# Cleaning Variables #

# Find total of variable 
tabsum <- table(commcareyr) # make and save table 

addmargins(tabsum) # calculate overall sums

# Check variables exist
ls()        # lists all the variables that have been defined
exists("x") # returns TRUE or FALSE, depending upon whether x has been defined.

# collapse all misc. missing data into NAs across DF
# 1: create object with all NA variations in it
# Clean NAs
common_na_numbers
common_na_strings
# write out all the offending strings
na_strings <- c("No answer/refused",
                "Don't know",
                "Missing data",
                "Proxy",
                "Item not applicable")
#Then you write ~.x %in% na_strings - which reads as “does this
#value occur in the list of NA strings”.
subset_pms07 %>%
  replace_with_na_all(is.factor, ~.x %in% na_strings)


# Table to check 
subset_pms07 %>%
  select(res_sex, diag) %>%
  tbl_summary(by = res_sex) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# basic barplot to check for NAs

ggplot(subset_pms07, aes(x=diag))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

# Renaming factor levels dplyr
subset_pms07$diag <- recode_factor(subset_pms07$diag, 
                                   "sev dep ep" = "Sev Dep",
                                   "mild dep ep" = "Mild Dep",
                                   "mod dep ep" = "Mod Dep",
                                   "panic" = "Panic",
                                   "social phob" = "Soc Phob",
                                   "Spec iso phob" = "Specific Phob")

# Individual fct_collapse for variables 
# Variable Cleanup
# diag var collapse  - works

cmdgroup <- fct_collapse(subset_pms07$diag,
                         "MixedA&D" = c("MAD"),
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
                         NULL = c("No answer/refused","Don't
                                  know","Missing data",
                                  "Proxy","Item not applicable"))
# Res_sex
sex <- fct_collapse(subset_pms07$res_sex,
                    Male = c("Male"),
                    Female = c("Female"),
                    NULL = c("No answer/refused","Don't know",
                             "Missing data",
                             "Proxy","Item not applicable"))


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



evdrug <- fct_collapse(subset_pms07$drugever,
                       "Yes" = c("Yes"),
                       "No" = c("No"),
                       NULL = c("No answer/refused","Don't know","Missing data","Schedule not applicable","Proxy","Item not applicable"))


#all meds into 1 variable
#psychmed <- unite(subset_pms07, med_dep, med_hyp, med_anx, med_adhd, med_psyc, sep = "_", remove = TRUE, na.rm = FALSE)

# Bind newly fct_collapse variables to data frame
subset_pms07 <- cbind(subset_pms07, evdrug, age10yrs, cmdgroup, commcareyr, employstat, eqhouseinc, ethnicgrp, hiqual, maritalstat, numcmd, region, sex, treatment)

subset_pms07 <-subset_pms07[,order(colnames(subset_pms07))] %>% rename_all(tolower)


# Visulisation # 

# Common Mental Disorder Groups
subset_pms07 %>%
  drop_na() %>% # drop NAs from graph
  ggplot(aes(x = fct_infreq(cmdgroup), # sort from highest result to lowest
             y = prop.table(stat(count)),
             fill = fct_infreq(cmdgroup), # sort key from high to low
             label = scales::percent(prop.table(stat(count))))) + # get percentages
  geom_bar() + 
  geom_text(stat = 'count',
            position = position_dodge(.9), # position bars
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + # scale percent
  #scale_fill_viridis_d() + # add colour theme different from default
  labs(title="Survey Makeup (2007) \n by Common Mental Disorder Group", 
       x = 'Respondant CMD Group', 
       y = 'Percentage',
       fill = 'Condition',
       caption = "(Test Footnote 2021)") + # add footnote
  annotate("text", x = 6, y = .6, size = 3, label = "n = 1,277") + # manual annotation
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) # adjust labels, 0 = left, 0.5 = middle, 1 = right

# Tables 
# with gtsummary 
#e.g.
# Tables test
subset_pms07 %>% # your dataset
  select(sex, treatment) %>% # select variables of interest
  tbl_summary(by = sex, # sort by
              missing = "no") %>% # missing = takes away NAs (shown as Unknown in this)
  add_p() %>% # add p-value
  add_overall() %>% # add overall number of people represented
  add_n() %>% # add number actually answered 
  bold_labels() # bold labels

# cmd by hhold and sex 
#subset_pms07 %>%
#  select(sex, cmdgroup, eqhouseinc) %>%
#  tbl_summary(by = eqhouseinc,
#              missing = "no") %>%
#  add_overall() %>%
#  add_n() %>%
#  bold_labels()

# cmd by region and sex 
#subset_pms07 %>%
#  select(sex, region, cmdgroup) %>%
#  tbl_summary(by = region,
#              missing = "no") %>%
#  add_overall() %>%
#  add_n() %>%
#  bold_labels()

# cmd by sex and employment 
#subset_pms07 %>%
#  select(sex, employstat, cmdgroup) %>%
#  tbl_summary(by = employstat,
#              missing = "no") %>%
#  add_overall() %>%
#  add_n() %>%
#  bold_labels()

# cmd by sex and qual
#subset_pms07 %>%
#  select(sex, hiqual, cmdgroup) %>%
#  tbl_summary(by = hiqual,
#              missing = "no") %>%
#  add_overall() %>%
#  add_n() %>%
#  bold_labels()

## make y percentage 100% - limits = c(0,1)
