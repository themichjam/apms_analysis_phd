rm(list = ls())



# load libraries 
pacman::p_load(skimr, 
               knitr, 
               tidyverse, 
               haven,
               dataMaid,
               janitor)



# read in 1993
source_pms93 <- haven::read_por("pms93.por", 
                                user_na = TRUE) %>%
  as_factor()

table(source_pms93$SEX01)
source_pms93$SEX01 %>% attr('labels')

# read in 2000
source_pms00 <- haven::read_sav("pms00.sav", 
                                user_na = TRUE) %>%
  as_factor()



# read in 2007
source_pms07 <- haven::read_sav("pms07.sav", 
                                user_na = TRUE) %>%
  as_factor() 



#### variables of interest subset 2007 ####
subset_pms07 <- source_pms07 %>% 
  dplyr::select("pserial","ResSex","Health6","Language","Age10yr",
                "Age20yr","ETHNIC4","Origin","EDQUAL5","AnyQuals",
                "HiQuals","ResMarDF",
                "DVILO3a","DVPrac","Care1","Care2","Care3","Care4",
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
                ,"anydaca","wt_ints1","ipsu_0714","istrata_0714") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  janitor::remove_empty("rows") 

# arrange alphebetically and make colnames lowercase

subset_pms07 <-subset_pms07[,order(colnames(subset_pms07))] %>% rename_all(tolower)
source_pms00 <-source_pms00[,order(colnames(source_pms00))] %>% rename_all(tolower)
source_pms93 <-source_pms93[,order(colnames(source_pms93))] %>% rename_all(tolower)

head(subset_pms07, 5)
head(source_pms00, 5)
head(source_pms93, 5)


#### Finding Common Variables Across Datasets ####
#common vars in 00 and 07
intersect(names(subset_pms07), names(source_pms00))

#colnames in 07 but not 00
setdiff(names(subset_pms07), names(source_pms00))

## subsetting for matching vars of interest 2000
subset_pms00 <- source_pms00 %>%
  dplyr::select("acidever","acidyear","age10yr","age20yr","amphever","amphyear",
                "amylever","amylyear","anabever","anabyear","anyquals",
                "artther","auditgp","audsad2","cannever","cannyear","cc2ay",
                "cc2y1","cc2y2","cc2y3","cc2y5","cc2y6","cc2y7","cc2y8","cc2y9",
                "chldinst","cisrfour","cldnyr","cnslhav","cnsllng","cnsltak",
                "cocaever","cogther","counsel","cpnyr", "cracever","cracyear",
                "daywht1","daywht2","daywht3","daywht4","dayy","dep","diffjob",
                "dlss4","dlss5","docpsyc","doctalk","docyear","drnkprob",
                "drugdep","drugdep2","drugever","drugyear","dsh10","dsh5","dsh9",
                "dvilo3a","dvlastwk","ecstever","ecstyear","edqual5","empno",
                "empsty","ethnic4","everwk","ftptwk","gad","glueever","glueyear",
                "gross4","gross4a","heroever","heroyear","hiquals","hmhelpyr",
                "hrsweek","hrswork","inqtrmen","inwhy","jbreas","jobstm","lacare","language","llord",   
                "lookatal","looked","looknot1","looknot2","looknot3","looknow",
                "looksto2","looksto3","manage","marither","menthos","methever",
                "methyear","mushever","mushyear","neurotic","nosymp","notwk",
                "ocd","oreachyr","origin","othnseyr","othther","outqtrme","outwhy",  
                "panic","phob","pmatnum","psycther","psylgtyr","psytrtyr","sadqgp","sc","seg","sempsty",
                "sexther","sf1","sf11","sf12","sf6","sf7","sf9","sfhelpyr",
                "soctrain","socwrkyr","solo","srcinc1","srcinc2","srcinc3",
                "srcinc4","srcinc5","srcinc6","stat","suicatlf","suicatwk",
                "suicatyr","suicthlf","suicthwk","suicthyr","ten1","tenure",
                "tranever","tranyear","trtment","wkshel1","wkshel2","yinact","respsex","sstrtreg","resmarst","dtjbl","jbreas","commany",
                "numchild","docwhat","treat","wt2","parents","mencenyr","othdayyr",
                "daycenyr","doctalk","suicthlf","suicatlf","q20","q23","b3a",
                "govreggb","numadult","pmatoday","indebt01","futrjbw1","mad",
                "psycdrug","drglong","vignass","schizo","psycho","drinknow") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  janitor::remove_empty("rows") 


#common vars in 07 and 93
intersect(names(subset_pms07), names(source_pms93))
# in 00 and 93
intersect(names(subset_pms00), names(source_pms93))

#colnames in 07 but not 93
setdiff(names(subset_pms07), names(source_pms93))
#colnames in 00 but not 93
setdiff(names(subset_pms00), names(source_pms93))


## subset 1993 vars of interest
subset_pms93 <- source_pms93 %>%
  dplyr::select("age10yr","dep","ethnic4","gad","ocd","panic","phob",
                "sc","seg","tenure", "mad","eacid","espeed","eglue","be10d",
                "nchldren","a10","bb1a01","bb1d01","bb1eyr01","bb2",
                "bb2a01","ehash","bc2","bc3","bc4a01","bc4b01","bc4c01",
                "bc4d01m1","bb2bmt01","bb2byr01","bb2bne01","bb2cyr01",
                "bb2cmt01","bb2d01","ecoke","eheroin","eopium","eecstasy",
                "esleep","etranx","ab2","ab3a","bf2","bf4","bf8b","bf13",
                "region","area","hhold","sex01","age01","race01","a4","a5",
                "a5a","a5b","a11","a15","bg1","bg1a","bg1a1","bg1b","bg1c",
                "bg1d","bg1f","bg2","bg2d","bg2e","bg2f","bg2g","bg2h","bg2i",
                "bg2j","bg2k","bg3a","bg3b","bg4","bg4a","bf12a","bf12b","bf14","bf15a","bf16","bf17","bf17am1","bf18dna",
                "bf18","bf19","bf21","bf21a","bf21a1m1","bf22m1","bf24",
                "bf25","bb1a01","diagno11","subethn","submarst","workstat",
                "timeunem") %>
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  janitor::remove_empty("rows")


 

# view 
#subset_pms07 %>% sjPlot::view_df()# no longer needed
# makeCodebook now works after reading in 
# .csv with as.factor()
#makeCodebook(subset_pms07, vol = "10",
#             reportTitle = NULL, file = NULL)


skim(subset_pms07)
##########################################################

table(subset_pms07$res_sex)
str(subset_pms07$res_sex)

# Cleaning Subset

## Re-Coding Names

# look at variable names 
colnames(subset_pms07)

# rename variables for easier reading 
subset_pms07 <- subset_pms07 %>% 
  rename(GenHeal = SF1,
         Paidwk = Everwk,
         DefPsych = Psycdis,
         Sex = ResSex,
         EthnicGrp = ETHNIC4,
         HiEduQualGrp = EDQUAL5,
         MariStat = ResMarDF, 
         EmpStatGrp = DVILO3a, 
         DaLivHelp = DVPrac, 
         LeftJob = yearjbl,
         JobReas = JbReas,
         LookWk4 = Looked,
         NotLookWk = YInAct,
         Working = Stat,
         WkSolo = Solo,
         TimeLWk = DVLastWk,
         NotWkRea = NotWk,
         SocEcGrp = SEG,
         SocCl = SC,
         LessWkEmo = SF6,
         LessCarEmo = SF7,
         FelCal = SF9,
         FelDownH = SF11,
         ProbInter = SF12,
         PosEx = CONHOMD,
         CurMeds = Medic,
         ComCaYr = CC2aY,
         ComCaPsychi = CC2Y1,
         CommCaPsychol = CC2Y2,
         ComCarCPN = CC2Y3,
         ComCarON = CC2Y5,
         ComCarSW = CC2Y6,
         ComCaSHGrp = CC2Y7,
         ComCaHH = CC2Y8,
         ComCaOW = CC2Y9,
         GenHeal = SF1
  )

# check it worked 
colnames(subset_pms07)

# Descriptive Statistics 



# prop 
prop.table(table(subset_pms07$Sex))

## Age Respondant Variable 


CrossTable(subset_pms07$Age10yr,
           subset_pms07$Sex,prop.r=FALSE,prop.t=FALSE,
           prop.chisq=FALSE)

## General Employment

# by age
CrossTable(subset_pms07$Paidwk,
           subset_pms07$Age20yr,prop.r=FALSE,prop.t=FALSE,
           prop.chisq=FALSE)

# by sex 
CrossTable(subset_pms07$Paidwk,
           subset_pms07$Sex,prop.r=FALSE,prop.t=FALSE,
           prop.chisq=FALSE)


# by reported mh cond
CrossTable(subset_pms07$Paidwk,
           subset_pms07$Health6,prop.r=FALSE,prop.t=FALSE,
           prop.chisq=FALSE)

## Mental Health Variable

CrossTable(subset_pms07$Health6,
           subset_pms07$Sex,prop.r=FALSE,prop.t=FALSE,
           prop.chisq=FALSE)

CrossTable(subset_pms07$DefPsych,
           subset_pms07$Sex,prop.r=FALSE,prop.t=FALSE,
           prop.chisq=FALSE)


# Employment 

prop.table(table(subset_pms07$Paidwk))
prop.table(table(subset_pms07$Health6))
                                                  
                                                  
# bar chart 
ggplot(subset_pms07,aes(x=Paidwk))+
geom_bar()+
facet_grid(~Health6)
                                                  
# 2-Way Cross Tabulation
CrossTable(subset_pms07$Sex, subset_pms07$Health6)
                                                  
# 3-Way Frequency Table
# gender by ever having a paid job and mentioned 
# having a mental health condition 
 mytable <- table(subset_pms07$Sex, subset_pms07$Paidwk,
subset_pms07$Health6)
ftable(mytable)
                                    
                                                  
# Severe Mental Illness
                                                 
# Visulisation 
                                                  
# Non-Severe Mental Illness
                                                  
# Severe Mental Illness
## Age 
                                              
                                                 
                                                  
## Mental Health

ggplot(subset_pms07,aes(x=Health6))+
geom_bar()+
facet_grid(~Sex)
                                                  
                                                  
# visualisation
ggplot(subset_pms07,aes(x=DefPsych))+
geom_bar()+
facet_grid(~Sex)

                                                  
## Employment 
                                                  

# bar chart 
ggplot(subset_pms07,aes(x=Paidwk))+
geom_bar()+
facet_grid(~Health6)
                                                  

                                                  
# Tests of Independance 
## Chi-Sqaure 

 # significance level.
 chisq.test(subset_pms07$Paidwk, subset_pms07$DefPsych)
                                                  
## Correlations
# Codebook
                                                    

makeCodebook(subset_pms07, vol = "9", 
reportTitle = NULL, file = NULL)

# checking package versions ----------------------------------------------------
sessionInfo()
                                                    
                                                    