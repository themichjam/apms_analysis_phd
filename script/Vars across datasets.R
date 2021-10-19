#### 2000 variable that are similar to the no matches 07####

"respsex","sstrtreg","resmarst","dtjbl","jbreas","commany",
"numchild","docwhat","treat","wt2","parents","mencenyr","othdayyr",
"daycenyr","doctalk","suicthlf","suicatlf","q20","q23","b3a",
"govreggb","numadult","pmatoday","indebt01","futrjbw1","mad",
"psycdrug","drglong","vignass","schizo","psycho","drinknow"



## 00
"acidever","acidyear","age10yr","age20yr","amphever","amphyear",
"amylever","amylyear","anabever","anabyear","anyquals",
"artther","auditgp","audsad2","cannever","cannyear","cc2ay",
"cc2y1","cc2y2","cc2y3","cc2y5","cc2y6","cc2y7","cc2y8","cc2y9",
"chldinst","cisrfour","cldnyr","cnslhav","cnsllng","cnsltak",
"cocaever","cogther","counsel","cpnyr", "cracever","cracyear",
"daywht1","daywht2","daywht3","daywht4","dayy","dep","diffjob",
"dlss4","dlss5","docpsyc","doctalk","docyear","drnkprob",
"drugdep","drugdep2","drugever","drugyear","dsh10","dsh5","dsh9",
"dvilo3a"  "dvlastwk" "ecstever","ecstyear","edqual5","empno",
"empsty","ethnic4","everwk","ftptwk","gad","glueever","glueyear",
"gross4","gross4a","heroever","heroyear","hiquals","hmhelpyr",
"hrsweek","hrswork","inqtrmen","inwhy","jbreas","jobstm","lacare",   "language" "llord"   
"lookatal","looked","looknot1","looknot2","looknot3","looknow",
"looksto2","looksto3","manage","marither","menthos","methever",
"methyear","mushever","mushyear","neurotic","nosymp","notwk",
"ocd","oreachyr","origin","othnseyr","othther","outqtrme","outwhy",  
"panic","phob","pmatnum","psycther","psylgtyr","psytrtyr","sadqgp",   "sc"       "seg"      "sempsty" 
"sexther","sf1","sf11","sf12","sf6","sf7","sf9","sfhelpyr",
"soctrain","socwrkyr","solo","srcinc1","srcinc2","srcinc3",
"srcinc4","srcinc5","srcinc6","stat","suicatlf","suicatwk",
"suicatyr","suicthlf","suicthwk","suicthyr","ten1","tenure",
"tranever","tranyear","trtment","wkshel1","wkshel2","yinact","respsex","sstrtreg","resmarst","dtjbl","jbreas","commany",
"numchild","docwhat","treat","wt2","parents","mencenyr","othdayyr",
"daycenyr","doctalk","suicthlf","suicatlf","q20","q23","b3a",
"govreggb","numadult","pmatoday","indebt01","futrjbw1","mad",
"psycdrug","drglong","vignass","schizo","psycho","drinknow" 


# make colnames lowercase
#sublow93 <-source_pms93 %>% rename_all(tolower), [,order(colnames(sub07low))]
#sublow00 <-source_pms00 %>% rename_all(tolower)
#sublow07 <-source_pms07 %>% rename_all(tolower)



# arrange alphebetically and make colnames lowercase

sorted07 <-subset_pms07[,order(colnames(subset_pms07))] %>% rename_all(tolower)
sorted00 <-source_pms00[,order(colnames(source_pms00))] %>% rename_all(tolower)
sorted93 <-source_pms93[,order(colnames(source_pms93))] %>% rename_all(tolower)




#common vars in 00 and 07
intersect(names(sorted07), names(sorted00))

#colnames in 07 but not 00
setdiff(names(sorted07), names(sorted00))


#common vars in 07 and 93
intersect(names(sorted07), names(sorted93))

#colnames in 07 but not 93
setdiff(names(sorted07), names(sorted93))


#### 93 vars
"age10yr","dep","ethnic4","gad","ocd","panic","phob",
"sc","seg","tenure", "mad","eacid","espeed","eglue","be10d",
"nchldren","a10","bb1a01","bb1d01","bb1eyr01","bb2",
"bb2a01","ehash","bc2","bc3","bc4a01","bc4b01","bc4c01",
"bc4d01m1","bb2bmt01","bb2byr01","bb2bne01","bb2cyr01",
"bb2cmt01","bb2d01","ecoke","eheroin","eopium","eecstasy",
"esleep","etranx","ab2","ab3a","bf2","bf4","bf8b","bf13",
"region","area","hhold","sex01","age01","race01","a4","a5",
"a5a","a5b","a11","a15","bg1","bg1a","bg1a1","bg1b","bg1c",
"bg1d","bg1f","bg2","bg2d","bg2e","bg2f","bg2g","bg2h","bg2i",
"bg2j","bg2k","bg3a","bg3b","bg4","bg4a","bf12a","bf12b",
" bf13a","bf14","bf15a","bf16","bf17","bf17am1","bf18dna",
"bf18","bf19","bf21","bf21a","bf21a1m1","bf22m1","bf24",
"bf25","bb1a01","diagno11","subethn","submarst","workstat",
"timeunem"




#### descriptives test

subset_pms07 %>%
  table1(age10yr, sf1, everwk)
