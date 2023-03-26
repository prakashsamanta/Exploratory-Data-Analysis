library(dplyr)
library(stringr)
library(ggplot2)

######################################################
#Reading Data
df1 <- read.csv("Data2.csv",as.is = c(1:5,18),na.strings = '')
########################################################
#Data cleaning

df2 <- df1[-1]

vec1 <- c("age","gender","location","profession","income_per_month","covid_affected","vaccinated_no_of_doses",
          "lockdown_effect_finance","lockdown_effect_physically","lockdown_effect_mentally",
          "comorbidity","exercise_freq","view_on_exercise","traveling_freq_work",
          "traveling_freq_emergency","traveling_freq_vacation","covid_precautions",
          "covid_effect_on_work","profession_change","focus_on_physical_health",
          "focus_on_mental_health","focus_on_finance")

colnames(df2) <- vec1

df2$age <- as.integer(df2$age)

df3 <- df2%>%
  mutate_at(c(3,4),tolower)

df3 <- df3[complete.cases(df3$age),]
df3$age_grp <- cut(df3$age,5)
levels(df3$age_grp) <- c("(17,27]", "(27,36]", "(36,46]", "(46, 55]", "(55,65]")


levels(df3$vaccinated_no_of_doses) <- c(0,2,1)

df3$covid_affected1 = df3$covid_affected
levels(df3$covid_affected1) <- c(0,0,"undefined",1)
df3$profession1 <- df3$profession

df3$profession1[which(str_detect(df3$profession1,c("student|nursing|studying|student |nursing|nursing student|medical student|mba student|graphic designer|completed bachelors, studying for cfa |design student |college student|mbbs|bsc nursing|students |nursing students|b.sc nursing|b. sc nursing|students |bsc nursing |nursing |students")))] = "student"
df3$profession1[which(str_detect(df3$profession1,c("service|production engineer|engineer|it professional|government servant|employee in private company|job at it|graduate|software developer|private sector (business)|educator |job|it specialist|marketing|athlete/accountant|doctor|private |sales executive |trader|private job|graduate engineer|government service|govt employee|gorvenment servant|government|digital creator, photografer, baker|business|government job|insurance advisor|government officer|govt job|pvt job |pvt  job|govt service|president|executive |govt servant |self employed|govt. service|professor in pharmacy|government employee|advocate |govt servent |govt servant|doctor |serving |teacher|self employment|government job |govt. service |data scientist|executive in supply chain management |govt.employee|government service |it|corporate professional|govt. servant |central government services |service |govt officer|print media employee |physiotherapist|hardware engineer|officer|invigilator for idp|teaching|nurse|assistant manager|agriculture|job |nursing tutor|govt. servant|pvt job")))] = "employed"
df3$profession1[which(str_detect(df3$profession1,c("retired govt service|retd ongc officer|retired|retired |home maker|retired from ongc |household|retired government servant|house manager |housewife |house wife|no|un employees|housewife")))] = "unemployed"
df3$profession1[which(str_detect(df3$profession1,c("student|unemployed"),negate = T))] = "employed" 

df3$covid_precautions1 <- as.character(lapply(strsplit(c(df3$covid_precautions),";"),length))

df3$gender[is.na(df3$gender)] <- "Prefer not to say"

#################################################################
#age-wise effect of lockdown financially

age_count <- df3 %>% 
  group_by(age_grp) %>% 
  summarise(count= n())


df_age <- df3 %>% 
  group_by(age_grp,profession_change) %>% 
  summarise(count = n())

tab <- data.frame(table(df_age$age_grp))

df_age$total <- rep(age_count$count,times=tab$Freq)
df_age$freqden <- df_age$count/df_age$total



ggplot(data=df_age ,aes(x= age_grp,y= freqden ,fill= profession_change ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Age group\nFigure 1.1",title="Effect on profession during Covid",
       fill= "Change of Profession", subtitle = "Age wise")+
  scale_y_continuous(labels = scales::percent_format())


#####################################
#Income group wise effect of lock down financially

df_fin1 <- subset(df3,profession1 != "student",select = c("income_per_month","lockdown_effect_finance"))

igrp_count <- df_fin1 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())


df_fin <- df_fin1 %>% 
  group_by(income_per_month,lockdown_effect_finance) %>% 
  summarise(count = n())

tab1 <- data.frame(table(df_fin$income_per_month))


df_fin$total <- rep(igrp_count$count,times=tab1$Freq)
df_fin$freqden <- df_fin$count/df_fin$total

ggplot(data=df_fin ,aes(x= income_per_month,y= freqden ,fill = lockdown_effect_finance ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Income Group\nFigure 1.2",title="Effect of lockdown on Financial Condition",
       fill= "Effect of lockdown Financially", subtitle = "income group wise")+
  scale_y_continuous(labels = scales::percent_format())


#####################################################
#Change of profession in every income group


df_fin2 <- subset(df3,profession1 != "student",select = c("income_per_month","profession_change"))

igrp_count1 <- df_fin2 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())


df_fina <- df_fin2 %>% 
  group_by(income_per_month,profession_change) %>% 
  summarise(count = n())

tab2 <- data.frame(table(df_fina$income_per_month))


df_fina$total <- rep(igrp_count1$count,times=tab2$Freq)
df_fina$freqden <- df_fina$count/df_fina$total

ggplot(data=df_fina ,aes(x= income_per_month,y= freqden ,fill= profession_change ))+
  geom_bar(position="dodge",stat="identity",width = 0.5)+
  labs(y="Percentage",x="Income Group\nFigure 1.3",title="Change of profession",
       fill= "Change of profession", subtitle = "income group wise")+
  scale_y_continuous(labels = scales::percent_format())

##############################################################
#change in work life in different income groups


df_fin3 <- subset(df3,profession1 != "student",select = c("income_per_month","covid_effect_on_work"))

igrp_count2 <- df_fin3 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())


df_finan <- df_fin3 %>% 
  group_by(income_per_month,covid_effect_on_work) %>% 
  summarise(count = n())

tab3 <- data.frame(table(df_finan$income_per_month))

df_finan$total <- rep(igrp_count2$count,times=tab3$Freq)
df_finan$freqden <- df_finan$count/df_finan$total

ggplot(data=df_finan ,aes(x= income_per_month,y= freqden ,fill= as.factor(covid_effect_on_work) ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Income Group\nFigure 1.4",title="Change in Worklife",
       fill= "Change in worklife\n(not affected to very affected)",
       subtitle = "income group wise")+
  scale_y_continuous(labels = scales::percent_format())

###########################################################
#focus of people in different income groups during Covid


df_fin4 <- subset(df3,profession1 != "student",select = c("income_per_month","focus_on_finance"))

igrp_count3 <- df_fin4 %>% 
  group_by(income_per_month) %>% 
  summarise(count= n())

df_fin_focus <- df_fin4 %>% 
  group_by(income_per_month,focus_on_finance) %>% 
  summarise(count = n())

tab4 <- data.frame(table(df_fin_focus$income_per_month))


df_fin_focus$total <- rep(igrp_count3$count,times=tab4$Freq)
df_fin_focus$freqden <- df_fin_focus$count/df_fin_focus$total

ggplot(data=df_fin_focus ,aes(x= income_per_month,y= freqden ,fill= focus_on_finance ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Income Group\nFigure 1.5",title="Focus on Finance during Covid",
       fill= "Focus on Finance", subtitle = "income group wise")+
  scale_y_continuous(labels = scales::percent_format())


############################################################
#people who has co morbidity how their physical health is affected after getting infected with covid



df_covid <- subset(df3,covid_affected1 == 1 , select = c(comorbidity,lockdown_effect_physically))
df_covid <- df_covid[complete.cases(df_covid),]


comorb_count <- df_covid %>% 
  group_by(comorbidity) %>% 
  summarise(count= n())

df_covid1 <- df_covid %>% 
  group_by(comorbidity,lockdown_effect_physically) %>% 
  summarise(count = n())

tab5 <- data.frame(table(df_covid1$comorbidity))

df_covid1$total <- rep(comorb_count$count,times = tab5$Freq)
df_covid1$freqden <- df_covid1$count/df_covid1$total

ggplot(data=df_covid1 ,aes(x= comorbidity,y= freqden ,fill= lockdown_effect_physically ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Comorbidity\nFigure 1.6",title="Effect of Covid on Physical Health",
       fill= "Effect of Covid on Physical Health", subtitle = "Covid Infected population")+
  scale_y_continuous(labels = scales::percent_format())


##################################################################
#people infected with covid vs change of profession

df_covidp <- select(df3,covid_affected,profession_change)
df_covidp <- df_covidp[complete.cases(df_covidp),]


infected_count <- df_covidp %>% 
  group_by(covid_affected) %>% 
  summarise(count= n())


df_covid2 <- df_covidp %>% 
  group_by(covid_affected,profession_change) %>% 
  summarise(count = n())

levels(df_covid2$covid_affected) <- c("No I never had \n symptoms of covid"                        
                                      ,"No, I had covid related symptoms \n  but was tested negative"
                                      ,"No, I had symptoms of covid \n but never tested"            
                                      ,"Yes, I had symptoms \n and was detected positive" )

tab6 <- data.frame(table(df_covid2$covid_affected))

df_covid2$total <- rep(infected_count$count,times = tab6$Freq)
df_covid2$freqden <- df_covid2$count/df_covid2$total


ggplot(data=df_covid2 ,aes(x= covid_affected,y= freqden ,fill= profession_change ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="covid infection\nFigure 1.7",title="Effect of Covid on profession",
       fill= "change of profession")+
  scale_y_continuous(labels = scales::percent_format())

#########################################################################
#going out for vacation vs vaccination dose

df_vac <- select(df3,vaccinated_no_of_doses,traveling_freq_vacation)


grp_count <- df_vac %>% 
  group_by(vaccinated_no_of_doses) %>% 
  summarise(count= n())

df_vac_travel <- df_vac %>% 
  group_by(vaccinated_no_of_doses,traveling_freq_vacation) %>% 
  summarise(count = n())

tab7 <- data.frame(table(df_vac_travel$vaccinated_no_of_doses))

df_vac_travel$total <- rep(grp_count$count,times=tab7$Freq)
df_vac_travel$freqden1 <- df_vac_travel$count/df_vac_travel$total

ggplot(data=df_vac_travel ,aes(x=vaccinated_no_of_doses ,y= freqden1 ,fill= traveling_freq_vacation ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="No of Doses\nFigure 1.8",title="vaccinated population going out for vacation",
       fill= "going out for vacation", subtitle = "vaccination dose count wise")+
  scale_y_continuous(labels = scales::percent_format())


##################################################################
#vaccinated population vs precaution

df_vac1 <- select(df3,vaccinated_no_of_doses,covid_precautions1)

grp_count <- df_vac1 %>% 
  group_by(vaccinated_no_of_doses) %>% 
  summarise(count= n())

df_vac_prec <- df_vac1 %>% 
  group_by(vaccinated_no_of_doses,covid_precautions1) %>% 
  summarise(count = n())

tab8 <- data.frame(table(df_vac_prec$vaccinated_no_of_doses))

df_vac_prec$total <- rep(grp_count$count,times=tab8$Freq)
df_vac_prec$freqden1 <- df_vac_prec$count/df_vac_prec$total

ggplot(data=df_vac_prec ,aes(x=vaccinated_no_of_doses ,y= freqden1 ,fill= as.factor(covid_precautions1 )))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="No of Doses\nFigure 1.9",title="vaccinated population going out for vacation",
       fill= "following no of precautions", subtitle = "vaccination dose count wise")+
  scale_y_continuous(labels = scales::percent_format())
####################################################################
### focus of people on different factors after covid age wise
####################################
##Focus physical health

focus_area <- select(df3, age_grp, focus_on_physical_health, focus_on_mental_health, focus_on_finance)
focus_area <- focus_area[!is.na(focus_area$age),]


focus_physical_health <- focus_area %>%
  group_by(age_grp, focus_on_physical_health) %>%
  summarise(count = table(focus_on_physical_health))
focus_physical_health

ggplot(focus_physical_health) +
  geom_bar(aes(x = age_grp, y = count, fill = focus_on_physical_health),
           stat = "Identity", position = "fill",width = 0.6) + 
  labs(y="percentage",x="Age Group\nFigure 1.10",title="Focus on Physical Health During Covid",
       fill= "Focus on Physical Health", subtitle = "Age group Wise")+
  scale_y_continuous(labels = scales::percent_format())+coord_polar()



##########################################
##Focus Mental health
focus_mental_health <- focus_area %>%
  group_by(age_grp, focus_on_mental_health) %>%
  summarise(count = table(focus_on_mental_health))

ggplot(focus_mental_health) +
  geom_bar(aes(x = age_grp, y = count, fill = focus_on_mental_health), 
           stat = "Identity", position = "fill",width = 0.7)+ 
  labs(y="percentage",x="Age Group\nFigure 1.11",title="Focus on Mental Health During Covid",
       fill= "Focus on Mental Health", subtitle = "Age group Wise")+
  scale_y_continuous(labels = scales::percent_format())+coord_polar()
##########################################################
#covid infection vs taking precautions

df_vacp <- select(df3,covid_affected1,covid_precautions1)
df_vacp <- df_vacp[complete.cases(df_vacp),]

grp_count <- df_vacp %>% 
  group_by(covid_affected1) %>% 
  summarise(count= n())

df_aff_prec <- df_vacp%>% 
  group_by(covid_affected1,covid_precautions1) %>% 
  summarise(count = n())

tab9 <- data.frame(table(df_aff_prec$covid_affected1))

df_aff_prec$total <- rep(grp_count$count,times = tab9$Freq)
df_aff_prec$freqden1 <- df_aff_prec$count/df_aff_prec$total

ggplot(data=df_aff_prec ,aes(x=covid_affected1 ,y= freqden1 ,fill= as.factor(covid_precautions1 )))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="covid infection\nFigure 1.12",title="Taking precautions vs covid infection",
       fill= "following no of precautions")+
  scale_y_continuous(labels = scales::percent_format())

#############################################################################
#No of precautions people taking while going out for work


df_vac_1 <- select(df3,traveling_freq_work,covid_precautions1)
df_vac_1 <- df_vac_1[complete.cases(df_vac_1),]

grp_count <- df_vac_1 %>% 
  group_by(traveling_freq_work) %>% 
  summarise(count= n())

df_traW_prec <- df_vac_1%>% 
  group_by(traveling_freq_work,covid_precautions1) %>% 
  summarise(count = n())

tab10 <- data.frame(table(df_traW_prec$traveling_freq_work))

df_traW_prec$total <- rep(grp_count$count,times = tab10$Freq)
df_traW_prec$freqden1 <- df_traW_prec$count/df_traW_prec$total

ggplot(data=df_traW_prec ,aes(x=traveling_freq_work ,y= freqden1 ,fill= as.factor(covid_precautions1 )))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Frequency of Traveling for work\nFigure 1.13",title="Taking Precautions While going out for Work",
       fill= "following no of precautions")+
  scale_y_continuous(labels = scales::percent_format())


#############################################################################
#No of precautions people taking while going out for vacation


df_vac_2 <- select(df3,traveling_freq_vacation,covid_precautions1)
df_vac_2 <- df_vac_2[complete.cases(df_vac_2),]

grp_count <- df_vac_2 %>% 
  group_by(traveling_freq_vacation) %>% 
  summarise(count= n())

df_traV_prec <- df_vac_2%>% 
  group_by(traveling_freq_vacation,covid_precautions1) %>% 
  summarise(count = n())

tab11 <- data.frame(table(df_traV_prec$traveling_freq_vacation))



df_traV_prec$total <- rep(grp_count$count,times = tab11$Freq)
df_traV_prec$freqden1 <- df_traV_prec$count/df_traV_prec$total

ggplot(data=df_traV_prec ,aes(x=traveling_freq_vacation ,y= freqden1 ,fill= as.factor(covid_precautions1 )))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Frequency of Traveling for vacation\nFigure 1.14",title="Taking Precautions While going out for vacation",
       fill= "following no of precautions")+
  scale_y_continuous(labels = scales::percent_format())


#############################################################################
#No of precautions people taking while going out for emergency

df_vac_3 <- select(df3,traveling_freq_emergency,covid_precautions1)
df_vac_3 <- df_vac_3[complete.cases(df_vac_3),]

grp_count <- df_vac_3 %>% 
  group_by(traveling_freq_emergency) %>% 
  summarise(count= n())

df_traE_prec <- df_vac_3%>% 
  group_by(traveling_freq_emergency,covid_precautions1) %>% 
  summarise(count = n())

tab12 <- data.frame(table(df_traE_prec$traveling_freq_emergency))

df_traE_prec$total <- rep(grp_count$count,times = tab12$Freq)
df_traE_prec$freqden1 <- df_traE_prec$count/df_traE_prec$total

ggplot(data=df_traE_prec ,aes(x=traveling_freq_emergency ,y= freqden1 ,fill= as.factor(covid_precautions1 )))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Frequency of traveling for emergency\nFigure 1.15",title="Taking Precautions While going out for emergency",
       fill= "following no of precautions")+
  scale_y_continuous(labels = scales::percent_format())

#################################################################
#Age wise freq of exercise

age_count1 <- df3 %>% 
  group_by(age_grp) %>% 
  summarise(count= n())


df_age1 <- df3 %>% 
  group_by(age_grp,exercise_freq) %>% 
  summarise(count = n())


tab13 <- data.frame(table(df_age1$age_grp))


df_age1$total <- rep(age_count1$count,times=tab13$Freq)
df_age1$freqden <- df_age1$count/df_age1$total



ggplot(data=df_age1 ,aes(x= age_grp,y= freqden ,fill= exercise_freq ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Age group\nFigure 1.16",title="Exercise Frequency in different Age group",
       fill= "Exercise Frequency", subtitle = "Age wise")+
  scale_y_continuous(labels = scales::percent_format())

#################################################################
#age-wise physically affected by lockdown

age_count2 <- df3 %>% 
  group_by(age_grp) %>% 
  summarise(count= n())


df_age2 <- df3 %>% 
  group_by(age_grp,lockdown_effect_physically) %>% 
  summarise(count = n())


tab14 <- data.frame(table(df_age2$age_grp))


df_age2$total <- rep(age_count2$count,times=tab14$Freq)
df_age2$freqden <- df_age2$count/df_age2$total


ggplot(data=df_age2 ,aes(x= age_grp,y= freqden ,fill= lockdown_effect_physically ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Age group\nFigure 1.17",title="Effect on Physical Health due to Lockdown",
       fill= "Effect on physical health", subtitle = "Age wise")+
  scale_y_continuous(labels = scales::percent_format())

#################################################################
#age-wise mentally affected by lockdown

age_count3 <- df3 %>% 
  group_by(age_grp) %>% 
  summarise(count= n())



df_age3 <- df3 %>% 
  group_by(age_grp,lockdown_effect_mentally) %>% 
  summarise(count = n())


tab15 <- data.frame(table(df_age3$age_grp))


df_age3$total <- rep(age_count3$count,times=tab15$Freq)
df_age3$freqden <- df_age3$count/df_age3$total



ggplot(data=df_age3 ,aes(x= age_grp,y= freqden ,fill= lockdown_effect_mentally ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="Percentage",x="Age group\nFigure 1.18",title="Effect on Mental Health due to Lockdown",
       fill= "Effect on Mental health", subtitle = "Age wise")+
  scale_y_continuous(labels = scales::percent_format())


#################################################################
#age-wise mentally affected by change of profession

prof_count <- df3 %>% 
  group_by(profession_change) %>% 
  summarise(count= n())


df_prof <- df3 %>% 
  group_by(profession_change,lockdown_effect_mentally) %>% 
  summarise(count = n())

tab16 <- data.frame(table(df_prof$profession_change))


df_prof$total <- rep(prof_count$count,times=tab16$Freq)
df_prof$freqden <- df_prof$count/df_prof$total



ggplot(data=df_prof ,aes(x= profession_change,y= freqden ,fill= lockdown_effect_mentally ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="percentage",x="Change of Profession\nFigure 1.19",title="Effect on Mental Health due to Change of profession",
       fill= "Effect on Mental health")+
  scale_y_continuous(labels = scales::percent_format())


#################################################################
#age-wise financally affected by change of profession


prof_count1 <- df3 %>% 
  group_by(profession_change) %>% 
  summarise(count= n())


df_prof1 <- df3 %>% 
  group_by(profession_change,lockdown_effect_finance) %>% 
  summarise(count = n())


tab17 <- data.frame(table(df_prof1$profession_change))


df_prof1$total <- rep(prof_count1$count,times=tab17$Freq)
df_prof1$freqden <- df_prof1$count/df_prof1$total



ggplot(data=df_prof1 ,aes(x= profession_change,y= freqden ,fill= lockdown_effect_finance ))+
  geom_bar(position="dodge",stat="identity",width =0.5)+
  labs(y="percentage",x="Change of profession\nFigure 1.20",title="Effect on Financial condition due to Profession change",
       fill= "Effect on Financial Condition")+
  scale_y_continuous(labels = scales::percent_format())


###############################################################################
########Pie Plot
################################################
library(webr)

PD <- df3 %>% group_by(gender, income_per_month) %>%
  summarise(count = n())

PieDonut(PD, aes(gender, income_per_month, count = count),
         title = "Gender Wise Income distribution")

############################################################

PD1 <- df3 %>% group_by(gender, covid_affected) %>%
  summarise(count = n())

PieDonut(PD1, aes(gender, covid_affected),
         title = "Gender wise covid affected")
############################################################

PD2 <- df3 %>% group_by(age_grp, profession_change) %>%
  summarise(count = n())

PieDonut(PD2, aes(age_grp, profession_change),
         title = "Age group and Profession Change")

###############################################################

df12 <- as.data.frame(df3$income_per_month)
colnames(df12) <- c("Income_Groups")

df13 <- data.frame(table(df12$Income_Groups))
df13

pie(df13$Freq, labels = c("17.1%", "15.3%", "10%", "14.9%", "20.8%", "21.8%"), col = rainbow(6),
    main = "Income Distribution")
legend("topright", legend = df13$Var1,
       cex = 0.75, fill = rainbow(length(df13$Freq)))

#############################################################

library(plotrix)
df14 <- as.data.frame(df3$covid_affected)
colnames(df14) <- c("Covid_affected")

df15 <- data.frame(table(df14$Covid_affected))
df15

pie3D(df15$Freq, labels = c("58.2%", "12.4%", "9%", "20.4%"), col = rainbow(6),
    main = "Covid Affected or not")

legend("topright", legend = df15$Var1,
       cex = 0.75, fill = rainbow(length(df15$Freq)))

##############################################################

df16 <- as.data.frame(df3$profession_change)
colnames(df16) <- c("Profession_Change")

df18 <- data.frame(table(df16$Profession_Change))
df18

pie3D(df18$Freq, labels = c("3.5%", "87.6%", "1.5%", "7.4%"),
      main = "Profession Change")
legend("topright", legend = df18$Var1,
       cex = 0.75, fill = rainbow(length(df18$Var1)))
#################################################################

df20 <- as.data.frame(df3$gender)
colnames(df20) <- c("Gender")

df22 <- data.frame(table(df20$Gender))
df22

pie3D(df22$Freq, labels = c("39%", "60%", "1%"), col = rainbow(6),
    main = "Gender Distribution")
legend("topright", legend = df22$Var1,
       cex = 0.75, fill = rainbow(length(df22$Freq)))

