#Set Up of Workspace----
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gsubfn)
library(magrittr)
library(tm)
library(sqldf)


#Importing of datasets----

#RawLeadCopper is the Lead and Copper Testing Database
#MasterDF is the excel sheet with school info shared by Dr. Tob
#SchoolPWSs is the NTCs in SDWIS filtered by schools
#EECFPWSs is the NTCs in SDWIS filtered by daycares
RawLeadCopper <- read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/LeadCopper.xlsx")
MasterDF<- read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/Master File WIIN EAP 25_Oct_2021.xlsx", sheet = "Facilities")
SchoolPWSs<- read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/PWS Summaries (schools).xlsx")
EECFPWSs<-read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/PWS Summaries (daycares).xlsx")
MasterDF1617<- read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/Masterfile_JET_final 20162017_Oct2020rev.xlsx", sheet = "Schools")
MasterDF_buildingage<- read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/Masterfile-2017-2018 17 Sept 2018 with building age.xlsx", sheet = "Facilities")
MasterDF1718<- read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/Masterfile-2017-2018 JET Oct2020.xlsx", sheet = "Facilities")
WaterSystemDetail<-read_excel("C:/Users/liama/OneDrive/Desktop/School/Thesis/Water System Detail.xlsx")

#Cleaning of the Lead Copper Data----

#set some columns to uppercase for later ease of parsing
#created column for the year of testing
#set numeric columns to numeric
#creates column called test location by taking the numbers out of LocationCode
#creates column called TestType which extracts the characters from LocationCode (should be P or F) if entered correctly
CleanLeadCopper<-RawLeadCopper%>%
  mutate(LocationCode=toupper(LocationCode),
         LocationDescription=toupper(LocationDescription),
         Year=str_sub(CollectedDate,1,4),
         Result=as.numeric(Result),
         LabReportedDetectionLimit=as.numeric(LabReportedDetectionLimit),
         RequiredDetectionLimit=as.numeric(RequiredDetectionLimit),
         TestLocation=gsub("[^[:digit:]]","",LocationCode),
         TestType=gsub("[[:digit:]]","",LocationCode),
         Result=ifelse(is.na(Result),0,Result))#set ND results to 0 for now so that when the 
         #data is merged we can tell which had undetected tests and which didn't test at all


#Assigning PWS IDs----

#This is a master dataframe of the 4 dfs, Dr.Tobiason shared, it has columns for 
#PWSID, School Name, Org Code, and Location
MASTERDF<-MasterDF1617%>%
  select(`PWS ID`,`School Name`,`ORG Code`,`Town`)%>%
  set_colnames(c("PWS ID","Facility Name","ORG Code","Location"))%>%
  rbind(MasterDF_buildingage%>%
          select(`PWS ID`,`Facility Name`,`ORG Code`,`Location`),
        MasterDF1718%>%
          select(`PWS ID`,`Facility Name`,`ORG Code`,`Location`),
        MasterDF%>%
          select(`PWSID`,`Facility Name`,`Org Code`,`PPC Town`)%>%
                   set_colnames(c("PWS ID","Facility Name","ORG Code","Location")))%>%
  mutate(Location=toupper(Location))%>%
  filter(!is.na(`ORG Code`))%>%
  distinct()

#this dataframe is a DF of all the different schools in the LeadCopper DF
#there are 1516 distinct schools
facilities<-CleanLeadCopper%>%
  select(`School/EECFName`,Town,LCCAFacilityID)%>%
  mutate(Town=toupper(Town))%>%
  set_colnames(c("Facility Name","Location","ORG Code"))%>%
  distinct()%>%
  merge(select(MASTERDF,`PWS ID`,`ORG Code`),all.x=T)

#the 3 chunks below just select and rname columns for the 3 different SDWIS databases

EECFclean<-EECFPWSs%>%
  select(`PWS Name`,`PWS ID`,`City Name`)%>%
  mutate(`PWS Name`=str_to_title(`PWS Name`))%>%
  set_colnames(c("Facilty Name",'PWS ID',"Location"))

Schoolsclean<-SchoolPWSs%>%
  select(`PWS Name`,`PWS ID`,`City Name`)%>%
  mutate(`PWS Name`=str_to_title(`PWS Name`))%>%
  set_colnames(c("Facilty Name",'PWS ID',"Location"))

Watersystemsclean<-WaterSystemDetail%>%
  select(`PWS Name`,`PWS ID`,`City Name`)%>%
  mutate(`PWS Name`=str_to_title(`PWS Name`))%>%
  set_colnames(c("Facilty Name",'PWS ID',"Location"))

#Combines all the distinct nonCWSs from SDWIS with the facility data
#I then manually went through and looked for schools that were matching PWS names
NonCWSs=EECFclean%>%
  full_join(Schoolsclean)%>%
  full_join(Watersystemsclean)%>%
  left_join(select(facilities,-`PWS ID`),by="Location")

#this assigns the PWSID to each of the schools that have their own PWS as determined by me
#from manually looking at the previous dataframe
facilities<-facilities%>%
  mutate(`PWS ID` = case_when(
    `Facility Name`=="Alphabet Soup Preschool Plus of Granby" ~ "1111037",
    `Facility Name`=="Country Montessori Preschool of Sutton" ~ "2290029",
    `Facility Name`=="Bay Path Regional Vocational Technical High School" ~ "2054031",
    `Facility Name`=="Brimfield Elementary" ~ "1043014",
    `Facility Name`=="Colrain Central" ~ "1066004",
    `Facility Name`=="Florence Sawyer School" ~ "2034024",
    `Facility Name`=="Hampshire Regional High" ~ "1331003",
    `Facility Name`=="Harry Lee Cole" ~ "3038009",
    `Facility Name`=="Hawlemont Regional" ~ "1053007",
    `Facility Name`=="Holland Elementary" ~ "1135003",
    `Facility Name`=="Nashoba Regional" ~ "2034010",
    `Facility Name`=="New Hingham Regional Elementary" ~ "1060004",
    `Facility Name`=="Pearl E Rhodes Elementary" ~ "1156001",
    `Facility Name`=="Pioneer Valley Regional" ~ "1217002",
    `Facility Name`=="Sanderson Academy" ~ "1013001",
    `Facility Name`=="Shutesbury Elementary" ~ "1272002",
    `Facility Name`=="Spofford Pond" ~ "3038008",
    `Facility Name`=="Swift River" ~ "1204001",
    `Facility Name`=="Warwick Community School" ~ "1312012",
    `Facility Name`=="Westhampton Elementary School" ~ "1331007",
    `Facility Name`=="Westport Elementary" ~ "4334017",
    TRUE~`PWS ID`
  ))

LeadCopperDF<-LeadCopperDF%>%
  merge(select(facilities,`ORG Code`, `PWS ID`),by.x = "LCCAFacilityID",by.y="ORG Code",all.x = T)

#Cleaning P and F measurements----
#dataframe of the different test types, should only be P and F when data is clean
TestTypes<-as.data.frame(table(LeadCopperDF$TestType))%>%
  set_colnames(c("TestType","n"))

ggplot(TestTypes,aes(TestType,n))+
  geom_col()
  
#mislabeled_df is a dataframe of all the places that weren't labeled with a number followed by a P or F
mislabeled_df<-CleanLeadCopper%>%
  filter(!(TestType=="F"|TestType=="P"))

#This dataframe takes all the likely P measurements by searching for different patterns
#in the location description column, it then converts all of those into P tests
Ps<-mislabeled_df%>%
  filter(grepl(", P|-P|P-|P -| P |FIRST DRAW|1ST DRAW|1ST FL|INITIAL",LocationDescription))%>%
  filter(!grepl("SECOND|2ND DRAW|30 SECOND|FLUSH",LocationDescription))%>%
  mutate(TestType="P")%>%
  rbind(mislabeled_df%>%#this section adds in points that are likely P measurements based on the TestType field
          filter(TestType=="AP"|TestType=="OOP"|TestType=="B-P"|
                 TestType==" P")%>%
          mutate(TestType="P"))

#this dataframe was then examined to confirm that all of the measuements were actually Ps


#This dataframe takes all the likely F measurements by searching for different patterns
#in the location description column, it then converts all of those into F tests
Fs<-mislabeled_df%>%
  filter(grepl(", F|-F|F-|F -| F |SECOND DRAW|2ND DRAW|FLUSH|2 MIN FLUSH|10 MIN FLUSH|30 SEC|30 SEC FLUSH|30 SECOND| MIN",LocationDescription))%>%
  filter(!grepl("FIRST|2ND FLOOR",LocationDescription))%>%
  rbind(mislabeled_df%>%
          filter(grepl("FIRST BAPTIST & 30 SEC",LocationDescription)))%>%
  mutate(TestType="F")%>%
  rbind(mislabeled_df%>%#this section adds in points that are likely F measurements based on the TestType field
          filter(TestType=="AF"|TestType=="B-F"|TestType=="F  SEC"|
                   TestType=="F (NEW)  SEC"|TestType=="-F"|TestType=="F  SECOND"|
                   TestType=="F "|TestType==" F")%>%
          mutate(TestType="F"))

Full<-rbind(Ps,Fs)#dataframe that combines P and F types
mislabeled_df <- anti_join(select(mislabeled_df,-TestType),select(Full,-TestType))#filters out all the points which have been assigned


#morefixed is a dataframe of additional fixed measurements based on inspection of the points that still needed to be labelled
morefixed<-mislabeled_df%>%
  filter(Town=="HOPKINTON")%>%
  filter(grepl("INIT",LocationDescription))%>%
  mutate(TestType="P")%>%
  rbind(mislabeled_df%>%
          filter(Town=="HOPKINTON")%>%
          filter(grepl("FL",LocationDescription))%>%
          mutate(TestType="F"))


#adds to the Full dataframe
Full=rbind(Full,morefixed)%>%
  distinct()
mislabeled_df <- anti_join(mislabeled_df,select(Full,-TestType))

#check creates a dataframe of all the points which were assigned a P and and F measurement
#upon visual inspection all of these were Ps and so they got set to that, filtered down and 
#joined back to the full dataframe
check<-Full%>%
  group_by_at(vars(-TestType)) %>%  
  mutate(num_rows = sum(n())) %>% 
  filter(num_rows > 1)%>%
  mutate(TestType="P")%>%
  distinct()
Full<-Full%>%
  group_by_at(vars(-TestType)) %>%  
  mutate(num_rows = sum(n())) %>% 
  filter(num_rows == 1)
Full=rbind(Full,check)%>%
  select(-num_rows)

#All remaining points were determined to be unknown and labeled with an N
mislabeled_df <- anti_join(mislabeled_df,select(Full,-TestType))%>%
  mutate(TestType="N")
#This is the last iteration of the Full database and it considers all the fixed points and the ones
#which were unable to be corrected
Full<-rbind(Full,mislabeled_df)%>%
  distinct()

#This below code takes the cleaned data and groups tests done at the same location together
#with a P and an F columns for the results of the primary and flush samples
LeadCopperDF<-CleanLeadCopper%>%
  filter(TestType=="F"|TestType=="P")%>%
    rbind(Full)%>%#filtering out all the tests that weren't imputed properly
  select(-LocationCode)%>%# deselecting location code because we made our own and if we don't deselect it the rows won't match
  group_by(across(c(-Result)))%>%#grouping by everything except result (groups are each fixture at each school on the same day, same test type)
  mutate(Result=mean(Result))%>%#taking the average of measurements at locations where there were multiple tests on the same day of the same type
  distinct()%>%#combining rows that are the same into 1 (places where there were multiple of the same tests of the same fixture on the same day)
  spread(TestType,Result)#spreading the results from two rows into one row with a column for each test type

#Adding Location Types----

missingLT<-LeadCopperDF%>%
  filter(is.na(LocationType))%>%
  mutate(LocationType = case_when(
    grepl("\\DW$|^[DW]|WATER FILLING|DRINKING WATER BUBBLER|BUBBLER|BOTTLE FILL|FOUNTAIN",LocationDescription) ~ "DW",
    grepl("\\WC$|^[WC]| WC |W.C.|WATER COOLER",LocationDescription) ~ "WC",
    grepl("\\CF$|^[CF]|CLASSROOM FAUCET|CLASSROOM SINK|CLASSROOM",LocationDescription) ~ "CF",
    grepl("\\KK$|^[KK]|KITCHEN KETTLE",LocationDescription) ~ "KK",
    grepl("\\KC$|^[KC]|KITCHEN FAUCET|KITCHEN SINK|FOOD PREP|SINK KITCHEN|SINK IN KITCHEN|KITCHEN PREP SINK",LocationDescription) ~ "KC",
    grepl("\\KI$|^[KI]|KITCHEN ICE MAKER|ICE MACHINE KITCHEN",LocationDescription) ~ "KI",
    grepl("\\EC$|^[EC]|HOME ECONOMICS",LocationDescription) ~ "EC",
    grepl("\\BF$|^[BF]|BOYS RM|GIRLS RM|RESTROOM|LOCKER RM|BATHROOM| BATHRM| WOMAN'S| MENS|LOCKER ROOM| WOMEN'S| MEN'S",LocationDescription) ~ "BF",
    grepl("\\NS$|^[NS]|NURSE'S|NURSE FAUCET|NURSE PREP| NURSES",LocationDescription) ~ "NS",
    grepl("\\SC$|^[SC]|SERVICE CONNECTOR",LocationDescription) ~ "SC",
    grepl("\\OT$|^[OT]",LocationDescription) ~ "OTHER",
    TRUE~"UNKNOWN"
  ))

LeadCopperDF<-LeadCopperDF%>%
  #filter(!is.na(LocationType))%>%
  #rbind(missingLT)%>%
  mutate(LocationType2=substr(LocationType, start = 1, stop = 2))%>%
  mutate(LocationType2=ifelse(is.na(LocationType2),"NA",LocationType2))

ldf<- filter(LeadCopperDF,is.na(LocationType2))#as.data.frame(table(LeadCopperDF$LocationType2))
addedLT<-filter(missingLT)%>%
  filter(!is.na(LocationType))


#Plots----
ggplot(LeadCopperDF,aes(TestType))+
  geom_bar()

pwsplot<-facilities%>%
  mutate(`Has PWS ID`=(!is.na(`PWS ID`)))%>%
  ggplot(aes(`Has PWS ID`))+
  geom_bar()

testlocplot<-LeadCopperDF%>%
  ggplot(aes(LocationType2))+
  geom_bar()

  




