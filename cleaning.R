library(shiny)
library(tidyverse)
library(readxl)
library(plyr)
library(scales)
library(dplyr)
library(tm)
library(wordcloud)
library(memoise)
library(sjmisc)
library(shinydashboard)
library(DT)
library(formatR)


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}



#new files 

FirstDestinations <- read_xlsx("C:\\Users\\matth\\Downloads\\2019 First Destination Survey Info with Student IDs - For MZ (1).xlsx", sheet = 2)

FirstDestinations2 <- read_xlsx("C:\\Users\\matth\\Downloads\\2019 First Destination Survey Info with Student IDs - For MZ (1).xlsx", sheet = 3)

FirstDestinations3 <- read_xlsx("C:\\Users\\matth\\Downloads\\2019 First Destination Survey Info with Student IDs - For MZ (1).xlsx", sheet = 4)

FirstDestinations4 <- read_xlsx("C:\\Users\\matth\\Downloads\\2019 First Destination Survey Info with Student IDs - For MZ (1).xlsx", sheet = 5)

FirstDestinations5 <- read_xlsx("C:\\Users\\matth\\Downloads\\2019 First Destination Survey Info with Student IDs - For MZ (1).xlsx", sheet = 6)


FirstDestinations[7:36] <- NULL
FirstDestinations2[7:36] <- NULL
FirstDestinations3[7:36] <- NULL
FirstDestinations4[8:36] <- NULL
FirstDestinations5[2:36] <- NULL



FirstDestinations<-rename(FirstDestinations, c(id="STUDENT ID NUMBER"))




FirstDestinations2<-rename(FirstDestinations2, c(id="STUDENT ID NUMBER"))
FirstDestinations3<-rename(FirstDestinations3, c(id="STUDENT ID NUMBER"))
FirstDestinations4<-rename(FirstDestinations4, c(id="STUDENT ID NUMBER"))
FirstDestinations5<-rename(FirstDestinations5, c(id="STUDENT ID NUMBER"))

FirstDestinations2$Employment_status <- c("Volunteer")
FirstDestinations$Employment_status <- c("Full Time")
FirstDestinations3$Employment_status <- c("Part Time")
FirstDestinations4$Employment_status <- c("In School")
FirstDestinations5$Employment_status <- c("Seeking")


FD_1_2<-rbind(FirstDestinations, FirstDestinations2)

FD_1_2_3 <- rbind(FD_1_2, FirstDestinations3)


colnames(FirstDestinations4)[4] <- c("ORGANIZATION")
colnames(FirstDestinations4)[2] <- c("POSITION TITLE")
FirstDestinations4$CONCENTRATION <- NULL

FD_1_2_3_4 <- rbind(FD_1_2_3, FirstDestinations4)



colnames<-colnames(FirstDestinations)


colnames(FirstDestinations5) <- colnames(FirstDestinations)

FirstDestinations5$ORGANIZATION <- NA
FirstDestinations5$"POSITION TITLE" <- NA
FirstDestinations5$CITY <- NA
FirstDestinations5$STATE <- NA
FirstDestinations5$COUNTRY <- NA
FirstDestinations5$Employment_status <- c("Seeking")



FD_1_2_3_4_5 <- rbind(FD_1_2_3_4, FirstDestinations5)

##data is a little wack

FD<-FD_1_2_3_4_5

indexRemove <- which(is.na(FD$id))

FD <- FD[-indexRemove,]


####################



fin_aid_data <- read_xlsx("C:\\Users\\matth\\Downloads\\Financial Aid Data.xlsx", sheet = 9)

fin_aid_data_2019 <- fin_aid_data %>% filter(`Graduation Date` == as.POSIXct("2019-06-15", tz = "UCT"))

fin_aid_data_2019$`Level of Financial Need` <- as.numeric(fin_aid_data_2019$`Level of Financial Need`)

fin_aid_data_2019[is.na(fin_aid_data_2019)] <- 0 

crossOvers <- fin_aid_data_2019[fin_aid_data_2019$`Student ID` %in% FD$id,]

sum(FD$id %in% crossOvers$`Student ID`)



#old files 

counseling <- read_excel_allsheets("C:\\Old matt stuff\\Matthew Zacharski\\Desktop\\rj project\\REPORT 1_Counseling.xlsx")



info_session<- read_excel_allsheets("C:\\Old matt stuff\\Matthew Zacharski\\Desktop\\rj project\\REPORT 2_InformationSessions.xls"
)



apps <- read_excel_allsheets("C:\\Old matt stuff\\Matthew Zacharski\\Desktop\\rj project\\REPORT 3_Job - Applications & Interviews_OCR and Non-OCR (1).xlsx"
)

apps$`2018-19`$`Location: Label` <- NULL

apps_2019 <- as.data.frame(apps[1])


programs <- read_excel_allsheets("C:\\Old matt stuff\\Matthew Zacharski\\Desktop\\rj project\\REPORT 4_Program Participation (EXCLUDES Pam).xlsx"
)


unique_programs_2019 <- as.data.frame(programs[c(2)])




#tidying up the programs data 
programs <- read_excel_allsheets("C:\\Old matt stuff\\Matthew Zacharski\\Desktop\\rj project\\REPORT 4_Program Participation (EXCLUDES Pam).xlsx"
)

programs_class_of_2019 <- programs[c(2,4,6,8)]
programs_class_of_2019<- ldply(programs_class_of_2019, data.frame)
programs_class_of_2019$Student.Profile..Graduation.Date <-as.Date(programs_class_of_2019$Student.Profile..Graduation.Date)
programs_class_of_2019 <- programs_class_of_2019 %>% filter(Student.Profile..Graduation.Date == "2019-06-15")
programs_class_of_2019$student..Level.of.Financial.Need <- as.numeric(programs_class_of_2019$student..Level.of.Financial.Need )
programs_class_of_2019[is.na(programs_class_of_2019)] <- 0
programs_class_of_2019<-rename(programs_class_of_2019, c('First Generation Status' ="Student.Profile..First.Generation"))


#tidying up the apps data
apps_class_of_2019<- ldply(apps, data.frame)
apps_class_of_2019$Student.Account..Graduation.Date <-as.Date(apps_class_of_2019$Student.Account..Graduation.Date)
apps_class_of_2019 <- apps_class_of_2019 %>% filter(Student.Account..Graduation.Date == "2019-06-15")
apps_class_of_2019$student..Level.of.Financial.Need <- as.numeric(apps_class_of_2019$student..Level.of.Financial.Need )
apps_class_of_2019$Student.Account..Graduation.Date <-NULL
apps_class_of_2019$.id <- NULL
apps_class_of_2019$OCR.Application.Interview..Interview.Time <- NULL
apps_class_of_2019$OCR.Application.Interview..Submitted.On <- NULL
apps_class_of_2019[is.na(apps_class_of_2019)] <- 0
apps_class_of_2019<-rename(apps_class_of_2019, c('First Generation Status' ="Student.Profile..First.Generation"))



#tidying up the counseling data...
counseling_class_of_2019<-counseling[c(2,4,6,8,10,12)]
counseling_class_of_2019<- ldply(counseling_class_of_2019, data.frame)
counseling_class_of_2019$Student.Profile..Graduation.Date <-as.Date(counseling_class_of_2019$Student.Profile..Graduation.Date)
counseling_class_of_2019 <- counseling_class_of_2019 %>% filter(Student.Profile..Graduation.Date == "2019-06-15")
counseling_class_of_2019$Student..Level.of.Financial.Need <- as.numeric(counseling_class_of_2019$Student..Level.of.Financial.Need )
counseling_class_of_2019[is.na(counseling_class_of_2019)] <- 0
counseling_class_of_2019<-rename(counseling_class_of_2019, c('First Generation Status' ="Student.Profile..First.Generation"))


#tidying up info session
info_session<-read_excel_allsheets("C:\\Old matt stuff\\Matthew Zacharski\\Desktop\\rj project\\REPORT 2_InformationSessions.xls")
info_session<-info_session[c(2,4,6,8,10,12)]
info_session<- ldply(info_session, data.frame)
info_session$Student.Profile..Graduation.Date <-as.Date(info_session$Student.Profile..Graduation.Date)
info_session <- info_session %>% filter(Student.Profile..Graduation.Date == "2019-06-15")
info_session$student..Level.of.Financial.Need <- as.numeric(info_session$student..Level.of.Financial.Need )
info_session[is.na(info_session)] <- 0





##FD build out stuff, build out w/ counseling data...

subset <-subset(counseling_class_of_2019, Student..Student.ID %in% FD$id)

attributes_grab<-subset[,c(11:19)]

attributes_grab$id <- attributes_grab$Student..Student.ID

fd_w_all <- merge(attributes_grab, FD, by.y = "id", by.x = "Student..Student.ID" )

fd_w_all <- fd_w_all[,-c(3,7)]

unique_val<-unique(fd_w_all[,1])




#should be like 285...,, found 285 matches from the counseling data in the fd data, could be more in other datasets


index_285<- match(unique(fd_w_all$id),fd_w_all$id)


fd_w_all <- fd_w_all[index_285,]

# ##renaming, may break code???
# fd_w_all <- rename(fd_w_all, c(Gender = "Student..Gender"))
# fd_w_all <- rename(fd_w_all, c(FinAid = "Student..Level.of.Financial.Need"))
# fd_w_all <- rename(fd_w_all, c(Major = "Student.Profile..Major"))
# fd_w_all <- rename(fd_w_all, c(Race/Ethnicity = "Student.Profile..Race.Ethnicity"))
# fd_w_all <- rename(fd_w_all, c(EmploymentStatus = "Employment_status"))
# fd_w_all <- fd_w_all[,c(-1)]




#finding some stragglers that could not be build out in the first FD build out
notInFDAll <- FD[!(FD$id %in% fd_w_all$id),]


tenMore <- subset(apps_class_of_2019, apps_class_of_2019$student..Student.ID %in% notInFDAll$id)

#no notInFDAll data in programs...

tenMore$student..Student.ID


fourMore<-subset(info_session, info_session$student..Student.ID %in% notInFDAll$id)



tenMore$id <- tenMore$student..Student.ID

#index the ten more ids in the FD data, put them in the fd_w_all data...trying to fix the shitshow


fd_w_all3 <- merge(tenMore, fd_w_all, by.y = "id", by.x = "student..Student.ID", all = TRUE )

indexF <- match(unique(fd_w_all3$student..Student.ID), fd_w_all3$student..Student.ID)

#finally unique
fd_w_all3 <-fd_w_all3[indexF,]

##address 5 extra found in the info session data
fd_w_all4 <- merge(fourMore, fd_w_all3, by = 'student..Student.ID', all = TRUE )

#making it unique one last time
indexG <- match(unique(fd_w_all4$student..Student.ID), fd_w_all4$student..Student.ID)

#finally unique and done!
fd_w_all4 <-fd_w_all3[indexG,]

####cleaning up some columns 
fd_clean <- fd_w_all4[,-c(2:10)]
fd_clean <- fd_clean[-c(292:294),]




fd_clean[missingValueIndex,]



missingValueIndex <- which(is.na(fd_clean$Student..Gender))

##using missing ids to create a dataset of the missing values 


a<-subset(FD_1_2_3_4_5, FD_1_2_3_4_5$id %in% fd_clean[missingValueIndex,]$student..Student.ID)
b<-subset(tenMore, tenMore$student..Student.ID %in% fd_clean[missingValueIndex,]$student..Student.ID)
indexH <- match(unique(b$student..Student.ID), b$student..Student.ID)
b <- b[indexH,]

missingValueData<-cbind(b,a)

missingValueData <- missingValueData[, -c(1:2,7,10,11)]


fd_clean <- rename(fd_clean, c("First Generation Status" = "First Generation Status.y"))



##do all renaming here...

missingValueData <- rename(missingValueData, c(Gender = "student..Gender"))
missingValueData <- rename(missingValueData, c(FinAid = "student..Level.of.Financial.Need"))
missingValueData <- rename(missingValueData, c(Major = "Student.Account..Major"))
missingValueData <- rename(missingValueData, c(RaceEthnicity = "Student.Account..Race.Ethnicity"))
missingValueData <- rename(missingValueData, c(EmploymentStatus = "Employment_status"))
missingValueData <- rename(missingValueData, c(Id = "student..Student.ID"))
missingValueData<-rename(missingValueData, c(City = "CITY"))
missingValueData<-rename(missingValueData, c(State = "STATE"))
missingValueData<-rename(missingValueData, c(Country = "COUNTRY"))
missingValueData<-rename(missingValueData, c(Position = "POSITION TITLE"))
missingValueData<-rename(missingValueData, c(Organization = "ORGANIZATION"))
missingValueData<-rename(missingValueData, c(FirstGenStatus = 'Student.Account..First.Generation'))




fd_clean<-rename(fd_clean, c(Id = "student..Student.ID"))
fd_clean<-rename(fd_clean, c(City = "CITY"))
fd_clean<-rename(fd_clean, c(State = "STATE"))
fd_clean<-rename(fd_clean, c(Country = "COUNTRY"))
fd_clean<-rename(fd_clean, c(Position = "POSITION TITLE"))
fd_clean<-rename(fd_clean, c(Organization = "ORGANIZATION"))
fd_clean<-rename(fd_clean, c(RaceEthnicity = 'Student.Profile..Race.Ethnicity'))
fd_clean<-rename(fd_clean, c(FirstGenStatus = 'First Generation Status'))
fd_clean<-rename(fd_clean, c(Gender = 'Student..Gender'))
fd_clean<-rename(fd_clean, c(FinAid = 'Student..Level.of.Financial.Need'))
fd_clean<-rename(fd_clean, c(EmploymentStatus = 'Employment_status'))
fd_clean<-rename(fd_clean, c(Major = 'Student.Profile..Major'))
fd_clean <- fd_clean[,-c(2,5)]





fd_clean <- rbind(missingValueData,fd_clean)

NaIndex <- which(is.na(fd_clean$Gender))

fd_clean <- fd_clean[-NaIndex,]

####okay all should work now??? --walking dog now...



uh<-table(apps_class_of_2019$student..Student.ID)

uh2<-as.data.frame(uh)
Freq<-uh2$Freq
#decending order...
uh2<-uh2[order(-Freq),]


#counseling
huh<-table(counseling_class_of_2019$Student..Student.ID)

huh2<-as.data.frame(huh)
Freq<-huh2$Freq
#decending order...
huh2<-huh2[order(-Freq),]


#info sessions
aaa<-table(info_session$student..Student.ID)

aaa2<-as.data.frame(aaa)
Freq<-aaa2$Freq
#decending order...
aaa2<-aaa2[order(-Freq),]




#programs
bbb<-table(programs_class_of_2019$student..Student.ID)

bbb2<-as.data.frame(bbb)
Freq<-bbb2$Freq
#decending order...
bbb2<-bbb2[order(-Freq),]





############################
uh2<-rename(uh2, c(ApplicationCount = "Freq"))
huh2<-rename(huh2, c(CareerCounselingVisits = "Freq"))
aaa2<-rename(aaa2, c(InfoSessionCounts = "Freq"))
bbb2<-rename(bbb2, c(ProgramParticipationCounts = "Freq"))


uh_huh<-merge(uh2,huh2, by = "Var1", all = TRUE)
#note, this is just 0 recorded applications...
uh_huh[is.na(uh_huh)] <- 0

#also note, sometimes people go to info sessions but don't write their name down...
uh_huh_aaa<-merge(uh_huh, aaa2, by = "Var1", all = TRUE)
uh_huh_aaa[is.na(uh_huh_aaa)] <- 0

uh_huh_aaa_bbb<-merge(uh_huh_aaa, bbb2, by = "Var1", all = TRUE)
uh_huh_aaa_bbb[is.na(uh_huh_aaa_bbb)] <- 0

##^^use this data for a KNN classifier???!!! v exciting....



indexI <- which(uh_huh_aaa_bbb$Var1 %in% fd_clean$Id)



final_merge <- uh_huh_aaa_bbb[indexI,]



fd_clean <- merge(fd_clean, final_merge, by.x = "Id", by.y = "Var1")

