setwd("C:/Users/vinee/OneDrive/Desktop/MScA/DataMining/Final Project")

data <- read.csv("mental-heath-in-tech-v1.csv", header=T, na.strings=c("","NA"))

head(data)

colnames(data)[colnames(data)=="What.is.your.gender."] <- "Gender"

install_load <- function (packages)  {   
  
  # Start loop to determine if each package is installed
  for(package in packages){
    
    # If package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}
libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
install_load(libs)

libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)

Clean_Data<-data
#Clean_Data<- Clean_Data[ , !(names(Clean_Data) %in% "state")]
Clean_Data <-Clean_Data[ , !(names(Clean_Data) %in% "Timestamp")]
Clean_Data <-Clean_Data[, !(names(Clean_Data) %in% "comments")]
Clean_Data

colnames(survey_data)
colnames(Clean_Data)


# Gender column:

Clean_Data$Gender%<>%str_to_lower()

#male_str <- c('Male', 'male', 'Male ', 'M','m','man',"I'm a man why didn't you make this a drop down question. You should of asked sex? And I would of answered yes please. Seriously how much text can this take? ",'Cis male', 'Male.','male 9:1 female,roughly','Male (cis)','nb masculine', 'Man','Sex is male','cis male','Genderfluid','Dude','mail', 'M|', 'Male/genderqueer','male ','Cis Male','Male (trans, FtM)','cisdude','cis man','MALE')

#human_str <- c('Bigender', 'non-binary','Transitioned',' M2F','Other/Transfeminine','Androgynous','nan','Other','none of your business','genderqueer','Human','Enby','Malr','genderqueer woman','mtf','Queer','Agender','Fluid','Nonbinary','human','Unicorn','Genderqueer','no feelings about gender','AFAB','Transgender woman' )

#female_str <- c('Female', 'female','I identify as female.','female ','Female assigned at birth ', 'F','Woman','fm', 'f','Cis female ','Genderfluid (born female)','Female or Multi-Gender Femme', 'Female ','woman', 'female/woman','Cisgender Female', 'fem','Female (props for making this a freeform field, though)','Female','Cis-woman','Genderflux demi-girl','female-bodied')


male_str <- c( "male.", "male (cis)",  "sex is male", "dude",  "i'm a man why didn't you make this a drop down question. you should of asked sex? and i would of answered yes please. seriously how much text can this take? ", "m|")

human_str <- c("i identify as female.", "bigender", "human", "transitioned, m2f", "genderfluid (born female)", "other/transfeminine", "female or multi-gender femme","androgynous", "male 9:1 female, roughly", "n/a", "other","genderfluid", "enby", "malr", "queer", "fluid","agender", "male/genderqueer", "nonbinary", "unicorn",  "male (trans, ftm)", "genderflux demi-girl", "female-bodied; no feelings about gender", "NA", "afab", "transgender woman" )

female_str <- c("female assigned at birth ", "cis female ", "cisgender female","female (props for making this a freeform field, though)", " female", "cis-woman")

Clean_Data$Gender <- sapply(as.vector(Clean_Data$Gender), function(x) if(x %in% male_str) "male" else x )
Clean_Data$Gender <- sapply(as.vector(Clean_Data$Gender), function(x) if(x %in% female_str) "female" else x )
Clean_Data$Gender <- sapply(as.vector(Clean_Data$Gender), function(x) if(x %in% human_str) "human" else x )

unique(Clean_Data$Gender)
#rs <- paste("\"",as.character(v),"\"",collapse=", ",sep="")
#cat(rs)

#v1
#typeof(v)
# State column:

#Clean_Data$state <- sapply(as.vector(Clean_Data$state), function(x) if(x %in% NA) "NA" else x )
#unique(Clean_Data$state)

# work interface:

#Clean_Data$work_interfere <- sapply(as.vector(Clean_Data$work_interfere), function(x) if(x %in% NA) "NA" else x )
#unique(Clean_Data$work_interfere)

#self employed:

#Clean_Data$self_employed <- sapply(as.vector(Clean_Data$self_employed), function(x) if(x %in% NA) "NA" else x )
#unique(Clean_Data$self_employed)

# Age:
#Age: <=25: 1
# >25 and <=32:2
# >32 and <=39:3
# >39 then :4

options(scipen=999)
Clean_Data$Age<- as.numeric(Clean_Data$Age)
Clean_Data%<>% filter(Age > 0) 

unique(Clean_Data$Age)
class(Clean_Data$Age)
typeof(Clean_Data$Age)

Clean_Data$Age_cat<-ifelse(Clean_Data$Age <=25 ,"1",ifelse( Clean_Data$Age <=32, "2",ifelse(Clean_Data$Age <=39, "3","4")))

unique(Clean_Data$Age_cat)

#sqldf("select distinct Age,Age_cat from Clean_Data")
Clean_Data$No_Employees
# no of employees:
unique(Clean_Data$No_Employees)
#1: more than 1000
#2: less than 1000
Clean_Data$No_Employees <- ifelse(Clean_Data$No_Employees == "More than 1000", "1", "2") 

Clean_Data_backup <- Clean_Data

Clean_Data$No_Employees<-ifelse(Clean_Data$No_Employees %in% c("25-Jun", "26-100","5-Jan","100-500") ,"1",ifelse( Clean_Data$No_Employees == "500-1000", "2",ifelse(Clean_Data$No_Employees == "More than 1000", "3")))




Clean_Data$No_Employees<- car::recode(Clean_Data$No_Employees,"'25-Jun'='1';'26-100'='1';'5-Jan'='1';'100-500'='1';'500-1000'='2';'More than 1000'='3'")

unique(Clean_Data$No_Employees)

#changing all columns to factors:
colnames(Clean_Data)

is.na(Clean_Data) <- Clean_Data == "NA"

nms <- c("Age","Gender","Country","state","self_employed","family_history","treatment","work_interfere","No_Employees","remote_work","tech_company","benefits","care_options","wellness_program","seek_help","anonymity","leave","mental_health_consequence","phys_health_consequence","coworkers","supervisor","mental_health_interview","phys_health_interview","mental_vs_physical","obs_consequence","Age_cat") 
nms <- colnames(Clean_Data)
Clean_Data[nms] <- lapply(Clean_Data[nms], as.factor) 

str(Clean_Data)

write.csv(Clean_Data,file='Clean_Data.csv')


