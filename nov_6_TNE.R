## Creating the projects
## reading the directory, setting and creating
## navigation in the directory
## reading the table and finding error/ warning using stat overflow. 


getwd()
list.files(pattern = ".csv")

?read.csv

stud_perf <- read.csv(file = "StudentsPerformance.csv",header = T)

summary(stud_perf)

stud_perf$gender <- as.factor(stud_perf$gender)
summary(stud_perf)

unique(stud_perf$race.ethnicity)
stud_perf$race.ethnicity <- as.factor(stud_perf$race.ethnicity)
summary(stud_perf)


colnames(stud_perf)[3]<-"parent_education_level"
summary(stud_perf)



math_avg<-mean(stud_perf$math.score)
rad_avg<- mean(stud_perf$reading.score)


race_avg<-tapply(stud_perf$math.score,stud_perf$race.ethnicity,FUN = mean)
race_sd<-tapply(stud_perf$math.score,stud_perf$race.ethnicity, FUN=sd)


math <- data.frame(math_mean=race_avg,math_sd=race_sd)

?write.csv()
write.csv(math,file="math_mean.csv")
