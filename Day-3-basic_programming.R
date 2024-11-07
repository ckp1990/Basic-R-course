##Day 3

##after revision 


##Conditional statements. 

## if statements

x <- 100
y <- 100

if (y < x) {
  z=x+y
  print("y is less equal to  x")
  print(z)
}


## if else statement

if (y<x){
  print("x is grater then y")
} else {
  print("y is grater than x")
}

## if else if
if(x>y){
  print("x is bigger")
} else if (x==y){
  print("they are equal")
} else {print("y is bigger")}



##nested if statements
x <- 41

if (x > 30) {
  print("it is above 30")
  if (x > 40) {
    print("and also above 40")
  } else {
    print("but not above 40.")
  }
} else {
  print("it is below 30.")
}

## multiple conditions
x<-20L
if(is.integer(x)==TRUE & x>30){
  print("x is integer and less than 30")
}


##########################################################################################################

## loop or iteration 

##While loop

##  as long as condition is true

i=1
z=0
while (i%%38!=0) {
  z=z+i
  print(z)
  i=i+1
}

#for loop
for(i in 1:10){
  print(i)
  
}

## for over vector 

vec_names<-c("chandan","sumit","kate")
for (i in vec_names){
  print(i)
}

sample_number<-sample(1:100,30,replace = T)
sample_number
t=1
for(x in 1:30){
  print(x)
  sample_number[x]<-sample_number[x]+10
  t=t+1
}

sample_number
sum(sample_number)
cumsum(sample_number)
mean(sample_number)
var(sample_number)
sd(sample_number)
log(sample_number,10)
log(sample_number)
exp(sample_number)
sqrt(sample_number)

uni_sample_number<-unique(sample_number)

length(uni_sample_number)

m<-matrix(1:100,25,4)
dim(m)
nrow(m)
ncol(m)
diag(m)
## user defined function 
sample_number

geom_mean <- function(sample_vector) {
  sample_prod <- prod(sample_vector)
  z = sample_prod^(1/length(sample_vector))
  return(z)
}






##area of circle

area_of_circle <- function(radius){
  area<-pi*radius*radius
  return(area)
}
y<-area_of_circle(radius = 6)
x<-area_of_circle(4)

##nested function

#1. call function with function 

area_of_circle(area_of_circle(5))
x<-area_of_circle(5)
x
area_of_circle(x)

#2. write a function within another function 

func1 <- function(x) {
  func <- function(y) {
    a <- x + y
    return(a)
  }
  return (func)
}

output <- func1(3) # To call the Outer_func
output(5)
### Recursion
##find the sum of n integers
summ_fun<-function(n_interger){
  if(n_interger>0){
    flag<-n_interger+summ_fun(n_interger-1)
    return(flag)
  } else {
    flag=0
  }
  return(flag)
}
summ_fun(10)
### handing the tables 

## read table from computer

getwd()
setwd("C:/Users/chand/OneDrive/CWS_CKP/Non-PhD/R_workshop/")
list.files()
form_response<-read.csv("Form_Responses_1.csv",header = T)
names(form_response)
str(form_response)
summary(form_response)
##looking at data
head(form_response)
tail(form_response)
range(form_response$Your.height.in.cm)

## clearly there cannot be person with 5.3 cm 
## data cleanup
form_response<-subset(form_response,form_response$Your.weight..kgs.<200)

boxplot(form_response$Your.weight..kgs.~form_response$Gender)
hist(form_response$Your.height.in.cm,breaks = 20)
##to draw a pie chart
gender<-unique(form_response$Gender)
number_of_each_gender<-c(0,0)
for(n in 1:nrow(form_response)){
  k<-form_response$Gender[n]
  if(k==gender[1]){
    number_of_each_gender[1]<-number_of_each_gender[1]+1
  } else number_of_each_gender[2]<-number_of_each_gender[2]+1
}
pie_gender<-data.frame(gender,number_of_each_gender)
pie(x=pie_gender$number_of_each_gender,labels = pie_gender$gender,
    col = c("red","blue"),main = "Gender pie char")
#advance labeling 
additional_data<-round((number_of_each_gender/(sum(number_of_each_gender)))*100,2)
lab<-paste(gender,additional_data,"%",sep = "-")

pie(x=pie_gender$number_of_each_gender,labels = lab,
    col = c("red","blue"),main = "Gender pie char")

## example of with a package called dplyr
library(dplyr)
new_pie<-group_by(form_response,Gender)%>%
  summarise(count=n())%>%
  as.data.frame()
new_pie<-mutate(new_pie,percentage=round((count)/sum(count),6)*100)
new_pie<-mutate(new_pie,lab=paste(Gender,percentage,"%",sep="-"))
new_pie
pie(x=new_pie$count,labels = new_pie$lab,col = c("red","darkgreen"),
    main="pie chart in easy way")

## scatter plot

plot(x=form_response$Your.height.in.cm,y=form_response$Your.weight..kgs.,
     xlab = "Height (cm)",ylab = "weight (kgs)",
     pch=20,col=as.factor(form_response$Gender))

legend("topright",c("Female","Male"),col = c("black","red"),pch = 20)

## Ploting mean and SD for weight as barplot
bar_plot<-group_by(form_response,Did.you.get.the.meaning.of.VIRUS)%>%
  summarise(weight=mean(Your.weight..kgs.),SD=sd(Your.weight..kgs.),count1=n())
plot<-barplot(bar_plot$weight~bar_plot$Did.you.get.the.meaning.of.VIRUS,ylim=c(0,100),
        xlab = "Did you get the meaning of Virus",ylab = "Weight(kgs)")

# Add error bars
arrows(x0 = plot,                           
       y0 = bar_plot$weight + (bar_plot$SD)/sqrt(bar_plot$count1),
       y1 = bar_plot$weight - (bar_plot$SD)/sqrt(bar_plot$count1),
       angle = 90,
       code = 3,
       length = 0.1)

#pair wise plot
pairs(form_response[,c(10,4,5)],lower.panel = panel.cor,diag.panel = panel.hist,
      upper.panel = panel.smooth)

##there are many way but for example we are using following URL
#install.packages("tidyverse",dependencies = T)
#install.packages("rvest",dependencies = T)
library(tidyverse)
library(rvest)
# Reading in the table from Wikipedia
page = read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_life_expectancy")
# Obtain the piece of the web page that corresponds to the "wikitable" node
my.table = html_node(page, ".wikitable")
# Convert the html table element into a data frame
my.table = html_table(my.table, fill = TRUE)
# Extracting and tidying a single column from the table and adding row names
x = as.numeric(gsub("\\[.*","",my.table[,4]))
names(x) = gsub("\\[.*","",my.table[,2])
# Excluding non-states and averages from the table
life.expectancy = x[!names(x) %in% c("United States", "Northern Mariana Islands", "Guam", "American Samoa", "Puerto Rico", "U.S. Virgin Islands")]
my.table
