#'Econ294 R Lab
# assignment 2 example script.
# Naihao Guo
# Winter 2016
# https://github.com/

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 0
NaihaoGuoAssignment2<-list(
  firstName = "Naiahao",
  lastName  = "Guo",
  email     = "naguo@ucsc.edu",
  studentID = 1505045
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 1
library(foreign)
install.packages("RCurl")
require(RCurl) 
diamondsURL<-getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
diamonds<-read.csv(text = diamondsURL)
rm(diamondsURL)
NaihaoGuoAssignment2$s1a<-nrow(diamonds)  # 7 observations
NaihaoGuoAssignment2$s1b<-ncol(diamonds)  # 4 columns
NaihaoGuoAssignment2$s1c<-names(diamonds) # header names are "carat","cut","clarity" and "price"
NaihaoGuoAssignment2$s1d<-summary(diamonds$price)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 2
df.td<-read.table(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",sep="\t",header=T)
NaihaoGuoAssignment2$s2a<-nrow(df.td)  # 4785 observations
NaihaoGuoAssignment2$s2b<-ncol(df.td)  # 9 columns
NaihaoGuoAssignment2$s2c<-names(df.td) # HHX,FMX,FPX,SEX,BMI,SLEEP,educ,height,weight
NaihaoGuoAssignment2$s2d<-mean(df.td$weight)   # mean of weight = 266
NaihaoGuoAssignment2$s2e<-median(df.td$weight) # median of weight = 175
hist(df.td$weight)
df.td$weight<-ifelse(df.td$weight>=996,NA,df.td$weight)
NaihaoGuoAssignment2$s2f<-mean(df.td$weight,na.rm=T)  # new mean = 155
NaihaoGuoAssignment2$s2g<-median(df.td$weight,na.rm=T)# new median = 164
men<-subset(df.td,SEX==1)
women<-subset(df.td,SEX==2)
NaihaoGuoAssignment2$s2h<-summary(women)
NaihaoGuoAssignment2$s2i<-summary(men)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 3
vec<-c(letters,LETTERS)
NaihaoGuoAssignment2$s3a<-vec[c(1:52)%%2==0]
which(vec=="W")
which(vec=="e")
which(vec=="i")
NaihaoGuoAssignment2$s3b<-paste(vec[c(49,5,9)],collapse="")
arr<-array(c(letters,LETTERS),dim = c(3,3,3))
NaihaoGuoAssignment2$s3c<-arr[,1,2]
NaihaoGuoAssignment2$s3d<-arr[2,,]
NaihaoGuoAssignment2$s3e<-paste(arr[2,2,3],arr[2,2,1],arr[3,3,1],sep="")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 4
org_example<-read.dta(file="http://people.ucsc.edu/~aspearot/Econ_217_Data/org_example.dta")
df<-subset(org_example,select=c(year,month,educ,rw))
NaihaoGuoAssignment2$s4<-aggregate(df$rw,list(year=df$year,month=df$month,educ=df$educ),mean,na.rm=T)

