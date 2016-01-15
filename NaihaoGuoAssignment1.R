###Econ294 R Lab### 

#0#
firstname<-"Naihao"
lastname<-"Guo"
print(paste(firstname,lastname))
studentid<-1505045
print(studentid)

#1#
library(foreign)
df.dta<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv<-read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td<-read.table(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.RData<-load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
print(df.RData)
#"NHIS_2007_RData" is the assigned name to the RData file.

#2#
download.file("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta",
              "~/294A2/Naihao-s-Assignment-1/NHIS_2007_dta.dta") #18KB
download.file("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv",
              "~/294A2/Naihao-s-Assignment-1/NHIS_2007_csv.csv") #138KB
download.file("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
              "~/294A2/Naihao-s-Assignment-1/NHIS_2007_TSV.txt") #138KB
download.file("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData",
              "~/294A2/Naihao-s-Assignment-1/NHIS_2007_RData.RData") #45KB
print("By comparing the size of each file, we can find out that RData is smallest.")

#3#
typeof(NHIS_2007_RData)
print("type of data is list")
length(NHIS_2007_RData)
print("length of data is 9")
dim(NHIS_2007_RData)
print("dim of data is 4758 9")
nrow(NHIS_2007_RData)
print("number of row is 4758")
ncol(NHIS_2007_RData)
print("number of column is 9")
summary(NHIS_2007_RData)

#4#
df.org<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(df.org)
summary(df.org$rw)
print("1119754 observations and 30 variables in the dataset")
print("min=1.8, mean=19.8; median=15.9; max=354.8; 1stQ=10.7; 3rdQis=24.4")
print("NA's=521279")

#5#
v<-c(1,2,3,4,5,6,7,4,NULL,NA)
paste(length(v))
# the number of value do not match the reported number is because R has ingored Null value.
print(mean(v,na.rm=TRUE))

#6#
x<-matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=TRUE)
print(t(x))
eig<-eigen(x)
print(eig$values)
print(eig$vectors)
y<-matrix(c(1,2,3,3,2,1,2,3,0),nrow=3,ncol=3,byrow=TRUE)
inverse<-solve(y)
print(inverse)
print(y%*%inverse)
#From linear algebra, this new matrix should be called as identity matrix

#7#
### What is the mean price?
carat<-c(5,2,0.5,1.5,5,NA,3)
cut<-c("fair","good","very good","good","fair","ideal","fair")
clarity<-c("SI1","I1","VI1","VS1","IF","VVS2",NA)
price<-c(850,450,450,NA,750,980,420)
diam<-data.frame(carat,cut,clarity,price,check.rows=FALSE)
m<-mean(diam$price,na.rm=TRUE)
print(m)
# mean price = 650.

### What is the mean price of cut "fair"?
cut1<-subset(diam,cut=="fair")
mcs1<-mean(cut1$price)
print(mcs1)
# mean price of cut "fair" = 673.333

### What is the mean price of cut "good", "very good", and "ideal"?
cut2<-subset(diam,cut!="fair")
mcs2<-mean(cut2$price,na.rm=TRUE)
print(mcs2)
# The mean price of cut "good","very good",and "Ideal" is 626.6667

### Midian price for diamonds with greater than 2 carats and cut "ideal" or "very good")? 
cut3<-subset(diam,carat>=2&cut=="very good"|cut=="ideal")
print(cut3)
# According to result,the number of carat is "NA", which means we are not able to find the median price.