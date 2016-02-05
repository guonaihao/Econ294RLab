#'Econ294 R Lab
# assignment #3
# Naihao Guo
# Winter 2016
# https://github.com/guonaihao/Econ294RLab

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 0
NaihaoGuoAssignment3<-list(
  firstName = "Naiahao",
  lastName  = "Guo",
  email     = "naguo@ucsc.edu",
  studentID = 1505045
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 1
library(foreign)
df.ex<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
class(df.ex)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 2
install.packages("dplyr")
require(dplyr)
df.ex.2<- df.ex %>%
  dplyr::filter(
    year==2013 & month==12)
print(nrow(df.ex.2))
df.ex.2<-df.ex %>%
  dplyr::filter(
    year==2013 & (month==7|month==8|month==9))
print(nrow(df.ex.2))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 3
df.ex.3a<-df.ex %>%
  dplyr::arrange(
    year,month)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 4
df.ex.4a<-df.ex %>%
  dplyr::select(
    year:age)
df.ex.4b<-df.ex %>%
  dplyr::select(
    year,month,starts_with("i"))
unique(select(df.ex,state))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 5
stndz<-function(x){
  (x - mean(x,na.rm=T))/sd(x,na.rm=T)
}
nrmlz<-function(x){
  (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
}
df.ex.5a<-df.ex %>%
  mutate(
    rw.stndz=stndz(df.ex$rw),
    rw_nrmlz=nrmlz(df.ex$rw))
date<-group_by(df.ex.5a,year,month)
df.ex.5b<-mutate( 
  summarise(date,count=n())
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 6
G<-group_by(df.ex,year,month,state)
df.ex.6<-summarise(G,
   min=min(rw,na.rm=T),
   firstQ=quantile(rw,0.25,na.rm=T),
   mean=mean(rw,na.rm=T),
   thirdQ=quantile(rw,0.75,na.rm=T),
   max=max(rw,na.rm=T),
   total=n())
