#'Econ294 R Lab
# assignment #5
# Naihao Guo
# Winter 2016
# https://github.com/guonaihao/Econ294RLab

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 0
NaihaoGuoAssignment5<-list(
  firstName = "Naiahao",
  lastName  = "Guo",
  email     = "naguo@ucsc.edu",
  studentID = 1505045
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 1
#Part A
library(foreign)
library(ggplot2)
Q1a <- ggplot(diamonds, aes(x=x*y*z,y=price))
Q1a + geom_point(aes(colour=clarity, size=carat))+scale_x_log10()+scale_y_log10()

#Part B
Q1b <- ggplot(diamonds, aes(x=carat, fill=clarity,..density..))
Q1b + geom_histogram()+facet_grid(cut~.)
levels(diamonds$cut)

#Part C
Q1c <- ggplot(diamonds, aes(x=cut,y=price))
Q1c + geom_violin()+geom_jitter(alpha=0.02)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 3
#Part A
library(foreign)
library(ggplot2)
d<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
d1<-d %>% group_by(year,month) %>%
    summarise(
        Median.RW=median(rw,na.rm=T),
        q.1 = quantile(rw,0.25,na.rm=T),
        q.3= quantile(rw,0.75,na.rm=T),
        d.one = quantile(rw,0.1,type=5,na.rm=T),
        d.nine = quantile(rw,0.9,type=5,na.rm=T)
        ) %>%
  mutate(date = paste(year,month,"01",sep = "-"),
         date = as.Date(date, format = "%Y-%m-%d"))
Q3a<-ggplot(d1,aes(x=date,y=Median.RW))
Q3a+geom_line()+geom_ribbon(aes(ymin=d.one, ymax=d.nine),alpha=0.2
              )+geom_ribbon(aes(ymin=q.1, ymax=9.3),alpha=0.6)
#Part B
d2<- d %>% group_by(year,month,educ) %>% 
  summarize(Median.RW=median(rw,na.rm=T)) %>%
  mutate(date = paste(year, month, "01", sep = "-"
        ),date = as.Date(date, format = "%Y-%m-%d"))
q3b<-ggplot(d2,aes(x=date,y=Median.RW,group=educ))
q3b+geom_line(aes(colour=educ))

