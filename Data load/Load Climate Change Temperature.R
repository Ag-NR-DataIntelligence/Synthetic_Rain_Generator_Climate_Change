# Climate change monthly Temperature data -------------------
library(readxl)
file="https://www.dropbox.com/s/ytagatrndira7in/Philadelphia%20Temperature.csv?dl=1"
PHL_MonthT <- read_csv(file) %>% mutate(Loc="PHL")
file="https://www.dropbox.com/s/slcv2huiovx8ary/BOS%20%20Temperature.csv?dl=1"
BOS_MonthT <- read_csv(file) %>% mutate(Loc="BOS")
file="https://www.dropbox.com/s/q5izlalq7lag7hh/NYC%20%20Temperature.csv?dl=1"
NYC_MonthT <- read_csv(file) %>% mutate(Loc="NYC")
Raw_dt %>% 
  group_by(Loc,Yr=year(Time),Mon=month(Time))  %>% 
  summarise(MonT=mean(Temp,na.rm=T)) %>% 
  filter(!is.na(MonT)) %>% 
  ungroup %>% 
  mutate(Date=ymd(glue("{Yr}-","{Mon}","-01")),
         Emission=NA,
         Model="History") %>% 
  select(Date,Emission,Loc,Model,MonT)->His_MonthT

MonthT_all=bind_rows(PHL_MonthT,BOS_MonthT,NYC_MonthT) %>% 
  mutate(Date=ymd(Date)) %>% 
  gather(.,Model,MonT,-Date,-Emission,-Loc) %>% 
  bind_rows(His_MonthT)

rm(BOS_MonthT)
rm(PHL_MonthT)
rm(NYC_MonthT)
rm(His_MonthT)