library(tidyverse)
library(lubridate)
library(magrittr)
library(RcppRoll)
library(glue)
remotes::install_github("tylermorganwall/rayshader")
library(rayshader)


source("https://raw.githubusercontent.com/Ag-NR-DataIntelligence/Synthetic_Rain_Generator_Climate_Change/master/Data%20load/Load%20Raw%20Data.R")


ReadSynPressPerd=function(fileNM) 
{
  read_rds(paste0(path,fileNM))[[2]] %>%
    mutate(runID=substr(fileNM,9,nchar(fileNM)-4)) %>% 
    select(SyncTime,Precip,Loc,Press_Evt_lab,Press_Evt,MonT) %>% 
    rename(Time=SyncTime,SynMonT=MonT,NewPress_perd=Press_Evt,HisPressEvt_lab=Press_Evt_lab) %>% 
    arrange(Time) %>% 
    Precip_Evt_Sep(IntE_P = 4) %>% 
    #Get new pressure events
    group_by(NewPress_perd) %>% 
    summarise(Sum_Precip=sum(Precip),
              StTime=min(Time),
              St_Rain=sum(StR),
              St_Dry=sum(StD),
              Loc=min(Loc),
              HisPressEvt_lab=as.numeric(min(HisPressEvt_lab)),
              SynMonT=min(SynMonT)) %>%
    left_join(Raw_dt_Evt %>% select(Loc,Press_Evt_lab,Sum_Press_Delta,Dur),by=c("Loc"="Loc","HisPressEvt_lab"="Press_Evt_lab")) %>% 
    mutate(Yr=year(StTime),
           Mon=month(StTime)) %>% 
    mutate(Season = as.character(cut(
      month(StTime),
      c(2, 5, 8, 11),
      c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>% 
    mutate(runid=n)
}




path="/home/ziwen.yu/ondemand/data/HisValidate/"
HisValid_PCEs=map_dfr(list.files(path),ReadSynPressPerd)

path="/home/ziwen.yu/ondemand/data/Synthetic/"
Sync_PCEs=map_dfr(list.files(path),ReadSynPressPerd) 


