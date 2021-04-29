
source("https://raw.githubusercontent.com/Ag-NR-DataIntelligence/Synthetic_Rain_Generator_Climate_Change/master/Data%20load/Load%20Raw%20Data.R")
source("https://raw.githubusercontent.com/Ag-NR-DataIntelligence/Synthetic_Rain_Generator_Climate_Change/master/Data%20load/Load%20Climate%20Change%20Temperature.R")
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/Non_stationaryGenerator/Non_StationaryInterMFunctions.R')
source('https://raw.githubusercontent.com/Ag-NR-DataIntelligence/Synthetic_Rain_Generator_Climate_Change/master/Algorithm/Generator%20Functions.R')


MonthT=MonthT_all %>% filter(Loc=="NYC",Emission=="A2")

library(glue)
library(magrittr)
# GCM models to simulate "MIROC"
set.seed(11)
for (j in 1:1)
{
  SyncPrecip=SyncP_Generate(GCM = 'MIROC') 
  SyncPrecip[[1]] %>% mutate(Runid=j)
  SyncPrecip[[2]] %>% mutate(Runid=j)
  
  write_rds(SyncPrecip,path=glue("/home/ziwen.yu/ondemand/data/Synthetic/SynDt{j}.rds"),compress="gz")
  Sys.sleep(0.1)
  print(glue("Syn-{j}"))
}


MonthT=MonthT_all %>% filter(Loc=="NYC",is.na(Emission))

library(glue)
library(magrittr)
# GCM models to simulate "MIROC"
set.seed(11)
for (j in 1:100)
{
  SyncPrecip=SyncP_Generate(GCM = 'History',StTime=ymd("1970-01-01"),FinalYear=ymd("2011-01-01")) 
  SyncPrecip[[1]] %>% mutate(Runid=j)
  SyncPrecip[[2]] %>% mutate(Runid=j)
  
  write_rds(SyncPrecip,path=glue("/home/ziwen.yu/ondemand/data/HisValidate/SynHisDt{j}.rds"),compress="gz")
  Sys.sleep(0.1)
  print(glue("His-{j}"))
}