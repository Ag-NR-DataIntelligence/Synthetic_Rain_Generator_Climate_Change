library(tidyverse)
library(magrittr)
library(lubridate)
library(glue)
library(RcppRoll)
library(glue)

source("https://raw.githubusercontent.com/Ag-NR-DataIntelligence/Synthetic_Rain_Generator_Climate_Change/master/Analysis/Plotting%20Theme.R")
load(url("https://www.dropbox.com/s/ernxah3f4zzfuus/DtforAnalysis.rdata?dl=1"))



Line_cols=c("Validate"="azure3","Historic"='grey',"Synthetic"="black")
Line_size=c("Validate"=3,"Historic"=2,"Synthetic"=1)
Line_type=c("Validate"=3,"Historic"=2,"Synthetic"=1)


bind_rows(HisValid_PCEs %>% 
            mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
                   Sum_Precip=as.numeric(Sum_Precip),
                   HisSync="Validate") %>%
            filter(Sum_Precip>0) %>% 
            select(Sum_Press_Delta,Sum_Precip,HisSync),
          Raw_dt_Evt %>% 
            mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
                   Sum_Precip=as.numeric(Sum_Precip),
                   HisSync="Historic") %>%
            filter(Sum_Precip>0) %>% 
            select(Sum_Press_Delta,Sum_Precip,HisSync))%>% 
  ggplot(aes(x=Sum_Press_Delta,y=Sum_Precip*.254))+
  stat_density_2d(aes(color=HisSync,linetype=HisSync,size=HisSync),bins=5)+
  geom_smooth(aes(linetype=HisSync,size=HisSync),color="black", method = "loess",  se = FALSE)+
  scale_y_log10()+
  scale_color_manual("",values=Line_cols,guide = guide_legend(keywidth = 4))+
  scale_size_manual("",values=Line_size,guide = guide_legend(keywidth = 4))+
  scale_linetype_manual("",values=Line_type,guide = guide_legend(keywidth = 4))+
  ylab('Precipitation Depth (PD) (mm) in log scale')+
  xlab('Event Pressure Change (EPC) (hPa)')+
  Plot_theme


bind_rows(HisValid_PCEs %>% 
            mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
                   Sum_Precip=as.numeric(Sum_Precip),
                   HisSync="Validate") %>%
            filter(Sum_Precip>0) %>% 
            select(Sum_Press_Delta,Sum_Precip,HisSync),
          Sync %>% 
            mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
                   Sum_Precip=as.numeric(Sum_Precip),
                   HisSync="Historic") %>%
            filter(Sum_Precip>0) %>% 
            select(Sum_Press_Delta,Sum_Precip,HisSync))%>% 
  ggplot(aes(x=Sum_Press_Delta,y=Sum_Precip*.254))+
  stat_density_2d(aes(color=HisSync,linetype=HisSync,size=HisSync),bins=5)+
  geom_smooth(aes(linetype=HisSync,size=HisSync),color="black", method = "loess",  se = FALSE)+
  scale_y_log10()+
  scale_color_manual("",values=Line_cols,guide = guide_legend(keywidth = 4))+
  scale_size_manual("",values=Line_size,guide = guide_legend(keywidth = 4))+
  scale_linetype_manual("",values=Line_type,guide = guide_legend(keywidth = 4))+
  ylab('Precipitation Depth (PD) (mm) in log scale')+
  xlab('Event Pressure Change (EPC) (hPa)')+
  Plot_theme