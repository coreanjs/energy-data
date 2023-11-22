library(tidyverse)
library(readxl)
options(scipen=999)
library(showtext)
library(ggtext)
showtext_auto()
font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')


##단위 MWh
setwd("C:/R/Rproject/Energy&Data")

PV_regional <- read_excel('./230911_PV_regional/PV_regional_selected.xlsx') %>%
  select(-MWh) %>%
  mutate(`2010` = as.numeric(`2010`)) %>% 
  pivot_longer(-c(region, type), names_to = 'year', values_to ="MWh") %>% 
 # filter(region!='전국') %>% 
  mutate(MWh = ifelse(is.na(MWh), 0, MWh),
         GWh = MWh/1000,
         TWh = GWh/1000,
         year = as.numeric(year)) 

PV_regional %>% 
  filter(year == 2020 & region =="전남")


PV_regional %>% 
  group_by(type,year) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(x = year, y = TWh, group = type, color = type))+
  geom_line()+
  labs(title = "2005-2020 대한민국 태양광 발전량 사업용/자가용 구분")


  PV_regional %>% 
  filter(year == 2020) %>% 
  group_by(region) %>% 
  summarise(TWh = sum(TWh)) %>% 
  arrange(desc(TWh)) %>% pull(region) -> region_order

region_order



PV_regional %>% 
  group_by(region, year) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(x = year, y = TWh, group = region, color =region))+
  geom_line()+
  theme_bw()+
  theme_minimal()+
  labs(x = '지역',
       title = "2020년 대한민국 지역별 태양광 발전량(사업용/자가용 합쳐서)")+
  theme(text = element_text(family ='Nanum Myeongjo'),
        plot.title = element_text(size = 16))



PV_regional %>% 
  filter(year == 2020) %>% 
  group_by(type, region) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(x = fct_relevel(region, rev(region_order)), y = TWh, group = type, fill =type))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits = c(0, 4.5))+
  coord_flip()+
  theme_bw()+
  theme_minimal()+
  #scale_fill_manual(values = c('#15607a', '#f7744a'))+
  scale_fill_brewer(palette="Set2")+
  stat_summary(fun = sum, aes(label = round(..y.., 2), group = region), 
               geom = "text",  hjust = -0.3, family ='Nanum Myeongjo')+
  labs(x = '',
       y = "",
       title = "'20년 대한민국 지역별 사업용·자가용 태양광 생산량(TWh)",
       caption = "Source : KESIS, Graphic : Jiseok")+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size= 16, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = c(0.8, 0.6)
  )+
  guides(fill=guide_legend(title="구분"))


setwd("V:/2023 정책연구실 주요사업/61. KIER 기술정책플랫폼/[Energy&Data]/resources/images/231006_pv_regional")
#setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231006_pv_regional")
ggsave("fig1.png",  width= 600, height = 600, units ="px", dpi = 100)



library(scales)

PV_regional %>% 
  filter(year == 2020) %>% 
  group_by(type, region) %>% 
  summarise(TWh = sum(TWh)) %>%
  ungroup() %>% 
  group_by(region) %>% 
  mutate(region_sum = sum(TWh),
         pct = TWh/region_sum*100) %>% 
  filter(type =="자가용") %>% 
  arrange(desc(pct)) %>% pull(region) ->region_level_private

PV_regional %>% 
  filter(year == 2020) %>% 
  group_by(type, region) %>% 
  summarise(TWh = sum(TWh)) %>%
  ungroup() %>% 
  group_by(region) %>% 
  mutate(region_sum = sum(TWh),
         pct = TWh/region_sum*100) %>% 
  ggplot(aes(x = fct_relevel(region, rev(region_level_private)), y = pct, group = type, fill =type))+
  geom_bar(stat="identity")+
  geom_text(aes(label = round(pct, 0)), position = position_stack(vjust=0.5),  family ='Nanum Myeongjo')+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  coord_flip()+
  theme_bw()+
  theme_minimal()+
  labs(x = '',
       y = '',
       title = "'20년 대한민국 지역별 사업용·자가용 태양광 생산량 비율(%)",
       caption = "Source : KESIS, Graphic : Jiseok")+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size= 16, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot"
  )+
  scale_fill_brewer(palette="Set2")+
  guides(fill=guide_legend(title="구분"))



setwd("V:/2023 정책연구실 주요사업/61. KIER 기술정책플랫폼/[Energy&Data]/resources/images/231006_pv_regional")

#setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231006_pv_regional")
ggsave("fig2.png",  width= 600, height = 600, units ="px", dpi = 100)



 



PV_with_sum<- PV_regional %>% 
  select(region, year, type, GWh) %>% 
  pivot_wider(names_from = 'type',  values_from = 'GWh', id_cols = c('region', 'year')) %>% 
  mutate(`태양광 생산량 전체` = 사업용 + 자가용) %>% 
  pivot_longer(-c('region', 'year'), names_to="type", values_to ="GWh")

library(zoo)


#전국만


national <- PV_with_sum %>% 
  filter(type != "태양광 생산량 전체") %>% 
  filter(region== '전국') %>% 
  mutate(year = as.Date(as.yearmon(year)),
         TWh = GWh/1000) %>% 
  ggplot(aes(x = year,  y =TWh, group = type, fill =type))+
  geom_col()+
  theme_bw()+
  #theme_minimal()+
  #scale_x_continuous(breaks = seq(2005, 2020, 5))+
  scale_y_continuous(labels =comma)+
  scale_x_date(labels = date_format("'%y"))+
  
  labs(x = '연도',
      #title = "전국",
       #caption = "Source : KESIS, Graphic : Jiseok"
      )+
  facet_wrap(~region)+
  theme(text = element_text(size = 12, family ='Nanum Myeongjo'),
        plot.title = element_text(size= 20, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.background = element_blank(),
        strip.text = element_text(face ="bold", size = 16, hjust = 0, vjust = 0)
  )+
  guides(fill=guide_legend(title="구분"))+
  scale_fill_brewer(palette="Set2")

national
#setwd("V:/2023 정책연구실 주요사업/61. KIER 기술정책플랫폼/[Energy&Data]/resources/images/231006_pv_regional")

#setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231006_pv_regional")
#ggsave("fig3.png",  width= 1500, height = 750, units ="px", dpi = 100)




regional <- PV_with_sum %>% 
  filter(region!= '전국') %>%
  filter(type != "태양광 생산량 전체") %>% 
  mutate(year = as.Date(as.yearmon(year)),
         TWh = GWh/1000) %>% 
  ggplot(aes(x = year,  y =TWh, group = type, fill =type))+
  geom_col()+
  theme_bw()+
  #theme_minimal()+
  facet_wrap(~fct_relevel(region, region_order), nrow = 2,
             
             #scales="free_x"
             )+
  #scale_y_continuous(labels =comma, limits = c(0, 5))+
  #geom_vline(xintercept =as.Date(2010))+
  scale_x_date(labels = date_format("'%y"))+
  
  labs(x = '연도',
       #title = "대한민국 지역별 사업용·자가용 태양광 생산량 트렌드(TWh)",
       )+
  theme(text = element_text(size = 12, family ='Nanum Myeongjo'),
        plot.title = element_text(size= 20, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.background = element_blank(),
        strip.text = element_text(face ="bold", size = 16, hjust = 0, vjust = 0),
        legend.position = "none"
  )+
  guides(fill=guide_legend(title="구분"))+
  scale_fill_brewer(palette="Set2")


regional

setwd("V:/2023 정책연구실 주요사업/61. KIER 기술정책플랫폼/[Energy&Data]/resources/images/231006_pv_regional")

#setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231006_pv_regional")

library(patchwork)

national+regional+
  plot_layout(widths = c(.5, 2))+
  plot_annotation(title= "2005-2020년 대한민국 지역별 태양광 생산량(TWh) 트렌드",
                  subtitle = "전국 태양광 생산량(왼쪽)과 지역별 태양광 생산량(오른쪽)을 나타내었음",
                  caption = 'Source : KESIS, Graphic : Jiseok',
                  theme=theme(plot.title=element_text(size = 20, face ='bold'),
                              plot.subtitle = element_text(size = 16),
                              plot.caption = element_text(size= 12),
                              text = element_text(family = 'Nanum Myeongjo')))

ggsave("fig3-1.png",  width= 1500, height = 700, units ="px", dpi = 100)


## 서울과 대전만

PV_with_sum %>% 
  filter(type != "태양광 생산량 전체" & region %in% c("서울", "대전", "부산", "울산")) %>%  
  mutate(year = as.Date(as.yearmon(year))) %>% 
  ggplot(aes(x = year,  y =GWh, group = type, fill =type))+
  geom_col()+
  theme_bw()+
  #theme_minimal()+
  facet_wrap(~fct_relevel(region, region_order), ncol = 6, scales="free_x")+
  #scale_x_continuous(breaks = seq(2005, 2020, 5))+
  scale_y_continuous(labels =comma)+
  geom_vline(xintercept =as.Date(2010))+
  scale_x_date(labels = date_format("'%y"))+
  
  labs(x = '연도',
       title = "자가용 생산량 증가폭이 높은 서울, 부산, 울산, 대전의 트렌드(GWh)",
       caption = "Source : KESIS, Graphic : Jiseok")+
  theme(text = element_text(size = 14, family ='Nanum Myeongjo'),
        plot.title = element_text(size= 20, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.background = element_blank(),
        strip.text = element_text(face ="bold", size = 16, hjust = 0, vjust = 0)
  )+
  guides(fill=guide_legend(title="구분"))+
  scale_fill_brewer(palette="Set2")


setwd("V:/2023 정책연구실 주요사업/61. KIER 기술정책플랫폼/[Energy&Data]/resources/images/231006_pv_regional")

#setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231006_pv_regional")
#ggsave("fig3.png",  width= 1500, height = 700, units ="px", dpi = 100)
ggsave("fig4.png",  width= 900, height = 400, units ="px", dpi = 100)



 