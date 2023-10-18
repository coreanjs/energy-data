library(tidyverse)
library(RColorBrewer)
library(scales)
library(lubridate)
library(ggh4x)
library(plotly)
library(zoo)
library(extrafont)
library(showtext)                                     
library(gghighlight)
library(echarts4r)
library(ggh4x)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(viridis)
library(calendR)
library(sugrrants)
library(ggrepel)
library(ggExtra)
library(gghighlight)
library(treemapify)

library(ggthemr)

library(patchwork)
library(ggtext)
library(knitr)
library(kableExtra)
library(readxl)
options(digits=2)
options (scipen = 999)


############ 한글 폰트 꺠짐 해별 방법
theme_set(theme_bw(base_family='nanumgothic'))
font_add_google(name =  'Nanum Gothic Coding',
                family = 'Nanum Gothic Coding' )



font_add_google(name =  'Nanum Myeongjo',
                family = 'Nanum Myeongjo')


font_families()

font_add_google("Roboto Condensed", "Roboto Condensed")

showtext_auto(TRUE)


setwd("C:/R/Rproject/Energy&Data/230728_KEPCO_generation_2022")

KEPCO.Generation<- read_excel("KEPCO_Statistics.xlsx") %>% 
    pivot_longer(-region, names_to="source", values_to ="MWh") %>% 
    mutate(TWh = MWh/1000000) %>% 
    select(-MWh)


KEPCO.Generation







## 지역별
KEPCO.Generation %>% 
    group_by(region) %>% 
    summarise(TWh = sum(TWh)) %>% 
    arrange(desc(TWh)) 



KEPCO.Generation %>% 
    summarise(TWh = sum(TWh))



KEPCO.Generation %>% 
    group_by(region) %>% 
    summarise(TWh = sum(TWh)) %>% 
    arrange(desc(TWh)) %>% 
    pull(region) -> region_level_gen



unique(KEPCO.Generation$source)

## 연료원별별

KEPCO.Generation %>% 
    group_by(source) %>% 
    summarise(TWh = sum(TWh)) %>% 
    arrange(desc(TWh))


KEPCO.Generation %>% 
    group_by(source) %>% 
    summarise(TWh = sum(TWh)) %>% 
    arrange(desc(TWh)) %>% 
    ggplot(aes(x = reorder(source, TWh), y = TWh))+
    geom_bar(stat="identity")+
    coord_flip()






KEPCO.Generation %>% 
    group_by(source) %>% 
    summarise(TWh = sum(TWh)) %>% 
    arrange(desc(TWh)) %>% 
    pull(source)-> source_level_gen


source_level_gen




#
display.brewer.all()


f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
color <- (cols <- f("Paired"))

color<- color[c(1:11)]




color

source_level_gen2 <- source_level_gen[c(4:11)]


str(color_source)

color_source <- data.frame(source_level_gen, color) %>% 
    rename(source = source_level_gen,
           color_code= color)


## 색 바꾸기
color_source <-color_source %>% 
    mutate(color_code = case_when(source =="석탄" ~ "black",
                                  TRUE~color_code))


KEPCO.Generation_withcolor <-left_join(KEPCO.Generation, color_source, by = "source")


KEPCO.Generation_withcolor



c('석탄'=      "darkolivegreen",
  'LNG' =      "burlywood",
  '원자력'=    "coral2",
  '태양광' =   "#33A02C",
  '신재생 기타'= "#FB9A99",
  '바이오' =  "#E31A1C",
  '양수'=    "brown",
  '풍력'=    "#FF7F00",
  '수력'=    "#CAB2D6",
  '기타'=    "#6A3D9A",
  '유류'=    "#ffbf00") -> source_palette

#### 지역별 그래프로 나타내기 

library(ggtext)   ## element_markdown은 ggtext










### 전체 발전량

KEPCO.Generation %>% 
    summarise(TWh=round(sum(TWh), 0)) -> KEPCO_TWh_2022


KEPCO_TWh_2022



#### 연도

KEPCO_year <- 2022


KEPCO_year




gen_1<-KEPCO.Generation %>% 
    ggplot(aes( x = fct_relevel(region, rev(region_level_gen)), y = TWh, 
                #            fill = fct_relevel(source, source_level_gen),
                fill = source
    ))+
    geom_bar(stat="identity")+
    scale_fill_manual(values = source_palette)+
    coord_flip()+
    stat_summary(fun = sum, aes(label = round(..y.., 1), group = region), 
                 geom = "text",  hjust = -0.3, family ='Nanum Myeongjo')+
    #scale_y_continuous(limits =c(0, 125), breaks = seq(0, 125, 25))+
    scale_y_continuous(limits = c(0, 200), breaks = c(0, 50, 100, 150))+
    theme_bw()+
    theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo', size = 14),
          plot.title = element_text(size = 16, face="bold"),
          plot.subtitle =element_markdown(size = 14),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.85, 0.5),
          panel.grid.major.y = element_blank(),
          plot.title.position = "plot",
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          plot.caption = element_text(color = "azure4", face="bold"),
    )+
    labs(title = paste("대한민국 행정구역별/발전원별 발전량\nPower generation by province in South Korea in", KEPCO_year),
         subtitle = paste(KEPCO_year, "년 기준이며 총 발전량은", round(KEPCO_TWh_2022, 1), "TWh임."),
         caption = "Source : KEPCO, Graphic : Jiseok",
         fill ="발전원 구분",
         x = "")

gen_1
### 이미지 경로
setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230728_KEPCO_Generation_2022")


ggsave( file= "KEPCO_gen_1.png",  width =530, height = 700, units ="px", dpi = 100)





gen_2<-KEPCO.Generation %>%
    group_by(source) %>% 
    mutate(source_total = sum(TWh)) %>% 
    ungroup()%>% 
    mutate(regional_pct = round(TWh/source_total*100, 1),
           MWh = TWh*1000) %>% 
    ggplot(aes(x = fct_relevel(source, rev(source_level_gen)), y= MWh, 
               #fill = fct_relevel(source, source_level_gen),
               fill = source))+
    scale_fill_manual(values = source_palette)+    geom_bar(stat="identity")+
    geom_text(aes(label = comma(round(MWh,0))), family = 'Nanum Myeongjo' , hjust =-.1, size = 3.5)+
    coord_flip()+
    scale_y_continuous(labels =comma, limits =c(0, 120000), breaks = seq(0, 120000, 40000))+
  
    facet_wrap(~fct_relevel(region, region_level_gen), ncol = 6, scales ="free_y")+
    theme_bw()+
    theme(text = element_text(size = 14, family = 'Nanum Myeongjo'),
          plot.title = element_text(size = 18, face ="bold"),
          plot.subtitle =element_text(size = 12),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
         # panel.grid.major.x = element_blank(),
          #axis.text.x = element_blank(),
          #  axis.text.x = element_text(size = 10, angle = 45, vjust = .5),
          legend.position = "none",
         axis.text = element_text(size = 10),
          plot.caption = element_text(color = "azure4", face="bold"),
          
    )+
    labs(title = paste("대한민국 행정구역별/발전원별 발전량\nPower generation by province in South Korea in", KEPCO_year),
         subtitle =paste0(KEPCO_year, "년 기준이며, 지역별 개별 그래프로 나타냄"),
         x = "",
         caption = "Source : KEPCO, Graphic : Jiseok")


gen_2
ggsave(file= "KEPCO_gen_2.png",  width =1400, height = 800, units ="px", dpi = 100)

#ggsave(file= "KEPCO_gen_2.png",  width =800, height = 1400, units ="px", dpi = 100)















#KEPCO.Generation %>% 
#  filter(source != "기타") %>% 
#  ggplot(aes( x = fct_relevel(source, rev(source_level_gen)), y = TWh, fill = fct_relevel(region, region_level_gen)))+
#  geom_bar(stat="identity")+
#  scale_fill_brewer(palette="Set2")+
#  coord_flip()+
#  facet_wrap(~fct_relevel(region, region_level_gen))+
#  theme_bw()+
#  #theme_minimal()+
#  theme(text = element_text(family = 'Nanum Myeongjo', size = 14),
#        plot.title = element_text(size = 20, face = "bold"),
#        plot.subtitle =element_text(size = 12),
#        legend.position ="none")+
#  labs(title = "ddddd)",
#       subtitle ="ddd",
#       caption = "Source : KEPCO, Graphic : Jiseok")
#



library(treemapify)

gen_3<-KEPCO.Generation %>% 
    mutate(korea_total = sum(TWh)) %>% 
    group_by(region) %>% 
    mutate(region_total =sum(TWh), 
           region_total_label = round(region_total, 0),
           region_pct = round(region_total/korea_total*100, 1), 
           region_name = paste0(region, region_pct)) %>% 
    ungroup() %>% 
    mutate(source_pct = round(TWh/region_total*100,0), 
           source_name = paste0(source, '\n', source_pct, "%")) %>%
    ggplot(aes(area = TWh, fill=fct_relevel(source, source_level_gen), subgroup = paste0(region, region_total_label)))+
    geom_treemap()+
    scale_fill_manual(values = source_palette)+  
    geom_treemap_subgroup_text(family = 'Nanum Myeongjo',
                               padding.x = grid::unit(.3, "cm"), padding.y = grid::unit(0.2, "cm"),
                               color = "white", place = "left", grow = T, alpha =.35,
                               fontface ="bold",
                               #size = 50,
    )+
    geom_treemap_text(aes(label = source_name),  color = "black", place = "center", 
                      family = 'Nanum Myeongjo')+
    
    geom_treemap_subgroup_border(color ="white")+
    geom_treemap_subgroup_border(color ="white")+
    theme(text = element_text(family = 'Nanum Myeongjo', size = 14),
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle =element_markdown(),
          legend.position ="none",
          plot.caption = element_text(color = "azure4", face="bold"),
    )+
    labs(title = "대한민국 행정구역별/연료원별 발전량 트리맵 \nTreemap for power Generation by Province & Source in South Korea",
         subtitle =paste("2022년 기준이며, 총 발전량은", KEPCO_TWh_2022, "TWh. 지역과 함께 나타난 숫자의 단위는 TWh이며, \n발전원에 표기된 비중(%)는 해당 지역에서 그 발전원의 비중임."),
         caption = "Source : KEPCO, Graphic : Jiseok")

gen_3

ggsave(file= "KEPCO_gen_3.png",  width =1200, height = 800, units ="px", dpi = 100)






#### 연료원별 그래프로 나타내기

gen_4<-KEPCO.Generation %>%
    group_by(source) %>% 
    summarise(TWh = sum(TWh)) %>% 
    ungroup()%>% 
    mutate(total = sum(TWh),
           pct = round(TWh/total*100, 1)) %>% 
    ggplot(aes( x = fct_relevel(source, rev(source_level_gen)), y = TWh, fill = fct_relevel(source, source_level_gen)))+
    geom_bar(stat="identity")+
    coord_flip()+
    geom_text(aes(label = round(TWh, 1)), hjust = -.2, family = 'Nanum Myeongjo')+
    scale_fill_manual(values = source_palette)+  
    scale_y_continuous(limits = c(0, 220), breaks = c(0, 50, 100, 150, 200))+
    theme_bw()+
    theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo', size = 16),
          plot.title = element_text(size = 16, face="bold"),
          plot.subtitle =element_text(size = 14),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_text(color = "azure4", face="bold"),
          axis.title.x = element_text(size = 12),
    )+
    labs(title = paste("대한민국 발전원별 전력생산량\nPower generation by source in Korea in", KEPCO_year),
         subtitle =paste0(KEPCO_year, "년 기준이며, 총 전력생산량은", KEPCO_TWh_2022, "TWh임."),
         x = "",
         caption = "Source : KEPCO, Graphic : Jiseok")


gen_4


ggsave(file= "KEPCO_gen_4.png",   width =500, height = 600, units ="px", dpi = 100)



#### 지역별 그래프로 나타내기  - facet  (기타 제외)

gen_5<-KEPCO.Generation %>%
    mutate(MWh = TWh*1000) %>% 
    #filter(source %in% c("석탄", "LNG", "원자력")) %>% 
    ggplot(aes( x = fct_relevel(region, rev(region_level_gen)), y = MWh, fill = fct_relevel(source, source_level_gen)))+
    geom_bar(stat="identity")+
    scale_fill_manual(values = source_palette)+   coord_flip()+
    scale_y_continuous(labels = comma, limits = c(0, 130000), breaks = seq(0, 120000, 40000))+
    facet_wrap(~fct_relevel(source, source_level_gen), ncol = 6, scales ="free_y")+
    theme_bw()+
    geom_text(aes(label = comma(round(MWh, 0))), hjust= -.2, family = 'Nanum Myeongjo')+
    #theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo', size = 12),
          plot.title = element_text(size = 20, face="bold"),
          plot.subtitle =element_text(size = 14),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_text(color = "azure4", face="bold"),
    )+
    labs(x= "", 
         title = paste("대한민국 발전원별/행정구역별 발전량\nPower generation by source in Korea in", KEPCO_year),
         subtitle =paste0(KEPCO_year, "년 기준이며, 발전원별 개별 그래프로 나타냄"),
         caption = "Source : KEPCO, Graphic : Jiseok")


gen_5
ggsave(file= "KEPCO_gen_5.png",  width =1400, height = 700, units ="px", dpi = 100)







gen_6<-KEPCO.Generation %>% 
    left_join(color_source, by = "source") %>% 
    filter(!source %in% c("석탄", "LNG", "원자력")) %>%
    mutate(MWh = round(TWh*1000, 1)) %>% 
    ggplot(aes( x = fct_relevel(region, rev(region_level_gen)), y = MWh, fill = fct_relevel(source, source_level_gen2)))+
    geom_col()+
    scale_fill_manual(values = source_palette)+  
    coord_flip()+
    scale_y_continuous(labels = comma, limits = c(0, 8000), breaks = seq(0, 8000, 2000))+
    facet_wrap(~fct_relevel(source, source_level_gen), nrow = 2, scales ="free_y")+
    geom_text(aes(label = comma(round(MWh, 0))), hjust= -.2, family = 'Nanum Myeongjo')+
    theme_bw()+
    #  geom_text(aes(label = round(TWh, 1)), hjust= -.2)+
    #theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo', size = 12),
          plot.title = element_text(size = 20, face="bold"),
          plot.subtitle =element_text(size = 14),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_text(color = "azure4", face="bold"),
    )+
    labs(x= "", 
         title = "석탄/원자력/LNG를 제외한 대한민국 발전원별/행정구역별 발전량\nPower generation by source in Korea in 2022",
         subtitle =paste0(KEPCO_year, "년 기준이며, 주요 발전원을 제외한 나머지 발전원을 개별 그래프로 나타냄."),
         caption = "Source : KEPCO, Graphic : Jiseok")

gen_6
### 이미지 경로
setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230728_KEPCO_Generation_2022")


ggsave(file= "KEPCO_gen_6.png",  width =900, height = 700, units ="px", dpi = 100)





############## 합치기


gen_1+gen_5





gen_1_re<-KEPCO.Generation %>% 
  ggplot(aes( x = fct_relevel(region, rev(region_level_gen)), y = TWh, 
              #            fill = fct_relevel(source, source_level_gen),
              fill = fct_relevel(source, source_level_gen)
  ))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = source_palette)+
  coord_flip()+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = region), 
               geom = "text",  hjust = -0.3, family ='Nanum Myeongjo')+
  #scale_y_continuous(limits =c(0, 125), breaks = seq(0, 125, 25))+
  scale_y_continuous(limits = c(0, 200), breaks = c(0, 50, 100, 150))+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo', size = 14),
        plot.title = element_text(size = 16, face="bold"),
        plot.subtitle =element_markdown(size = 14),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.8, 0.5),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        plot.caption = element_text(color = "azure4", face="bold"))+
  labs(fill ="발전원 구분",
       x = "")

gen_1_re



gen_5_re<-KEPCO.Generation %>%
  mutate(MWh = TWh*1000) %>% 
  #filter(source %in% c("석탄", "LNG", "원자력")) %>% 
  ggplot(aes( x = fct_relevel(region, rev(region_level_gen)), y = MWh, fill = fct_relevel(source, source_level_gen)))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = source_palette)+   coord_flip()+
  scale_y_continuous(labels = comma, limits = c(0, 140000), breaks = c(0, 50000, 100000))+
  facet_wrap(~fct_relevel(source, source_level_gen), ncol = 6, scales ="free_y")+
  theme_bw()+
  geom_text(aes(label = comma(round(MWh, 0))), hjust= -.2, family = 'Nanum Myeongjo')+
  #theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo', size = 12),
        plot.title = element_text(size = 20, face="bold"),
        plot.subtitle =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(color = "azure4", face="bold"),
  )+
  labs(x= "")

gen_1_re + gen_5_re+ plot_layout(widths = c(1, 3))+
  plot_annotation(
  title = "대한민국 행정구역별/발전원별 발전량\nPower generation by province in South Korea in 2022",
  subtitle = '읜쪽의 행정구역별 발전량 그래프의 단위는 TWh이며, 오른쪽의 발전원별 발전량 그래프의 단위는 MWh임',
  caption = "Source : KEPCO, Graphic : Jiseok",
  theme=theme(plot.title=element_text(size = 24, face ='bold'),
              plot.subtitle = element_text(size = 16),
              plot.caption = element_text(size= 12),
              text = element_text(family = 'Nanum Myeongjo')))



setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230728_KEPCO_Generation_2022")
ggsave(file= "KEPCO_gen_1_5_re.png",  width =1600, height = 800, units ="px", dpi = 100)





#############




gen_4_re<-KEPCO.Generation %>%
  group_by(source) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ungroup()%>% 
  mutate(total = sum(TWh),
         pct = round(TWh/total*100, 1)) %>% 
  ggplot(aes( x = fct_relevel(source, rev(source_level_gen)), y = TWh, fill = fct_relevel(source, source_level_gen)))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label = round(TWh, 1)), hjust = -.2, family = 'Nanum Myeongjo')+
  scale_fill_manual(values = source_palette)+  
  scale_y_continuous(limits = c(0, 240), breaks = c(0, 50, 100, 150, 200))+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo', size = 16),
        plot.title = element_text(size = 16, face="bold"),
        plot.subtitle =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(color = "azure4", face="bold"),
       # axis.title.x = element_text(size = 12),
  )+
  labs(x = "")



gen_4_re



gen_2_re<-KEPCO.Generation %>%
  group_by(source) %>% 
  mutate(source_total = sum(TWh)) %>% 
  ungroup()%>% 
  mutate(regional_pct = round(TWh/source_total*100, 1),
         MWh = TWh*1000) %>% 
  ggplot(aes(x = fct_relevel(source, rev(source_level_gen)), y= MWh, 
             #fill = fct_relevel(source, source_level_gen),
             fill = source))+
  scale_fill_manual(values = source_palette)+    geom_bar(stat="identity")+
  geom_text(aes(label = comma(round(MWh,0))), family = 'Nanum Myeongjo' , hjust =-.3, size = 3.5)+
  coord_flip()+
 scale_y_continuous(labels =comma, limits =c(0, 175000))+
  facet_wrap(~fct_relevel(region, region_level_gen), ncol = 6, scales ="free_y")+
  theme_bw()+
  theme(text = element_text(size = 14, family = 'Nanum Myeongjo'),
        plot.title = element_text(size = 18, face ="bold"),
        plot.subtitle =element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #  axis.text.x = element_text(size = 10, angle = 45, vjust = .5),
        legend.position = "none",
        plot.caption = element_text(color = "azure4", face="bold"),
        
  )+
  labs(x = "")

gen_2_re




gen_4_re + gen_2_re+ plot_layout(widths = c(1, 5))+
  plot_annotation(
    title = "대한민국 발전원별/행정구역별 발전량\nPower generation by province in South Korea in 2022",
    subtitle = '읜쪽의 대한민국 전체의 발전원별 발전량 그래프 단위는 TWh이며, 오른쪽의 행정구역별 발전량 그래프의 단위는 MWh임',
    caption = "Source : KEPCO, Graphic : Jiseok",
    theme=theme(plot.title=element_text(size = 24, face ='bold'),
                plot.subtitle = element_text(size = 16),
                plot.caption = element_text(size= 12),
                text = element_text(family = 'Nanum Myeongjo')))



setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230728_KEPCO_Generation_2022")
ggsave(file= "KEPCO_gen_4_2_re.png",  width =1600, height = 800, units ="px", dpi = 100)

