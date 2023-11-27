
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(showtext)
library(extrafont)
library(ggtext)
library(gghighlight)
library(scales)

font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")

showtext_auto()

options(scipen=999)

setwd("C:/R/Rproject/Energy&Data")



##단위 MWh



nuclear_cost_overrun_raw <- read_excel('./PPT_research_topic/korea_nuclear_cost_overrun.xlsx', skip = 1) %>% 
  filter(!name %in% c('새울 3,4', '신한울 1,2'))### 건설중인 발전소는 제외. 데이터 불확실함

nuclear_cost_overrun_raw %>% 
  arrange(initial_cost) %>% 
  pull(name) ->name_level
name_level


nuclear_cost_overrun_raw %>% 
  mutate(label = paste0(name, "(", MW, "MW)")) %>% 
  arrange(initial_cost) %>% 
  pull(label) ->label_level

label_level

nuclear_cost_overrun <- read_excel('./PPT_research_topic/korea_nuclear_cost_overrun.xlsx', skip = 1) %>%
  filter(!name %in% c('새울 3,4', '신한울 1,2')) %>% ### 건설중인 발전소는 제외. 데이터 불확실함
  select(-end_date) %>% 
  mutate(initial_per_capa = initial_cost/MW) %>% 
  relocate(initial_per_capa, .before =start_date) %>%
  mutate(increased_pct = round(increased_cost/initial_cost*100, 0)) %>% 
  pivot_longer(-c(name, MW, increased_pct, increased_cost, initial_per_capa, start_date), names_to ="type", values_to ="ukwon") %>% 
  mutate(jowon = ukwon/10000,
         label = fct_relevel(paste0(name, "(", MW, "MW)"), label_level)) 

nuclear_cost_overrun




nuclear_cost_overrun %>% 
  ggplot(aes(x = name, y = jowon, fill = type))+
  geom_bar(stat="identity", position =position_dodge())

nuclear_cost_overrun %>% 
  ggplot(aes(x = label, y = jowon, color = type))+
  geom_line(aes(group = name), color ="red")+
  geom_text(data = . %>% filter(type == "final_cost"), aes(label = paste0(increased_pct, "%")),hjust =-.3)+
  scale_color_manual(values = c('red','blue'))+
  scale_y_continuous(limits = c(0, 10))+
  geom_point(size = 2.5)+
  coord_flip()+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
        axis.text = element_text(size = 14),
        axis.ticks.x = element_line(linewidth = .2,
                                    color = 'black'),
        axis.ticks.length = unit(.08, "cm"),
        axis.line.x = element_line(colour = "gray80", 
                                   size = .5, linetype = "solid"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 14),
        strip.background = element_blank(),
        legend.position ="none",
        strip.text = element_text(face ="bold", size = 16, hjust = 0, vjust = 0)
  )+
  labs(title = "Cost Overrun for Korean Nuclear Power Plants",
       subtitle = "<span style = 'color:blue;'><b>Initial cost</b></span> and<span style = 'color:red;'>
       <b>Final cost</b></span> for different nuclear power plants in Korea.<br>  % indicates cost change[(final - initial)/initial] in percent increase.",
       x ="Name of plant",
       y = "Construction cost(unit : trillion Won)",
       caption = "Source : YTN, Graphic : Jiseok")


setwd("C:/R/Rproject/Energy&Data/PPT_research_topic")

ggsave("fig1.png",  width=800, height =600, units ="px", dpi = 100, bg='white')




nuclear_cost_overrun%>% 
  arrange(desc(in)) %>% 
  mutate(y_position = rev(1:nrow(.))) %>% 
  select(country, y_position) ->y_position



range(monthly_elec_gen$year)

monthly_elec_gen %>% 
  group_by(year, type) %>% 
  summarise(TWh = sum(TWh)) %>% 
  filter(year == 2022) %>% 
  arrange(desc(TWh)) %>% pull(type) %>% as.character -> type_level 

type_level

monthly_elec_gen %>% 
  ggplot(aes(x = month, y = TWh, group = year, color = year))+
  geom_line()+  geom_line(data = . %>% filter(year == 2022), 
                          color ="#e3000b",
                          linewidth = .8)+
  geom_line(data = . %>% filter(year == 2021), color ="#f7a102", linewidth = .8)+
  facet_wrap(~fct_relevel(type, type_level))


library(ggdist)

monthly_elec_gen %>% 
  ggplot(aes(x = month, y = TWh))+
  stat_lineribbon(
    data = . %>% filter(!year %in% c(2022, 2021)),
    .width = c(.5, .8, .95), linetype ="dashed", linewidth = .2, color = "black", alpha = .8)+
  geom_line(data = . %>% filter(year == 2022), 
            color ="#1f5c99",
            linewidth = 1.2)+
  geom_line(data = . %>% filter(year == 2021), color ="#f7a102", linewidth = 1.2)+
  #scale_fill_brewer(palette="Greens")+
  scale_fill_grey(start = 0.9, end = 0.6)+
  scale_x_continuous(limits = c(1, 12), breaks = c(1, 3, 6, 9, 12))+
  scale_y_continuous(limits = c(1, 23), breaks = seq(0, 20, 5))+
  facet_wrap(~fct_relevel(type, type_level), nrow=2, scales="free_x")+
  theme_bw()+
  geom_vline(xintercept = c(3, 6, 9, 12), linetype ="dashed", alpha = .3, linewidth= .3)+
  # theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
        axis.ticks.x = element_line(linewidth = .2,
                                    color = 'black'),
        axis.ticks.length = unit(.08, "cm"),
        axis.line.x = element_line(colour = "gray80", 
                                   size = .5, linetype = "solid"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 14),
        strip.background = element_blank(),
        legend.position ="none",
        strip.text = element_text(face ="bold", size = 16, hjust = 0, vjust = 0)
  )+
  labs(title = "2017~2022 주요 발전원별 발전량 변화",
       subtitle = "한국전력통계 발전량 추이 데이터를 활용하여 주요 발전원의 2017~2020년<br>
      월별 발전량의 분포와, 
    <span style = 'color:#f7a102;'><b>2021년</b></span>과<span style = 'color:#1f5c99;'>
       <b>2022년</b></span> 발전량을 그래프로 나타내었음.<br>
       유연탄은 감소 추세이며, 원자력, 복합화력, 신재생은 증가 추세임.",
       x ="월",
       caption = "Source : KEPCO, Graphic : Jiseok")


#setwd("./231027_elec_generation_by_month_KEPCO")

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231102_elec_gen_2017_2022")

ggsave("fig1.png",  width=600, height =750, units ="px", dpi = 100, bg='white')









####### 총계 기타 포함
####### 총계 기타 포함
####### 총계 기타 포함
####### 총계 기타 포함
####### 총계 기타 포함

monthly_elec_gen_with_total <- read_excel('./231027_elec_generation_by_month_KEPCO/data.xlsx', sheet ="Sheet2") %>%
  mutate(date = as.yearmon(paste(year, month, sep="-"), format ="%Y-%m")) %>% 
  pivot_longer(-c(date, year, month), names_to = 'type', values_to ="MWh") %>% 
  mutate(TWh = MWh/1000000,
         type = factor(type)) %>% 
  select(-MWh) %>% 
  mutate(type = factor(type, levels=  c('총계', '유연탄', '원자력', '복합화력',
                                        '집단', '신재생', '기타')))


monthly_elec_gen_with_total %>% 
  filter(type !='총계') %>% 
  ggplot(aes(date, y = TWh, group =type, color = type))+
  geom_area()


a<- monthly_elec_gen_with_total %>% 
  filter(type !='총계') %>% 
  group_by(year, type) %>%
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(year, TWh, group =type, fill = type))+
  geom_col()+
  theme_bw()+
  theme_minimal()+
  #  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = c('maroon','orange','coral','darkolivegreen','limegreen','gray'))+
  scale_x_continuous(limits = c(2016.5, 2022.5), breaks = seq(2017, 2022, 1))+
  scale_y_continuous(limits = c(0, 650), breaks = c(0, 200, 400, 600))+
  stat_summary(fun = sum, aes(label = round(..y.., 0), group = year), geom = "text", vjust = -.3)+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
        axis.ticks.x = element_line(linewidth = .2,
                                    color = 'black'),
        axis.ticks.length = unit(.08, "cm"),
        axis.line.x = element_line(colour = "gray80", 
                                   size = .5, linetype = "solid"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 14),
        #legend.position ="none"
  )+
  labs(title = "발전원별 발전량(TWh)",
       x ="연도",
       y = "발전량(TWh)")

a
setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231102_elec_gen_2017_2022")

ggsave("fig2.png",  width=400, height =500, units ="px", dpi = 100, bg='white')




b<- monthly_elec_gen_with_total %>% 
  filter(type !='총계') %>% 
  group_by(year, type) %>%
  summarise(TWh = sum(TWh)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(year_sum = sum(TWh),
         pct = TWh/year_sum*100) %>% 
  ggplot(aes(year, pct, 
             group =type, 
             fill = factor(type, levels= rev(c('기타', '신재생', '집단', '복합화력', '원자력', '유연탄')))))+
  geom_bar(stat="identity")+
  scale_x_continuous(limits = c(2016.5, 2022.5), breaks = seq(2017, 2022, 1))+
  scale_fill_manual(values = c('maroon','orange','coral','darkolivegreen','limegreen','gray'))+
  geom_text(aes(label = round(pct, 0)), position=position_stack(vjust= .5))+
  guides(fill = guide_legend("구분"))+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
        axis.ticks.x = element_line(linewidth = .2,
                                    color = 'black'),
        axis.ticks.length = unit(.08, "cm"),
        axis.line.x = element_line(colour = "gray80", 
                                   size = .5, linetype = "solid"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 14),
        legend.position = 'none'
  )+
  labs(title = "발전원별 발전량 비중(%)",
       x ="연도",
       y = "비중(%)")

b

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/231102_elec_gen_2017_2022")

library(patchwork)

a/b+plot_annotation('2017-2022 주요 발전원별 발전량 및 발전비중', 
                    subtitle ="발전량은 2019, 2020년 감소하였다가 다시 증가 추세이며,\n유연탄의 발전 비중은 꾸준히 감소(41->31%)하고 있음",
                    caption = 'Source : KEPCO, Graphic : Jiseok',
                    theme=theme(plot.title=element_text(size = 20, face ='bold'),
                                plot.subtitle = element_text(size = 14),
                                plot.caption = element_text(size= 12),
                                
                                text = element_text(family = 'Nanum Myeongjo')))

ggsave("fig3.png",  width=600, height =800, units ="px", dpi = 100, bg='white')

