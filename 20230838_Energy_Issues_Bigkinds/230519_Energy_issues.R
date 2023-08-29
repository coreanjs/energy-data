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
library(patchwork)
library(markdown)
library(ggtext)
library(TTR)
library(ggExtra)
library(ggbeeswarm)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggdist) 
library(readxl)
library(tidytext)




## 사전 설치


install.packages("remotes")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
useNIADic()
useSejongDic()








############ 한글 폰트 꺠짐 해별 방법
theme_set(theme_bw(base_family='nanumgothic'))
font_add_google(name =  'Nanum Gothic Coding',
                family = 'Nanum Gothic Coding' )


font_add_google("Noto Serif Korean")

font_add_google(name =  'Noto Serif Korean',
                family = 'Noto Serif Korean')



font_add_google(name =  'Nanum Myeongjo',
                family = 'Nanum Myeongjo')


showtext_auto(TRUE)




setwd("C:/R/Rproject/2023_project_KIER/20230838_Energy_Issues_Bigkinds")


news.raw<- read_excel("climate change.xlsx")

str(news.raw)

news.selected<- news.raw %>% 
    select(일자, 언론사, `특성추출(가중치순 상위 50개)`) %>% 
    #  mutate(키워드 = str_replace_all(키워드, ',', ' ')) %>% 
    rename(keyword = `특성추출(가중치순 상위 50개)`,
           date = 일자,
           source = 언론사) %>% 
    mutate(date = as.Date(date, format="%Y%m%d"),
           president = case_when(date >= as.Date('2008-02-05') & date <= as.Date('2013-02-24') ~ '이명박',
                                 date >= as.Date('2013-02-25') & date <= as.Date('2017-03-10')~ '박근혜',
                                 date >= as.Date('2017-05-10') & date <= as.Date('2022-05-09') ~ '문재인',
                                 date >= as.Date('2022-05-10')  ~ '윤석열'),
           president = factor(president, levels = c('이명박', '박근혜', '문재인', '윤석열')))


news.selected

## complete.cases를 활용해 결측행만 보기

news.selected[!complete.cases(news.selected),]

## complete.cases를 활용해 박근혜와 문재인 사이 빈 기간 날리기

news.selected <- news.selected %>% filter(complete.cases(news.selected))



head(news.selected)

tail(news.selected)



## 2357개 사설
nrow(news.selected)


news.selected %>% 
    group_by(president) %>% 
    count(n = n()) %>%
    ungroup() %>% 
    mutate(pct = nn/n*100)






### 월별로 정리해서 보기 - 모든 기간
news.selected %>% 
    mutate(ym = as.yearmon(date)) %>% 
    group_by(ym, president) %>% 
    count( n = n()) %>% 
    ggplot(aes(x = as.Date(ym), y = nn))+
    geom_col(fill ="#1f5c99")+
    scale_x_date(date_breaks = "1 year", date_labels = "'%y. %b.")+ 
    geom_vline(xintercept = as.Date('2008-02-05'), color ="red", linetype ="dashed")+
    geom_vline(xintercept = as.Date('2013-02-25'), color ="red", linetype ="dashed")+
    geom_vline(xintercept = as.Date('2017-05-10'), color ="red", linetype ="dashed")+
    geom_vline(xintercept = as.Date('2022-05-10'), color ="red", linetype ="dashed")+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none")+
    labs(y = '사설 수',
         x = '날짜')

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")


ggsave('number_of_column_trend.png',   width =600, height = 400, units ="px", dpi = 100)




### 월별로 정리해서 보기 - 정권별로 구분

news.selected %>% 
    mutate(ym = as.yearmon(date)) %>% 
    group_by(ym, president) %>% 
    count( n = n()) %>% 
    ggplot(aes(x = as.Date(ym), y = nn))+
    geom_col(fill ="#1f5c99")+
    scale_x_date(date_breaks = "1 year", date_labels = "'%y. %b.")+ facet_wrap(~president, ncol = 1)+
    
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none")+
    labs(y = '사설 수',
         x = '날짜')+
    geom_smooth(color ="yellow")

ggsave('number_of_column_trend_facet.png',   width =600, height = 400, units ="px", dpi = 100)



### 공통키워드를 찾아볼까???





############ 230710

##unnest tokens는 tidytext 라이브러링

library(tidytext)



news.selected

news.selected %>% 
    select(keyword) %>% 
    separate_rows(keyword, sep= ",")


news.keyword<- news.selected %>% 
    separate_rows(keyword, sep= ",") %>% 
    rename(word = keyword) %>% 
    filter(str_length(word) > 1) %>% 
    count(president, word, sort = TRUE) %>%
    filter(!word %in% c('만큼', '그동안', '각국'))


news.keyword

### 어짜피 키워드로 구분되어있으니까 unnest_tokens 안 써도 된다
#news.keyword<- news.selected %>% 
#  select(date, source, keyword, president) %>% 
#  unnest_tokens(input = keyword,
#                output = word,
#  ) %>% 
#  filter(str_length(word) > 1) %>% 
#  count(president, word, sort = TRUE) %>% 
#  filter(!word %in% c('만큼', '그동안', '각국'))
#


news.keyword %>% 
    filter(president =="문재인")




## slice 활용해서 상위 n개만 보기
news.keyword %>% 
    group_by(president) %>%
    mutate(row_number = row_number(),
           word = reorder_within(word, n, president)) %>% 
    slice(1:20) %>% 
    ggplot(aes(x = word, y = n))+
    geom_col(fill ="#1f5c99")+
    coord_flip()+
    scale_x_reordered()+
    facet_wrap(~president, scales = "free_y", nrow = 1)+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none")+
    labs(y = '사설 수',
         x = '날짜')



ggsave('number_of_keyword_by_president_keyword50.png',   width =800, height = 600, units ="px", dpi = 100)






## unnest_token으로 분류
library(tidytext)




news.keyword %>% 
    group_by(word) %>% 
    summarise(n = sum(n)) %>% 
    top_n(20) %>% 
    ggplot(aes(x = reorder(word, n), y = n))+
    geom_col(fill ="#1f5c99")+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none")+
    labs(y = '단어 수',
         x = '단어')

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")


ggsave('keyword_keyword50.png',   width =500, height = 700, units ="px", dpi = 100)



### 특정 키워드만 ('미국) 보기  - 미국이 들어간 컬럼만 보기
news.selected %>% 
    mutate(keyword_detect = ifelse(str_detect(keyword, '미국'), 'Y', 'N')) %>% 
    filter(keyword_detect =='Y') %>% 
    separate_rows(keyword, sep= ",") %>% 
    rename(word = keyword) %>% 
    filter(str_length(word) > 1) %>% 
    count(president, word, sort = TRUE) %>% 
    group_by(president) %>%
    mutate(row_number = row_number(),
           word = reorder_within(word, n, president)) %>% 
    slice(1:20) %>% 
    ggplot(aes(x = word, y = n))+
    geom_col(fill ="#1f5c99")+
    coord_flip()+
    scale_x_reordered()+
    facet_wrap(~president, scales = "free_y", nrow = 1)+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 20),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none")+
    labs(title = '미국이 들어간 칼럼만 추출한 결과',
         y = '사설 수',
         x = '날짜')




########### 함수로 만들어서



keyword_top5<- c('기후변화', '미국', '한국', '우리나라', '중국', '배출량', 
                 '온실가스', '전문가', '가능성', '녹색성장', '코로나19',
                 '창조경제', '탈원전')


keyword_top5



for (i in keyword_top5) {
    
    keyword_top5_by_president = news.selected %>% 
        mutate(keyword_detect = ifelse(str_detect(keyword, i), 'Y', 'N')) %>% 
        filter(keyword_detect =='Y') %>% 
        separate_rows(keyword, sep= ",") %>% 
        rename(word = keyword) %>% 
        filter(str_length(word) > 1) %>% 
        count(president, word, sort = TRUE) %>% 
        group_by(president) %>%
        mutate(row_number = row_number(),
               word = reorder_within(word, n, president)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x = word, y = n))+
        geom_col(fill ="#1f5c99")+
        coord_flip()+
        scale_x_reordered()+
        facet_wrap(~president, scales = "free_y", nrow = 1)+
        theme(
            text = element_text(family = 'Nanum Myeongjo'),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 20),
            #panel.grid.major.x = element_blank(),
            #    panel.grid.major.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_blank(),
            strip.text.x = element_text(size = 11),
            legend.position = "none")+
        labs(title = paste('특정 키워드가 포함된 (키워드 :', i ,  ')칼럼만 추출하여 빈도 분석'),
             y = '사설 수',
             x = '날짜')
    
    #setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue/keyword_by_president")
    setwd("C:/R/Rproject/2023_project_KIER/20230838_Energy_Issues_Bigkinds")
    
    ggsave(plot = keyword_top5_by_president, file =paste0("keyword_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
    
}









### 정권별로 구분해서?   - count로 
news.keyword %>% 
    group_by(word) %>% 
    mutate(total = sum(n),
           pct = round(n/total*100, 0)) %>% 
    #top_n(20) %>% 
    filter(n>=50) %>% 
    ggplot(aes(x = reorder(word, n, sum), y = n, fill = president))+
    geom_col()+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        #legend.position = "none"
    )+
    labs(y = '단어 수',
         x = '단어')

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")



### 정권별로 구분해서?   - pct로 
news.keyword %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    #top_n(20) %>% 
    filter(pct >.4) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        #legend.position = "none"
    )+
    labs(
        x = '단어')




news.keyword %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    #top_n(20) %>% 
    filter(pct >.4) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        #legend.position = "none"
    )+
    labs(
        x = '단어')





########## pct

news.keyword.is.na<- news.keyword %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    select(president, word, pct) %>% 
    pivot_wider(names_from= president, values_from = pct) %>% 
    mutate(pct = rowSums(.[2:5], na.rm=T),
           is_na = rowSums(is.na(.[2:5]))) %>% 
    arrange(-pct)




news.keyword.is.na 

news.keyword.is.na %>% 
    select(-pct) %>% 
    pivot_longer(-c(word, is_na), names_to="president", values_to = 'pct') %>% 
    slice(1: 100) %>% 
    ggplot(aes(x=word, y = pct))+
    geom_col()




### 두 정권에서만 나온 키워드 5개
news.keyword.is.na %>% 
    filter(is_na ==2) %>% 
    slice(1:5) %>%  pull(word) -> keyword_is_na_2


keyword_is_na_2



for (i in keyword_is_na_2) {
    
    keyword_is_na_2_graph = news.selected %>% 
        mutate(keyword_detect = ifelse(str_detect(keyword, i), 'Y', 'N')) %>% 
        filter(keyword_detect =='Y') %>% 
        separate_rows(keyword, sep= ",") %>% 
        rename(word = keyword) %>% 
        filter(str_length(word) > 1) %>% 
        count(president, word, sort = TRUE) %>% 
        group_by(president) %>%
        mutate(row_number = row_number(),
               word = reorder_within(word, n, president)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x = word, y = n))+
        geom_col(fill ="#1f5c99")+
        coord_flip()+
        scale_x_reordered()+
        facet_wrap(~president, scales = "free_y", nrow = 1)+
        theme(
            text = element_text(family = 'Nanum Myeongjo'),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 20),
            #panel.grid.major.x = element_blank(),
            #    panel.grid.major.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_blank(),
            strip.text.x = element_text(size = 11),
            legend.position = "none")+
        labs(title = paste('두 정권에서만 나타난 키워드 (', i ,  ') 칼럼만 추출하여 빈도 분석'),
             y = '사설 수',
             x = '날짜')
    
    #setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue/keyword_by_president")
    setwd("C:/R/Rproject/2023_project_KIER/20230838_Energy_Issues_Bigkinds")
    
    ggsave(plot = keyword_is_na_2_graph, file =paste0("keyword_is_na_2_graph_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
    
}





## 한 정권에서만 나온 단어

news.keyword.is.na %>% 
    filter(is_na ==3) %>% 
    slice(1:5) %>%  pull(word) -> keyword_is_na_1


keyword_is_na_1



for (i in keyword_is_na_1) {
    
    keyword_is_na_1_graph = news.selected %>% 
        mutate(keyword_detect = ifelse(str_detect(keyword, i), 'Y', 'N')) %>% 
        filter(keyword_detect =='Y') %>% 
        separate_rows(keyword, sep= ",") %>% 
        rename(word = keyword) %>% 
        filter(str_length(word) > 1) %>% 
        count(president, word, sort = TRUE) %>% 
        group_by(president) %>%
        mutate(row_number = row_number(),
               word = reorder_within(word, n, president)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x = word, y = n))+
        geom_col(fill ="#1f5c99")+
        coord_flip()+
        scale_x_reordered()+
        facet_wrap(~president, scales = "free_y", nrow = 1)+
        theme(
            text = element_text(family = 'Nanum Myeongjo'),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 20),
            #panel.grid.major.x = element_blank(),
            #    panel.grid.major.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_blank(),
            strip.text.x = element_text(size = 11),
            legend.position = "none")+
        labs(title = paste('한 정권에서만 나타난 키워드 (', i ,  ') 칼럼만 추출하여 빈도 분석'),
             y = '사설 수',
             x = '날짜')
    
    #setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue/keyword_by_president")
    setwd("C:/R/Rproject/2023_project_KIER/20230838_Energy_Issues_Bigkinds")
    
    ggsave(plot = keyword_is_na_1_graph, file =paste0("keyword_is_na_1_graph_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
    
}




### 모든 키워드 포함해서
### 모든 키워드 포함해서
### 모든 키워드 포함해서
### 모든 키워드 포함해서


news.keyword %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    #top_n(20) %>% 
    filter(pct >.4) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        #legend.position = "none"
    )+
    labs(
        x = '단어')+
    facet_wrap(~president, scales= "free_y", nrow = 1)


## 모든 정권에서 나타난 단어는 제외하고

news.keyword.is.na %>% 
    arrange(desc(pct)) %>% 
    filter(is_na>1) %>%  ################################ 여기
    mutate(row_number = row_number()) %>% 
    select(-is_na) %>% 
    rename(value = pct) %>% 
    pivot_longer(-c(word, value, row_number), names_to = "president", values_to = 'pct') %>% 
    filter(row_number<= 30) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        #legend.position = "none"
    )+
    labs(
        x = '단어')+
    facet_wrap(~fct_relevel(president, levels = c('이명박', '박근혜', '문재인', '윤석열')), scales= "free_y", nrow = 1)






library(ggraph)
library(tidygraph)  ## 네트워크 그래프
library(widyr)   ##pairwise_count




### 함수로 만들기 전에 원본?



news.keyword



news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>%
    separate_rows(keyword, sep= ",") %>% 
    #  unnest_tokens(input = keyword,
    #                output = word,
    #                to_lower = FALSE) %>% 
    #  filter(str_length(word) > 1) %>%  
    #  filter(!word %in% c('만큼', '그동안')) %>%  
    pairwise_count(item = keyword,     ## pairwise_count  : 단어 동시 출현 빈도
                   feature = rowid,
                   sort = T) %>% 
    filter(n >= 30) %>%  ##
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(group_louvain()))  %>% 
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                   alpha = 0.2) +             # 엣지 명암
    
    geom_node_point(aes(size = centrality,    # 노드 크기
                        color = group,
                        alpha =0.5),       # 노드 색깔
                    show.legend = F) +        # 범례 삭제
    scale_size(range = c(3, 15)) +            # 노드 크기 범위
    geom_node_text(aes(label = name),         # 텍스트 표시
                   repel = T,                 # 노드밖 표시
                   size = 5,                  # 텍스트 크기
                   family = 'Nanum Myeongjo') +  # 폰트
    
    theme_graph()+                             # 배경 삭제
    theme( text = element_text(family = 'Nanum Myeongjo'))


## 이미지 저장 폴더
setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")








news.keyword



news.keyword<- news.selected %>% 
    separate_rows(keyword, sep= ",") %>% 
    rename(word = keyword) %>% 
    filter(str_length(word) > 1) %>% 
    count(president, word, sort = TRUE) %>%
    filter(!word %in% c('만큼', '그동안', '각국'))




news.keyword

1+1

news.selected%>% 
    select(keyword, president) %>% 
    rowid_to_column() %>% 
    filter(president =="이명박") %>% ############################# 
separate_rows(keyword, sep= ",") %>% 
    pairwise_count(item = keyword,     ## pairwise_count  : 단어 동시 출현 빈도
                   feature = rowid,
                   sort = T) %>% 
    filter(n >= 20) %>%  ##
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(group_louvain()))  %>% 
    ggraph(
        #layout = "fr"
    )+      # 레이아웃
    geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                   alpha = 0.2) +             # 엣지 명암
    
    geom_node_point(aes(size = centrality,    # 노드 크기
                        color = group,
                        alpha =0.5),       # 노드 색깔
                    show.legend = F) +        # 범례 삭제
    scale_size(range = c(3, 15)) +            # 노드 크기 범위
    geom_node_text(aes(label = name),         # 텍스트 표시
                   repel = T,                 # 노드밖 표시
                   size = 5,                  # 텍스트 크기
                   family = 'Nanum Myeongjo') +  # 폰트
    
    theme_graph()+                             # 배경 삭제
    theme( text = element_text(family = 'Nanum Myeongjo'))




















######## 함수로 만들어서 ggssave!!!!!!!!!!!!!


number <- c(100, 150, 200, 250, 300, 350, 400)
number

?tibble

for (i in number) {
    
    test = news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE) %>% 
        filter(str_length(word) > 1) %>%  
        pairwise_count(item = word,     ## pairwise_count  : 단어 동시 출현 빈도
                       feature = rowid,
                       sort = T) %>% 
        filter(n >= i) %>%  ##
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(group_louvain()))  %>% 
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                       alpha = 0.2) +             # 엣지 명암
        
        geom_node_point(aes(size = centrality,    # 노드 크기
                            color = group,
                            alpha =0.5),       # 노드 색깔
                        show.legend = F) +        # 범례 삭제
        scale_size(range = c(3, 15)) +            # 노드 크기 범위
        geom_node_text(aes(label = name),         # 텍스트 표시
                       repel = T,                 # 노드밖 표시
                       size = 5,                  # 텍스트 크기
                       family = 'Nanum Myeongjo') +  # 폰트
        
        theme_graph()+                             # 배경 삭제
        theme( text = element_text(family = 'Nanum Myeongjo'))+
        labs(title = paste("1990-2023 신문 사설을 활용한 키워드 동시출현빈도 네트워크", "(filter, n >=", i, ")"))
    
    
    
    ggsave(plot = test, file =paste0("pairwise_count_", i, ".png"),  width =1000, height = 700, units ="px", dpi = 100)
    
}













## 연결중심성   pairwise_count  : 단어 동시 출현 빈도


pairwise_count_1<- news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE) %>% 
    filter(str_length(word) > 1) %>%  
    pairwise_count(item = word,     ## pairwise_count  : 단어 동시 출현 빈도
                   feature = rowid,
                   sort = T) %>% 
    filter(n >=200) %>%  ##
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(group_louvain()))  %>% 
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                   alpha = 0.2) +             # 엣지 명암
    
    geom_node_point(aes(size = centrality,    # 노드 크기
                        color = group,
                        alpha =0.5),       # 노드 색깔
                    show.legend = F) +        # 범례 삭제
    scale_size(range = c(3, 15)) +            # 노드 크기 범위
    geom_node_text(aes(label = name),         # 텍스트 표시
                   repel = T,                 # 노드밖 표시
                   size = 5,                  # 텍스트 크기
                   family = "nanumgothic") +  # 폰트
    
    theme_graph()+                             # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 키워드 동시출현빈도 네트워크")



ggsave(plot =pairwise_count_1, "pairwise_count_1.png",  width =1000, height = 700, units ="px", dpi = 100)








## centrality_edge_betweenness() 


centrality_edge_betweenness_1<- news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE) %>% 
    filter(str_length(word) > 1) %>% 
    pairwise_count(item = word,    
                   feature = rowid,
                   sort = T) %>% 
    filter(n >=280) %>% 
    as_tbl_graph(directed = F) %>% 
    activate(edges) %>% ## betweenness 하려면 edge랑 node active 해야함
    mutate(centrality = centrality_edge_betweenness(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           # group = as.factor(group_louvain())
    )  %>% 
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_diagonal(aes(edge_width = centrality, label = round(centrality, 2)), 
                       angle_calc = "along", alpha = 0.5, vjust = -1,
                       color = "lightblue") +
    geom_node_circle(aes(r = 0.2), fill = "lightblue", color = "navy") +
    geom_node_text(aes(label = name)) +
    coord_equal() +
    theme_graph()+              # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 centrality_edge_betweenness()  네트워크")


ggsave(plot =centrality_edge_betweenness_1, "centrality_edge_betweenness_1.png",  width =1200, height = 700, units ="px", dpi = 100)




## 상관계수 phi-coefficient 


phi_coefficient_1<- news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE) %>% 
    filter(str_length(word) > 1) %>%
    add_count(word) %>% 
    filter(n >=200) %>%   ## 절대적 기준 없음
    ## pairwise_count가 아니고 pairwise_cor 임
    pairwise_cor(item = word,      
                 feature = rowid,
                 sort = T) %>% 
    filter(correlation >= 0.20) %>% ## 이거 꼭 넣어야함 안 그럼 개판(절대적 기준 없음) 
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(
               group_louvain()
               #group_infomap()
           ))  %>%
    
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(color = "gray50",
                   aes(edge_alpha = correlation,   # 엣지 명암
                       edge_width = correlation),  # 엣지 두께
                   show.legend = F) +              # 범례 삭제
    scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
    
    geom_node_point(aes(size = centrality,
                        color = group,
                        alpha = .3),
                    show.legend = F) +
    scale_size(range = c(2, 10)) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 4,
                   family = "nanumgothic") +
    
    theme_graph()+                          # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 단어 간 상관관계(phi-coefficient) 네트워크")



ggsave(plot =phi_coefficient_1, "phi_coefficient_1.png",  width =1200, height = 700, units ="px", dpi = 100)




correlation_coef <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
correlation_coef

for (i in correlation_coef) {
    
    phi = news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE) %>% 
        filter(str_length(word) > 1) %>%
        add_count(word) %>% 
        filter(n >=200) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        pairwise_cor(item = word,      
                     feature = rowid,
                     sort = T) %>% 
        filter(correlation >= i) %>% ## 이거 꼭 넣어야함 안 그럼 개판(절대적 기준 없음) 
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(edge_alpha = correlation,   # 엣지 명암
                           edge_width = correlation),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group,
                            alpha = .3),
                        show.legend = F) +
        scale_size(range = c(2, 10)) +
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("1990-2023 신문 사설을 활용한 단어 간 상관관계(phi-coefficient) 네트워크", "(correlation coeff=", i, ")"))
    
    
    
    ggsave(plot = phi, file =paste0("phi_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}






























## 바이그램

ngram_1 <-news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE,
                  token ="ngrams", ### ngrams 토큰
                  n =2) %>%    ### n = 2 바이그램, n = 3 트라이그램
    separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
    count(word1, word2, sort = T) %>%
    na.omit() %>% 
    filter(n >=50) %>%   ## 절대적 기준 없음
    ## pairwise_count가 아니고 pairwise_cor 임
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(
               group_louvain()
               #group_infomap()
           ))  %>%
    
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(color = "gray50",
                   aes(color = "gray50",            # 엣지 색깔
                       alpha = 0.5),  # 엣지 두께
                   show.legend = F) +              # 범례 삭제
    scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
    
    geom_node_point(aes(size = centrality,
                        color = group),
                    show.legend = F) +
    scale_size(range = c(2, 7)) +
    
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 4,
                   family = "nanumgothic") +
    
    theme_graph()+                          # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 ngram 단어쌍 빈도 네트워크")

ggsave(plot =ngram_1, "ngram_1.png",  width =1200, height = 700, units ="px", dpi = 100)







ngram_coef <- c(50, 100, 150, 200, 250, 300, 350, 400)
ngram_coef

for (i in ngram_coef) {
    
    ngram =  news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE,
                      token ="ngrams", ### ngrams 토큰
                      n =2) %>%    ### n = 2 바이그램, n = 3 트라이그램
        separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
        count(word1, word2, sort = T) %>%
        na.omit() %>% 
        filter(n >= i ) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(color = "gray50",            # 엣지 색깔
                           alpha = 0.5),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group),
                        show.legend = F) +
        scale_size(range = c(2, 7)) +
        
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("바이그램(ngreams, n = 2) 네트워크", "(filter>=", i, ")"))
    
    
    
    ggsave(plot = ngram, file =paste0("ngram_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}





ngram_coef <- c(50, 100, 150, 200, 250, 300, 350, 400)
ngram_coef

for (i in ngram_coef) {
    
    bigram =  news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE,
                      token ="ngrams", ### ngrams 토큰
                      n =2) %>%    ### n = 2 바이그램, n = 3 트라이그램
        separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
        count(word1, word2, sort = T) %>%
        na.omit() %>% 
        filter(n >= i ) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(color = "gray50",            # 엣지 색깔
                           alpha = 0.5),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group),
                        show.legend = F) +
        scale_size(range = c(2, 7)) +
        
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("바이그램(n = 2) 네트워크", "(filter>=", i, ")"))
    
    
    
    ggsave(plot = bigram, file =paste0("bigram_n=2_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}



for (i in ngram_coef) {
    
    trigram =  news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE,
                      token ="ngrams", ### ngrams 토큰
                      n =3) %>%    ### n = 2 바이그램, n = 3 트라이그램
        separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
        count(word1, word2, sort = T) %>%
        na.omit() %>% 
        filter(n >= i ) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(color = "gray50",            # 엣지 색깔
                           alpha = 0.5),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group),
                        show.legend = F) +
        scale_size(range = c(2, 7)) +
        
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("트라이그램(n = 3) 네트워크", "(filter>=", i, ")"))
    
    
    
    ggsave(plot = trigram, file =paste0("trigram_n=3_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}









