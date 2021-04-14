# Atenção: este arquivo foi feito para fins de estudos preliminares e precisa ser atualizado.

library(googlesheets)
library(ggplot2)
library(tidyverse)
library(lubridate)

# Importa dados das operações obtidos via LAI
dados_lai <- gs_title("SAER_LAI")
saer <- dados_lai %>% gs_read(ws = "4625-SAER_LAI") # com a divisão por dias 
saer17 <- dados_lai %>% gs_read(ws = "3075-SAER_LAI_2017")

################################## BY_HORA_INICIO ###############################s
saer17$dur <- as.numeric(difftime(saer17$HORA_TERMINO,saer17$HORA_INICIO), units = "mins")

legenda_dia <- glue::glue("Total of ", length(saer17$HORA_INICIO[!is.na(saer17$HORA_INICIO)])," operations with starting 
                          hours identified in 2017.")

hr_start <- hour(saer17$HORA_INICIO)[!is.na(saer17$HORA_INICIO)]

df_start <-data.frame(hr_start)

# Define o horário escolar
df_start$escolar <- df_start$hr_start %in% seq(6, 8)

saer_hora <- ggplot(df_start, aes(x = df_start$hr_start, fill = escolar)) + geom_bar(colour = "grey") + coord_polar(start = 0) + 
  theme_minimal() + 
  scale_fill_brewer(labels = c("No","Yes")) + ylab("Total of operations") + 
  ggtitle("Starting hour of operations with helicopters") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
  labs(fill = "School entrance time", caption = legenda_dia) + 
  theme(plot.title=element_text(family="Rajdhani", size=20, colour = "#d53228"),
        plot.subtitle = element_text(family="Roboto Mono", size=13, colour = "black"),
        plot.caption = element_text(family="Roboto Mono", size=10, colour = "black"),
        text = element_text(family="Roboto Mono"),
        panel.background = element_blank()) 


#Salva gráfico
ggsave(saer_hora,filename = "graficos/saer_hora_eng.png")


# Por duração
horas <- saer17$dur[!is.na(saer17$dur)]
length(horas[horas < 90])

hist_dura <-  qplot(horas,main = "Histograma com a duração das operações",ylab = "Total de operações",
                    xlab = "Duração da operação em minutos",xlim = c(0,600),ylim = c(0,25)) 

ggsave(hist_dura,filename = "graficos/saer_dura.png")


########################### COM DISPARO ############################
disparos <- dfgeral[dfgeral$DISPARO == 1,]  

#Configura campos
#disparos$DATA <- dmy(disparos$DATA)

rk_opmes <- disparos %>% filter(DATA > "2018-01-01") %>%  filter(DATA < "2020-01-01") %>% 
  group_by(trimestre = floor_date(DATA, unit = "month")) %>% summarise(total = sum(DISPARO))

rk_opmes$trimestre <- as.character(rk_opmes$trimestre)

tw$month <- as.character(tw$month)
rk_opmes <- as.data.frame(rk_opmes)

write_csv2(rk_opmes, path = 'twitter/rk_opmes.csv')

rk_opmes$trimestre <- as.character(rk_opmes$trimestre)

tw$month <- floor_date(tw$month, unit = "month")

merge(tw,rk_opmes,by.x = month, by.y = trimestre)

rk_opmes$trimestre <- as.factor(rk_opmes$trimestre)

por_trimestre <- ggplot(rk_opmes, aes(trimestre, total)) + 
  ggtitle("Indícios de disparos a partir de helicópteros (2018/2019) - Medialab/UFRJ") + 
  geom_line(aes(rk_opmes$trimestre,as.integer(rk_opmes$total), group = 1), stat = 'identity') + 
  geom_line(aes(y = tw$total), color="steelblue") +
  theme_minimal() + 
  scale_x_discrete() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.ticks.y = element_blank(),legend.position = "none")

por_trimestre


ggsave(por_trimestre,filename = "graficos/trimestre.png")

#Pega estatísticas
#Total
sum(freq$total)
#Média
mean(freq$total)
range(freq$total)

#Plota gráfico
serie <- ggplot(freq, aes(as.POSIXct(freq$mes),total)) +
  geom_line(group = 1) +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.ticks.y = element_blank(),legend.position = "none") +
  scale_x_datetime(labels = date_format("%m"), breaks = "2 month") +
  geom_smooth(method='lm',formula=y~x, se = FALSE, alpha = 0.1, size = 0.1, colour = "red") +
  #Anotações
  geom_vline(xintercept = as.POSIXct(freq$mes)[7], linetype = 3) +
  annotate("text", x = as.POSIXct(freq$mes)[7], y = 25, label = "07/2017") +  
  geom_vline(xintercept = as.POSIXct(freq$mes)[1], linetype = 3) +
  annotate("text", x = as.POSIXct(freq$mes)[1], y = 35, label = "01/2018") +  
  labs(y = "Total", x = "Mês")+
  annotate("text", x= as.POSIXct(freq$mes)[6], y = 40, label ="39 utilizações em junho de 2018", color = "red")
