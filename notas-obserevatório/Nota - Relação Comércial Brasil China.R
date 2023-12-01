# Pacotes / Diretorio 
library(ggplot2)
library(tidyverse)
library(treemapify)
library(scales)

setwd("C:\\Users\\user\\Desktop\\novo bd")


#### 1) Adicionando NCM em PORTUGUES, pci_db e filtrando para RMC == 1, CHINA == 160 ####
NCMz <- read.csv2("NCM_SH (1).csv")[,5:6 ] %>%
  rename(SH4 = CO_SH4) 
NCMz$SH4 <- as.factor(NCMz$SH4)

pci_db <- pci_db%>%
  left_join(NCMz)

sp_trade <- sp_trade %>%
  filter(RMC == 1) %>% 
  left_join(pci_db)

sp_trade <- sp_trade %>%
  left_join(co_pais)

sp_trade_geral <- sp_trade # sp_trade_geral = TODOS OS PAISES INCLUINDO CHINA

sp_trade <- sp_trade %>% # sp_trade APENAS COM CHINA
  filter(CO_PAIS == "160")

sp_trade2 <- sp_trade # Banco de dados backup APENAS CHINA


#### 2) Agrupando por CO_ANO, SH4, NO_SH4_POR, CO_PAIS e criando "Valor das Exp." e "Valor das Imp." para ano igual e superior a 2000 ####
sp_trade2 <- sp_trade2 %>% 
  group_by(CO_ANO, SH4, NO_SH4_POR, CO_PAIS, `Grau de Complexidade`)%>%
  summarise("Valor das Exp." =sum(VL_FOB_EXP,na.rm=T),
            "Valor das Imp." = sum(VL_FOB_IMP, na.rm=T))%>%
  ungroup()%>%
  select(CO_ANO, SH4, NO_SH4_POR,  `Valor das Exp.`, `Valor das Imp.`, `Grau de Complexidade`, CO_PAIS)


#### 3) Modelo Log-Lin para crescimento de EXP e IMP CHINA-RMC 2000-2022 ####
mod_log_lin <- sp_trade2 %>%
  group_by(CO_ANO) %>%
  summarise(totalexp = sum(`Valor das Exp.`),
            totalimp = sum(`Valor das Imp.`)) %>%
  ungroup()

mod_exp <- lm(log(totalexp) ~ CO_ANO, mod_log_lin)
summary(mod_exp) # 24.57


mod_imp <- lm(log(totalimp) ~ CO_ANO, mod_log_lin)
summary(mod_imp) # 32.05


# Variação acumulado de 2022 e 2021
var_acum_22_21 <- mod_log_lin %>%
  filter(CO_ANO %in% c(2021, 2022))

# ((45441043163/25439777388)-1)*100 = 78.62%
# ((2452004920/1683939118)-1)*100 = 45.61%

#### 4) Quanto a China representa das importações e exportações totais, bem como a evolução no tempo ####
# Exportação
repr_china_exp <- sp_trade_geral %>%
  group_by(CO_PAIS, CO_ANO) %>%
  summarise_at(c("VL_FOB_EXP"),sum,na.rm=TRUE) %>%
  pivot_wider(., 
              names_from = CO_ANO, 
              values_from = c(`VL_FOB_EXP`)) %>%
  ungroup() %>%
  mutate(País=CO_PAIS,
         `Participação Exp. 22`=round(`2022`/sum(`2022`,na.rm=T),4),
         `Participação Exp. 21`=round(`2021`/sum(`2021`,na.rm=T),4),
         `Participação Exp. 20`=round(`2020`/sum(`2020`,na.rm=T),4),
         `Participação Exp. 19`=round(`2019`/sum(`2019`,na.rm=T),4),
         `Participação Exp. 18`=round(`2018`/sum(`2018`,na.rm=T),4),
         `Participação Exp. 17`=round(`2017`/sum(`2017`,na.rm=T),4),
         `Participação Exp. 16`=round(`2016`/sum(`2016`,na.rm=T),4),
         `Participação Exp. 15`=round(`2015`/sum(`2015`,na.rm=T),4),
         `Participação Exp. 14`=round(`2014`/sum(`2014`,na.rm=T),4),
         `Participação Exp. 13`=round(`2013`/sum(`2013`,na.rm=T),4),
         `Participação Exp. 12`=round(`2012`/sum(`2012`,na.rm=T),4),
         `Participação Exp. 11`=round(`2011`/sum(`2011`,na.rm=T),4),
         `Participação Exp. 10`=round(`2010`/sum(`2010`,na.rm=T),4),
         `Participação Exp. 09`=round(`2009`/sum(`2009`,na.rm=T),4),
         `Participação Exp. 08`=round(`2008`/sum(`2008`,na.rm=T),4),
         `Participação Exp. 07`=round(`2007`/sum(`2007`,na.rm=T),4),
         `Participação Exp. 06`=round(`2006`/sum(`2006`,na.rm=T),4),
         `Participação Exp. 05`=round(`2005`/sum(`2005`,na.rm=T),4),
         `Participação Exp. 04`=round(`2004`/sum(`2004`,na.rm=T),4),
         `Participação Exp. 03`=round(`2003`/sum(`2003`,na.rm=T),4),
         `Participação Exp. 02`=round(`2002`/sum(`2002`,na.rm=T),4),
         `Participação Exp. 01`=round(`2001`/sum(`2001`,na.rm=T),4),
         `Participação Exp. 00`=round(`2000`/sum(`2000`,na.rm=T),4)) %>%
  select(País:ncol(.))

repr_china_exp <-repr_china_exp%>%
  filter(País == 160)

repr_china_exp <- as.data.frame(t(repr_china_exp))

repr_china_exp <- repr_china_exp[2:24, ]

repr_china_exp <- as.data.frame(repr_china_exp)

repr_china_exp <- repr_china_exp %>%
  mutate(year = c(2022:2000)) %>%
  select(year, `Variação Exp.` = repr_china_exp)

ggplot(repr_china_exp, aes(x = year, y = `Variação Exp.`))+
  geom_line()

# Importação
repr_china_imp <- sp_trade_geral %>%
  group_by(CO_PAIS, CO_ANO) %>%
  summarise_at(c("VL_FOB_IMP"),sum,na.rm=TRUE) %>%
  pivot_wider(., 
              names_from = CO_ANO, 
              values_from = c(`VL_FOB_IMP`)) %>%
  ungroup() %>%
  mutate(País=CO_PAIS,
         `Participação Imp. 22`=round(`2022`/sum(`2022`,na.rm=T),4),
         `Participação Imp. 21`=round(`2021`/sum(`2021`,na.rm=T),4),
         `Participação Imp. 20`=round(`2020`/sum(`2020`,na.rm=T),4),
         `Participação Imp. 19`=round(`2019`/sum(`2019`,na.rm=T),4),
         `Participação Imp. 18`=round(`2018`/sum(`2018`,na.rm=T),4),
         `Participação Imp. 17`=round(`2017`/sum(`2017`,na.rm=T),4),
         `Participação Imp. 16`=round(`2016`/sum(`2016`,na.rm=T),4),
         `Participação Imp. 15`=round(`2015`/sum(`2015`,na.rm=T),4),
         `Participação Imp. 14`=round(`2014`/sum(`2014`,na.rm=T),4),
         `Participação Imp. 13`=round(`2013`/sum(`2013`,na.rm=T),4),
         `Participação Imp. 12`=round(`2012`/sum(`2012`,na.rm=T),4),
         `Participação Imp. 11`=round(`2011`/sum(`2011`,na.rm=T),4),
         `Participação Imp. 10`=round(`2010`/sum(`2010`,na.rm=T),4),
         `Participação Imp. 09`=round(`2009`/sum(`2009`,na.rm=T),4),
         `Participação Imp. 08`=round(`2008`/sum(`2008`,na.rm=T),4),
         `Participação Imp. 07`=round(`2007`/sum(`2007`,na.rm=T),4),
         `Participação Imp. 06`=round(`2006`/sum(`2006`,na.rm=T),4),
         `Participação Imp. 05`=round(`2005`/sum(`2005`,na.rm=T),4),
         `Participação Imp. 04`=round(`2004`/sum(`2004`,na.rm=T),4),
         `Participação Imp. 03`=round(`2003`/sum(`2003`,na.rm=T),4),
         `Participação Imp. 02`=round(`2002`/sum(`2002`,na.rm=T),4),
         `Participação Imp. 01`=round(`2001`/sum(`2001`,na.rm=T),4),
         `Participação Imp. 00`=round(`2000`/sum(`2000`,na.rm=T),4)) %>%
  select(País:ncol(.))

repr_china_imp <-repr_china_imp%>%
  filter(País == 160)

repr_china_imp <- as.data.frame(t(repr_china_imp))

repr_china_imp <- repr_china_imp[2:24, ]

repr_china_imp <- as.data.frame(repr_china_imp)

repr_china_imp <- repr_china_imp %>%
  mutate(year = c(2022:2000)) %>%
  select(year, `Variação das Imp.` = repr_china_imp)

ggplot(repr_china_imp, aes(x = year, y = `Variação das Imp.`))+
  geom_line()

# Grafico representacao da china no total de exp e imp
repr_chinaIMP_EXP <- merge(repr_china_exp, repr_china_imp, by = "year", all = TRUE)


repr_chinaIMP_EXP2 <- repr_chinaIMP_EXP %>%
  gather(key = "Variaveis", value = "Participação do Total EXP/IMP", -year)

grafico_z <- ggplot(repr_chinaIMP_EXP2, aes(x = year, y = `Participação do Total EXP/IMP`)) + 
  geom_line(aes(color = Variaveis)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(labels = comma)

print(grafico_z + labs(
  x = "Ano", y = "Percentual do Total EXP/IMP"
))

ggsave("grafico_z.tiff", units = "in", width = 13, height = 6, dpi = 300, compression = 'lzw')

#### 5) Quanto a importação/exportação por grupo de complexidade representa do total de exportação/importação regional de cada grupo de complexidade (TODOS OS PAISES) ####
# Exportação
part_geral_complex_exp <- sp_trade_geral%>%
  filter(CO_ANO >= 2022)%>%
  mutate(`Grau de Complexidade`=replace_na(`Grau de Complexidade`,"IN"))%>%
  group_by(CO_ANO, `Grau de Complexidade`)%>%
  summarise("Valor das Exp."=sum(VL_FOB_EXP,na.rm=T))%>%
  pivot_wider(., 
              id_cols = `Grau de Complexidade`, 
              names_from = CO_ANO, 
              values_from = c(`Valor das Exp.`)) %>%
  ungroup() %>%
  mutate("Valor das Exp.z 22"=round(`2022`/1000000,2)) %>%
  mutate("% do Total 22z"=round(`2022`/sum(`2022`),4)) %>%
  select(`Grau de Complexidade`, `Valor das Exp.z 22`, `% do Total 22z`)

part_geral_complex_exp <- part_geral_complex_exp[-c(3), ]

# Importação
part_geral_complex_imp <- sp_trade_geral%>%
  filter(CO_ANO >= 2022)%>%
  mutate(`Grau de Complexidade`=replace_na(`Grau de Complexidade`,"IN"))%>%
  group_by(CO_ANO, `Grau de Complexidade`)%>%
  summarise("Valor das Imp."=sum(VL_FOB_IMP,na.rm=T))%>%
  pivot_wider(., 
              id_cols = `Grau de Complexidade`, 
              names_from = CO_ANO, 
              values_from = c(`Valor das Imp.`)) %>%
  ungroup() %>%
  mutate("Valor das Imp.z 22"=round(`2022`/1000000,2)) %>%
  mutate("% do Total 22z"=round(`2022`/sum(`2022`),4)) %>%
  select(`Grau de Complexidade`, `Valor das Imp.z 22`, `% do Total 22z`)

part_geral_complex_imp <- part_geral_complex_imp[-c(3), ]

balanca_geral <- merge(part_geral_complex_exp, part_geral_complex_imp, by = "Grau de Complexidade", all = TRUE)


#### 6) Comercio bilateral (CHINA E RMC APENAS)
# Exportação
part_bilat_compl_exp <- sp_trade%>%
  filter(CO_ANO >= 2022)%>%
  mutate(`Grau de Complexidade`=replace_na(`Grau de Complexidade`,"IN"))%>%
  group_by(CO_ANO, `Grau de Complexidade`)%>%
  summarise("Valor das Exp."=sum(VL_FOB_EXP,na.rm=T))%>%
  pivot_wider(., 
              id_cols = `Grau de Complexidade`, 
              names_from = CO_ANO, 
              values_from = c(`Valor das Exp.`)) %>%
  ungroup() %>%
  mutate("Valor das Exp. 22"=round(`2022`/1000000,2)) %>%
  mutate("% do Total 22"=round(`2022`/sum(`2022`),4)) %>%
  select(`Grau de Complexidade`, `Valor das Exp. 22`, `% do Total 22`)

part_bilat_compl_exp <- part_bilat_compl_exp[-c(3), ]

# Importação
part_bilat_compl_imp <- sp_trade%>%
  filter(CO_ANO >= 2022)%>%
  mutate(`Grau de Complexidade`=replace_na(`Grau de Complexidade`,"IN"))%>%
  group_by(CO_ANO, `Grau de Complexidade`)%>%
  summarise("Valor das Imp."=sum(VL_FOB_IMP,na.rm=T))%>%
  pivot_wider(., 
              id_cols = `Grau de Complexidade`, 
              names_from = CO_ANO, 
              values_from = c(`Valor das Imp.`)) %>%
  ungroup() %>%
  mutate("Valor das Imp. 22"=round(`2022`/1000000,2)) %>%
  mutate("% do Total 22"=round(`2022`/sum(`2022`),4)) %>%
  select(`Grau de Complexidade`, `Valor das Imp. 22`, `% do Total 22`)

part_bilat_compl_imp <- part_bilat_compl_imp[-c(3), ]

balanca <- merge(part_bilat_compl_exp, part_bilat_compl_imp, by = "Grau de Complexidade", all = TRUE)

# Participacao da china no total de exportação e importação por complexidade
participacao_total <- balanca_geral %>%
  left_join(balanca) %>%
  select(`Grau de Complexidade`, `Valor das Exp. Totais. 22.` = `Valor das Exp.z 22`, `Valor das Imp. Totais. 22.` = `Valor das Imp.z 22`, `Valor das Exp. Bilateral. 22.` = `Valor das Exp. 22`, `Valor das Imp. Bilateral. 22.` = `Valor das Imp. 22`)

participacao_total <- participacao_total %>%
  mutate(`Participação da China no Total EXP` = (((`Valor das Exp. Bilateral. 22.`)*100)/(`Valor das Exp. Totais. 22.`)))%>%
  mutate(`Participação da China no Total IMP` = (((`Valor das Imp. Bilateral. 22.`)*100)/(`Valor das Imp. Totais. 22.`)))


#### 7) Treemap exportações 2022 JANEIRO - JULHO ####
treemap_exp <- sp_trade2 %>%
  filter(CO_ANO == 2022) %>%
  mutate(`Grau de Complexidade` = case_when(`Grau de Complexidade` == "M?dia-alta" ~ "Média-alta",
                                            `Grau de Complexidade` == "M?dia-baixa" ~ "Média-baixa",
                                            `Grau de Complexidade` == "Alta" ~ "Alta",
                                            `Grau de Complexidade` == "Baixa" ~ "Baixa")) %>%
  drop_na()

ggplot(treemap_exp, aes(area= `Valor das Exp.`, fill = `Grau de Complexidade`, 
                        label = paste(`NO_SH4_POR`,paste(round((`Valor das Exp.`/sum(`Valor das Exp.`)*100), 2),"%"), sep="\n"), 
                        subgroup=`Grau de Complexidade`)) + 
  geom_treemap() + 
  geom_treemap_text(colour = "Black",
                    place = "centre",
                    size = 12, 
                    reflow = T,
                    grow = T)+
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position="bottom", 
        legend.title = element_text(size=9),
        legend.text = element_text(size = 7)) 


#### 8) Treemap importações 2022 JANEIRO - JULHO ####
treemap_imp <- sp_trade2 %>%
  filter(CO_ANO == 2022) %>%
  mutate(`Grau de Complexidade` = case_when(`Grau de Complexidade` == "M?dia-alta" ~ "Média-alta",
                                            `Grau de Complexidade` == "M?dia-baixa" ~ "Média-baixa",
                                            `Grau de Complexidade` == "Alta" ~ "Alta",
                                            `Grau de Complexidade` == "Baixa" ~ "Baixa")) %>%
  drop_na()

ggplot(treemap_imp, aes(area= `Valor das Imp.`, fill = `Grau de Complexidade`, 
                        label = paste(`NO_SH4_POR`,paste(round((`Valor das Imp.`/sum(`Valor das Imp.`)*100), 2),"%"), sep="\n"), 
                        subgroup=`Grau de Complexidade`)) + 
  geom_treemap() + 
  geom_treemap_text(colour = "Black",
                    place = "centre",
                    size = 12, 
                    reflow = T,
                    grow = T)+
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position="bottom", 
        legend.title = element_text(size=9),
        legend.text = element_text(size = 7))


#### 9) Dados dos municipios ####
options(scipen=999)
co_mun_uf <- read.csv("CO_UF_MUN.csv")

sp_trade_mun <- sp_trade %>%
  filter(CO_ANO == 2022) %>%
  group_by(CO_ANO, SH4, CO_PAIS, CO_MUN, NO_SH4_POR)%>%
  summarise("Valor das Exp." =sum(VL_FOB_EXP,na.rm=T),
            "Valor das Imp." = sum(VL_FOB_IMP, na.rm=T))%>%
  ungroup()%>%
  select(CO_ANO, SH4, NO_SH4_POR, `Valor das Exp.`, `Valor das Imp.`, `CO_MUN`)

sp_trade_mun <- sp_trade_mun %>%
  left_join(co_mun_uf, by=c("CO_MUN"="CO_MUN_GEO"))

sp_trade_mun2 <- sp_trade_mun

sp_trade_mun <- sp_trade_mun %>%
  group_by(SH4, NO_SH4_POR, CO_MUN, CO_ANO, NO_MUN) %>%
  summarise(exp1 = sum(`Valor das Exp.`),
            imp1 = sum(`Valor das Imp.`))%>%
  ungroup()%>%
  select(exp1, imp1, CO_MUN, NO_MUN, NO_SH4_POR, SH4)

# Participação em % por produto nas imp e exp
sp_trade_mun <- sp_trade_mun%>%
  mutate(partprodexp = exp1/sum(exp1)*100,
         partprodimp = imp1/sum(imp1)*100)

# Participação total dos municipios imp e exp
sp_trade_mun2 <- sp_trade_mun2 %>%
  select(CO_ANO, SH4, `Valor das Exp.`, `Valor das Imp.`, NO_MUN)  %>%
  group_by(NO_MUN) %>%
  summarize(totalexp = sum(`Valor das Exp.`),
            totalimp = sum(`Valor das Imp.`))

sp_trade_mun2 <- sp_trade_mun2 %>%
  mutate(`Participação Exportação em %` = totalexp/sum(totalexp)*100,
         `Participação Importação em %` = totalimp/sum(totalimp)*100)

# Grafico participacao do municipio na exportacao 
ggplot(sp_trade_mun2, aes(y = `Participação Exportação em %`, x = NO_MUN)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))

# Grafico participacao do municipio na importacao 
ggplot(sp_trade_mun2, aes(y = `Participação Importação em %`, x = NO_MUN)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))


#### 10) Exportação e Importação bilateral (CHINA-RMC) GERAL para 2000-2022 ####
geral_exp_imp <- sp_trade2 %>% 
  filter(CO_ANO >= 2000) %>%
  group_by(CO_ANO) %>%
  summarize("Exportação" = sum(`Valor das Exp.`),
            "Importação" = sum(`Valor das Imp.`)) %>%
  ungroup()

grafico_exp_imp_geral <- geral_exp_imp %>%
  gather(key = "Variaveis", value = "Valor FOB", -CO_ANO)

# Grafico geral EXP e IMP
grafico_1 <- ggplot(grafico_exp_imp_geral, aes(x = CO_ANO, y = `Valor FOB`)) + 
  geom_line(aes(color = Variaveis)) + 
  scale_color_manual(values = c("steelblue", "darkred")) +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(labels = comma)

print(grafico_1 + labs(
  x = "Ano", y = "Valor FOB em bilhões"
))


#### 11) Grafico exportacao por produtos mais exportados em 2022 em funcao do tempo ####
produtos_exp22 <- sp_trade2 %>%  # Utilizado apenas para selecionar os top 10 produtos utilizados no proximo passo
  filter(CO_ANO == 2022) %>%
  arrange(desc(`Valor das Exp.`))

produtos_exp22 <- sp_trade2%>%
  group_by(CO_ANO, SH4)%>%
  mutate(CO_ANO = as.numeric(as.character(CO_ANO)))%>%
  filter(SH4 == 8414|
           SH4 == 8502|
           SH4 == 8462|
           SH4 == 2905|
           SH4 == 8542)%>%
  #SH4 == 3004|
  #SH4 == 2907| 
  #SH4 == 3808|
  #SH4 == 2921|
  #SH4 == 8466|
  #SH4 == 8501|
  #SH4 == 3901|
  #SH4 == 8708)%>%
  mutate(SH4 = case_when(SH4 == 8414 ~ "Bombas de ar ou de vácuo, compressores de ar",
                         SH4 == 8502 ~ "Grupos eletrogêneos e conversores rotativos elétricos",
                         SH4 == 8462 ~ "Máquinas para trabalhar metais",
                         SH4 == 2905 ~ "Álcoois acíclicos e seus derivados",
                         SH4 == 8542 ~ "Circuitos integrados e microconjuntos electrónicos"))
#SH4 == 3004 ~ "Medicamentos",
#SH4 == 2907 ~ "Fenóis; fenóis-álcoois",
#SH4 == 3808 ~ "Inseticidas e defensivos agricolas",
#SH4 == 2921 ~ "Compostos de função amina",
#SH4 == 8466 ~ "Partes e acessórios para máquinas",
#SH4 == 8501 ~ "Motores e geradores",
#SH4 == 3901 ~ "Polímeros de etileno, em formas primárias",
#SH4 == 8708 ~ "Partes e acessórios dos veículos"))

# Grafico produtos EXPORTADOS 2000 - 2022 
grafico_2 <- ggplot(produtos_exp22, aes(x = CO_ANO, y = `Valor das Exp.`, color = SH4)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_y_continuous(labels = comma)

print(grafico_2 + labs(
  x = "Ano", y = "Valor FOB exportações em milhões", color = "Produto"
))


#### 12) Grafico importação por produtos mais importado em 2022 em funcao do tempo ####
produtos_imp22 <- sp_trade2 %>% # Utilizado apenas para selecionar os top 10 produtos utilizados no proximo passo
  filter(CO_ANO == 2022) %>%
  arrange(desc(`Valor das Imp.`))

produtos_imp22 <- sp_trade2 %>%
  group_by(CO_ANO, SH4) %>%
  mutate(CO_ANO == as.numeric(as.character(CO_ANO)))%>%
  filter(SH4 == 2933|
           SH4 == 2931|
           SH4 == 3808|
           SH4 == 8517|
           SH4 == 8541|
           SH4 == 8542)%>%
  #SH4 == 8473|
  #SH4 == 2932|
  #SH4 == 8471)%>%
  mutate(SH4 = case_when(SH4 == 2933 ~ "Compostos heterocíclicos",
                         SH4 == 2931 ~ "Outros compostos organo-inorgânicos",
                         SH4 == 3808 ~ "Insecticidas, defensivos agrícolas",
                         SH4 == 8517 ~ "Telefones",
                         SH4 == 8541 ~ "Diodos, transistores e dispositivos semelhantes a semicondutores",
                         SH4 == 8542 ~ "Circuitos integrados e microconjuntos eletrónicos"))
#SH4 == 8473 ~ "Partes e Acessórios de Veículos",
#SH4 == 2932 ~ "Compostos heterocíclicos, exclusivamente de hetero-átomo2",
#SH4 == 8471 ~ "Máquinas para processamento de dados"))

# Grafico produtos IMPORTADOS 2012 - 2022 
grafico_3 <- ggplot(produtos_imp22, aes(x = CO_ANO, y = `Valor das Imp.`, color = SH4)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))+
  scale_y_continuous(labels = comma)

print(grafico_3 + labs(
  x = "Ano", y = "Valor FOB importação em bilhões", color = "Produto"
))


#### 13) Grafico EXP e IMP geral por grau de complexidade 2008-2022 ####
imp_exp_geral_complex <- sp_trade %>%
  left_join(co_pais)%>%
  filter(CO_PAIS == "160",
         CO_ANO >= 2000)%>%
  group_by(CO_ANO, CO_PAIS, `Grau de Complexidade`)%>%
  summarise("Valor das Exp." =sum(VL_FOB_EXP,na.rm=T),
            "Valor das Imp." = sum(VL_FOB_IMP, na.rm=T))%>%
  ungroup()%>%
  drop_na()%>%
  select(CO_ANO, `Valor das Exp.`, `Valor das Imp.`, `Grau de Complexidade`)

imp_exp_geral_complex$`Grau de Complexidade` <- str_replace_all(imp_exp_geral_complex$`Grau de Complexidade`, '\\?', 'é')

# Grafico de EXP E GRAU DE COMPLEXIDADE 
grafico_4 <- ggplot(imp_exp_geral_complex, aes(x = CO_ANO, y = `Valor das Exp.`, color = `Grau de Complexidade`)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))+
  scale_y_continuous(labels = comma)

print(grafico_4 + labs(
  x = "Ano", y = "Valor FOB exportação em bilhões"
))

# Grafico IMP geral por grau de complexidade 
grafico_5 <- ggplot(imp_exp_geral_complex, aes(x = CO_ANO, y = `Valor das Imp.`, color = `Grau de Complexidade`)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))+
  scale_y_continuous(labels = comma)

print(grafico_5 + labs(
  x = "Ano", y = "Valor FOB exportação em bilhões"
))
