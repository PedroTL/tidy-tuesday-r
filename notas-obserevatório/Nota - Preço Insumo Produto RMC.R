#### Pacotes ####
library(tidyverse)
library(ggplot2)
library(plm)

#### Filtrando para todos os anos 2012 - 2022 ####
# sp_trade tem que estar aberto (pegar da boletim)
CustoUniComer2_TodosAnos <- sp_trade %>%
  select("CO_ANO", "KG_LIQUIDO", "VL_FOB_IMP", "SH4", "RMC")%>%
  left_join(pci_db)%>%
  filter(CO_ANO < 2022)%>%
  group_by(CO_ANO, SH4, SH4_N)%>%
  summarize("Valor das Imp."= sum(VL_FOB_IMP, na.rm=T),
            "KG Liquido" = sum(KG_LIQUIDO, na.rm=T))%>%
  ungroup() %>%
  mutate(NCM=SH4,
         Produto=SH4_N, 
         `Imp./Kg` = `Valor das Imp.`/`KG Liquido`, 
         CO_ANO = as.numeric(CO_ANO))%>%
  select("CO_ANO", "NCM", "Produto", "Valor das Imp.", "KG Liquido", "Imp./Kg")

#### Outra tentativa com base 100 para os graficos ####
# Transformando em Painel
p_cut22 <- pdata.frame(CustoUniComer2_TodosAnos, index = c("Produto", "CO_ANO"))

p_cuc_ano2012 <- p_cut22%>%
  filter(CO_ANO == 2012)%>%
  select(Imp..Kg_2012 = Imp..Kg, NCM)

p_cut22 <- p_cut22%>%
  left_join(p_cuc_ano2012)%>%
  mutate(index = Imp..Kg/Imp..Kg_2012*100)

# Tirando de painel
p_cut22_edit <- data.frame(p_cut22)

##### Adicionando Filtro para os dados na base 100 p_cut22
p_cut22_graf1 <- p_cut22_edit%>%
  arrange(desc(Valor.das.Imp.))%>%
  filter(NCM == 2709|
           NCM == 2710|
           NCM == 8708|
           NCM == 8515|
           NCM == 3808|
           NCM == 8542|
           NCM == 3004|
           NCM == 2933|
           NCM == 3002|
           NCM == 8473|
           NCM == 8471|
           NCM == 8803|
           NCM == 8411|
           NCM == 8471|
           NCM == 8483|
           NCM == 8443)%>%
  mutate(NCM = case_when(NCM == 2709 ~ "Oleo Petroleo",
                         NCM == 2710 ~ "Oleo Petroleo com 70% ou mais petroleo",
                         NCM == 8708 ~ "Partes e acessorios de Veiculos",
                         NCM == 8515 ~ "Telefones",
                         NCM == 3808 ~ "Fertilizantes",
                         NCM == 8542 ~ "Circuitos eletronicos",
                         NCM == 3004 ~ "Medicamentos",
                         NCM == 2933 ~ "Composto Hetorociclico com nitrogeneo",
                         NCM == 3002 ~ "Vacinas, sangue",
                         NCM == 8473 ~ "Partes e acessorios",
                         NCM == 8471 ~ "Maquinas processamento de dados",
                         NCM == 8803 ~ "Parts of goods of headings",
                         NCM == 8411 ~ "Turbo-jets, Turbo-Propellers",
                         NCM == 8471 ~ "Maquinas de processamento de dados",
                         NCM == 8483 ~ "Transmission shaft",
                         NCM == 8443 ~ "Maquinas de impressao"))%>%
  mutate(CO_ANO = as.numeric(as.character(CO_ANO))) # Tive que adicionar o CO_ANO como numerico DEPOIS do filtro pois quando adicionava filtro ele transformava ano em fator novamente

#### Adicionando media ponderada para um novo grafico 2012 - 2021 ####
p_cut22_graf2 <- p_cut22_graf1%>%
  group_by(CO_ANO)%>%
  mutate(imp_ano = sum(Valor.das.Imp.),
         part = Valor.das.Imp./imp_ano) %>%
  summarise(media = weighted.mean(index,part))

#### Apenas os itens que apresentaram crescimento ####
p_cut22_graf6 <- p_cut22_edit%>%
  arrange(desc(Valor.das.Imp.))%>%
  filter(
    NCM == 8708|
      NCM == 8542|
      NCM == 2933|
      NCM == 3002|
      NCM == 8473)%>%
  mutate(NCM = case_when(
    NCM == 8708 ~ "Partes e acessorios de Veiculos",
    NCM == 8542 ~ "Circuitos eletronicos",
    NCM == 2933 ~ "Composto Hetorociclico com nitrogeneo",
    NCM == 3002 ~ "Vacinas, sangue",
    NCM == 8473 ~ "Partes e acessorios"))%>%
  mutate(CO_ANO = as.numeric(as.character(CO_ANO)))

#### Construindo indice index para 2022 com base os 3 meses 2021 ####
CustoUniComer2_TodosAnosT <- sp_trade %>%
  select("CO_ANO","CO_MES", "KG_LIQUIDO", "VL_FOB_IMP", "SH4", "RMC")%>%
  left_join(pci_db)%>%
  filter(CO_ANO >= 2021, CO_MES < 4)%>%
  group_by(CO_ANO, SH4, SH4_N)%>%
  summarize("Valor das Imp."= sum(VL_FOB_IMP, na.rm=T),
            "KG Liquido" = sum(KG_LIQUIDO, na.rm=T))%>%
  ungroup() %>%
  mutate(NCM=SH4,
         Produto=SH4_N, 
         `Imp./Kg` = `Valor das Imp.`/`KG Liquido`, 
         CO_ANO = as.numeric(CO_ANO))%>%
  select("CO_ANO", "NCM", "Produto", "Valor das Imp.", "KG Liquido", "Imp./Kg")

#### Transformando em painel para construir indice index 2022 com base nos 3 primeiros meses 2021 ####
# Transformando em Painel
p_cutz <- pdata.frame(CustoUniComer2_TodosAnosT, index = c("Produto", "CO_ANO")) # Duvida CO_ANO, CO_MES?

p_cuc_ano2021z <- p_cutz%>%
  filter(CO_ANO == 2021) %>% # Ano 2021 com os 3 primeiros meses apenas
  select(Imp..Kg_2021 = Imp..Kg, NCM) # Imp..Kg de 2021 3 primeiros meses apenas

p_cutz <- p_cutz%>%
  filter(CO_ANO == 2022)%>% # Apenas 2022 (3 primeiros meses do banco de dados)
  left_join(p_cuc_ano2021z)%>% # Dados dos 3 primeiros meses de 2021
  mutate(index2 = Imp..Kg/Imp..Kg_2021*100) # Index utilizando dados de 2022 sobre 2021 (apenas 3 primeiros meses)

# Tirando de painel
p_cutz_edit <- data.frame(p_cutz) 

#### Filtro Ultimo indice de 2022 com base nos 3 primeiros meses 2021 ####
p_cutz_graf10 <- p_cutz_edit%>%
  arrange(desc(Valor.das.Imp.))%>%
  filter(NCM == 2709|
           NCM == 2710|
           NCM == 8708|
           NCM == 8515|
           NCM == 3808|
           NCM == 8542|
           NCM == 3004|
           NCM == 2933|
           NCM == 3002|
           NCM == 8473|
           NCM == 8471|
           NCM == 8803|
           NCM == 8411|
           NCM == 8471|
           NCM == 8483|
           NCM == 8443)%>%
  mutate(NCM = case_when(NCM == 2709 ~ "Oleo Petroleo",
                         NCM == 2710 ~ "Oleo Petroleo com 70% ou mais petroleo",
                         NCM == 8708 ~ "Partes e acessorios de Veiculos",
                         NCM == 8515 ~ "Telefones",
                         NCM == 3808 ~ "Fertilizantes",
                         NCM == 8542 ~ "Circuitos eletronicos",
                         NCM == 3004 ~ "Medicamentos",
                         NCM == 2933 ~ "Composto Hetorociclico com nitrogeneo",
                         NCM == 3002 ~ "Vacinas, sangue",
                         NCM == 8473 ~ "Partes e acessorios",
                         NCM == 8471 ~ "Maquinas processamento de dados",
                         NCM == 8803 ~ "Parts of goods of headings",
                         NCM == 8411 ~ "Turbo-jets, Turbo-Propellers",
                         NCM == 8471 ~ "Maquinas de processamento de dados",
                         NCM == 8483 ~ "Transmission shaft",
                         NCM == 8443 ~ "Maquinas de impressao"))%>%
  mutate(CO_ANO = as.numeric(as.character(CO_ANO)))

#### Adicionando media ponderada de 2022 com base nos 3 primeiros meses de 2021 ####
p_cutz_graf11 <- p_cutz_graf10%>%
  group_by(CO_ANO)%>%
  mutate(imp_ano = sum(Valor.das.Imp.),
         part = Valor.das.Imp./imp_ano) %>%
  summarise(media = weighted.mean(index2,part))

#### Realizando a soma do indice de 2022 com base nos 3 primeiros meses de 2021 ao grafico que vai de 2012 - 2021 adicionando 2022 ####
p_cut22_graf2x <- p_cut22_graf2

mediap_cutz_graf11 <- p_cutz_graf11[1, 2] - 100
mediap_cut22_graf2 <- p_cut22_graf2x[10, 2]

soma <- mediap_cutz_graf11 + mediap_cut22_graf2


p_cut22_graf2xx = p_cut22_graf2x%>%
  rbind(c(2022, 129.9811))

#### Graficos utilizados para a nota ####
ggplot(p_cut22_graf6, aes(x = CO_ANO, y = index, color = NCM)) + # Grafico index top 5
  geom_line()

ggplot(p_cut22_graf2xx, aes(x = CO_ANO, y = media)) + # 
  geom_line() +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))
