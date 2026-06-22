## Script to mapping of urban segregation individual-based  using OD 2023



library(pacman)

p_load(haven, rlang, dplyr, tidyverse, foreign, scales, viridis, sf, janitor, 
       survey, ggpubr, car, rstatix, emmeans, ggplot2, knitr, kableExtra, htmltools, 
       Hmisc, broom, corrplot, ggridges, rmarkdown, rgdal, lubridate, reshape2, 
       patchwork, gapminder, PMCMRplus, officer, writexl, hms)



# INPUT DATA ----

OD_2022_original <- read_sav("G:/Meu Drive/UFABC/Pesquisa/Segregação Urbana - Pesquisa OD 2023/Site_190225_PesquisaOD2023/Site_190225/Banco2023_divulgacao_190225.sav")


OD_2022_original$per_capta <- OD_2022_original$renda_fa/OD_2022_original$no_moraf

## Definição permanência por atividade  ----

#Definição origem e destino por atividade ----

#colnames
colnames(OD_2022_original)


OD_2022 <- OD_2022_original %>% dplyr::select(zona, id_pess, fe_pess, f_pess,idade, 
                                              cd_ativi, n_viag, motivo_o, motivo_d, 
                                              servir_o, servir_d, h_saida, min_saida, h_cheg, 
                                              min_cheg, dia_sem, duracao )%>%
  mutate(n_viag_novo = n_viag - 1)

OD_2022$atividade <- OD_2022$motivo_d

OD_2022$origem <- OD_2022$motivo_o

OD_2022$atividade  <- factor(OD_2022$atividade,
                             levels = c(1:11),
                             labels = c("1",
                                        "1",
                                        "1",
                                        "4",
                                        "5",
                                        "6",
                                        "7",
                                        "8",
                                        "9",
                                        "10",
                                        "11"))


OD_2022$origem  <- factor(OD_2022$origem,
                          levels = c(1:11),
                          labels = c("1",
                                     "1",
                                     "1",
                                     "4",
                                     "5",
                                     "6",
                                     "7",
                                     "8",
                                     "9",
                                     "10",
                                     "11"))


OD_2022 <- OD_2022 %>% 
  mutate(
    saida   = hms::parse_hm(paste0(h_saida, ":", min_saida)),
    chegada = hms::parse_hm(paste0(h_cheg,  ":", min_cheg))
  )

OD_2022$CONCAT_D <- paste(OD_2022$id_pess, OD_2022$atividade, OD_2022$n_viag, sep = "_")
OD_2022$CONCAT_O <- paste(OD_2022$id_pess, OD_2022$origem, OD_2022$n_viag_novo, sep = "_")

OD_2022_CONCAT_D <- OD_2022 %>%
  select(zona, id_pess, fe_pess, f_pess,idade, cd_ativi, n_viag,origem, motivo_o, atividade, motivo_d, saida, chegada, CONCAT_D)

OD_2022_CONCAT_O <- OD_2022 %>%
  select(zona, id_pess, fe_pess, f_pess,idade, cd_ativi, n_viag,origem,motivo_o, atividade, motivo_d, saida, chegada, CONCAT_O)

OD_CONCAT <- dplyr::left_join(OD_2022_CONCAT_D, OD_2022_CONCAT_O, by=c("CONCAT_D"="CONCAT_O"))


#Exclui NA e filtra indivíduos maiores que 18 anos ----


OD_CONCAT$n_viag.x[is.na(OD_CONCAT$n_viag.x)] <- 0

OD_CONCAT_NULL <- OD_CONCAT[OD_CONCAT$n_viag.x==0,]

OD_CONCAT <- OD_CONCAT[!OD_CONCAT$n_viag.x ==0,]

OD_CONCAT<-OD_CONCAT%>%filter(OD_CONCAT$idade.x>=18)


#Tratamento do horario para cálculo da permanência ----



OD_CONCAT$permanencia <- as.numeric(OD_CONCAT$saida.y-OD_CONCAT$chegada.x, units="mins")

OD_CONCAT<-OD_CONCAT %>%
  mutate(diferença=if_else(as.POSIXct(OD_CONCAT$chegada.x)> as.POSIXct(OD_CONCAT$saida.y),1,0))


OD_CONCAT <- OD_CONCAT %>%
  mutate(
    saida_y_c = if_else(
      diferença == 1,
      hms::as_hms(saida.y + 24*60*60),
      saida.y
    )
  )



OD_CONCAT$permanencia_Total <- as.numeric(OD_CONCAT$saida_y_c-OD_CONCAT$chegada.x, units="mins")
#Se a chegada for maior que a saída → houve virada de dia.

OD_CONCAT <- OD_CONCAT %>%
  mutate(
    saida_y_c_posix = as.POSIXct(saida_y_c, origin = "1970-01-01", tz = "UTC"),
    chegada_x_posix = as.POSIXct(chegada.x, origin = "1970-01-01", tz = "UTC")
  )


OD_CONCAT <- OD_CONCAT %>%
  mutate(
    permanencia_Total = as.numeric(saida_y_c_posix - chegada_x_posix, units = "mins")
  )





# Definção atividade ----

OD_CONCAT$atividade<-OD_CONCAT$atividade.x


#Calcula permanências dia e noite ----

OD_CONCAT <- OD_CONCAT %>%
  mutate(
    # garantir objetos hms (apenas uma vez)
    chegada_h = hms::as_hms(chegada.x),
    saida_h   = hms::as_hms(saida.y),
    saida_h_c = hms::as_hms(saida_y_c),   # supondo que esta coluna já exista e seja hms
    
    # constantes em segundos (meia-noite + HH:MM:SS)
    s_06 = as.numeric(hms::as_hms("06:00:00")),
    s_18 = as.numeric(hms::as_hms("18:00:00")),
    
    # agora calcular Perm_Dia em minutos (sempre numeric)
    Perm_Dia = case_when(
      # 1) chegou >= 06:00 e saiu <= 18:00  -> conta tudo (saida - chegada)
      as.numeric(chegada_h) >= s_06 & as.numeric(saida_h) <= s_18 ~
        (as.numeric(saida_h) - as.numeric(chegada_h)) / 60,
      
      # 2) chegou durante o dia (>=06) e saiu depois das 18:00 -> conta até 18:00
      as.numeric(chegada_h) > s_06 & as.numeric(saida_h) > s_18 ~
        (s_18 - as.numeric(chegada_h)) / 60,
      
      # 3) chegou antes de 06:00 (madrugada) -> conta de 06:00 até a saída corrigida
      as.numeric(chegada_h) < s_06 ~
        (as.numeric(saida_h_c) - s_06) / 60,
      
      # caso padrão (nenhuma condição) -> 0 minutos
      TRUE ~ 0
    ),
    
    # remover negativos por segurança
    Perm_Dia = ifelse(Perm_Dia < 0, 0, Perm_Dia)
  ) %>%
  # opcional: remover colunas auxiliares se não quiser guardá-las
  select(-chegada_h, -saida_h, -saida_h_c, -s_06, -s_18)


  #calcular permanencia noite

OD_CONCAT <- OD_CONCAT %>%
  mutate(
    permanencia_Total = as.numeric(permanencia_Total),
    Perm_Dia = as.numeric(Perm_Dia),
    
    # cálculo da permanência noturna
    Perm_Noite = permanencia_Total - Perm_Dia,
    
    # garantir que não existam minutos negativos
    Perm_Noite = if_else(Perm_Noite < 0, 0, Perm_Noite)
  )


##Cálculo permanência de pessoas que chegam no destino e não faz mais nehuma viagem até o dia seguinte

#Identificar pessoas cuja permanência_total ficou NA

OD_CONCAT_C <- OD_CONCAT %>% 
  filter(is.na(permanencia_Total))


#2. Identificar tabela das primeiras viagens do dia

OD_CONCAT_B <- OD_CONCAT %>% 
  filter(n_viag.x == 1)

#3. Fazer o join pela pessoa

OD_CONCAT_N <- left_join(OD_CONCAT_C, OD_CONCAT_B, by = "id_pess.x")


#4. Converter corretamente as colunas duplicadas para POSIXct

OD_CONCAT_N <- OD_CONCAT_N %>% 
  mutate(
    chegada_ct = as.POSIXct(chegada.x.x),
    saida_ct   = as.POSIXct(saida.x.y)
  )


#5. Ajustar o caso em que a chegada ocorre após a saída (cruzou meia-noite)

OD_CONCAT_N <- OD_CONCAT_N %>%
  mutate(
    Dia = if_else(
      chegada_ct > saida_ct,
      saida_ct + lubridate::hours(24),  # virou madrugada
      saida_ct
    )
  )


#6. Recalcular a permanência total corretamente

OD_CONCAT_N <- OD_CONCAT_N %>%
  mutate(
    permanencia_Total = as.numeric(Dia - chegada_ct, units = "mins")
  )


#7. Ajustar atividade (caso precise consolidar)


OD_CONCAT_N$atividade <- OD_CONCAT_N$atividade.x.x.x



# cálculo permanencia

OD_CONCAT_N <- OD_CONCAT_N %>%
  mutate(
    # garantir objetos hms
    chegada_h = hms::as_hms(chegada.x.x),
    saida_h   = hms::as_hms(saida.y.x),
    saida_h_c = hms::as_hms(saida_y_c.x),  # ajuste se necessário
    
    # constantes em segundos
    s_06 = as.numeric(hms::as_hms("06:00:00")),
    s_18 = as.numeric(hms::as_hms("18:00:00")),
    
    # cálculo da permanência no dia
    Perm_Dia = case_when(
      # 1) chegou >= 06:00 e saiu <= 18:00  -> toda estadia está no dia
      as.numeric(chegada_h) >= s_06 & as.numeric(saida_h) <= s_18 ~
        (as.numeric(saida_h) - as.numeric(chegada_h)) / 60,
      
      # 2) chegou de dia (>=6) mas saiu depois das 18 → conta até 18:00
      as.numeric(chegada_h) >= s_06 & as.numeric(saida_h) > s_18 ~
        (s_18 - as.numeric(chegada_h)) / 60,
      
      # 3) chegou antes das 06 → conta de 06 até saída corrigida
      as.numeric(chegada_h) < s_06 ~
        (as.numeric(saida_h_c) - s_06) / 60,
      
      # fallback
      TRUE ~ 0
    ),
    
    # limpar negativos
    Perm_Dia = ifelse(Perm_Dia < 0, 0, Perm_Dia),
    
    # converter permanencia total para numeric
    permanencia_Total = as.numeric(permanencia_Total),
    
    # permanência noturna = total - diurna
    Perm_Noite = permanencia_Total - Perm_Dia,
    
    # limpar negativos
    Perm_Noite = ifelse(Perm_Noite < 0, 0, Perm_Noite)
  ) %>%
  # remover colunas auxiliares se quiser deixar limpo
  select(-chegada_h, -saida_h, -saida_h_c, -s_06, -s_18)






#Base dados permanências ----

# Bases normal e especial
Base_normal  <- OD_CONCAT %>% 
  select(id_pess.x, atividade, Perm_Dia, Perm_Noite)

Base_especial <- OD_CONCAT_N %>% 
  select(id_pess.x, atividade, Perm_Dia, Perm_Noite)

# Junta tudo
Base_calculos <- full_join(Base_normal, Base_especial,
                           by = c("id_pess.x", "atividade"))



# Escolhe Perm_Dia e Perm_Noite corretos (preferindo o normal)
Base_calculos <- Base_calculos %>%
  mutate(
    Perm_Dia   = coalesce(Perm_Dia.x,   Perm_Dia.y),
    Perm_Noite = coalesce(Perm_Noite.x, Perm_Noite.y)
  ) %>%
  select(id_pess.x, atividade, Perm_Dia, Perm_Noite)

# Somar permanências por pessoa/atividade
Base_calc_Dia <- Base_calculos %>%
  group_by(id_pess.x, atividade) %>%
  summarise(Perm_Dia = sum(Perm_Dia), .groups = "drop")

Base_calc_Noite <- Base_calculos %>%
  group_by(id_pess.x, atividade) %>%
  summarise(Perm_Noite = sum(Perm_Noite), .groups = "drop")

# Pegar atividade com maior permanência
Base_calc_Dia <- Base_calc_Dia %>%
  group_by(id_pess.x) %>%
  slice_max(Perm_Dia, n = 1)

Base_calc_Noite <- Base_calc_Noite %>%
  group_by(id_pess.x) %>%
  slice_max(Perm_Noite, n = 1)





#Permanência ao longo do dia

#Calcula a permanência no período do dia (entre 6am / 18pm) - De quem chegou em casa nesse período

OD_CONCAT_N <- OD_CONCAT_N%>%mutate(Perm_Dia1 =ifelse( hms::as_hms(OD_CONCAT_N$chegada.x.x) >= hms::as_hms("06:00:00") &  hms::as_hms(OD_CONCAT_N$chegada.x.x) <= hms::as_hms("18:00:00"),hms::as_hms("18:00:00")-hms::as_hms(OD_CONCAT_N$chegada.x.x), "X"))
OD_CONCAT_N$Perm_Dia1<-as.numeric(OD_CONCAT_N$Perm_Dia1)
OD_CONCAT_N$Perm_Dia1<-OD_CONCAT_N$Perm_Dia1/60 #valores da permanência em minutos
OD_CONCAT_N$Perm_Dia1[is.na(OD_CONCAT_N$Perm_Dia1)] <- 0


#Calcula a permanência no período do dia - De quem chegou a partir das 6h 

OD_CONCAT_N <-OD_CONCAT_N%>%mutate(Perm_Dia2 =ifelse(hms::as_hms(OD_CONCAT_N$Dia)>= hms::as_hms("06:00:00"), (hms::as_hms(OD_CONCAT_N$Dia)-hms::as_hms("06:00:00"))/60, 0))


OD_CONCAT_N <-OD_CONCAT_N%>%mutate(Perm_Dia = OD_CONCAT_N$Perm_Dia1+OD_CONCAT_N$Perm_Dia2)

OD_CONCAT_N$Perm_Dia2<-NULL
OD_CONCAT_N$Perm_Noite<-NULL

#Calcula a permanência no período do noite 

OD_CONCAT_N <-OD_CONCAT_N%>%mutate(Perm_Noite = OD_CONCAT_N$permanencia_Total - OD_CONCAT_N$Perm_Dia)

OD_CONCAT_N$Perm_Noite<-as.numeric(OD_CONCAT_N$Perm_Noite)






# Base dados permanências ----



Perm_Dia<-OD_CONCAT%>%select(id_pess.x, atividade,Perm_Dia,Perm_Noite)

Perm_Noite<-OD_CONCAT_N%>%select(id_pess.x, atividade,Perm_Dia,Perm_Noite)

Base_calculos<-full_join(Perm_Dia,Perm_Noite,  by = c("id_pess.x", "atividade"))

Base_calculos$Perm_Dia.x<-as.character (Base_calculos$Perm_Dia.x)
Base_calculos<-Base_calculos %>% replace_na(list(Perm_Dia.x = "X"))

Base_calculos$Perm_Dia.y<-as.character (Base_calculos$Perm_Dia.y)
Base_calculos$Perm_Dia.x<-if_else(Base_calculos$Perm_Dia.x == "X", Base_calculos$Perm_Dia.y, Base_calculos$Perm_Dia.x)

Base_calculos$Perm_Noite.x<-as.character (Base_calculos$Perm_Noite.x)
Base_calculos<-Base_calculos %>% replace_na(list(Perm_Noite.x = "X"))
Base_calculos$Perm_Noite.y<-as.character (Base_calculos$Perm_Noite.y)

Base_calculos$Perm_Noite.x<-if_else(Base_calculos$Perm_Noite.x == "X", Base_calculos$Perm_Noite.y, Base_calculos$Perm_Noite.x)

Base_calculos$Perm_Noite.y<-NULL
Base_calculos$Perm_Dia.y<-NULL

Base_calculos$Perm_Dia.x<-as.numeric (Base_calculos$Perm_Dia.x)

Base_calc_Dia<-Base_calculos %>% group_by(id_pess.x,atividade) %>% summarise(Perm_Dia = sum(Perm_Dia.x))

Base_calculos$Perm_Noite.x<-as.numeric (Base_calculos$Perm_Noite.x)

Base_calc_Noite<-Base_calculos %>% group_by(id_pess.x,atividade) %>% summarise(Perm_Noite = sum(Perm_Noite.x))

Base_calc_Dia <-Base_calc_Dia %>% slice(which.max(Perm_Dia))

Base_calc_Noite <-Base_calc_Noite %>% slice(which.max(Perm_Noite))


### Join permanencia atividades com características dos indivíduos


OD_processed_2022 <- OD_2022_original %>%
  select(zona, id_pessoa = id_pess, fator_exp = fe_pess, sit_fam, zona_orig = zona_o, 
         lon_orig = co_o_x, lat_orig =  co_o_y, zona_dest = zona_d, lon_dest = co_d_x, 
         lat_dest = co_d_y,
         mun_o = muni_o, mun_d = muni_d,ocup=DS_OCUP_TRAB1 ,motivo_o = motivo_o, motivo_d = motivo_d,
         h_saida, min_saida,h_cheg, min_cheg, dur_viag = duracao, n_viag,  tot_viag,classe_econ = criteriobr,
         renda = renda_fa,educacao = grau_ins, sexo ,raça ,modo_transp = modoprin,
         idade, cond_ativ = cd_ativi,trab1_re, trabext1, servir_o, servir_d,vinc1,per_capta, pe_bici) 


OD_processed_2022$n_viag[is.na(OD_processed_2022$n_viag)] <- 0
OD_processed_2022_NULL <- OD_processed_2022[OD_processed_2022$n_viag==0,]
OD_processed_2022 <- OD_processed_2022[!OD_processed_2022$n_viag==0,]
OD_processed_2022<-OD_processed_2022%>%filter(OD_processed_2022$idade>=18)

OD_processed_2022$atividade <- OD_processed_2022$motivo_d

OD_processed_2022$atividade <- factor(OD_processed_2022$atividade,
                                      levels = c(1:11),
                                      labels = c("1",
                                                 "1",
                                                 "1",
                                                 "4",
                                                 "5",
                                                 "6",
                                                 "7",
                                                 "8",
                                                 "9",
                                                 "10",
                                                 "11"))


#Excluir permanências maiores de 24h

Base_calc_Dia<-rename(Base_calc_Dia, id_pessoa=id_pess.x)

Base_calc_Noite<-rename(Base_calc_Noite, id_pessoa=id_pess.x)


Base_calc_Noite <- Base_calc_Noite %>%
  filter(Perm_Noite<= 1440)


#Join permanência com características dos indivíduos


Base_medidas_D<- dplyr::left_join(Base_calc_Dia, OD_processed_2022, by = c("id_pessoa", "atividade"))

Base_medidas_D <- Base_medidas_D[!(is.na(Base_medidas_D$Perm_Dia)),]

Base_medidas_N<- dplyr::left_join(Base_calc_Noite, OD_processed_2022, by = c("id_pessoa", "atividade"))

Base_medidas_N <- Base_medidas_N[!(is.na(Base_medidas_N$Perm_Noite)),]

Base_medidas_N<-Base_medidas_N %>% distinct(id_pessoa, atividade, .keep_all = TRUE)
Base_medidas_D<-Base_medidas_D %>% distinct(id_pessoa, atividade, .keep_all = TRUE)





## Renomear variáveis - Base Medidas


Base_medidas_D$classe_econ <- factor(Base_medidas_D$classe_econ,
                                     levels = c(0:6),
                                     labels = c("SC",
                                                "A",
                                                "B1", 
                                                "B2",
                                                "C1",
                                                "C2",
                                                "DE"))

Base_medidas_D$sexo <- factor(Base_medidas_D$sexo,
                              levels = c(1:2),
                              labels = c("Masculino",
                                         "Feminino"))

Base_medidas_D$raça <- factor(Base_medidas_D$raça,
                              levels = c(1:6),
                              labels = c("Branca",
                                         "Preta",
                                         "Amarela",
                                         "Parda",
                                         "Indígena",
                                        "Sem declaração"))


Base_medidas_D$educacao <- factor(Base_medidas_D$educacao,
                                  levels = c(1:5),
                                  labels = c("Não-Alfabetizado/Fundamental I Incompleto",
                                             "Fundamental I Completo/Fundamental II Incompleto",
                                             "Fundamental II Completo/Médio Incompleto",
                                             "Médio Completo/Superior Incompleto",
                                             "Superior Completo"))

Base_medidas_D$cond_ativ <- factor(Base_medidas_D$cond_ativ,
                                   levels = c(1:8),
                                   labels = c("Tem trabalho regular",
                                              "Faz bico",
                                              "Em Licença Médica",
                                              "Aposentado/Pensionista",
                                              "Sem Trabalho",
                                              "Nunca Trabalhou",
                                              "Dona de Casa",
                                              "Estudante"))



Base_medidas_D$motivo_o <- factor(Base_medidas_D$motivo_o,
                                  levels = c(1:11),
                                  labels = c("Trabalho Industria",
                                             "Trabalho Comercio",
                                             "Trabalho Servicos", 
                                             "Escola",
                                             "Compras",
                                             "Saude",
                                             "Lazer",
                                             "Residencia",
                                             "Procurar Emprego",
                                             "Assuntos Pessoais",
                                             "Refeicao"))

Base_medidas_D$motivo_d <- factor(Base_medidas_D$motivo_d,
                                  levels = c(1:11),
                                  labels = c("Trabalho Industria",
                                             "Trabalho Comercio",
                                             "Trabalho Servicos", 
                                             "Escola",
                                             "Compras",
                                             "Saude",
                                             "Lazer",
                                             "Residencia",
                                             "Procurar Emprego",
                                             "Assuntos Pessoais",
                                             "Refeicao"))

Base_medidas_D$atividade <- factor(Base_medidas_D$atividade,
                                   levels = c(1:11),
                                   labels = c("Trabalho",
                                              "Trabalho",
                                              "Trabalho", 
                                              "Escola",
                                              "Compras",
                                              "Saude",
                                              "Lazer",
                                              "Residencia",
                                              "Procurar Emprego",
                                              "Assuntos Pessoais",
                                              "Refeicao"))


Base_medidas_D$modo_transp <- factor(Base_medidas_D$modo_transp,
                                     levels = c(1:18),
                                     labels = c("Metrô",
                                                "Trem",
                                                "Monotrilho",
                                                "Ônibus/micro-ônibus/van do município de São Paulo",
                                                "Ônibus/micro-ônibus/van de outros municípios",
                                                "Ônibus/micro-ônibus/van metropolitano",
                                                "Transporte Fretado",
                                                "Transporte Escolar",
                                                "Dirigindo Automóvel",
                                                "Passageiro de Automóvel",
                                                "Táxi Convencional",
                                                "Táxi não Convencional / aplicativo",
                                                "Dirigindo Moto",
                                                "Passageiro de Moto",
                                                "Passageiro de Mototáxi",
                                                "Bicicleta",
                                                "A Pé",
                                                "Outros" ))


Base_medidas_D$sit_fam <- factor(Base_medidas_D$sit_fam,
                                 levels = c(1:7),
                                 labels = c("Pessoa Resp",
                                            "Cônjuge",
                                            "Filho",
                                            "Outro Parente",
                                            "Agregado",
                                            "Empregado Res",
                                            "Parente do Empregado Res"))

Base_medidas_D$trab1_re <- factor(Base_medidas_D$trab1_re,
                                  levels = c(1:3),
                                  labels = c("Sim",
                                             "Não",
                                             "Sem endereço fixo")) 



Base_medidas_D$vinc1 <- factor(Base_medidas_D$vinc1,
                               levels = c(1:9),
                               labels = c("Assalariado com carteira",
                                          "Assalariado sem carteira",
                                          "Funcionário público",
                                          "Profissional liberal",
                                          "Autônomo",
                                          "Autônomo",
                                          "Empregador",
                                          "Dono de negócio familiar",
                                          "Trabalhor familiar")) 


Base_medidas_D$pe_bici <- factor(Base_medidas_D$pe_bici,
                               levels = c(1:9),
                               labels = c("Pequena distância",
                                          "Condução cara",
                                          "Ponto/Estação distante",
                                          "Condução demora para passar",
                                          "Viagem demorada",
                                          "Condução lotada",
                                          "Atividade física",
                                          "Medo de contágio",
                                          "Outros motivos"
                               ))

#filtro idade:

Base_medidas_D$F_etaria<-ifelse(Base_medidas_D$idade <= 24,"Jovem","nao")
Base_medidas_D$F_etaria<-ifelse(Base_medidas_D$idade >= 25 & Base_medidas_D$idade <=64 ,"Adulta",Base_medidas_D$F_etaria)
Base_medidas_D$F_etaria<-ifelse(Base_medidas_D$idade >= 65,"Idosa",Base_medidas_D$F_etaria)


## Renomear variáveis - Base Medidas (Noite)

Base_medidas_N$classe_econ <- factor(Base_medidas_N$classe_econ,
                                     levels = c(0:6),
                                     labels = c("SC",
                                                "A",
                                                "B1", 
                                                "B2",
                                                "C1",
                                                "C2",
                                                "DE"))

Base_medidas_N$sexo <- factor(Base_medidas_N$sexo,
                              levels = c(1:2),
                              labels = c("Masculino",
                                         "Feminino"))

Base_medidas_N$raça <- factor(Base_medidas_N$raça,
                              levels = c(1:6),
                              labels = c("Branca",
                                         "Preta",
                                         "Amarela",
                                         "Parda",
                                         "Indígena",
                                         "Sem declaração"))

Base_medidas_N$educacao <- factor(Base_medidas_N$educacao,
                                  levels = c(1:5),
                                  labels = c("Não-Alfabetizado/Fundamental I Incompleto",
                                             "Fundamental I Completo/Fundamental II Incompleto",
                                             "Fundamental II Completo/Médio Incompleto",
                                             "Médio Completo/Superior Incompleto",
                                             "Superior Completo"))

Base_medidas_N$cond_ativ <- factor(Base_medidas_N$cond_ativ,
                                   levels = c(1:8),
                                   labels = c("Tem trabalho regular",
                                              "Faz bico",
                                              "Em Licença Médica",
                                              "Aposentado/Pensionista",
                                              "Sem Trabalho",
                                              "Nunca Trabalhou",
                                              "Dona de Casa",
                                              "Estudante"))

Base_medidas_N$motivo_o <- factor(Base_medidas_N$motivo_o,
                                  levels = c(1:11),
                                  labels = c("Trabalho Industria",
                                             "Trabalho Comercio",
                                             "Trabalho Servicos", 
                                             "Escola",
                                             "Compras",
                                             "Saude",
                                             "Lazer",
                                             "Residencia",
                                             "Procurar Emprego",
                                             "Assuntos Pessoais",
                                             "Refeicao"))

Base_medidas_N$motivo_d <- factor(Base_medidas_N$motivo_d,
                                  levels = c(1:11),
                                  labels = c("Trabalho Industria",
                                             "Trabalho Comercio",
                                             "Trabalho Servicos", 
                                             "Escola",
                                             "Compras",
                                             "Saude",
                                             "Lazer",
                                             "Residencia",
                                             "Procurar Emprego",
                                             "Assuntos Pessoais",
                                             "Refeicao"))

Base_medidas_N$atividade <- factor(Base_medidas_N$atividade,
                                   levels = c(1:11),
                                   labels = c("Trabalho",
                                              "Trabalho",
                                              "Trabalho", 
                                              "Escola",
                                              "Compras",
                                              "Saude",
                                              "Lazer",
                                              "Residencia",
                                              "Procurar Emprego",
                                              "Assuntos Pessoais",
                                              "Refeicao"))

Base_medidas_N$modo_transp <- factor(Base_medidas_N$modo_transp,
                                     levels = c(1:18),
                                     labels = c("Metrô",
                                                "Trem",
                                                "Monotrilho",
                                                "Ônibus/micro-ônibus/van do município de São Paulo",
                                                "Ônibus/micro-ônibus/van de outros municípios",
                                                "Ônibus/micro-ônibus/van metropolitano",
                                                "Transporte Fretado",
                                                "Transporte Escolar",
                                                "Dirigindo Automóvel",
                                                "Passageiro de Automóvel",
                                                "Táxi Convencional",
                                                "Táxi não Convencional / aplicativo",
                                                "Dirigindo Moto",
                                                "Passageiro de Moto",
                                                "Passageiro de Mototáxi",
                                                "Bicicleta",
                                                "A Pé",
                                                "Outros" ))

Base_medidas_N$sit_fam <- factor(Base_medidas_N$sit_fam,
                                 levels = c(1:7),
                                 labels = c("Pessoa Resp",
                                            "Cônjuge",
                                            "Filho",
                                            "Outro Parente",
                                            "Agregado",
                                            "Empregado Res",
                                            "Parente do Empregado Res"))

Base_medidas_N$trab1_re <- factor(Base_medidas_N$trab1_re,
                                  levels = c(1:3),
                                  labels = c("Sim",
                                             "Não",
                                             "Sem endereço fixo")) 

Base_medidas_N$vinc1 <- factor(Base_medidas_N$vinc1,
                               levels = c(1:9),
                               labels = c("Assalariado com carteira",
                                          "Assalariado sem carteira",
                                          "Funcionário público",
                                          "Profissional liberal",
                                          "Autônomo",
                                          "Autônomo",
                                          "Empregador",
                                          "Dono de negócio familiar",
                                          "Trabalhor familiar")) 

Base_medidas_N$pe_bici <- factor(Base_medidas_N$pe_bici,
                                 levels = c(1:9),
                                 labels = c("Pequena distância",
                                            "Condução cara",
                                            "Ponto/Estação distante",
                                            "Condução demora para passar",
                                            "Viagem demorada",
                                            "Condução lotada",
                                            "Atividade física",
                                            "Medo de contágio",
                                            "Outros motivos"
                                 ))

# Faixa etária
Base_medidas_N$F_etaria <- ifelse(Base_medidas_N$idade <= 24, "Jovem", "nao")
Base_medidas_N$F_etaria <- ifelse(Base_medidas_N$idade >= 25 & Base_medidas_N$idade <= 64, "Adulta", Base_medidas_N$F_etaria)
Base_medidas_N$F_etaria <- ifelse(Base_medidas_N$idade >= 65, "Idosa", Base_medidas_N$F_etaria)



##Cálculo segregação - Períodos (Dia x Noite)

#Define proporção de indivíduos residentes na área de estudo \> de 18 anos


#VERSAO GPT

## Cálculo segregação - Períodos (Dia x Noite)

# Define proporção de indivíduos residentes na área de estudo > 18 anos
OD_razao_2022 <- OD_processed_2022 %>% 
  filter(idade >= 18)

# Mantém apenas um registro por indivíduo
OD_razao_2022_filter <- distinct(OD_razao_2022, id_pessoa, .keep_all = TRUE)


# ---------------------------------------------------------------------------
# Cálculo do FPC (tamanho real da população por zona, estimada pelos pesos)
# ---------------------------------------------------------------------------

fpc22_res <- aggregate(
  fator_exp ~ zona,
  data = OD_razao_2022_filter,
  FUN = "sum"
)

# Renomeia corretamente as colunas, sem erros
names(fpc22_res) <- c("zona", "fpc")  


# Verifica estrutura da coluna (agora existe)
str(fpc22_res$fpc)


# ---------------------------------------------------------------------------
# Junta FPC à base principal
# ---------------------------------------------------------------------------

od2_22_res <- OD_razao_2022_filter %>% 
  inner_join(fpc22_res, by = "zona")


# ---------------------------------------------------------------------------
# Ajuste do tipo da classe econômica
# ---------------------------------------------------------------------------

od2_22_res$classe_econ <- as.numeric(od2_22_res$classe_econ)

any(is.na(od2_22_res$classe_econ))


#Define o desenho amostral

#names(OD_processed_2022)
#names(OD_razao_2022_filter)
#names(od2_22_res)



# DESENHO AMOSTRAL
# ---------------------------------------------------------------------------

od3_22_res <- svydesign(
  data   = od2_22_res,
  ids    = ~1,
  strata = ~zona,
  fpc    = ~fpc,          # corrigido
  weights = ~fator_exp
)

# ---------------------------------------------------------------------------
# TABELA DE FREQUÊNCIAS PONDERADAS (zona x classe econômica)
# ---------------------------------------------------------------------------

class_22_res <- as_tibble(
  svytable(~ zona + classe_econ, od3_22_res)
)

class_22_res$zona         <- as.numeric(class_22_res$zona)
class_22_res$classe_econ  <- as.numeric(class_22_res$classe_econ)


# ---------------------------------------------------------------------------
# TABELA EM FORMATO LARGO
# ---------------------------------------------------------------------------

tabela_class_22_res <- class_22_res %>% 
  spread(classe_econ, n)


# ---------------------------------------------------------------------------
# RENOMEAR AS CLASSES (A, B1, B2, C1, C2, DE)
# ---------------------------------------------------------------------------

names(tabela_class_22_res)[2:7] <- c(
  "Dia_A", 
  "Dia_B1", 
  "Dia_B2", 
  "Dia_C1", 
  "Dia_C2", 
  "Dia_DE"
)


# ---------------------------------------------------------------------------
# ADICIONAR TOTAL POR CLASSE
# ---------------------------------------------------------------------------

Tabela_totais_res <- adorn_totals(
  tabela_class_22_res,
  where = "col",
  name = "Dia_Total"
)

sum(Tabela_totais_res$Dia_Total) #9935274


#Proporcao grupos area de estudo 

Tabela_prop_res<- colSums(Tabela_totais_res[,-1])
classes_22 <- data.frame(Tabela_prop_res)


classes_22$razao <- classes_22$Tabela_prop_res/9935274
                                                

classes_22["%"]<-percent(classes_22$razao)

#classes_22 <- classes_22 %>% slice(-1)

summary(OD_razao_2022_filter)



#Definição da população presente em cada zona - Dia ----


## Definição da população presente em cada zona - Dia


fpci22_Dia <- aggregate(fator_exp ~ zona_dest, data =Base_medidas_D, FUN = "sum")      

names(fpci22_Dia) <- c("zona_dest", "fpci22_Dia") 

od2_22_Dia <- Base_medidas_D %>% inner_join(fpci22_Dia, by = "zona_dest")  


od3_22_Dia <- svydesign(data = od2_22_Dia,
                        ids = ~1,
                        strata = ~zona_dest,
                        fpc = ~fpci22_Dia,
                        weights = ~fator_exp)

Classes_Dia <- as_tibble(svytable(~ zona_dest + classe_econ, od3_22_Dia))

Classes_Dia$zona_dest <- as.numeric(Classes_Dia$zona_dest)  

Tabela_classes_Dia <-  Classes_Dia %>% spread(classe_econ, n)  

names(Tabela_classes_Dia)[2:8] <- c("Dia_A","Dia_B1", "Dia_B2","Dia_C1","Dia_C2","Dia_DE","Dia_SC") 

Totais_classes_Dia<-adorn_totals(Tabela_classes_Dia, where = c("col"), name = "Dia_Total")

sum(Totais_classes_Dia$Dia_Total)

#Proporcao grupos area de estudo 

Tabela_prop_Dia<- colSums(Totais_classes_Dia[,-1])
classes_Dia<-data.frame(Tabela_prop_Dia)


classes_Dia$razao <- classes_Dia$Tabela_prop_Dia/9935274

classes_Dia["%"]<-percent(classes_Dia$razao)

Classe_grafico<-Classes_Dia


#Uni população presentes nas zonas de maior permanência durante o dia

Base_calculo_D<- Base_medidas_D %>% inner_join(Totais_classes_Dia, by = "zona_dest")



#Índice exposição - Dia


options(scipen=999)
indice_Dia <- Base_calculo_D %>%
  select(-Dia_SC)


grupos <- c("A", "B1", "B2", "C1", "C2", "DE") 

for (i in grupos) {
  k <- paste0("Dia_", i)
  for (j in grupos) {
    l <- paste0("Dia_", j)
    indice_Dia[[paste0("Exp_", i, "_", j)]] <- ifelse(indice_Dia[["classe_econ"]] == i, (indice_Dia[[l]]/indice_Dia[["Dia_Total"]]), NA)
  }
}  

#Índice normalizado


indice_Dia_norm <- Base_calculo_D  %>%
  select(-Dia_SC)

grupos <- c("A", "B1", "B2", "C1", "C2", "DE") 


for (i in grupos) {
  k <- paste0("Dia_", i)
  for (j in grupos) {
    l <- paste0("Dia_", j)
    razao <- classes_22 %>% mutate(name = rownames(.)) %>% filter(name == paste0("Dia_", j))
    indice_Dia_norm[[paste0("Exp_", i, "_", j)]] <- ifelse(indice_Dia_norm[["classe_econ"]] == i, (indice_Dia_norm[[l]]/indice_Dia_norm[["Dia_Total"]]/razao[["razao"]]), NA)
  }
}  







#Definição da população presente em cada zona - Noite    


## Definição da população presente em cada zona - Noite ----

fpci22_Noite <- aggregate(fator_exp ~ zona_dest, data =Base_medidas_N, FUN = "sum")      

names(fpci22_Noite) <- c("zona_dest", "fpci22_Noite") 

od2_22_Noite <- Base_medidas_N %>% inner_join(fpci22_Noite, by = "zona_dest")  


od3_22_Noite <- svydesign(data = od2_22_Noite,
                          ids = ~1,
                          strata = ~zona_dest,
                          fpc = ~fpci22_Noite,
                          weights = ~fator_exp)


Classes_Noite <- as_tibble(svytable(~ zona_dest + classe_econ, od3_22_Noite))

Classes_Noite$zona_dest <- as.numeric(Classes_Noite$zona_dest)  



Tabela_classes_Noite<-  Classes_Noite %>% spread(classe_econ, n)  


names(Tabela_classes_Noite)[2:8] <- c("Noite_A","Noite_B1", "Noite_B2","Noite_C1","Noite_C2","Noite_DE","Noite_SC") 

Tabela_classes_Noite <- Tabela_classes_Noite %>%
  select(-Noite_SC)

Totais_classes_Noite<-adorn_totals(Tabela_classes_Noite, where = c("col"), name = "Noite_Total")

#Uni população presentes nas zonas de maior permanência durante a noite

Base_calculo_N<- Base_medidas_N %>% inner_join(Totais_classes_Noite, by = "zona_dest")


#Proporcao grupos area de estudo 

Tabela_prop_Noite<- colSums(Totais_classes_Noite[,-1])
classes_Noite<-data.frame(Tabela_prop_Noite)
view(Totais_classes_Noite)

classes_Noite$razao <- classes_Noite$Tabela_prop_Noite/9932837.9
classes_Noite["%"]<-percent(classes_Noite$razao)



#Índice exposição - Noite

options(scipen=999)
indice_Noite <- Base_calculo_N

grupos <- c("A", "B1", "B2", "C1", "C2", "DE") 

for (i in grupos) {
  k <- paste0("Noite_", i)
  for (j in grupos) {
    l <- paste0("Noite_", j)
    indice_Noite[[paste0("Exp_", i, "_", j)]] <- ifelse(indice_Noite[["classe_econ"]] == i, (indice_Noite[[l]]/indice_Noite[["Noite_Total"]]), NA)
  }
}  

indice_Noite [sapply(indice_Noite, is.infinite)] <- NA



#Índice normalizado


indice_Noite_norm <- Base_calculo_N   

grupos <- c("A", "B1", "B2", "C1", "C2", "DE")  

for (i in grupos) {
  k <- paste0("Noite_", i)
  for (j in grupos) {
    l <- paste0("Noite_", j)
    razao <- classes_22 %>% mutate(name = rownames(.)) %>% filter(name == paste0("Dia_", j))
    indice_Noite_norm[[paste0("Exp_", i, "_", j)]] <- ifelse(indice_Noite_norm[["classe_econ"]] == i, (indice_Noite_norm[[l]]/indice_Noite_norm[["Noite_Total"]]/razao[["razao"]]), NA)
  }
}  

indice_Noite_norm [sapply(indice_Noite_norm, is.infinite)] <- NA


classes_Noite$razao <- classes_Noite$Tabela_prop_Noite/9932837.9





##
#Análise descritiva



indice_Noite_norm$Local<- ifelse(indice_Noite_norm$zona_dest == indice_Noite_norm$zona,"Sim","Não")

install.packages("table1")
library(table1)

#table1::label(indice_Noite_norm$sexo) <- "Gênero"
#table1::label(indice_Noite_norm$F_etaria) <- "Grupo etário"
#table1::label(indice_Noite_norm$sit_fam) <- "Situação familiar"
#table1::label(indice_Noite_norm$educacao) <-"Escolaridade"
#table1::label(indice_Noite_norm$cond_ativ) <- "Condição de trabalho"
#table1::label(indice_Noite_norm$vinc1) <- "Vínculo empregatício"
#table1::label(indice_Noite_norm$ocup) <- "Ocupação"
#table1::label(indice_Noite_norm$renda) <- "Renda familiar"
#table1::label(indice_Noite_norm$per_capta) <- "Renda familiar per capita"
#table1::label(indice_Noite_norm$atividade) <-"Atividade"
#table1::label(indice_Noite_norm$Local) <- "Local atividade igual domicílio"
#table1::label(indice_Noite_norm$trab1_re) <- "Trabalho igual residência"

table1::table1(~ sexo + F_etaria+ sit_fam + educacao + cond_ativ + vinc1 + ocup + renda + per_capta + atividade + Local +  trab1_re |classe_econ, data = indice_Noite_norm,
               render.continuous=c(.="Mean", .="Median", .="Min", .="Max",
                                   .="Q1", .="Q3", .="IQR"))

# Dia

indice_Dia_norm$Local<- ifelse(indice_Dia_norm$zona_dest == indice_Dia_norm$zona,"Sim","Não")
table1::label(indice_Dia_norm$sexo) <- "Sexo"
table1::label(indice_Dia_norm$F_etaria) <- "Grupo etário"
table1::label(indice_Dia_norm$sit_fam) <- "Situação familiar"
table1::label(indice_Dia_norm$educacao) <-"Escolaridade"
table1::label(indice_Dia_norm$cond_ativ) <- "Condição de trabalho"
table1::label(indice_Dia_norm$vinc1) <- "Vínculo empregatício"
table1::label(indice_Dia_norm$ocup) <- "Ocupação"
table1::label(indice_Dia_norm$renda) <- "Renda familiar"
table1::label(indice_Dia_norm$per_capta) <- "Renda familiar per capita"
table1::label(indice_Dia_norm$atividade) <-"Atividade"
table1::label(indice_Dia_norm$trab1_re) <- "Trabalho igual residência"

table1::table1(~ sexo + F_etaria+ sit_fam + educacao + cond_ativ + vinc1 + ocup + renda + per_capta + atividade + Local +  trab1_re |classe_econ, data = indice_Dia_norm,
               render.continuous=c(.="Mean", .="Median", .="Min", .="Max",
                                   .="Q1", .="Q3", .="IQR"))


#Proporções por atributo ----

# Populações totais corrigidas por classe econômica
pop_A  <- 525387.3
pop_B1 <- 813259.6
pop_B2 <- 2710936.8
pop_C1 <- 2849071.1
pop_C2 <- 2489957.9
pop_DE <- 544225.2

# Grupo A
Grupo_A <- indice_Noite_norm %>% filter(classe_econ == "A")
prop_mulheres_A <- aggregate(fator_exp ~ sexo, data = Grupo_A, FUN = "sum")
prop_mulheres_A$razao <- prop_mulheres_A$fator_exp / pop_A

prop_etario_A <- aggregate(fator_exp ~ F_etaria, data = Grupo_A, FUN = "sum")
prop_etario_A$razao <- prop_etario_A$fator_exp / pop_A

prop_raca_A <- aggregate(fator_exp ~ `raça`, data = Grupo_A, FUN = "sum")
prop_raca_A$razao <- prop_raca_A$fator_exp / pop_A

# Grupo B1
Grupo_B1 <- indice_Noite_norm %>% filter(classe_econ == "B1")
prop_mulheres_B1 <- aggregate(fator_exp ~ sexo, data = Grupo_B1, FUN = "sum")
prop_mulheres_B1$razao <- prop_mulheres_B1$fator_exp / pop_B1

prop_etario_B1 <- aggregate(fator_exp ~ F_etaria, data = Grupo_B1, FUN = "sum")
prop_etario_B1$razao <- prop_etario_B1$fator_exp / pop_B1

prop_raca_B1 <- aggregate(fator_exp ~ `raça`, data = Grupo_B1, FUN = "sum")
prop_raca_B1$razao <- prop_raca_B1$fator_exp / pop_B1

# Grupo B2
Grupo_B2 <- indice_Noite_norm %>% filter(classe_econ == "B2")
prop_mulheres_B2 <- aggregate(fator_exp ~ sexo, data = Grupo_B2, FUN = "sum")
prop_mulheres_B2$razao <- prop_mulheres_B2$fator_exp / pop_B2

prop_etario_B2 <- aggregate(fator_exp ~ F_etaria, data = Grupo_B2, FUN = "sum")
prop_etario_B2$razao <- prop_etario_B2$fator_exp / pop_B2

prop_raca_B2 <- aggregate(fator_exp ~ `raça`, data = Grupo_B2, FUN = "sum")
prop_raca_B2$razao <- prop_raca_B2$fator_exp / pop_B2

# Grupo C1
Grupo_C1 <- indice_Noite_norm %>% filter(classe_econ == "C1")
prop_mulheres_C1 <- aggregate(fator_exp ~ sexo, data = Grupo_C1, FUN = "sum")
prop_mulheres_C1$razao <- prop_mulheres_C1$fator_exp / pop_C1

prop_etario_C1 <- aggregate(fator_exp ~ F_etaria, data = Grupo_C1, FUN = "sum")
prop_etario_C1$razao <- prop_etario_C1$fator_exp / pop_C1

prop_raca_C1 <- aggregate(fator_exp ~ `raça`, data = Grupo_C1, FUN = "sum")
prop_raca_C1$razao <- prop_raca_C1$fator_exp / pop_C1

# Grupo C2
Grupo_C2 <- indice_Noite_norm %>% filter(classe_econ == "C2")
prop_mulheres_C2 <- aggregate(fator_exp ~ sexo, data = Grupo_C2, FUN = "sum")
prop_mulheres_C2$razao <- prop_mulheres_C2$fator_exp / pop_C2

prop_etario_C2 <- aggregate(fator_exp ~ F_etaria, data = Grupo_C2, FUN = "sum")
prop_etario_C2$razao <- prop_etario_C2$fator_exp / pop_C2

prop_raca_C2 <- aggregate(fator_exp ~ `raça`, data = Grupo_C2, FUN = "sum")
prop_raca_C2$razao <- prop_raca_C2$fator_exp / pop_C2

# Grupo DE
Grupo_DE <- indice_Noite_norm %>% filter(classe_econ == "DE")
prop_mulheres_DE <- aggregate(fator_exp ~ sexo, data = Grupo_DE, FUN = "sum")
prop_mulheres_DE$razao <- prop_mulheres_DE$fator_exp / pop_DE

prop_etario_DE <- aggregate(fator_exp ~ F_etaria, data = Grupo_DE, FUN = "sum")
prop_etario_DE$razao <- prop_etario_DE$fator_exp / pop_DE

prop_raca_DE <- aggregate(fator_exp ~ `raça`, data = Grupo_DE, FUN = "sum")
prop_raca_DE$razao <- prop_raca_DE$fator_exp / pop_DE







# Totais e proporções gerais (denominador geral corrigido)
total_pop <- 9932837.9

Grupos_Etario <- indice_Noite_norm
Grupos_Estarios <- aggregate(fator_exp ~ F_etaria, data = Grupos_Etario, FUN = "sum")
Grupos_Estarios$razao <- Grupos_Estarios$fator_exp / total_pop

Resumo_Grupos <- indice_Noite_norm
prop_mulheres <- aggregate(fator_exp ~ sexo, data = Resumo_Grupos, FUN = "sum")
prop_mulheres$razao <- prop_mulheres$fator_exp / total_pop

prop_idade <- aggregate(fator_exp ~ F_etaria, data = Resumo_Grupos, FUN = "sum")
prop_idade$razao <- prop_idade$fator_exp / total_pop

prop_ocup <- aggregate(fator_exp ~ ocup, data = Resumo_Grupos, FUN = "sum")
prop_ocup$razao <- prop_ocup$fator_exp / total_pop

prop_classe <- aggregate(fator_exp ~ classe_econ, data = Resumo_Grupos, FUN = "sum")
prop_classe$razao <- prop_classe$fator_exp / total_pop

prop_escola <- aggregate(fator_exp ~ educacao, data = Resumo_Grupos, FUN = "sum")
prop_escola$razao <- prop_escola$fator_exp / total_pop

prop_cond_ativ <- aggregate(fator_exp ~ cond_ativ, data = Resumo_Grupos, FUN = "sum")
prop_cond_ativ$razao <- prop_cond_ativ$fator_exp / total_pop

prop_sit_fam <- aggregate(fator_exp ~ sit_fam, data = Resumo_Grupos, FUN = "sum")
prop_sit_fam$razao <- prop_sit_fam$fator_exp / total_pop
