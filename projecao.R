
#########################################################
#                     Simulação                         #
#########################################################
#Filtrando o data frame a partir de 2020
df <- df %>% subset(Ano >= 2020)

#Total Atual e Projetado
df_total <- df %>% group_by(Ano) %>% count() %>% rename(Total = n)
df_total_proj
df_total <- rbind(df_total, df_total_proj)

#############################
#    Função da Simulação    #
#############################
# Função para criar a simulação
simulacao <- function(recorte, meta_proposta, ano_inicial = 2022, ano_final = 2027, visual = "grafico"){
  meta <- meta_proposta/100
  tempo <- ano_final - (as.numeric(format(Sys.Date(), "%Y")) - 1)
  
  #############################
  #    Criação dos recortes   #
  #############################
  if(recorte == "mulheres"){
    df_ <- subset(df, Gênero == "feminino") %>% group_by(Ano) %>% count() %>% rename(mulheres = n)
    df_ <- merge(df_, df_total, by ="Ano", all.y =T)
    df_[is.na(df_)] <- 0
    df_$simulado <- ifelse(df_$mulheres > 0, df_$mulheres, round(df_$Total * 0.168159))
  }
  if(recorte == "pcd"){
    df_ <- subset(df, PCD == "Sim") %>% group_by(Ano) %>% count() %>% rename(pcd = n)
    df_ <- merge(df_, df_total, by ="Ano", all.y =T)
    df_$pcd[df_$Ano == 2022] <- df_$pcd[df_$Ano == 2022] + 92
    df_[is.na(df_)] <- 0
    df_$simulado <- ifelse(df_$pcd > 0, df_$pcd, round(df_$Total * 0.013009))
  }
  if(recorte == "negras"){
    df_ <- subset(df, `Raça/Cor` %in% c("Parda", "Preta")) %>% group_by(Ano) %>% count() %>% rename(negras = n)
    df_ <- merge(df_, df_total, by ="Ano", all.y =T)
    df_[is.na(df_)] <- 0
    df_$simulado <- ifelse(df_$negras > 0, df_$negras, round(df_$Total * 0.296696))
  }
  if(recorte == "stem"){
    df_total_stem_fem <- subset(df, stem == "Sim") %>% group_by(Ano, Gênero) %>% count() %>% rename(stem_feminino = n) %>% subset(Gênero == "feminino") 
    df_total_stem_fem$Gênero <- NULL
    df_total_stem_masc <- subset(df, stem == "Sim") %>% group_by(Ano, Gênero) %>% count() %>% rename(stem_masculino = n) %>% subset(Gênero == "masculino")
    df_total_stem_masc$Gênero <- NULL
    df_ <- merge(df_total_stem_fem, df_total_stem_masc, by ="Ano")
    df_$stem_total <- df_$stem_feminino + df_$stem_masculino
    df_ <- merge(df_, df_total, by ="Ano", all.y =T)
    df_[is.na(df_)] <- 0
    df_$stem_fem_simulado <- round(df_$Total * 0.096)
    df_$stem_masc_simulado <- round(df_$Total * 0.69)
    df_$stem_total_simulado <- df_$stem_fem_simulado + df_$stem_masc_simulado
    df_$stem_fem_simulado <- ifelse(df_$stem_feminino > 0, df_$stem_feminino, df_$stem_fem_simulado)
    df_$stem_masc_simulado <- ifelse(df_$stem_masculino > 0, df_$stem_masculino, df_$stem_masc_simulado)
    df_$stem_total_simulado <- ifelse(df_$stem_total > 0, df_$stem_total, df_$stem_total_simulado)
    df_[2:4] <- NULL
    df_$`% feminino` <- df_$stem_fem_simulado / df_$stem_total_simulado
    df_$`% masculino` <- df_$stem_masc_simulado / df_$stem_total_simulado
    df_$percentual_fem <- df_$stem_fem_simulado / df_$Total
    df_$percentual_masc <- df_$stem_masc_simulado / df_$Total
    df_$percentual_total <- df_$stem_total_simulado / df_$Total
  }
  if(recorte == "lideranca_negra"){
    df_ <- read_excel("METAS DIVERSIDADE/df_rc_lideranca.xlsx")
  }
  if(recorte == "alta_gestao"){
    df_ <- read_excel("METAS DIVERSIDADE/df_alta_lideranca.xlsx")
  }
  if(recorte == "gerador_receita"){
    df_ <- read_excel("METAS DIVERSIDADE/df_gerador_receita.xlsx")
  }
  if(recorte == "primeiro_nivel"){
    df_ <- read_excel("METAS DIVERSIDADE/df_genero_funcao_primeiro_nivel.xlsx")
  }
  if(recorte == "mulheres_lideranca"){
    df_ <- read_excel("METAS DIVERSIDADE/df_genero_funcao.xlsx")
  }
  
  if(!recorte == "stem"){
    df_[[recorte]] <- NULL
    df_ <- df_ %>% subset(Ano >= ano_inicial & Ano <= ano_final)
  }else{
    df_ <- df_ %>% subset(Ano >= ano_inicial & Ano <= ano_final)
  }
  
  if(recorte == "lideranca_negra"){
    coluna <- "Negra"
    headcount <- "Headcount"
  }else if(recorte == "stem"){
    coluna <- "stem_fem_simulado"
    headcount <- "Total"
  }else if(recorte %in% c("alta_gestao", "gerador_receita", "primeiro_nivel", "mulheres_lideranca")){
    coluna <- "feminino"
    headcount <- "Headcount"
  }else{
    coluna <- "simulado"
    headcount <- "Total"
  }
  
  simulado_22 <- df_[[coluna]][df_$Ano == 2022]
  esforco <- meta * df_[[headcount]][df_$Ano == ano_final] - tail(df_[[coluna]], 1)
  contratacao_media <- esforco / tempo
  df_$vl_med <- ifelse(df_$Ano == 2022, simulado_22, ceiling(df_[[coluna]] + contratacao_media * (df_$Ano - 2022)))

  #############################
  #    Criação dos gráficos   #
  #############################
  if(visual == "grafico"){
    dados_ <- data.frame(Ano = df_$Ano, ValorAtual = df_[[coluna]], Esforco = df_$vl_med - df_[[coluna]])
    dados_long <- tidyr::pivot_longer(dados_, cols = c(ValorAtual, Esforco), names_to = "Variavel", values_to = "Valor")
    grafico <- ggplot(dados_long, aes(x = Ano, y = Valor, fill = Variavel)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = ifelse(Valor==0, "", Valor)), position = position_stack(vjust = 0.5), color = "#304D63", size = 4) +
      labs(title = paste0("Valores simulado de ", recorte, " e valor médio de contratação necessária para atingir a meta de ", meta_proposta, "%, em relação ao Headcount ", ano_final, ": ", tail(df_[[headcount]], 1)),
           x = "",
           y = "") +
      scale_fill_manual(values = c("#8FB9AA", "#ED8975"), labels = c(paste0("Esforço ", meta_proposta, "%"), recorte)) +
      theme_gray() +
      theme(plot.caption = element_text(hjust = 0, vjust = -1, margin = margin(t = 10))) +
      labs(caption = paste0("Esforço para a meta: ", ceiling(esforco)," - Média de contratação: ", ceiling(contratacao_media), " por ano"))
  }
  
  if(visual == "grafico"){
    return (grafico)
  }else{
    return (df_)
  }
}
simulacao("pcd", 3, 2022, 2027)

grafico_pcd <- simulacao("pcd", 3, 2022, 2027)
grafico_mulheres <- simulacao("mulheres", 21, 2022, 2027)
grafico_negras <- simulacao("negras", 35, 2022, 2027)
grafico_stem <- simulacao("stem", 12, 2022, 2027)
grafico_rc_lideranca <- simulacao("lideranca_negra", 30, 2022, 2027)
grafico_aa <- simulacao("alta_gestao", 35, 2022, 2027)
grafico_gerador_receita <- simulacao("gerador_receita", 17, 2022, 2027)
grafico_genero_funcao_primeiro_nivel <- simulacao("primeiro_nivel", 25, 2022, 2027)
grafico_genero_funcao <- simulacao("mulheres_lideranca", 25, 2022, 2027)

#tabelas
tab_fem <- simulacao("mulheres", 21, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_pcd <- simulacao("pcd", 5, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_raca <- simulacao("negras", 35, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_stem <- simulacao("stem", 12, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_rc_lideranca <- simulacao("lideranca_negra", 30, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_aa <- simulacao("alta_gestao", 35, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_gera_receita <- simulacao("gerador_receita", 17, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_prim_nvl <- simulacao("primeiro_nivel", 25, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

tab_genero_funcao <- simulacao("mulheres_lideranca", 25, 2022, 2027, "tabela") %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)




###################################
#       Tabela PCD x Genero       #
###################################
df_22 <- subset(df, Ano == 2022) 
df_22 %>% group_by(Gênero, PCD) %>% count()

table(df_22$Gênero, df_22$PCD, df_22$`Raça/Cor`)


tab_pcd_gen <- sjt.xtab(var.row = df_22$Gênero,
         var.col = df_22$PCD, 
         show.exp = F, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)


###################################
#      Tabela Negras x Genero     #
###################################
df_22$`Raça/Cor` <- ifelse(df_22$`Raça/Cor` %in% c("Parda", "Preta"), "Negra", df_22$`Raça/Cor`)
df_22 %>% group_by(Gênero, `Raça/Cor`) %>% count()

table(df_22$Gênero, df_22$`Raça/Cor`)


tab_raca_gen <- sjt.xtab(var.row = df_22$Gênero,
                var.col = df_22$`Raça/Cor`, 
                show.exp = F, 
                show.row.prc = TRUE, 
                show.col.prc = TRUE)


###################################
#       Tabela STEM x Genero      #
###################################
tab_stem_gen <- sjt.xtab(var.row = df_22$Gênero,
                         var.col = df_22$stem, 
                         show.exp = F, 
                         show.row.prc = TRUE, 
                         show.col.prc = TRUE)


###################################
#        Tabela STEM x Raça       #
###################################
tab_stem_raca <- sjt.xtab(var.row = df_22$stem,
                         var.col = df_22$`Raça/Cor`, 
                         show.exp = F, 
                         show.row.prc = TRUE, 
                         show.col.prc = TRUE)


###################################
#      Tabela Raça x Lideranca    #
###################################
df_22$`Raça/Cor` <- ifelse(df_22$`Raça/Cor` %in% c("Parda", "Preta"), "Negra", df_22$`Raça/Cor`)
df_22 %>% group_by(Gênero, `Raça/Cor`) %>% count()

table(df_22$Gênero, df_22$`Raça/Cor`)


tab_raca_gen <- sjt.xtab(var.row = df_22$Gênero,
                         var.col = df_22$`Raça/Cor`, 
                         show.exp = F, 
                         show.row.prc = TRUE, 
                         show.col.prc = TRUE)


###################################
#  Tabela Genero x Alta Gerencia  #
###################################
df_22 %>% group_by(Gênero, `Alta Liderança`) %>% count()

table(df_22$`Alta Liderança`, df_22$Gênero)


tab_genero_alta_lideranca <- sjt.xtab(var.row = df_22$Gênero,
                         var.col = df_22$`Alta Liderança`, 
                         show.exp = F, 
                         show.row.prc = TRUE, 
                         show.col.prc = TRUE)



