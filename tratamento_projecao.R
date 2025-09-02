
#############################################
#      Importação e Tratamento da base      #
#############################################
#Importação da base
df <- read_excel("METAS DIVERSIDADE/base2.xlsx")
str(df)

#Tratamentos
#Extração mês e ano da variável Data
df$Mes <- c(format(df$Data, "%m")) %>% as.numeric()
df$Ano <- c(format(df$Data, "%Y")) %>% as.numeric()

#Extração mês e ano da variável Data de Entrada
df$MesEntrada <- c(format(df$`Data de Entrada`, "%m")) %>% as.numeric()
df$AnoEntrada <- c(format(df$`Data de Entrada`, "%Y")) %>% as.numeric()
head(df, n =10)

#Removendo as colunas Data e Data de Entrada
df <- df[, -c(1:2)]

#Filtrando o dataset para retornar os dados a partir de 2020
ano_inicial <- 2012 #as.numeric(format(Sys.Date(), "%Y")) -3
JANELA_MEDIA <- 3
df <- subset(df, Ano >= ano_inicial) 
head(df, n= 10)

#Função para substituir valores NA
df$`Raça/Cor` <- ifelse(is.na(df$`Raça/Cor`) | df$`Raça/Cor` == "PB/Não atribuído", "Não Informado", df$`Raça/Cor`) 
head(df, n= 10)

#Adiciona uma coluna para identificar Gerador de Receita
df$`Gerador de Receita` <- ifelse(df$DIRETORIA %in% c('DC&L', 'DDP', 'DE&P', 'DRGN'), 'Sim', 'Não')

#Criando coluna de identificação de STEM
STEM <- c('Engenharia', 'Fisica', 'Geologia', 'Geodesia', 'Arquitetura', 'Atuariais', 'Exploracao', 'Controle Ambiental', 'Processos de Negoc', 'Agronom', 'Instrumentacao', 'Inspecao de', 'Inspeçao de Equipamentos', 'Pesquisa Oper', 'Caldeir', 'Operacao', 'Seguranca do Trabalho', 'Agricol', 'Informatica', 'Exatas', 'Dados', 'Navais', 'Eletro', 'Eletrica', 'Dutos', 'Edificaco', 'Economi', 'Oceanograf', 'Biolog', 'Mecanica', 'Perfurac', 'Telecomunic', 'Medicin', 'Segurança do Trabalho', 'Seguranca do Trabalho', 'Quimica', 'Petroleo', 'Analise de Sistemas', 'Estatistica')
df$stem <- ifelse(grepl(paste(STEM, collapse = "|"), df$Ênfase), "Sim", "Não")

#Criando coluna de identificação de Alta Liderança
AA <- c('Gerente Executivo', 'Diretor', 'Gerente Executivo(a)', 'Diretor(a)')
df<-subset(df, !is.na(Ênfase))
df$`Alta Liderança` <- ifelse(df$Função %in% AA,"Sim", "Não")

#Definindo os anos da projeção
df_anoproj <- c(2023, 2024, 2025, 2026, 2027)
#df_projetado <- data.frame(Ano = c(2023, 2024, 2025, 2026, 2027))
df_projetado <- function() {
  df_ <- data.frame(
    Ano = c(2023, 2024, 2025, 2026, 2027)
  )
  return(df_)
}

#############################################
#          Config função de Gráfico         #
#############################################
cria_grafico <- function(df, colunas, title, ytitle, pcd_legal=NULL) {
  cores <- c('rgb(173, 216, 230)', 'rgb(238, 130, 238)', 'rgb(255, 182, 193)', 'rgb(240, 128, 128)', 'rgb(144, 238, 144)', 'rgb(176, 196, 222)')
  cores_projecao <- c('rgba(173, 216, 230, 0.5)', 'rgba(238, 130, 238, 0.5)', 'rgba(255, 182, 193, 0.5)', 'rgba(240, 128, 128, 0.5)', 'rgb(144, 238, 144, 0.5)', 'rgba(176, 196, 222, 0.5)')
  
  # Criar um gráfico de linha com os dados agrupados
  fig <- plot_ly(width=1800, height=880)
  
  for (i in seq_along(colunas)) {
    coluna <- colunas[i]
    df_historico <- df[df$Ano <= 2022, ]
    df_projetado <- df[df$Ano >= 2022, ]
    valores <- df_projetado[[coluna]]
    if ('%' %in% coluna) {
      valores <- paste0(format(valores * 100, digits=2), "%")
    }
    fig <- fig %>% add_trace(type='scatter', fill='tozeroy', fillcolor=cores[i], textposition="top left", x=~df_historico$Ano, y=~df_historico[[coluna]], mode='lines+text', line=list(color='white'), name=coluna, connectgaps=TRUE)
    fig <- fig %>% add_trace(type='scatter', text=valores, fill='tozeroy', fillcolor=cores_projecao[i], textposition="top center", x=~df_projetado$Ano, y=~df_projetado[[coluna]], mode='lines+text', line=list(color='blue', dash='dash'), name=paste0(coluna, ' (Previsão)'))
  }
  
  if (!is.null(pcd_legal) && pcd_legal) {
    # percentual_cota_legal <- rep(0.05, length(pcd$Ano))
    fig <- fig %>% add_trace(type='scatter', x=~df$Ano, y=~df$percentual_cota_legal, mode='lines', line=list(color='red'), name='Cota Legal')
  }
  
  # Atualizar o layout do gráfico
  fig <- fig %>% layout(title=title, xaxis=list(title='Ano'), yaxis=list(title=ytitle))
  # Atualizar o eixo x para exibir todos os anos
  fig <- fig %>% layout(xaxis=list(dtick=2))
  fig <- fig %>% add_trace(mode="markers+lines+text")
  
  # Mostrar o gráfico
  fig %>% show()
}

cria_grafico_barra <- function(df, colunas, title, ytitle, pcd_legal=NULL) {
  cores <- c('rgb(173, 216, 230)', 'rgb(238, 130, 238)', 'rgb(255, 182, 193)', 'rgb(240, 128, 128)', 'rgb(144,238, 144)', 'rgb(176, 196, 222)')
  cores_projecao <- c('rgba(173, 216, 230, 0.5)', 'rgba(238, 130, 238, 0.5)', 'rgba(255, 182, 193, 0.5)', 'rgba(240, 128, 128, 0.5)', 'rgb(144, 238, 144, 0.5)', 'rgba(176, 196, 222, 0.5)')
  
  #Criar um gráfico de barras com os dados agrupados
  fig <- plot_ly(width=1800, height=880)
  
  for (i in seq_along(colunas)) {
    coluna <- colunas[i]
    valores <- df[[coluna]]
    if ('%' %in% coluna) {
      valores <- paste0(format(valores * 100, digits=2), "%")
    } else {
      valores <- round(valores)
    }
    fig <- fig %>% add_trace(type='bar', text=valores, textposition="auto", x=~df$Ano, y=~df[[coluna]], name=coluna)
  }
  
  if (!is.null(pcd_legal) && pcd_legal) {
    fig <- fig %>% add_trace(type='scatter', x=~df$Ano, y=~df$percentual_cota_legal, mode='lines', line=list(color='red'), name='Cota Legal')
    fig <- fig %>% add_trace(type='scatter', x=~df$Ano, y=~df$percentual_cota_concurso, mode='lines', line=list(color='blue'), name='Cota Concurso')
  }
  
  #Atualizar o layout do gráfico
  fig <- fig %>% layout(title=title, xaxis=list(title='Ano'), yaxis=list(title=ytitle, barmode='overlay'))
  
  #Atualizar o eixo x para exibir todos os anos
  fig <- fig %>% layout(xaxis=list(dtick=1))
  
  #Mostrar o gráfico
  fig %>% show()
}


#############################################
#              Dimensão Efetivo             #
#############################################
efetivo <- df %>% group_by(Ano) %>% count() %>% rename(Quantidade = n)
efetivo

#############################################
#             Dados Desligamento            #
#############################################
#Importação da base
df_des <- read_excel("METAS DIVERSIDADE/desligamentos.xlsx")
str(df_des)

#Remoção dos dados de desligamento por PDV, PIDV e PAI
desl <- c('PDV', 'PIDV', 'PAI')
df_des$tp_pdv <- ifelse(grepl(paste(desl, collapse = "|"), df_des$`Motivo da Medida`), "Sim", "Não")
df_des <- subset(df_des, tp_pdv == "Não")
df_des$tp_pdv <- NULL

#Extração mês e ano da variável Data de Início da Medida
df_des$Mes <- c(format(df_des$`Data de Início da Medida`, "%m")) %>% as.numeric()
df_des$Ano <- c(format(df_des$`Data de Início da Medida`, "%Y")) %>% as.numeric()
df_des <- subset(df_des, Ano >= ano_inicial)

#Transformação do dataframe para que traga o Ano e Quantidade de Desligamento
df_des <- df_des %>% group_by(Ano) %>% count() %>% rename(QuantidadeDesligamento = n)

#Junção com a dimensão de efetivo
df_des <- merge(df_des, efetivo)

#Criação da coluna de percentual de desligamentos
df_des$PercentualDesligamento <- df_des$QuantidadeDesligamento / df_des$Quantidade * 100

TaxaDesligamento <- tail(rollmean(df_des$PercentualDesligamento, k = 3), 1)
TaxaDesligamento_txt <- paste("A média percentual de desligamento nos últimos três anos é:", round(TaxaDesligamento, 2), "% do efetivo")

#############################################
#                 Dados PDV                 #
#############################################
#Importandos dados 
df_pdv <- read_excel("METAS DIVERSIDADE/projetado_pdv.xlsx")

#Substituindo valores NA por 0
df_pdv <- replace(df_pdv, is.na(df_pdv), 0)

#Extração mês e ano da variável Data
df_pdv$Mes <- c(format(df_pdv$Data, "%m")) %>% as.numeric()
df_pdv$Ano <- c(format(df_pdv$Data, "%Y")) %>% as.numeric()

#Consolidando os Desligados e Ativos em uma coluna -> quantidade de PDV
df_pdv$pdv <- df_pdv$Desligados + df_pdv$Ativos

#Quantidade de Ativos Suspensos
ATIVOS_SUSPENSOS <- df_pdv$`Ativos (suspensos)` %>% sum()

#Consolidando o dataframe por Ano e Quantidade de Desligamentos por PDV
df_pdv <- aggregate(pdv ~ Ano, data = df_pdv, FUN = sum) %>% rename(QuantidadeDesligamento = pdv)

#Soma os ativos suspensos no último ano da série
ultimo_ano <- max(df_pdv$Ano)
df_pdv <- df_pdv %>% mutate(QuantidadeDesligamento  = ifelse(Ano == ultimo_ano, QuantidadeDesligamento  + ATIVOS_SUSPENSOS, QuantidadeDesligamento ))

#Aplicação da taxa bruta de crescimento para os próximos anos
ultimo_efetivo <- tail(efetivo$Quantidade, 1)
TAXA_CRESCIMENTO <- c(1, 1.0634, 1.03, 1.02, 1.015, 1.01)
QUANTIDADE_PROJETADA <- numeric()

for (taxa in TAXA_CRESCIMENTO) {
  novo_efetivo <- ultimo_efetivo * taxa
  QUANTIDADE_PROJETADA <- c(QUANTIDADE_PROJETADA, round(novo_efetivo))
  ultimo_efetivo <- round(novo_efetivo)
}

#Assumindo as seguintes hipóteses:
efetivo_projetado <- data.frame(
  Ano = c(2022, 2023, 2024, 2025, 2026, 2027),
  Quantidade = QUANTIDADE_PROJETADA
)

#Juntando as informações de efetivo projetado com dados de PDV e Desligamentos
efetivo_projetado <- merge(efetivo_projetado, df_pdv[, c('Ano', 'QuantidadeDesligamento')], by = 'Ano', all.x = TRUE)
tx_desligamento <- c(0, 0.00466, rep(TaxaDesligamento/100, 4))

efetivo_projetado$Desligamentos <- round(efetivo_projetado$Quantidade * tx_desligamento)
efetivo_projetado$Ingressos <- c(0, diff(efetivo_projetado$Quantidade))
efetivo_projetado[is.na(efetivo_projetado)] <- 0

efetivo_projetado$Quantidade <- efetivo_projetado$Quantidade - efetivo_projetado$QuantidadeDesligamento - efetivo_projetado$Desligamentos

#Taxa líquida de crescimento do efetivo
efetivo_projetado$`Taxa Crescimento` <- c(efetivo_projetado$Quantidade/lag(efetivo_projetado$Quantidade))-1
TAXA_CRESCIMENTO_EFETIVO_LIQUIDO <- efetivo_projetado$`Taxa Crescimento`[-1]
TAXA_CRESCIMENTO_EFETIVO_LIQUIDO <- 1 + TAXA_CRESCIMENTO_EFETIVO_LIQUIDO

TAXA_CRESCIMENTO_EFETIVO_LIQUIDO


#Quantidade Projetada Líquida
QUANTIDADE_PROJETADA <- efetivo_projetado$Quantidade[-1]

#Total Projetado
df_total_proj <- efetivo_projetado[-1, 1:2] %>% rename(Total = Quantidade)


#############################################
#     Gráfico da evolução do efetivo        #
#############################################

# Criar um gráfico de linha com os dados agrupados
fig <- plot_ly(width=1800, height=880)

# Adicionar as linhas de previsão
fig <- add_trace(fig, x = ~efetivo$Ano, y = ~efetivo$Quantidade, type = 'scatter', mode = 'lines+text', line = list(color = 'blue'), name = 'Headcount')
fig <- add_trace(fig, x = ~efetivo_projetado$Ano, y = ~efetivo_projetado$Quantidade, type = 'scatter', mode = 'lines+text', line = list(color = 'red', dash = 'dash'), name = 'Headcount (Previsão)', text = ~efetivo_projetado$Quantidade, textposition = 'top center')

# Atualizar o layout do gráfico
fig <- layout(fig, title = 'Número de empregados por ano', xaxis = list(title = 'Ano'), yaxis = list(title = 'Quantidade de Empregados'))

# Atualizar o eixo x para exibir todos os anos
fig <- layout(fig, xaxis = list(dtick = 2))

# Mostrar o gráfico
fig


#############################################
#     Recorte de Diversidade no Efetivo     #
#############################################
#Funções para recorte de diversidade no efetivo
df_recorte <- function(column, ano_inicial = 2020, stem = NULL) {
  if(column == "PCD"){
    df_ <- df %>% group_by(Ano, PCD) %>% summarise(Quantidade = n()) %>% ungroup()
    df_$Quantidade[df_$Ano == 2022 & df_$PCD == "Sim"] <- df_$Quantidade[df_$Ano == 2022 & df_$PCD == "Sim"] + 92
    df_ <- pivot_wider(df_, names_from = PCD, values_from = Quantidade)
    df_ <- merge(df_, efetivo) %>% rename(Headcount = Quantidade)
    
    TAXA_MEDIA_HISTORICA_CATEGORIA <- tail(rollmean((df_$Sim/df_$Headcount), k = JANELA_MEDIA), 1)
    AJUSTE_TAXA_MEDIA <- (0.078 * 0.1 + TAXA_MEDIA_HISTORICA_CATEGORIA * 0.9) / (0.9 + 0.1)
    df_total_proj_ <- df_total_proj %>% rename(Headcount = Total)
    df_total_proj_$Sim <- round(QUANTIDADE_PROJETADA * AJUSTE_TAXA_MEDIA)
    df_total_proj_$Não <- df_total_proj_$Headcount - df_total_proj_$Sim
    
    df_ <- rbind(df_, df_total_proj_)
    
    p_columns <- names(df_)[2:3]
    
    for (coluna in p_columns) {
      nome <- paste0("% ", coluna)
      df_[[nome]] <- df_[[coluna]] / df_$Headcount
    }
    
  }
  
  else if (column == "Raça/Cor") {
    df_ <- df
    df_$`Raça/Cor` <- ifelse(df_$`Raça/Cor` %in% c("Parda", "Preta"), "Negra", df_$`Raça/Cor`)
    df_ <- df_ %>% group_by(Ano, `Raça/Cor`) %>% summarise(Quantidade = n()) %>% ungroup()
    df_ <- pivot_wider(df_, names_from = `Raça/Cor`, values_from = Quantidade)
    df_ <- merge(df_, efetivo) %>% rename(Headcount = Quantidade)
    df_total_proj_ <- df_total_proj %>% rename(Headcount = Total)
    
    n_columns <- names(df_)[2:6] 
    
    for (coluna in n_columns) {
      if(coluna == "Preta"){
        TAXA_MEDIA_HISTORICA_CATEGORIA <- tail(rollmean((df_$Preta/df_$Headcount), k = JANELA_MEDIA), 1)
        AJUSTE_TAXA_MEDIA <- (0.9 * TAXA_MEDIA_HISTORICA_CATEGORIA + 0.1 * 0.1) / (0.9 + 0.1) 
      }
      else if(coluna == "Não Informado"){
        AJUSTE_TAXA_MEDIA <- 0.122345
      } else {
        AJUSTE_TAXA_MEDIA <- tail(rollmean((df_[[coluna]]/df_$Headcount), k = JANELA_MEDIA), 1)
      }
      df_total_proj_[[coluna]] <- round(QUANTIDADE_PROJETADA * AJUSTE_TAXA_MEDIA)
    }
    
    df_ <- rbind(df_, df_total_proj_)
    
    p_columns <- names(df_)[2:6]
    
    for (coluna in p_columns) {
      nome <- paste0("% ", coluna)
      df_[[nome]] <- df_[[coluna]] / df_$Headcount
    }
    
  }
  else if(column == "Gênero"){
    df_ <- df %>% group_by(Ano, Gênero) %>% summarise(Quantidade = n()) %>% ungroup()
    df_ <- pivot_wider(df_, names_from = Gênero, values_from = Quantidade)
    df_ <- merge(df_, efetivo) %>% rename(Headcount = Quantidade)
    df_total_proj_ <- df_total_proj %>% rename(Headcount = Total)
    
    n_columns <- names(df_)[2:3]
    
    for (coluna in n_columns) {
      AJUSTE_TAXA_MEDIA <- tail(rollmean((df_[[coluna]]/df_$Headcount), k = JANELA_MEDIA), 1)
      df_total_proj_[[coluna]] <- round(QUANTIDADE_PROJETADA * AJUSTE_TAXA_MEDIA)
    }
    
    df_ <- rbind(df_, df_total_proj_)
    
    p_columns <- names(df_)[2:3]
    
    for (coluna in p_columns) {
      nome <- paste0("% ", coluna)
      df_[[nome]] <- df_[[coluna]] / df_$Headcount
    }
    
  }
  return(df_)
}


#Bases de diversidade no efetivo
df_rc <- df_recorte('Raça/Cor')
df_genero <- df_recorte('Gênero')
df_pcd <- df_recorte('PCD')

#############################################
#           Funções para liderança          #
#############################################
cria_dataframe_funcao <- function(supervisor=NULL, primeiro_nivel=NULL, receita=NULL, alta_lideranca=NULL){
  df_funcao <- df
  df_funcao <- df %>% subset(!`Grupo Função` %in% c("Sem função gratificada", "Supervisor", "Apoio Alta Adm.", "Especialista"))
  
  if(!is.null(supervisor)){
    df_funcao <- df
    df_funcao$`Grupo Função`[df_funcao$`Grupo Função` == "Supervisor"] <- "Gerencial" 
    df_funcao <- df_funcao %>% subset(!`Grupo Função` %in% c("Sem função gratificada", "Apoio Alta Adm.", "Especialista"))
  }
  
  if(!is.null(primeiro_nivel)){
    df_funcao <- df_funcao[grepl("Gerente Setorial|Coordenador", df_funcao$Função), ] 
  }
  
  if(!is.null(alta_lideranca)){
    df_funcao <- df_funcao[grepl("Gerente Executivo|Diretor", df_funcao$Função), ]
  }
  
  if(!is.null(receita)){
    df_funcao <- df_funcao %>% subset(`Gerador de Receita` == "Sim")
  }
  
  lideranca <- df_funcao %>% group_by(Ano, `Grupo Função`) %>% summarise(Quantidade = n()) %>% ungroup()
  lideranca <- aggregate(Quantidade ~ Ano, data = lideranca, FUN = sum) 
  lideranca$Headcount <- efetivo$Quantidade
  
  if(!is.null(receita)){
    efetivo_receita <- df_funcao %>% subset(`Gerador de Receita` == "Sim") %>% group_by(Ano) %>% summarise(Quantidade = n()) %>% ungroup()
    lideranca$Headcount <- efetivo_receita$Quantidade
  }
  if(!is.null(alta_lideranca)){
    efetivo_al <- df_funcao %>% subset(df_funcao$`Alta Liderança` == "Sim") %>% group_by(Ano) %>% summarise(Quantidade = n()) %>% ungroup()
    lideranca$Headcount <- efetivo_al$Quantidade
  }
  lideranca$`% Lideres` <- lideranca$Quantidade / lideranca$Headcount
  
  return (lideranca)
}


taxa_crescimento_lideranca <- function(){
  ultima_quantidade_lideranca <- lideranca$Quantidade %>% tail(1)
  TAXA_CRESCIMENTO_FUNCAO = TAXA_CRESCIMENTO_EFETIVO_LIQUIDO
  QUANTIDADE_PROJETADA_FUNCAO = vector("numeric")
  
  for (taxa in TAXA_CRESCIMENTO_FUNCAO){
    nova_lideranca <- ultima_quantidade_lideranca * taxa
    QUANTIDADE_PROJETADA_FUNCAO <- c(QUANTIDADE_PROJETADA_FUNCAO, round(nova_lideranca))
    nova_lideranca <- round(nova_lideranca)
  }
  
  return (QUANTIDADE_PROJETADA_FUNCAO)
}


lideranca <- cria_dataframe_funcao(supervisor=NULL)
QUANTIDADE_PROJETADA_FUNCAO = taxa_crescimento_lideranca()



aplica_projecoes_funcao <- function(taxas){
  df_ = df_projetado()
  df_$Headcount = QUANTIDADE_PROJETADA_FUNCAO
  
  for (coluna in names(taxas)) {
    valor <- taxas[[coluna]]
    nome_coluna <- substr(coluna, 3)
    df_[[coluna]] <- valor
    df_[[nome_coluna]] <- round(QUANTIDADE_PROJETADA_FUNCAO * df_[[coluna]])
  }
  
  return (df_)
}


taxas_medias_funcao <- function(df, column){
  taxas <- vector("numeric")
  
  for (coluna in colnames(df)){
    if ('%' %in% coluna){
      nome_coluna <- substring(coluna, 3)
      TAXA_MEDIA_HISTORICA_CATEGORIA <- tail(rollmean((df_[[coluna]]/df_$Headcount), k = JANELA_MEDIA), 1)
      LISTA_TAXA_MEDIA_HISTORICA_CATEGORIA <- rep(TAXA_MEDIA_HISTORICA_CATEGORIA, 5)
      
      taxas[[coluna]] <- LISTA_TAXA_MEDIA_HISTORICA_CATEGORIA
    }
  }
  
  return (taxas)
}


df_recorte_funcao <- function(lideranca, column, supervisor=NULL, primeiro_nivel=NULL, STEM=NULL, receita=NULL, alta_lideranca=NULL){
  df_ <- df %>% subset(!`Grupo Função` %in% c("Sem função gratificada", "Supervisor", "Apoio Alta Adm.", "Especialista"))
  
  if(!is.null(supervisor)){
    df_$`Grupo Função`[df$`Grupo Função` == "Supervisor"] <- "Gerencial" 
    df_ <- df_ %>% subset(!`Grupo Função` %in% c("Sem função gratificada", "Apoio Alta Adm.", "Especialista"))
  }
  if(!is.null(primeiro_nivel)){
    df_ <- df[grepl("Gerente Setorial|Coordenador", df$Função), ]
  }
  if(!is.null(STEM)){
    df_ <- df_ %>% subset(stem == "Sim")
  }
  if(!is.null(receita)){
    df_ <- df_ %>% subset(`Gerador de Receita` == "Sim")
  }
  if(!is.null(alta_lideranca)){
    df_ <- df_ %>% subset(`Alta Liderança` == "Sim")
  }
  if(column == "Raça/Cor"){
    df_$`Raça/Cor` <- ifelse(df_$`Raça/Cor` %in% c("Parda", "Preta"), "Negra", df_$`Raça/Cor`)
  }
  
  group_by(column) %>% summarise(Quantidade = n()) %>% ungroup()
  column <- column[!column %in% c("Ano", "Grupo Função")]
  df_ <- reshape(df_, idvar="Ano", timevar=column, direction="wide")
  columns_ <- colnames(df_)
  df_$Headcount <- lideranca$Quantidade
  
  for (coluna in columns_) {
    nome <- paste("%", coluna, sep=" ")
    df_[[nome]] <- df_[[coluna]] / df_$Headcount
  }
  
  df_ <- df_[!is.na(df_$Ano),]
  
  taxas <- taxas_medias_funcao(df_, column)
  projetado_ <- aplica_projecoes_funcao(taxas)
  df_ <- rbind(df_, projetado_)
  df_ <- df_[,-1]
  
  return (df_)
}
