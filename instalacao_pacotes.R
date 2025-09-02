#############################################
#   Instalação e carregamento dos pacotes   #
#############################################
pacotes <- c("plotly", "tidyverse", "ggrepel", "gridExtra", "zoo", "sjPlot", "PerformanceAnalytics", "kableExtra", "reshape2", "RJDBC", "odbc", "DBI", "caret", "ROCR", "e1071", "dplyr", "dbplyr", "readxl", "ggplot2")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)){
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}