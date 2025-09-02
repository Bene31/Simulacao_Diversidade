#Mulheres
tab_fem
grafico_mulheres1 <- simulacao("mulheres", 21, 2022, 2027)
grafico_mulheres2 <- simulacao("mulheres", 25, 2022, 2027)
grid.arrange(grafico_mulheres1, grafico_mulheres2, ncol = 2)

#PCD
tab_pcd
grafico_pcd1 <- simulacao("pcd", 3, 2022, 2027)
grafico_pcd2 <- simulacao("pcd", 5, 2022, 2027)
grid.arrange(grafico_pcd1, grafico_pcd2, ncol = 2)
tab_pcd_gen

#Raça/Cor
tab_raca
grafico_negras1 <- simulacao("negras", 35, 2022, 2027)
grafico_negras2 <- simulacao("negras", 40, 2022, 2027)
grid.arrange(grafico_negras1, grafico_negras2, ncol = 2)
tab_raca_gen

#Stem
tab_stem
grafico_stem1 <- simulacao("stem", 12, 2022, 2027)
grafico_stem2 <- simulacao("stem", 15, 2022, 2027)
grid.arrange(grafico_stem1, grafico_stem2, ncol = 2)
tab_stem_gen
tab_stem_raca

# % Liderança Negra (Sem função supervisão e sem especialistas)
simulacao("lideranca_negra", 30, 2022, 2027)
simulacao("lideranca_negra", 35, 2022, 2027)

# % Liderança Feminina Alta Gestão (Gerentes Executivas + Diretoria)
simulacao("alta_gestao", 35, 2022, 2027)
simulacao("alta_gestao", 40, 2022, 2027)

# % Liderança Feminina de áreas Geradoras de Receitas (DDP, E&P, RGN e DCL)
simulacao("gerador_receita", 17, 2022, 2027)
simulacao("gerador_receita", 25, 2022, 2027)

# % Liderança Feminina 1 Nível (Gerente Setorial + Coordenação)
simulacao("primeiro_nivel", 25, 2022, 2027)
simulacao("primeiro_nivel", 30, 2022, 2027)

# % Liderança Feminina (Sem supervisão e sem especialistas)
simulacao("mulheres_lideranca", 25, 2022, 2027)
simulacao("mulheres_lideranca", 30, 2022, 2027)



