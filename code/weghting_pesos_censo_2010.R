cultura <- readRDS('cultura_cnae_2010.RDS')

Estados<-data.frame(uf=c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA',
                         'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 'RJ', 'RN',
                         'RO', 'RR','RS', 'SC', 'SE', 'SP', 'TO'),
                    regiao=c('Norte','Nordeste','Norte','Norte','Nordeste','Nordeste','Centro-Oeste',
                             'Sudeste','Centro-Oeste','Nordeste','Sudeste','Centro-Oeste','Centro-Oeste','Norte',      
                             'Nordeste','Nordeste','Nordeste','Sul','Sudeste','Nordeste','Norte',
                             'Norte','Sul','Sul','Nordeste','Sudeste','Norte'),
                    V0001 =c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,
                                14,43,42,28,35,17)
)

library(dplyr)
library(survey)
Estados$V0001 = as.character(Estados$V0001)
cultura = cultura %>% left_join(Estados)

cultura <-
  cultura %>%
  group_by(V0011) %>%
  mutate(qtdPessoas = n()) %>%
  ungroup()

str(cultura)

design <- svydesign(
  ids = ~1,
  strata = ~V0011,
  fpc = ~qtdPessoas,
  weights = ~V0010,
  data = cultura
)

#-----------------------------------------------------------------------------
# Alguns testes de funcionamento da função
#-----------------------------------------------------------------------------
svytotal(~V1006, design)

svymean(~V1006, design)

#-----------------------------------------------------------------------------
# Comparando resultados sem peso e com peso
#-----------------------------------------------------------------------------
table(cultura$V0601)
prop.table(table(cultura$V0601))

svytotal(~V0601, design)
svymean(~V0601, design)

wtd.table(cultura$V0601, weights=cultura$V0010)
prop.table(wtd.table(cultura$V0601, weights=cultura$V0010))


wtd.mean(cultura$V5070, weights=cultura$V0010)
svymean(~V5070, design, na.rm = T)
#-----------------------------------------------------------------------------
# Comparando resultados com peso (pacote survey) e o resultado somado (pacote censobr)
#-----------------------------------------------------------------------------
df <- cultura |>
  group_by(V0606) |>
  summarize(higher_edu = sum(V0010[which(V6400=="Superior completo")]) / sum(V0010),
            pop = sum(V0010) ) 
df

svytotal(~interaction(V0606, V6400), design,na.rm = TRUE)

#-----------------------------------------------------------------------------
# Comparando resultados com peso (pacote survey) e o resultado somado (pacote censobr)
#-----------------------------------------------------------------------------

table(cultura$V0606, cultura$V5120)
wtd.table(cultura$V0606, cultura$V5120, weights=cultura$V0010)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Open variables layout from Excel file
dic_pes <- read_excel("./documentacao2010/Layout_microdados_Amostra.xls", sheet =2, skip = 1)
dic_pes <- dic_pes[,1:2]
print(dic_pes)

selecao= c('V0001','V0002','V0011','V0300','V0010','V1002','V1003','V1004','V1006','V0601','V6036','V0606','V6400','V0640','V0641','V0642', 
           'V6461','V6471',
           'V0648','V0650','V6513','V6525','V6527','V0653','V0661','V0662','V6900','V6910','V6920','V6930','V6940','V6121','V5070','V6462','V6472','V5110','V5120','V5030','V1005')

dic_pes = dic_pes %>% filter(dic_pes$VAR %in% selecao)


svytotal(~V5110, design)
svymean(~V5110, design)

svytotal(~V5120, design)
svymean(~V5120, design)



svytotal(~interaction(V0601, V5120), design,na.rm = TRUE)
343965/(343965+406194)

library(questionr)
library(ggtext)

#ggsurvey(design) +
#  aes(y = V5120) +
#  geom_bar()+
#  scale_x_continuous(name = 'Situação previdência oficial',labels = scales::percent) 

prop.table(wtd.table(cultura$V0601, cultura$V5120, weights=cultura$V0010),1)*100

ggsurvey(design) +
  aes(x = V0601, fill = V5120) +
  geom_bar(position = "fill")+
  scale_fill_manual('Situação previdência',values = c("1" = "blue", "2" = "red"),labels = c('Contribuinte','Não contribuinte'))+
  labs(x = 'Sexo',y='percentual',
       title = 'Gráfico 1 - Contribuição da previdência oficial dos trabalhadores da cultura  - Brasil/2010',
       subtitle = '45,8% das mulheres e 54,1% dos homens contribuem para a previdência',
       caption = "Fonte: Censo 2010. Processamento: Dataunirio") +
  theme_classic()+
  coord_flip()


ggsurvey(design) +
  aes(x = V0606, fill = V5120) +
  geom_bar(position = "fill")+
  scale_fill_manual('Situação previdência oficial',values = c("1" = "blue", "2" = "red"),labels = c('Contribuinte','Não contribuinte'))+
  labs(x = 'Sexo',y='percentual',
       subtitle = ) +
  theme_classic()+
  coord_flip()

library(forcats)

ggsurvey(design) +
  aes(x=uf, fill = V5120) +
  geom_bar(position = "fill")+
  scale_fill_manual('Situação previdência oficial',values = c("1" = "blue", "2" = "red"),labels = c('Contribuinte','Não contribuinte'))+
  labs(x = 'UF',y='percentual',
       subtitle = ) +
  theme_classic()+
  coord_flip()






"#5c997e"
scale_y_continuous(name = 'Proportion with higher education',
                     labels = scales::percent) 
  