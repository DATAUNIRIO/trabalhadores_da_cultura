library(readxl)
library(janitor)
library(tidyr)

cnae <- read_excel("./documentacao2010/Ocupação COD 2010 artistas.xls", sheet =1, skip = 1) %>% clean_names()
cnae <- na.omit(cnae) 
cnae <- cnae[nchar(cnae$codigo)>3,]


library(censobr)
selecao= c('V0001','V0002','V0011','V0300','V0010','V1002','V1003','V1004','V1006','V0601','V6036','V0606','V6400','V0640','V0641','V0642', 
           'V6461','V6471',
           'V0648','V0650','V6513','V6525','V6527','V0653','V0661','V0662','V6900','V6910','V6920','V6930','V6940','V6121','V5070','V6462','V6472','V5110','V5120','V5030','V1005')

populacao <- read_population(year = 2010,
                             columns = selecao,
                             add_labels = 'pt',
                             showProgress = FALSE)

dplyr::glimpse(populacao)

cultura_cnae <- populacao |>
  filter(V6461 %in% cnae$codigo) |>
  compute() |>
  collect()

saveRDS(cultura_cnae,file = 'cultura_cnae_2010.RDS')
