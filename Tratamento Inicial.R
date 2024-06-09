rm(list = ls())
gc()

library(tidyverse)
library(dplyr)
library(readxl)       
library(geobr)
library(geosphere)
library(tidyr)     
library(ggplot2)      
library(lubridate)   
library(stargazer)
library(Metrics)
library(np)
library(locfit)
 
setwd("~/Dropbox/PPGMMC/Dissertação/Script")

setwd('C:/Users/brads/Dropbox/PPGMMC/Dissertação/Script')

#### Identificar as licitações mais frequentes

TCE_PB_Licitacoes_2021 <- read_delim(
  "~/Dropbox/PPGMMC/Dissertação/Dados/Licitacoes/TCE-PB-Licitacoes_2021.csv", 
  delim = ";", escape_double = FALSE, trim_ws = TRUE)

TCE_PB_Licitacoes_2022 <- read_delim(
  "~/Dropbox/PPGMMC/Dissertação/Dados/Licitacoes/TCE-PB-Licitacoes_2022.csv", 
  delim = ";", escape_double = FALSE, trim_ws = TRUE)

TCE_PB_Licitacoes_2023 <- read_delim(
  "~/Dropbox/PPGMMC/Dissertação/Dados/Licitacoes/TCE-PB-Licitacoes_2023.csv", 
  delim = ";", escape_double = FALSE, trim_ws = TRUE)

lixo <- c("cod_unidade_gestora","ente","unidade_gestora","numero_licitacao",
          "data_cadastro","data_homologacao","valor_estimado","objeto", 
          "tipo_objeto", "situacao","cpf_cnpj_licitante","nome_licitante",
          "numero_protocolo", "tipo_protocolo","valor_proposta",
          "valor_homologacao","valor_empenhado","valor_liquidado", 
          "valor_pago", "valor_empenhado_total", "valor_pago_total")       


TCE_PB_Licitacoes_2021 <- TCE_PB_Licitacoes_2021 |>
  select(-one_of(lixo))

TCE_PB_Licitacoes_2022 <- TCE_PB_Licitacoes_2022 |>
  select(-one_of(lixo))

TCE_PB_Licitacoes_2023 <- TCE_PB_Licitacoes_2023 |>
  select(-one_of(lixo))

licit <- bind_rows(TCE_PB_Licitacoes_2021, TCE_PB_Licitacoes_2022,
                   TCE_PB_Licitacoes_2023)


frequencias <- table(licit$cod_modalidade_licitacao)

maior_frequencia <- max(frequencias)
print(maior_frequencia)

# Transformando as frequências em um dataframe para o gráfico
df_frequencias <- as.data.frame(frequencias)
names(df_frequencias) <- c("Modalidade", "Frequencia")


nomes_modalidades <- c("6" = "Pregão Pre.", "7" = "Pregão El.", "2" = "Conc.", 
                       "9" = "Disp. 8666", "12" = "Cham.P.", "8" = "Inex.",
                       "21" = "Disp. 14133")

# Aplicando o mapeamento ao dataframe
df_frequencias$Modalidade <- as.character(df_frequencias$Modalidade)
df_frequencias$Modalidade <- ifelse(
  df_frequencias$Modalidade %in% names(nomes_modalidades),
  nomes_modalidades[df_frequencias$Modalidade], 
                                    "Outras")

# Criando um gráfico de barras

df_frequencias <- df_frequencias[order(-df_frequencias$Frequencia), ]


ggplot(df_frequencias, aes(x = Modalidade, y = Frequencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frequência de Modalidades de Licitação em PB - 21-23",
       x = "Modalidade de Licitação",
       y = "Frequência") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


###### Identificar os produtos e compradores ###### 

# Função para juntar os dados das Notas Fiscais

processar_dados_ano <- function(ano) {
    nome_arquivo_prod <- paste0("Produtos - NFE - ", ano, ".csv")
    Prod_NF <- read_delim(nome_arquivo_prod, delim = ";", 
                          escape_double = FALSE, trim_ws = TRUE)
    
    Prod_NF <- Prod_NF |>
      filter(`Código EAN Produto` != "SEM GTIM") |>
      filter(`Código EAN Produto` != "SEM GTIN") |>
      subset(select = -c(`Ordem Produto`, id_produto, Lote, `Quantidade Lote`,
                         `Data Fabricação`, `Data Validade`, `Código ANP`, 
                         `Código ANVISA`))
    
    nome_arquivo_nfe <- paste0("NFE - ", ano, ".csv")
    NFE <- read_delim(nome_arquivo_nfe, delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)
    
    NFE <- NFE |>
      filter(`Cancelada?` == "Não") |>
      subset(select = -c(Número, Chave, `Informações Complementares`, 
                         `CEP Destinatário`, `Bairro Destinatário`, 
                         `Nº Destinatário`, `Logradouro Destinatário`, 
                         Telefone, CEP, Bairro, Nº, Logradouro, 
                         `Fantasia Emitente`, `Desc. CNAE`, 
                         `Cód. CNAE`, Emitente, `CNPJ Emitente`,
                         `Inscrição Estadual`, Série, Modelo, `Data Saída`))
    
    NF <- left_join(Prod_NF, NFE, by = "id_nfe")
    
    NF <- NF |>
      filter(`UF Destinatário` == "PB")
    
    return(NF)
}


# processar dados de vários anos

dados_2019 <- processar_dados_ano(2019)
dados_2020 <- processar_dados_ano(2020)
dados_2021 <- processar_dados_ano(2021)
dados_2022 <- processar_dados_ano(2022)
dados_2023 <- processar_dados_ano(2023)

#igualando os tipos de variáveis dos dataframes

colunas_valor <- grep("^Valor", names(dados_2020), value = TRUE)

dados_2019[colunas_valor] <- lapply(dados_2019[colunas_valor], 
                                    function(x) as.numeric(gsub(",", ".", x)))

dados_2020[colunas_valor] <- lapply(dados_2020[colunas_valor], 
                                    function(x) as.numeric(gsub(",", ".", x)))

dados_2021[colunas_valor] <- lapply(dados_2021[colunas_valor], 
                                    function(x) as.numeric(gsub(",", ".", x)))

dados_2022[colunas_valor] <- lapply(dados_2022[colunas_valor], 
                                    function(x) as.numeric(gsub(",", ".", x)))

dados_2023[colunas_valor] <- lapply(dados_2023[colunas_valor], 
                                    function(x) as.numeric(gsub(",", ".", x)))

dados_2020$`Código Município Destinatário` <- as.numeric(
  dados_2020$`Código Município Destinatário`)

dados_2021$`Código Município Destinatário` <- as.numeric(
  dados_2021$`Código Município Destinatário`)

dados_2022$`Código Município Destinatário` <- as.numeric(
  dados_2022$`Código Município Destinatário`)

dados_2023$`Código Município Destinatário` <- as.numeric(
  dados_2023$`Código Município Destinatário`)

#Acrescetando resto das informações da nota

fica_nf <- c("id_nfe",  "Chave", "Cód. CNAE")


NFE_2019 <- read_delim("NFE - 2019.csv",
                       delim = ";", escape_double = FALSE, 
                       trim_ws = TRUE) %>%
  dplyr::select(one_of(fica_nf))

NFE_2020 <- read_delim("NFE - 2020.csv",
                       delim = ";", escape_double = FALSE,
                       trim_ws = TRUE) %>%
  dplyr::select(one_of(fica_nf))

NFE_2021 <- read_delim("NFE - 2021.csv",
                       delim = ";", escape_double = FALSE,
                       trim_ws = TRUE) %>%
  dplyr::select(one_of(fica_nf))

NFE_2022 <- read_delim("NFE - 2022.csv",
                       delim = ";", escape_double = FALSE,
                       trim_ws = TRUE) %>%
  dplyr::select(one_of(fica_nf))

NFE_2023 <- read_delim("NFE - 2023.csv",
                       delim = ";", escape_double = FALSE, 
                       trim_ws = TRUE) %>%
  dplyr::select(one_of(fica_nf))


dados_2019 <- left_join(dados_2019, NFE_2019, by = "id_nfe")

dados_2020 <- left_join(dados_2020, NFE_2020, by = "id_nfe")

dados_2021 <- left_join(dados_2021, NFE_2021, by = "id_nfe")

dados_2022 <- left_join(dados_2022, NFE_2022, by = "id_nfe")

dados_2023 <- left_join(dados_2023, NFE_2023, by = "id_nfe")



# Combinando todos os dataframes em um único dataframe

nf_total <- bind_rows(dados_2019, dados_2020, dados_2021,
                             dados_2022, dados_2023)

#apagar dataframes intermediários

rm(dados_2019, dados_2020, dados_2021, dados_2022, dados_2023, NFE_2019, 
   NFE_2020, NFE_2021, NFE_2022, NFE_2023)

# Acrescentando dados de liquidação

liq <- readRDS("C:/Users/brads/Dropbox/PPGMMC/Dissertação/Dados/liq.rds")

fica_liq <- c("numero_empenho", "dt_Empenho", "cpf_cnpj_credor",
              "Valor_Liquidacao", "dt_Liquidacao", "nu_ChaveAcesso",
              "numero_licitacao", "codigo_modalidade_licitacao",
              "de_TipoLicitacao", "NomeFonteDeRecurso","nome_funcao",
              "nome_sub_funcao", "nome_categoria_economica",
              "nome_natureza_despesa", "nome_modalidade", "nome_elemento_despesa",
              "nome_sub_elemento")

liq <- liq |> 
  dplyr::select(one_of(fica_liq))

liq$nu_ChaveAcesso <- as.numeric(liq$nu_ChaveAcesso)

nf_todas <- left_join(nf_total, liq, by = c("id_nfe" = "nu_ChaveAcesso"))

###



# Identificar a Unidade mais frequente para cada Código EAN Produto
unidade_mais_frequente <- nf_todas %>%
  group_by(`Código EAN Produto`, Unidade) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  arrange(`Código EAN Produto`, desc(Contagem)) %>%
  group_by(`Código EAN Produto`) %>%
  slice(1) %>%
  ungroup() %>%
  select(`Código EAN Produto`, Unidade)



# Juntar essa informação com o dataframe original para filtrar as linhas
nf_ <- nf_todas %>%
  inner_join(unidade_mais_frequente, by = c("Código EAN Produto", "Unidade"))


###


# Obter dados de localização dos municípios
municipios <- read_municipal_seat()

# Extrair a latitude e a longitude da coluna geom
municipios <- municipios %>%
  mutate(
    lon = st_coordinates(geom)[, 1],
    lat = st_coordinates(geom)[, 2]
  )

# Unir os dados para obter as coordenadas de origem e destino
nf_ <- nf_ %>%
  left_join(municipios, by = c("Código Município" = "code_muni")) %>%
  rename(lat_origem = lat, lon_origem = lon) %>%
  left_join(municipios, by = c("Código Município Destinatário" = "code_muni")) %>%
  rename(lat_destino = lat, lon_destino = lon)

# Função para calcular a distância entre duas coordenadas
calcular_distancia <- function(lat1, lon1, lat2, lon2) {
  distGeo(c(lon1, lat1), c(lon2, lat2)) / 1000 # Converte a distância para quilômetros
}

# Calcular a distância para cada linha do data frame
nf_ <- nf_ %>%
  rowwise() %>%
  mutate(distancia = calcular_distancia(lat_origem, lon_origem, lat_destino, lon_destino)) %>%
  ungroup()


## indentificando os municípios geograficamente

data_pb <- read_csv("~/Dropbox/PPGMMC/Dissertação/Data_pb.csv") |>
  filter(ano == 2013)

# shapefile da Paraiba

pbmap <- st_read("C:/Users/brads/Dropbox/PPGMMC/Dissertação/Script/PB_Municipios_2020.shp")

#Unindo os dados

dados <- nf_ |>
  left_join(data_pb, by = c("Código Município Destinatário" = "cod_mundv"))

rm (data_pb, nf_todas, nf_total, unidade_mais_frequente, liq)

###### LIMPEZA

colnames(dados)

no_use <- c("id_nfe", "Código Produto", "Código NCMP Produto",
            "Código CEST Produto", "Descrição Produto", "Unidade",
            "Valor Produtos", "Data Emissão", "Código Município",
            "Município Emitente", "UF", "CNPJ Destinatário", "Destinatário",
            "UF Destinatário", "Valor Total Produtos",
            "Cancelada?", "Chave", "numero_empenho", "cpf_cnpj_credor",
            "Valor_Liquidacao", "name_muni.x", "code_state.x", "abbrev_state.x",
            "code_region.x", "name_region.x", "year.x", "geom.x", "lon_origem",
            "lat_origem", "name_muni.y", "code_state.y", "abbrev_state.y",
            "code_region.y", "name_region.y", "year.y", "geom.y", "lon_destino",
            "lat_destino", "cod_uf", "nome_uf", "cod_mesoregiao",
            "cod_microrregiao", "cod_municipio", "Cidade", "cod_sagres", "ano",
            "idhm_long", "idhm_renda")              


dados <- dados |>
  dplyr::select(-one_of(no_use))

# Restos a pagar

RaPg <- readxl::read_excel("RaPg.xlsx")

sem_uso <- c("cd UG", "Um Gestora", "NumEmp", "Unid Orcam")

RC <- readxl::read_excel("recita_corrente_2019-2023.xlsx")

RaPg <- RaPg |>
  dplyr::select(-one_of(sem_uso))

# Convertendo as colunas de data para o formato de data

RaPg$DtPgto <- ymd(RaPg$DtPgto) 
RaPg$`Dt Emp` <- ymd(RaPg$`Dt Emp`)

# Calculando o tempo médio de pagamento por município e por ano de pagamento
RaPg$Tempo_Pagamento <- as.numeric(difftime(RaPg$DtPgto, 
                                            RaPg$`Dt Emp`, units = "days"))

Tempo_Medio_Pagamento <- RaPg |>
  mutate(Ano_Pgto = year(DtPgto)) |>
  group_by(Municipio, Ano_Pgto) |>
  summarise(Tempo_Medio = mean(Tempo_Pagamento, na.rm = TRUE))

Tempo_Medio_Pagamento$Municipio <- toupper(Tempo_Medio_Pagamento$Municipio)|>
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT")


Tempo_Medio_Pagamento$Municipio <- gsub("[^a-zA-Z ]", "", 
                                        Tempo_Medio_Pagamento$Municipio)


nf_top <- left_join(
  nf_top, Tempo_Medio_Pagamento,
  by = c("Município Destinatário" = "Municipio", "Ano" = "Ano_Pgto"))

saveRDS(nf_top, file = "nf_top.rds")


#### Choropleth

pbmap$NM_MUN <- toupper(pbmap$NM_MUN)|>
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

pbmap$NM_MUN <- gsub("[^a-zA-Z ]", "", pbmap$NM_MUN)

Tempo_Medio_Pagamento <- 
  Tempo_Medio_Pagamento |>
  left_join(pbmap, by = c("Municipio" = "NM_MUN"))



p1 <- Tempo_Medio_Pagamento|>
  filter(Ano_Pgto == 2019) |>
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = Tempo_Medio)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Demora de Pagamento dos Municípios em 2019", 
       fill = "Atraso em dias") +
  theme_minimal()

p2 <- Tempo_Medio_Pagamento|>
  filter(Ano_Pgto == 2020) |>
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = Tempo_Medio)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Demora de Pagamento dos Municípios em 2020", 
       fill = "Atraso em dias") +
  theme_minimal()

p3 <- Tempo_Medio_Pagamento|>
  filter(Ano_Pgto == 2021) |>
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = Tempo_Medio)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Demora de Pagamento dos Municípios em 2021", 
       fill = "Atraso em dias") +
  theme_minimal()

p4 <- Tempo_Medio_Pagamento|>
  filter(Ano_Pgto == 2022) |>
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = Tempo_Medio)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Demora de Pagamento dos Municípios em 2022", 
       fill = "Atraso em dias") +
  theme_minimal()

# Combinando os mapas
mapas_combinados <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
print(mapas_combinados)


#Receita corrente líquida

receita_corrente_2019_2023 <- read_excel("recita_corrente_2019-2023.xlsx", 
                                         col_types = c("text", "text", "text", 
                                                       "numeric"))


receita_corrente_2019_2023 <- receita_corrente_2019_2023 |>
  mutate(dt_mesano = dmy(paste("01", 
                               dt_mesano, sep = "")),
         mes = month(dt_mesano))

receita_corrente_2019_2023$mes <- as.numeric(receita_corrente_2019_2023$mes)
receita_corrente_2019_2023$dt_Ano <- as.numeric(
  receita_corrente_2019_2023$dt_Ano)

receita_corrente_2019_2023$no_municipio <- toupper(
  receita_corrente_2019_2023$no_municipio)|>
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

receita_corrente_2019_2023$no_municipio <- gsub(
  "[^a-zA-Z ]", "", receita_corrente_2019_2023$no_municipio)

receita_corrente_2019_2023 <- receita_corrente_2019_2023 |>
  dplyr::select(-dt_mesano)

dados <- dados |>
  left_join(receita_corrente_2019_2023, 
            by = c("Município Destinatário" = "no_municipio", 
                   "Ano" = "dt_Ano" , "Mês" = "mes" ))

rm(municipios, receita_corrente_2019_2023)

saveRDS(nf_top, file = "nf_top.rds")

nf_top_clean <- readRDS("~/Dropbox/PPGMMC/Dissertação/Dados/nf_top_clean.rds")

#deflacionando

FIPE_19_23 <- read_excel("FIPE 19-23.xlsx")

FIPE_19_23$GeralAc <- cumsum(FIPE_19_23$Geral)

meses_mapa <- c("jan" = 1, "fev" = 2, "mar" = 3, "abr" = 4, 
                "mai" = 5, "jun" = 6, "jul" = 7, "ago" = 8, 
                "set" = 9, "out" = 10, "nov" = 11, "dez" = 12)


FIPE_19_23$Mês <- meses_mapa[FIPE_19_23$Mês]

dados <- left_join(dados, FIPE_19_23, by = c("Ano", "Mês"))

# Deflacionando os Preços da saúde

dados$Preço <- dados$`Valor Unitário`

dados$Preço  <- dados$Preço / (1 + dados$GeralAc)

# Deflacionando a Receita 

dados$ReceitaCorrente <- dados$ReceitaCorrente/
  (1 + dados$GeralAc)


## Criando a variável Custo de Transação Normalizado sem outliers

dados <- dados %>%
  group_by(`Código EAN Produto`) %>%
  mutate(
    media_preco = mean(Preço, na.rm = TRUE),
    sd_preco = sd(Preço, na.rm = TRUE)
  ) %>%
  filter(Preço >= (media_preco - 3 * sd_preco) & Preço <= (media_preco + 3 * sd_preco)) %>%
  mutate(
    custo_trans_nor = (Preço - media_preco) / sd_preco
  ) %>%
  ungroup() %>%
  filter(!is.na(custo_trans_nor)) %>%
  select(-media_preco, -sd_preco)

## Acrescentar dados eleitorais

Prefeitos_2016 <- read_excel("Prefeitos 2016.xlsx") %>%
  select(-ANO)

Prefeitos_2020 <- read_excel("Prefeitos 2020.xlsx")%>%
  select(-ANO)

Prefeitos_2020$Município <- toupper(Prefeitos_2020$Município)
Prefeitos_2020$Município <- iconv(Prefeitos_2020$Município, from = "UTF-8", to = "ASCII//TRANSLIT")

Prefeitos_2016$Município <- toupper(Prefeitos_2016$Município)
Prefeitos_2016$Município <- iconv(Prefeitos_2016$Município, from = "UTF-8", to = "ASCII//TRANSLIT")

# Remover caracteres não alfabéticos dos nomes de municípios
Prefeitos_2020$Município <- gsub("[^a-zA-Z ]", "", Prefeitos_2020$Município)

Prefeitos_2016$Município <- gsub("[^a-zA-Z ]", "", Prefeitos_2016$Município)


# Filtrar o data frame df para o ano de 2019
df_2019 <- df %>%
  filter(Ano == 2019)

# Filtrar o data frame df para os anos de 2020 ou maior
df_2020_onwards <- df %>%
  filter(Ano >= 2020)

# Unir df_2019 com Prefeitos_2016 pelo município
df_2019_merged <- df_2019 %>%
  left_join(Prefeitos_2016, by = c("Município Destinatário" = "Município"))

# Unir df_2020_onwards com Prefeitos_2020 pelo município
df_2020_merged <- df_2020_onwards %>%
  left_join(Prefeitos_2020, by = c("Município Destinatário" = "Município"))

# Combinar os dois data frames unidos em um único data frame
df_final <- bind_rows(df_2019_merged, df_2020_merged)

rm(df_2020_merged,df_2019_merged, df_2019, df_2020_onwards, Prefeitos_2016, Prefeitos_2020)


# Corrigir

dados$sexo <- as.factor(dados$sexo)
dados$partido <- as.factor(dados$partido)
dados$licit <- as.factor(dados$licit)


#Salvar
saveRDS(dados, file = "dados-novo.rds")





