rm(list = ls())
gc()

library(tidymodels)
tidymodels_prefer()
library(baguette)
library(ranger)
library(corrplot)
library(tidyverse)
library(GGally)
library(vip)
library(kableExtra)
library(dplyr)
library(scales)
library(kknn)
library(kernlab)
library(e1071)


setwd("~/Dropbox/PPGMMC/Dissertação/Script/Custo_Trans_Licit_Pb")

#setwd("C:/Users/brads/Dropbox/PPGMMC/Script/Custo_Trans_Licit_Pb")


df_top_padrao <- 
  readRDS("df_top_padrao.rds")



df_top_padrao <- df_top_padrao[!is.na(df_top_padrao$ReceitaCorrente) & !is.na(df_top_padrao$n_licit),] 
  
df_top_padrao <- df_top_padrao %>%
  mutate(Quantidade = as.numeric(gsub(",", ".", Quantidade)),
         Licit = as.factor(Licit),
         distancia = as.numeric(distancia),
         Custo_Tran_norm = as.vector(Custo_Tran_norm))



df_top_padrao <- df_top_padrao %>%
  select( c(Custo_Tran_norm, Quantidade, idhm,idhm_edu, idhm_long,
            idhm_renda, gini, area_km2, populacao, Tempo_Medio, 
            distancia, ReceitaCorrente, Licit, n_licit, Precatorio) )

#data_subset <- data_subset %>% slice_sample(prop = 0.7)

  
set.seed(58046088)
df_split <- initial_split(df_top_padrao, strata = Custo_Tran_norm, prop = 3/4)
df_train <- training(df_split)
df_test  <- testing(df_split)


df_folds <- 
  vfold_cv(df_train, strata = Custo_Tran_norm, repeats = 5)

## Dimensionalidade

# Análise Exploratória
#A modelagem foi feita utilizando as variáveis `Valor Produtos`, `Valor Total Produtos`, 
#`Valor Total Nota`, `idhm`, `idhm_edu`, `idhm_long`, `idhm_renda`, `gine`, `renda_km2`, 
#`populaçao`, `distancia`, `Tempo_Medio`, `ReceitaCorrente`.

#O primeiro passo foi observar a quantidade de variáveis independentes que continham valores ausentes. 
#As únicas variáveis com valores ausentes foram o Tempo_Medio e a ReceitaCorrente, que podem ser tratadas 
#com algum método de imputação. Apenas um valor era ausente em cada uma das independentes observadas.


variaveis <- c("Quantidade", "Código Município","Custo_Tran", "pct",
               "idhm", "idhm_edu", "idhm_long", "idhm_renda", "gini",
               "area_km2", "populacao", "Tempo_Medio", "distancia",
               "ReceitaCorrente", "Licit", "n_licit", "Precatorio")  


#As variáveis independentes utilizadas para a explicar a licitação não parecem ter uma 
#forte relação com a variável dependente. O gráfico abaixo nos mostra que todas elas tem uma 
#correlação abaixo de |0.3|. Isso pode acabar gerando resultados inesperados, sendo talvez necessário 
#utilizar alguma outra variável importante para explicar o preço da licitação.

tmwr_cols <- colorRampPalette(c("#196F3D", "#DFFF00"))
df_train %>% 
  select(all_of(variaveis)) %>%
  select(where(is.numeric)) %>% 
  cor(use = "complete.obs") %>% 
  corrplot(col = tmwr_cols(200), tl.col = "black", method = "ellipse")


#É possível observar também que há poucos valores acima de 150, o que pode significar uma baixa 
#performance dos modelos testados parar valores contidos nesse intervalo.

ggplot(df_top_padrao, aes(Custo_Tran_norm)) +
  geom_density() +
  xlim(0, 5) +
  xlab("Custo de Transação Normalizado") +
  ylab("Densidade") +
  theme_bw()


### RF

recipe_rf <- recipe(Custo_Tran_norm ~ Quantidade + `Município Destinatário` + 
                      Custo_Tran + pct + idhm + idhm_edu + idhm_long +
                      idhm_renda + gini + area_km2 + populacao + Tempo_Medio + 
                      distancia + ReceitaCorrente + Licit +
                      n_licit + Precatorio, data = df_train) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Especificar o modelo Random Forest
model_rf <- rand_forest(trees = 1000, mode = "regression") %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Criar um fluxo de trabalho combinando a receita e o modelo
workflow_rf <- workflow() %>%
  add_recipe(recipe_rf) %>%
  add_model(model_rf)

# Treinar o modelo
fit_rf <- workflow_rf %>%
  fit(data = df_train)

# Avaliar o modelo no conjunto de teste

results <- fit_rf %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = Custo_Tran_norm, estimate = .pred)

# Imprimir os resultados das métricas
print(results)

# Ou, se quiser ver a importância das variáveis:

vip <- fit_rf %>%
  pull_workflow_fit() %>%
  vip(geom = "col")

print(vip)





## Receita


receita <- 
  recipe(Custo_Tran_norm ~ Quantidade + `Código Município` + Custo_Tran + pct + 
         idhm + idhm_edu + idhm_long + idhm_renda + gini + 
         area_km2 + populacao + Tempo_Medio + distancia + 
         ReceitaCorrente + Licit + n_licit + Precatorio, data = df_train) 


linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("regression")

svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

knn_spec <- 
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

bag_cart_spec <- 
  bag_tree() %>% 
  set_engine("rpart", times = 50L) %>% 
  set_mode("regression")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

nnet_param <- 
  nnet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(1, 27)))

#WORKFLOW SET

model_vars <- 
  workflow_variables(outcomes = Custo_Tran_norm, 
                     predictors = variaveis)

wf_licit <- 
  workflow_set(
    preproc = list(simple = model_vars), 
    models = list(CART_bagged = bag_cart_spec,
                  RF = rf_spec, SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, 
                  KNN = knn_spec, neural_network = nnet_spec, 
                  linear_reg = linear_reg_spec, KNN = knn_spec)
  )

wf_licit <- 
  workflow_set(
    preproc = list(simple = model_vars), 
    models = list(RF = rf_spec)
  )


#TUNING

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

grid_results <-
  wf_licit %>%
  workflow_map(
    seed = 1981,
    resamples = dfTrain,
    grid = 25,
    control = grid_ctrl
  )

grid_results %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  select(model, .config, rmse = mean, rank)


autoplot(
  grid_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
  theme(legend.position = "none")


#Screening



