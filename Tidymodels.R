rm(list = ls())
gc()

library(tidymodels)
library(recipes)
library(dplyr)
library(purrr)
library(doParallel)
library(janitor)
library(stringr)
library(ggplot2)
library(vip)
library(knitr)


rm(list = ls(all = TRUE))

setwd("~/Dropbox/PPGMMC/Dissertação/Script")

# Resolvendo conflitos de pacotes:
tidymodels::tidymodels_prefer()

# speed up computation with parallel processing
registerDoParallel(cores = parallel::detectCores(logical = FALSE)) 

# Lendo dados e melhorando nomes ------------------------------------------
dados <- 
  readRDS("dados-novo.rds") |>
  janitor::clean_names() |>
  janitor::remove_empty("rows") |>
  janitor::remove_empty("cols")


# Separando dados de treino e teste --------------------------------------- 
set.seed(0)
dados_split <- initial_split(dados, prop = 0.8, strata = "custo_tran_norm")

# Dados de treinamento
dados_treinamento <- training(dados_split)

# Dados de teste
dados_teste <- testing(dados_split)

<<<<<<< HEAD
# Receita -----------------------------------------------------------------
=======
df_top_padrao <- df_top_padrao %>%
  select( c(Custo_Tran_norm, Quantidade, idhm, gini, area_km2, 
            populacao, Tempo_Medio, distancia, ReceitaCorrente, 
            Licit, n_licit, Precatorio) )

#data_subset <- data_subset %>% slice_sample(prop = 0.7)

df_top_padrao <- df_top_padrao %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(distancia = log(distancia + 1)) 


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



ggplot(df_top_padrao, aes(Custo_Tran_norm)) +
  geom_density() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  xlim(-3,4.5) +
  xlab("Custo de Transação Normalizado") +
  ylab("Densidade") +
  theme_bw()


## Baseado no script do Gabriel

# Modelo de regressão linear

modelo <- lm(Custo_Tran_norm ~ Quantidade + idhm + populacao + Tempo_Medio + 
               distancia + ReceitaCorrente + Licit +
               n_licit + Precatorio, data = df_train)

# Exibir o resumo do modelo

summary(modelo)



# Filtrar as colunas necessárias e criar o gráfico de pares

df_train %>%
  select(Custo_Tran_norm, idhm, populacao,
         Tempo_Medio, distancia, ReceitaCorrente, Licit,
         n_licit, Precatorio) %>%
  ggpairs(axisLabels = "none") +
  theme_bw()


#Distribuição das variáveis independentes

df_train %>%
  select( c(Quantidade, `Município Destinatário`, idhm,
            gini, area_km2, populacao, Tempo_Medio, distancia, 
            ReceitaCorrente, Licit, n_licit, Precatorio) ) %>% 
  select_if(is.numeric) |> 
  gather(cols, value) |>
  ggplot(aes(x = value)) + 
  geom_density(color = "black") +
  facet_wrap(.~cols, ncol = 3, nrow = 3, scales = "free") +
  theme_bw() +
  ylab("Densidade") +
  xlab("Valor")

## receita

# Selecionar apenas colunas numéricas
df_numeric <- df_train %>% select_if(is.numeric)

# Calcular a assimetria (skewness) e variância para cada variável numérica
skewness_values <- sapply(df_numeric, skewness, na.rm = TRUE)
variance_values <- sapply(df_numeric, var, na.rm = TRUE)

# checagem
print("Assimetria das variáveis:")
print(skewness_values)
print("Variância das variáveis:")
print(variance_values)

# Identificar variáveis numéricas com assimetria significativa para transformação logarítmica
numeric_skewed <- names(skewness_values[skewness_values > 1 | skewness_values < -1])

# Adicionar transformações ao recipe
recipe_rf <- recipe(Custo_Tran_norm ~ ., data = df_train) |>
  step_log(all_of(numeric_skewed), base = exp(1)) |>
  step_dummy(all_nominal_predictors()) |> 
  step_impute_bag(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors())


# receita individual para lasso
recipe_lasso <-  
  df_train |>
  recipe(Custo_Tran_norm ~ .) |>
  # criar variáveis dummies com cada classe de variáveis categóricas
  step_dummy(all_nominal_predictors()) |> 
  step_impute_bag(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |> 
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |> 
  step_factor2string(all_nominal_predictors())

# workflow
# o workflow serve para juntar a parte de preprocessamento e o modelo a ser utilizado

# workflow para random forest
workflow_rf <- 
  workflow() |>
  # modelo random forest
  add_model(rand_forest(mtry = tune(), trees = tune(), min_n = 2) |>
              set_engine("ranger", importance = "impurity", seed = 1) |>
              set_mode("regression")) |>
  # adicionar receita 
  add_recipe(recipe_rf)


# workflow para lasso
workflow_lasso <- workflow() |>
  # modelo lasso
  add_model(linear_reg(penalty = tune(), mixture = 1) |>
              set_engine("glmnet")
  ) |>
  # adiciona receita do lasso
  add_recipe(recipe_lasso)


# a otimização do modelo pode ser feita utilizando grid search, encontrar a melhor combinação entre os hiperparametros do modelo

# grid para random forest
tree_grid <- grid_regular(trees(range = c(350, 1000)),
                          mtry(range = c(7, 13)),
                          levels = 5)

# grid para lasso
lasso_grid <- grid_regular(penalty(),
                           levels = 50)

# criação de fold para observar as métricas do modelo em cada fold e tomar o melhor entre eles
folds <- vfold_cv(df_train, v = 5)

# tunagem random forest
tree_res <-
  workflow_rf |>
  tune_grid(
    resamples = folds,
    metrics = metric_set(rsq, rmse),
    grid = tree_grid
  )

# tunagem lasso
lasso_res <-
  workflow_lasso |>
  tune_grid(
    resamples = folds,
    metrics = metric_set(rsq, rmse),
    grid = lasso_grid
  )

#Métricas para cada combinação no Grid Search RF

tree_res %>%
  collect_metrics() %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(mtry, mean, color = trees)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
  ylab("Média") +
  xlab("Quantidade de variáveis independentes") +
  theme_bw()

# Métricas para cada combinação no Grid Search em cada Fold (Lasso)

lasso_res |> 
  collect_metrics() |> 
  ggplot(aes(penalty, mean)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
  ylab("Média") +
  xlab("Penalty") +
  theme_bw()

#Valores previstos x observados

# modelo lasso com o melhor penalty encontrados pelo grid search
final_wf_lasso <- 
  workflow_lasso %>% 
  finalize_workflow(
    lasso_res |> 
      select_best("rmse")
  ) |> 
  last_fit(df_split)


# modelo random forest com os melhores hiperparametros encontrados pelo grid search
final_wf_rf <- 
  workflow_rf %>% 
  finalize_workflow(
    tree_res |> 
      select_best(metric = "rmse")
  ) |> 
  last_fit(df_split)


final_wf_rf |> 
  collect_predictions() |>
  ggplot(aes(Custo_Tran_norm, .pred))+
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red",
              size = 1.5, linetype = "dashed") +
  theme_bw() +
  ylab("Valores Previstos") +
  xlab("Valores Observados")


final_wf_lasso |> 
  collect_predictions() |>
  ggplot(aes(Custo_Tran_norm, .pred))+
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red",
              size = 1.5, linetype = "dashed") +
  theme_bw() +
  ylab("Valores Previstos") +
  xlab("Valores Observados")

#Variáveis mais importantes na Random Forest

final_wf_rf |> 
  extract_fit_engine() |> 
  vip() +
  theme_bw()

# Previsões RF

rf_fit <- fit(workflow_rf, train_data)


predictions <- predict(rf_fit, test_data, type = "regression") %>%
  bind_cols(test_data)

# Calcular métricas
results <- predictions %>%
  metrics(truth = Custo_Tran_norm, estimate = .pred) %>%
  add_metric(metric = rmse, truth = Custo_Tran_norm, estimate = .pred) %>%
  add_metric(metric = rsq, truth = Custo_Tran_norm, estimate = .pred)



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


>>>>>>> parent of e18ae2d (Novo commit)
receita <- 
  dados_treinamento |>
  recipes::recipe(custo_tran_norm ~ ., data = _) |>
  recipes::step_dummy(all_nominal_predictors()) |> 
  recipes::step_nzv(all_predictors()) |>
  recipes::step_impute_knn(all_numeric_predictors(), impute_with = imp_vars(quantidade, precatorio)) |>
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)

# Pre-processamento separado do treinamento -------------------------------
receita_preparada <- prep(receita, training = dados_treinamento)
dados_treinamento_preparados <- bake(receita_preparada, new_data = NULL)

# Configurando o modelo ---------------------------------------------------
modelo <- rand_forest(min_n = tune::tune(), mtry = tune::tune()) |>
  set_engine('ranger', importance = "impurity", num.threads = parallel::detectCores()) |>
  set_mode('regression')

# Configurando o workflow -------------------------------------------------
wf <- workflow() |> 
  add_model(modelo) |>
  add_formula(custo_tran_norm ~ .)

# Validacao-cruzada -------------------------------------------------------
cv <- vfold_cv(dados_treinamento_preparados, v = 5L, strata = custo_tran_norm)

# Tunando hiperparametros -------------------------------------------------
tunagem <-
  tune_grid(
    wf,
    resamples = cv,
    grid = 20L,
    metrics = metric_set(rsq, yardstick::rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE)
  )

# Listando os 10 melhores modelos -----------------------------------------
tunagem |> 
  show_best(n = 10L)

ggplot2::autoplot(tunagem)

# Selecionando os melhores hiperparametros --------------------------------
wf <- 
  wf |> 
  remove_formula() |>
  add_recipe(receita) |>
  finalize_workflow(select_best(tunagem, metric = "rsq"))

# Avaliando o modelo ------------------------------------------------------
modelo <- 
  last_fit(
    wf, 
    dados_split, 
    metrics = metric_set(rsq, yardstick::rmse)
  )

# Visualizando as métricas finais -----------------------------------------
collect_metrics(modelo)

# Modelo final ------------------------------------------------------------
modelo_final <- fit(wf, dados)

# Salvando o modelo para usar depois --------------------------------------
saveRDS(modelo_final, file = "modelo_random_forest.rds")

# Lendo modelo treinado ---------------------------------------------------
modelo_carregado <- readRDS("modelo_random_forest.rds")

# Fazendo previsões no conjunto de dados de teste
previsoes <- predict(modelo_carregado, new_data = dados_treinamento)

# Combinando previsões com os valores reais
resultados <- bind_cols(dados_treinamento_preparados, previsoes) |>
  rename(previsao = .pred)

# Combinando previsões com os valores reais
#resultados <- bind_cols(real = dados$custo_tran_norm, estimado = previsoes) 

ggplot(resultados, aes(x = custo_tran_norm, y = previsao)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Valores Observados vs. Valores Previstos",
    x = "Valores Observados",
    y = "Valores Previstos"
  ) +
  theme_minimal()

# Extraindo o mecanismo de ajuste
fit_engine <- modelo_carregado |> 
  extract_fit_engine()

# Visualizando a importância das variáveis
vip_plot <- vip(fit_engine) +
  theme_bw()

# Mostrar o gráfico
print(vip_plot)

# Calculando as métricas R² e RMSE
metrics <- resultados |>
  metrics(truth = custo_tran_norm, estimate = previsao)

# Visualizando as métricas
print(metrics)

kable(metrics, format = "latex", booktabs = TRUE)
