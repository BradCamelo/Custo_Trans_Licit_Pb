library(tidymodels)
library(recipes)
library(dplyr)
library(purrr)
library(doParallel)
library(janitor)
library(stringr)
library(ggplot2)

rm(list = ls(all = TRUE))

# Resolvendo conflitos de pacotes:
tidymodels::tidymodels_prefer()

# speed up computation with parallel processing
registerDoParallel(cores = parallel::detectCores(logical = FALSE)) 

# Lendo dados e melhorando nomes ------------------------------------------
dados <- 
  readRDS("df_top_padrao.rds") |>
  janitor::clean_names() |>
  janitor::remove_empty("rows") |>
  janitor::remove_empty("cols") |>
  dplyr::select(
    c(custo_tran_norm, 
      quantidade, 
      idhm, 
      gini, 
      area_km2,
      populacao,
      tempo_medio,
      distancia,
      receita_corrente, 
      licit,
      n_licit,
      precatorio)
  ) |>
  dplyr::mutate(
    quantidade = as.integer(stringr::str_replace(quantidade, ",", ".")),
    custo_tran_norm = as.numeric(custo_tran_norm)
  )
  
# Separando dados de treino e teste --------------------------------------- 
set.seed(0)
dados_split <- initial_split(dados, prop = 0.8, strata = "custo_tran_norm")

# Dados de treinamento
dados_treinamento <- training(dados_split)

# Dados de teste
dados_teste <- testing(dados_split)

# Receita -----------------------------------------------------------------
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
    metrics = metric_set(rsq, rmse),
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
    metrics = metric_set(rsq, rmse)
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
resultados <- bind_cols(real = dados$custo_tran_norm, estimado = previsoes) 

ggplot(resultados, aes(x = real, y = previsao)) +
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
  metrics(truth = real, estimate = previsao)

# Visualizando as métricas
print(metrics)
