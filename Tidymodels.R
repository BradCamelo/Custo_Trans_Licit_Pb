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
library(xgboost)

rm(list = ls(all = TRUE))

#setwd("~/Dropbox/PPGMMC/Dissertação/Script")

setwd("C:/Users/brads/Dropbox/PPGMMC/Dissertação/Script/Custo_Trans_Licit_Pb")

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


# Modelo de regressão linear

modelo <- lm(Custo_Tran_norm ~ Quantidade + idhm + populacao + Tempo_Medio + 
               distancia + ReceitaCorrente + Licit +
               n_licit + Precatorio, data = df_train)

# Exibir o resumo do modelo

summary(modelo)



#Distribuição das variáveis independentes

dados_treinamento %>%
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

receita <- 
  dados_treinamento |>
  recipes::recipe(custo_tran_norm ~ ., data = _) |>
  recipes::step_dummy(all_nominal_predictors()) |> 
  recipes::step_nzv(all_predictors()) |>
  recipes::step_impute_knn(all_numeric_predictors(), 
                           impute_with = imp_vars(quantidade, valor_total_nota, precatorio, preco)) |>
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
previsoes <- predict(modelo_carregado, new_data = dados)

# Combinando previsões com os valores reais
resultados <- bind_cols(dados_teste_preparados, previsoes) |>
  rename(previsao = .pred)

# Combinando previsões com os valores reais
resultados <- bind_cols(real = dados$custo_tran_norm, estimado = previsoes) 

ggplot(resultados, aes(x = real, y = .pred)) +
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

## LASSO-----

recipe_lasso <-  
  dados_treinamento |>
  recipe(custo_tran_norm ~ .) |>
  step_dummy(all_nominal_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |>  
  step_impute_mode(all_nominal_predictors()) |>    
  step_normalize(all_numeric_predictors()) |> 
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |> 
  step_factor2string(all_nominal_predictors())

workflow_lasso <- workflow() |>
  add_model(linear_reg(penalty = tune(), mixture = 1) |>
              set_engine("glmnet")
  ) |>
  add_recipe(recipe_lasso)

# grid para lasso
lasso_grid <- grid_regular(penalty(),
                           levels = 50)


folds <- vfold_cv(dados_treinamento, v = 5)

lasso_res <-
  workflow_lasso |>
  tune_grid(
    resamples = folds,
    metrics = metric_set(rsq, rmse),
    grid = lasso_grid
  )


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


# Selecionar o melhor modelo com base no RMSE
best_lasso <- lasso_res %>% 
  select_best(metric = "rmse")

# Finalizar o workflow com os melhores hiperparâmetros
final_wf_lasso <- 
  workflow_lasso %>% 
  finalize_workflow(best_lasso)

# Ajustar o modelo final com o conjunto de dados de treino e teste
final_fit <- final_wf_lasso %>% 
  last_fit(dados_split)

#gráficos
final_fit |> 
  collect_predictions() |>
  ggplot(aes(custo_tran_norm, .pred))+
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red",
              size = 1.5, linetype = "dashed") +
  theme_bw() +
  ylab("Valores Previstos") +
  xlab("Valores Observados")


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
  xlab("Penalty")

# Calculando as métricas R² e RMSE

final_metrics <- final_fit %>%
  collect_metrics()

# Visualizando as métricas

kable(final_metrics, format = "latex", booktabs = TRUE)

## Gradient Boosting ------

modelo <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  learn_rate = tune()
) |>
  set_engine("xgboost", nthread = parallel::detectCores()) |>
  set_mode("regression")

# Configurando o workflow -------------------------------------------------
wf <- workflow() |> 
  add_model(modelo) |>
  add_recipe(receita)

# Validacao-cruzada -------------------------------------------------------
cv <- vfold_cv(dados_treinamento_preparados, v = 5L, strata = custo_tran_norm)

# Tunando hiperparametros -------------------------------------------------
tunagem <- 
  tune_grid(
    wf,
    resamples = cv,
    grid = 20L,
    metrics = metric_set(rsq, rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )

# Listando os 10 melhores modelos -----------------------------------------
top_models <- tunagem |> 
  show_best(metric = "rsq", n = 10L)

print(top_models)

ggplot2::autoplot(tunagem)

# Selecionando os melhores hiperparametros --------------------------------
wf <- 
  wf |> 
  finalize_workflow(select_best(tunagem, metric = "rsq"))

# Avaliando o modelo ------------------------------------------------------
split <- initial_split(dados_treinamento)
modelo_final <- 
  last_fit(
    wf, 
    split, 
    metrics = metric_set(rsq, rmse, mae)
  )

# Visualizando as métricas finais -----------------------------------------
results <- collect_metrics(modelo_final)
print(results)

# Modelo final ------------------------------------------------------------
modelo_final <- fit(wf, dados_teste)

#Apresentações
best_params <- select_best(tunagem, metric = "rmse")
kable(best_params, caption = "Melhores Hiperparâmetros Encontrados", format = "latex")

predicoes <- predict(modelo_final, dados_teste) |> 
  bind_cols(dados_teste)

# Gráfico de resultados reais vs previstos
ggplot(predicoes, aes(x = custo_tran_norm, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Valores previstos x observados: Gradient Boosting",
    x = "Valores Observados",
    y = "Valores previstos"
  ) +
  theme_minimal()
