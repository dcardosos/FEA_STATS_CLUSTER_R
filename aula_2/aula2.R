library(magrittr)
library(tidymodels)

tibble::tibble(
  nome = c('MIT', 'UColombia', 'UChicago', 'Michigan', 'Oxford', 'USP'),
  num_alunos = c(11067, 25045, 13557, 43147, 19720, 65711),
  num_funcionarios = c(2982, 6189, 2449, 6809, 6750, 5582),
  num_estudantes_estrangeiros = c(3717, 8105, 3379, 7527, 7353, 2086),
  anos = c(157, 264, 162, 201, 922, 191)) -> universidades

universidades %>% 
  recipes::recipe(nome ~ .)  %>% 
  recipes::step_normalize(all_predictors()) %>% 
  recipes::prep() %>% 
  recipes::bake(new_data = NULL) %>% 
  dplyr::select(-nome) %>% 
  dist(method = 'euclidean') %>% 
  hclust(method = 'single') ->
  clusters


factoextra::fviz_dend(clusters)


  
clusters %>% 
  as.dendrogram() %>%
  dendextend::set_labels(universidades$nome[clusters$order]) %>% 
  dendextend::set('branches_k_color') ->
  dendograma

dendograma %>%
  plot() 

dendograma %>% 
  dendextend::rect.dendrogram(k = 2, border = 2, lty = 5, lwd = 2)
