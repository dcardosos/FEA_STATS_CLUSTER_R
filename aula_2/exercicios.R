library(magrittr)

# EX 1- Obter a	matriz de distância Euclidiana	para as variáveis padronizadas.
tibble::tibble(
  
  nome = c('Angus Bacon', 'Quarteirão', 'Big Mac', 
           'McFish', 'Hamburguer', 'Cheddar McMelt'),
  valor_energetico = c(861, 558, 504, 373, 257, 458),
  carboidratos = c(57, 36, 41, 38, 31, 29),
  colesterol = c(145, 86, 54, 43, 22, 63),
  proteinas = c(54, 31, 25, 18, 13, 27)) ->
  
  burguers


(burguers %>% 
  recipes::recipe(nome ~ .) %>% 
  recipes::step_normalize(recipes::all_numeric_predictors()) %>% 
  recipes::prep() %>% 
  recipes::bake(new_data = NULL) %>% 
  dplyr::select(-nome) %>% 
  dist(method = 'euclidean') -> 
  matriz_euclideana)

### coisas a mais

matriz_euclideana %>% 
  hclust('single') -> clusters_b 

clusters_b %>% 
  as.dendrogram() %>% 
  dendextend::set_labels(burguers$nome[clusters_b$order]) %>% 
  plot()


# EX 2 - Faça o Gráfico de dispersão e agrupe os países em 3 grupos.

tibble::tibble(
  paises = c('Áustria', 'Espanha', 'Dinamarca', 'Bulgária', 'Reino Unido', 'Itália'),
  deficit = c(1.5, 6.8, 0.7, 1.2, 5.8, 2.8),
  gastos_educacao = c(5.6, 4.5, 7.9, 3.5, 6.1, 4.2)) -> countries


# grafico de dispersao
countries %>% 
  ggplot2::ggplot(ggplot2::aes(x = deficit, y = gastos_educacao, color = paises)) +
  ggplot2::geom_point(size = 8) + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = 'Déficit', 
                y = 'Gastos com educação', 
                title = 'Relação entre o déficit orçamentario e os gastos com educação da União')

# linked hierarquical cluster function
hclust_dendogram <- function(df, .target, .dist_method = 'euclidean', .hclust_method = 'single'){
  
  df %>%
    dplyr::select(-{{.target}}) %>%
    recipes::recipe() %>% 
    recipes::step_normalize(recipes::all_numeric()) %>% 
    recipes::prep() %>% 
    recipes::bake(new_data = NULL) %>% 
    dist(method = .dist_method) %>% 
    hclust(method = .hclust_method) -> clusters 

  clusters %>% 
    as.dendrogram() %>% 
    dendextend::set_labels(dplyr::pull(df, {{.target}})[clusters$order]) %>% 
    plot()
}

hclust_dendogram(countries, paises)
