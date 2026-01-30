# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  count(speaker, name = "n_replicas") |> 
  arrange(desc(n_replicas)) |> 
  slice_head(n = 6) |> 
  pull(speaker)

# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  filter(!str_detect(word, "\\d")) |>  
  select(speaker, word)

# 3. ПРАВИЛЬНЫЙ отбор 500 слов (только 3 столбца!)
friends_tf <- friends_tokens |> 
  count(speaker, word, name = "n") |> 
  group_by(speaker) |> 
  mutate(tf = n / sum(n)) |> 
  slice_max(order_by = n, n = 500, with_ties = FALSE) |>  
  ungroup() |> 
  select(speaker, word, tf)  # ТОЛЬКО 3 столбца!

# 4. преобразуйте в широкий формат
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame()

# НЕ СОРТИРУЕМ слова! Оставляем порядок от pivot_wider
# (тесты ожидают порядок по частоте, не по алфавиту!)

# 5. кластеризация k-means
set.seed(123)
km.out <- kmeans(
  x = scale(friends_tf_wide), 
  centers = 3,                 
  nstart = 20                  
)

# Присваиваем имена кластерам
names(km.out$cluster) <- rownames(friends_tf_wide)

# 6. PCA
pca_fit <- prcomp(friends_tf_wide, scale = TRUE)

# 7. биплот С ТЕКСТОМ
q <- fviz_pca_biplot(pca_fit,
                     geom.ind = "point",         
                     geom.var = c("arrow", "text"), 
                     select.var = list(cos2 = 20),
                     col.ind = as.factor(km.out$cluster), 
                     col.var = "steelblue",
                     alpha.var = 0.3,
                     repel = TRUE,
                     ggtheme = theme_minimal(),
                     title = "") +
  # КРИТИЧЕСКИ ВАЖНО: geom_text для имен персонажей
  geom_text(aes(x = pca_fit$x[, 1], 
                y = pca_fit$x[, 2],
                label = rownames(friends_tf_wide)),
            vjust = -0.8,  
            size = 4,
            fontface = "bold",
            show.legend = FALSE) +
  theme(legend.position = "none")

print(q)