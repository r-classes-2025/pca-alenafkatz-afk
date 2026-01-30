# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. отберите 6 главных персонажей (по количеству реплик)
top_speakers <- friends |> 
  count(speaker, name = "n_replicas") |> 
  arrange(desc(n_replicas)) |> 
  slice_head(n = 6) |> 
  pull(speaker)

# 2. токенизация и удаление цифр
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  filter(!str_detect(word, "\\d")) |>  
  select(speaker, word)

# 3. отбор 500 слов и расчет tf
friends_tf <- friends_tokens |> 
  count(speaker, word, name = "n") |> 
  group_by(speaker) |> 
  mutate(tf = n / sum(n)) |> 
  slice_max(order_by = n, n = 500, with_ties = FALSE) |>  
  ungroup() |> 
  select(speaker, word, tf)

# 4. преобразование в широкий формат
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame()

# 5. кластеризация k-means
set.seed(123)
km.out <- kmeans(
  x = scale(friends_tf_wide), 
  centers = 3,                 
  nstart = 20                  
)
names(km.out$cluster) <- rownames(friends_tf_wide)

# 6. PCA через явное масштабирование
# Вариант A: через scale() отдельно
scaled_data <- scale(friends_tf_wide, center = TRUE, scale = TRUE)
pca_fit <- prcomp(scaled_data, center = FALSE, scale. = FALSE)

# ИЛИ Вариант B: с другим округлением
# pca_fit <- prcomp(friends_tf_wide, scale = TRUE)
# pca_fit$sdev <- round(pca_fit$sdev, 6)
# pca_fit$rotation <- round(pca_fit$rotation, 6)
# pca_fit$center <- round(pca_fit$center, 6)
# pca_fit$scale <- round(pca_fit$scale, 6)
# pca_fit$x <- round(pca_fit$x, 6)

# 7. биплот
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
  geom_text(aes(x = pca_fit$x[, 1], 
                y = pca_fit$x[, 2],
                label = rownames(friends_tf_wide)),
            vjust = -0.8,  
            size = 4,
            fontface = "bold",
            show.legend = FALSE) +
  theme(legend.position = "none")

print(q)