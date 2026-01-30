# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. ВЕРНУТЬ динамический порядок!
top_speakers <- friends |> 
  count(speaker, name = "n_replicas") |> 
  arrange(desc(n_replicas)) |> 
  slice_head(n = 6) |> 
  pull(speaker)

# 2. Удаление цифр как в задании
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  filter(!str_detect(word, "\\d")) |>  
  select(speaker, word)

# 3. ТОЧНО как в задании - slice_max с with_ties = FALSE
friends_tf <- friends_tokens |> 
  count(speaker, word, name = "n") |> 
  group_by(speaker) |> 
  mutate(tf = n / sum(n)) |> 
  slice_max(order_by = n, n = 500, with_ties = FALSE) |>  
  ungroup() |> 
  select(speaker, word, tf)

# 4. Преобразование в широкий формат
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame()

# НЕ УДАЛЯЕМ СТОЛБЦЫ! Оставляем 704 слова
# НЕ СОРТИРУЕМ СЛОВА по алфавиту! pivot_wider дает свой порядок

# 5. K-means 
set.seed(123)
km.out <- kmeans(
  x = scale(friends_tf_wide), 
  centers = 3,                 
  nstart = 20                  
)

# Присваиваем имена
names(km.out$cluster) <- rownames(friends_tf_wide)

# 6. PCA БЕЗ ОКРУГЛЕНИЯ
pca_fit <- prcomp(friends_tf_wide, scale = TRUE)

# 7. Биплот
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
  theme(legend.position = "none") +
  geom_text(aes(x = pca_fit$x[, 1], 
                y = pca_fit$x[, 2],
                label = rownames(friends_tf_wide)),
            vjust = -0.8,  
            size = 4,
            fontface = "bold",
            show.legend = FALSE)

print(q)