# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. Используем ТОЧНО тот же порядок, что в тестах!
top_speakers <- c("Rachel Green", "Ross Geller", "Chandler Bing", 
                  "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")

# 2. КАК В САМОМ НАЧАЛЕ (просто фильтр)
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  filter(!str_detect(word, "\\d")) |>  
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
friends_tf <- friends_tokens |> 
  count(speaker, word, name = "n") |> 
  group_by(speaker) |> 
  mutate(tf = n / sum(n)) |>  
  arrange(speaker, desc(n), word) |>  
  group_by(speaker) |> 
  slice(1:500) |>  
  ungroup() |> 
  select(speaker, word, tf) 

# 4. преобразуйте в широкий формат
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame()

# СОРТИРОВКА: используем ТОЧНО тот же порядок персонажей
friends_tf_wide <- friends_tf_wide[top_speakers, ]
# Слова сортируем по алфавиту
friends_tf_wide <- friends_tf_wide[, sort(colnames(friends_tf_wide))]

cat("Финальный размер:", dim(friends_tf_wide), "\n")

# 5. кластеризация k-means
set.seed(123)
km.out <- kmeans(
  x = scale(friends_tf_wide), 
  centers = 3,                 
  nstart = 20                  
)

# КРИТИЧЕСКИ ВАЖНО: Присваиваем имена кластерам
names(km.out$cluster) <- rownames(friends_tf_wide)

# 6. PCA
pca_fit <- prcomp(friends_tf_wide, scale = TRUE)

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
  theme(legend.position = "none") +
  geom_text(aes(x = pca_fit$x[, 1], 
                y = pca_fit$x[, 2],
                label = rownames(friends_tf_wide)),
            vjust = -0.8,  
            size = 4,
            fontface = "bold",
            show.legend = FALSE)

print(q)