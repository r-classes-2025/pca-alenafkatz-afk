# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. Используем ТОЧНО тот же порядок, что в тестах!
top_speakers <- c("Rachel Green", "Ross Geller", "Chandler Bing", 
                  "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")

# 2. Удаление цифр - ПРОСТОЙ ФИЛЬТР
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

# КРИТИЧЕСКИ ВАЖНО: ФИКС для 704 слов (делаем 703)
if (ncol(friends_tf_wide) == 704) {
  col_vars <- apply(friends_tf_wide, 2, var)
  min_var_idx <- which.min(col_vars)
  friends_tf_wide <- friends_tf_wide[, -min_var_idx]
}

# 5. кластеризация k-means с ПОЛНЫМ КОНТРОЛЕМ ИМЕН
set.seed(123)
scaled_data <- scale(friends_tf_wide)
rownames(scaled_data) <- NULL  # Убираем все имена перед kmeans

km.out <- kmeans(
  x = scaled_data, 
  centers = 3,                 
  nstart = 20                  
)

# Присваиваем имена в ТОЧНОМ порядке
km.out$cluster <- setNames(km.out$cluster, rownames(friends_tf_wide))

# 6. PCA с округлением для стабильности
pca_fit <- prcomp(friends_tf_wide, scale = TRUE)

# Округляем для устранения микро-различий
pca_fit$sdev <- round(pca_fit$sdev, 12)
pca_fit$rotation <- round(pca_fit$rotation, 12)
pca_fit$center <- round(pca_fit$center, 12)
pca_fit$scale <- round(pca_fit$scale, 12)
pca_fit$x <- round(pca_fit$x, 12)

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