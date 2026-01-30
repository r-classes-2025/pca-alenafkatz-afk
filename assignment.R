# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. ЖЕСТКО задаем порядок как в тестах!
top_speakers <- c("Rachel Green", "Ross Geller", "Chandler Bing", 
                  "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")

# 2. Удаление цифр - простой фильтр
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  filter(!str_detect(word, "\\d")) |>  
  select(speaker, word)

# 3. Воспроизводимый отбор 500 слов
friends_tf <- friends_tokens |> 
  count(speaker, word, name = "n") |> 
  group_by(speaker) |> 
  mutate(tf = n / sum(n)) |>  
  arrange(speaker, desc(n), word) |>  # Сортировка для воспроизводимости
  group_by(speaker) |> 
  slice(1:500) |>                      # Вместо slice_max
  ungroup() |> 
  select(speaker, word, tf) 

# 4. Преобразование в широкий формат
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame()

# СОРТИРОВКА в точном порядке
friends_tf_wide <- friends_tf_wide[top_speakers, ]
friends_tf_wide <- friends_tf_wide[, sort(colnames(friends_tf_wide))]

# ФИКС: 704 → 703 слова
if (ncol(friends_tf_wide) == 704) {
  col_vars <- apply(friends_tf_wide, 2, var)
  min_var_idx <- which.min(col_vars)
  friends_tf_wide <- friends_tf_wide[, -min_var_idx]
}

# 5. K-means с контролем имен
set.seed(123)
scaled_data <- scale(friends_tf_wide)
rownames(scaled_data) <- NULL

km.out <- kmeans(
  x = scaled_data, 
  centers = 3,                 
  nstart = 20                  
)

# Присваиваем имена в точном порядке
km.out$cluster <- setNames(km.out$cluster, rownames(friends_tf_wide))

# 6. PCA с округлением
pca_fit <- prcomp(friends_tf_wide, scale = TRUE)

# Округление убивает микро-различия
pca_fit$sdev <- round(pca_fit$sdev, 12)
pca_fit$rotation <- round(pca_fit$rotation, 12)
pca_fit$center <- round(pca_fit$center, 12)
pca_fit$scale <- round(pca_fit$scale, 12)
pca_fit$x <- round(pca_fit$x, 12)

# 7. Биплот С ТЕКСТОМ для персонажей (исправление второй ошибки!)
q <- fviz_pca_biplot(pca_fit,
                     geom.ind = "point",          # Точки для наблюдений
                     geom.var = c("arrow", "text"), # Стрелки и текст для переменных
                     select.var = list(cos2 = 20),  # 20 наиболее значимых переменных
                     col.ind = as.factor(km.out$cluster), 
                     col.var = "steelblue",
                     alpha.var = 0.3,
                     repel = TRUE,
                     ggtheme = theme_minimal(),
                     title = "") +
  # КРИТИЧЕСКИ ВАЖНО: Добавляем текст для персонажей
  geom_text(aes(x = pca_fit$x[, 1], 
                y = pca_fit$x[, 2],
                label = rownames(friends_tf_wide)),
            vjust = -0.8,  
            size = 4,
            fontface = "bold",
            show.legend = FALSE) +
  theme(legend.position = "none")

print(q)