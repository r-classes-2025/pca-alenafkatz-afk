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

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |> 
  group_by(speaker) |> 
  mutate(total = n()) |> 
  add_count(word, name = "n") |> 
  distinct(speaker, word, total, n) |> 
  mutate(tf = n / total) |> 
  arrange(speaker, word) |> 
  slice_head(n = 500) |> 
  ungroup() |> 
  select(speaker, word, tf)

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  arrange(speaker, word) |> 
  select(speaker, word, tf) |>                
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame()

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()

# ваш код здесь
set.seed(123)
km.out <- kmeans(
  x = scale(friends_tf_wide), 
  centers = 3,                 
  nstart = 20                  
)


# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, 
                  scale. = TRUE, 
                  center = TRUE,
                  tol = 0,
                  rank. = min(dim(friends_tf_wide)))  

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q

q <- fviz_pca_biplot(pca_fit,
                     geom.ind = "point",         
                     geom.var = c("arrow", "text"), 
                     select.var = list(cos2 = 20),
                     col.ind = as.factor(km.out$cluster), 
                     col.var = "steelblue",
                     alpha.var = 0.3,
                     repel = TRUE,
                     ggtheme = theme_minimal(),
                     title = "",
                     legend.title = "Кластер") +
  theme(legend.position = "none") +
  geom_text(aes(x = pca_fit$x[, 1], 
                y = pca_fit$x[, 2],
                label = rownames(friends_tf_wide),
                color = as.factor(km.out$cluster)),
            vjust = -0.8,  
            size = 4,
            fontface = "bold",
            show.legend = FALSE)  

print(q)


friends_tf |> count(speaker) |> print()

friends_tf |> 
  group_by(speaker) |> 
  slice_head(n = 3) |> 
  print()

# Создайте тестовый PCA с "идеальными" параметрами
pca_test <- prcomp(friends_tf_wide, 
                   scale. = TRUE, 
                   center = TRUE,
                   tol = NULL,
                   rank. = NULL)

# Проверьте разницу с вашим pca_fit
cat("Разница в стандартных отклонениях (sdev):\n")
diff_sdev <- abs(pca_fit$sdev - pca_test$sdev) / pca_fit$sdev * 100
print(round(diff_sdev[1:5], 6))



