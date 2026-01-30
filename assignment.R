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

# Выведем результат для проверки
print(top_speakers)

# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |>  # фильтруем только топ-спикеров
  unnest_tokens(word, text) |>         # токенизируем реплики
  filter(!str_detect(word, "\\d")) |>  # удаляем токены, содержащие цифры
  select(speaker, word)                # оставляем только нужные столбцы

# Посмотрим на результат
head(friends_tokens)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |> 
  # Группируем по персонажу
  group_by(speaker) |> 
  # Для каждого персонажа считаем общее количество токенов
  mutate(total = n()) |> 
  # Считаем частотность слова внутри группы персонажа
  add_count(word, name = "n") |> 
  # Оставляем уникальные записи
  distinct(speaker, word, total, n) |> 
  # Считаем относительную частотность
  mutate(tf = n / total) |> 
  # Сортируем по убыванию частотности внутри группы
  arrange(speaker, -n) |> 
  # Отбираем 500 самых частотных слов для каждого персонажа
  slice_head(n = 500) |> 
  ungroup() 

head(friends_tf)

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  select(speaker, word, tf) |>                
  pivot_wider(                                
    names_from = word,                        
    values_from = tf,                         
    values_fill = 0                           
  ) |> 
  column_to_rownames(var = "speaker") |>      
  as.data.frame() 

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()

# ваш код здесь
set.seed(123)
km.out <- kmeans(
  x = scale(friends_tf_wide),  # масштабируем данные (центрирование и нормирование)
  centers = 3,                 # количество кластеров k = 3
  nstart = 20                  # количество случайных начальных наборов
)

km.out$cluster


# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, scale. = TRUE, center = TRUE)


# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q
summary(pca_fit)


q <- fviz_pca_biplot(pca_fit,
                     geom = c("text"),           # отображаем имена персонажей как текст
                     select.var = list(cos2 = 20),  # 20 наиболее значимых переменных по косинусу
                     habillage = as.factor(km.out$cluster),  # цветом закодируйте кластер
                     col.var = "steelblue",      # цвет для переменных (слов)
                     alpha.var = 0.3,            # прозрачность для переменных
                     repel = TRUE,               # избегаем наложения текста
                     ggtheme = theme_minimal()) +
  theme(legend.position = "none")                # скрываем легенду

# Выведем график
print(q)