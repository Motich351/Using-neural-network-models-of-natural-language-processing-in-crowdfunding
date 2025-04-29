# Загрузка необходимых библиотек
library(caret)
library(glmnet)
library(randomForest)
library(doParallel)
library(recipes)
library(dplyr)
library(tidyr)
library(ggplot2)

# Установка параллельной обработки для ускорения
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Загрузка подготовленных данных
model_data <- readRDS('kick_model_data.rds')

# Преобразование целевой переменной в фактор
model_data$target <- factor(model_data$target, levels = c(0, 1), labels = c("failed", "successful"))

# Проверка на наличие пропущенных значений в данных
na_count <- colSums(is.na(model_data))
print("Количество пропущенных значений в каждой колонке:")
print(na_count[na_count > 0])

# Выбор признаков для модели (без текстовых полей, которые требуют NLP обработки)
# Для начала построим модель на числовых и категориальных признаках
features <- c(
  "goal", "country", "currency", "category", "staff_pick", "spotlight",
  "campaign_duration_days", "preparation_time_days", "goal_usd",
  "story_length", "blurb_length", "name_length", "risks_length"
)

# Целевая переменная
target <- "target"

# Создание набора данных для моделирования
modeling_data <- model_data[, c(features, target)]

# Заполнение пропущенных значений в категориальных переменных
modeling_data$category[is.na(modeling_data$category)] <- "unknown"
modeling_data$country[is.na(modeling_data$country)] <- "unknown"
modeling_data$currency[is.na(modeling_data$currency)] <- "unknown"

# Заполнение пропущенных значений в числовых переменных
for(col in c("risks_length", "story_length", "blurb_length", "name_length")) {
  if(any(is.na(modeling_data[[col]]))) {
    modeling_data[[col]][is.na(modeling_data[[col]])] <- median(modeling_data[[col]], na.rm = TRUE)
  }
}

# Проверка, что все NA заполнены
na_count_after <- colSums(is.na(modeling_data))
print("Количество пропущенных значений после заполнения:")
print(na_count_after[na_count_after > 0])

# 1. Разделение данных на обучающую, валидационную и тестовую выборки
set.seed(42) # для воспроизводимости результатов

# Индексы для обучающей выборки (70%)
train_index <- createDataPartition(modeling_data[[target]], p = 0.7, list = FALSE)
train_data <- modeling_data[train_index, ]

# Оставшиеся данные разделяем на валидационную (15%) и тестовую (15%) выборки
remaining_data <- modeling_data[-train_index, ]
valid_index <- createDataPartition(remaining_data[[target]], p = 0.5, list = FALSE)
valid_data <- remaining_data[valid_index, ]
test_data <- remaining_data[-valid_index, ]

# Вывод размеров выборок
print(paste("Размер обучающей выборки:", nrow(train_data)))
print(paste("Размер валидационной выборки:", nrow(valid_data)))
print(paste("Размер тестовой выборки:", nrow(test_data)))

# 2. Предобработка признаков

# Определение числовых и категориальных признаков
numeric_features <- c("goal", "campaign_duration_days", "preparation_time_days", 
                     "goal_usd", "story_length", "blurb_length", "name_length", "risks_length")
categorical_features <- c("country", "currency", "category")
binary_features <- c("staff_pick", "spotlight")

# Создание рецепта предобработки данных
preprocess_recipe <- recipe(as.formula(paste(target, "~ .")), data = train_data) %>%
  # Преобразование категориальных признаков в факторы
  step_string2factor(all_of(categorical_features)) %>%
  # One-hot encoding для категориальных признаков
  step_dummy(all_of(categorical_features), one_hot = TRUE) %>%
  # Масштабирование числовых признаков
  step_normalize(all_of(numeric_features))

# Применение рецепта предобработки к данным
preprocess_obj <- prep(preprocess_recipe, training = train_data)
train_processed <- bake(preprocess_obj, new_data = train_data)
valid_processed <- bake(preprocess_obj, new_data = valid_data)
test_processed <- bake(preprocess_obj, new_data = test_data)

# Проверка на пропущенные значения после предобработки
na_count_processed <- colSums(is.na(train_processed))
if(sum(na_count_processed) > 0) {
  print("ВНИМАНИЕ: В предобработанных данных все еще есть пропущенные значения:")
  print(na_count_processed[na_count_processed > 0])
} else {
  print("Предобработка успешна: пропущенных значений нет")
}

# 3. Обучение модели

# 3.1 Логистическая регрессия (базовая модель)
logistic_model <- train(
  x = select(train_processed, -all_of(target)),
  y = train_processed[[target]],
  method = "glmnet",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE
  ),
  metric = "ROC",
  tuneLength = 10
)

# Оценка модели на валидационной выборке
valid_pred <- predict(logistic_model, newdata = select(valid_processed, -all_of(target)))
valid_pred_prob <- predict(logistic_model, newdata = select(valid_processed, -all_of(target)), type = "prob")
valid_actual <- valid_processed[[target]]

# Расчет метрик качества
valid_confusion <- confusionMatrix(
  valid_pred,
  valid_actual
)

# Вывод результатов
print("Результаты логистической регрессии на валидационной выборке:")
print(valid_confusion)

# 3.2 Random Forest (более сложная модель)
rf_model <- train(
  x = select(train_processed, -all_of(target)),
  y = train_processed[[target]],
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE
  ),
  metric = "ROC",
  tuneLength = 5
)

# Оценка модели на валидационной выборке
rf_valid_pred <- predict(rf_model, newdata = select(valid_processed, -all_of(target)))
rf_valid_pred_prob <- predict(rf_model, newdata = select(valid_processed, -all_of(target)), type = "prob")

# Расчет метрик качества
rf_valid_confusion <- confusionMatrix(
  rf_valid_pred,
  valid_actual
)

# Вывод результатов
print("Результаты Random Forest на валидационной выборке:")
print(rf_valid_confusion)

# 4. Оценка важности признаков
if (inherits(rf_model$finalModel, "randomForest")) {
  var_importance <- varImp(rf_model)
  print("Важность признаков:")
  print(var_importance)
  
  # Сохранение графика важности признаков
  png("feature_importance.png", width = 800, height = 600)
  plot(var_importance, top = 20)
  dev.off()
}

# 5. Финальная оценка на тестовой выборке
# Выбираем лучшую модель на основе результатов валидации
if (rf_valid_confusion$overall["Accuracy"] > valid_confusion$overall["Accuracy"]) {
  best_model <- rf_model
  print("Лучшая модель: Random Forest")
} else {
  best_model <- logistic_model
  print("Лучшая модель: Логистическая регрессия")
}

# Оценка на тестовой выборке
test_pred <- predict(best_model, newdata = select(test_processed, -all_of(target)))
test_confusion <- confusionMatrix(
  test_pred,
  test_processed[[target]]
)

# Вывод финальных результатов
print("Результаты лучшей модели на тестовой выборке:")
print(test_confusion)

# 6. Сохранение модели и предобработки
saveRDS(best_model, "kickstarter_best_model.rds")
saveRDS(preprocess_obj, "kickstarter_preprocess.rds")

# Завершение параллельной обработки
stopCluster(cl)

# Подсказка для использования модели на новых данных
cat("
# Для использования модели на новых данных:
# 1. Загрузить модель и препроцессор
model <- readRDS('kickstarter_best_model.rds')
preprocess <- readRDS('kickstarter_preprocess.rds')

# 2. Подготовить данные
new_data <- ... # Ваши новые данные
new_data_processed <- bake(preprocess, new_data = new_data)

# 3. Сделать предсказание
predictions <- predict(model, newdata = new_data_processed)
") 