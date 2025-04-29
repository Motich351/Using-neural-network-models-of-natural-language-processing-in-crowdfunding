data = readRDS('kick_full_2806.rds')

data <- readRDS("kick_prepared.rds")
write.csv(data, "kick_prepared.csv", row.names = FALSE)

data_prepared <- readRDS("kick_model_Data.rds")


print(head(data))

# Подготовка целевой переменной (бинарная классификация)
data$target <- ifelse(data$state == "successful", 1, 0)

# Проверка баланса классов после преобразования
binary_state_counts <- table(data$target)
print("\nРаспределение классов после бинаризации:")
print(binary_state_counts)
print(paste("Процент успешных проектов:", round(mean(data$target) * 100, 2), "%"))

# Базовые признаки для модели (числовые)
numeric_features <- c("goal", "backers_count")

# Проверка на пропущенные значения в основных признаках
print("\nКоличество пропущенных значений в признаках:")
print(colSums(is.na(data[numeric_features])))

# Сохраняем подготовленные данные
saveRDS(data, 'kick_prepared.rds')

# Проверка наличия дубликатов
total_rows <- nrow(data)
unique_rows <- nrow(unique(data))
duplicates <- total_rows - unique_rows
print(paste("Всего строк:", total_rows))
print(paste("Уникальных строк:", unique_rows))
print(paste("Количество дубликатов:", duplicates))

# Если есть дубликаты, удаляем их
if(duplicates > 0) {
  data <- unique(data)
}

# Анализ распределения состояний проектов
state_counts <- table(data$state)
print("\nРаспределение состояний проектов:")
print(state_counts)

# Процентное соотношение
state_percentages <- prop.table(state_counts) * 100
print("\nПроцентное соотношение состояний проектов:")
print(round(state_percentages, 2))

head(data)

colnames(data)

table(data$state)