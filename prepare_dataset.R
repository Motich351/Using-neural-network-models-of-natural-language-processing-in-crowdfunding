# Загрузка исходных данных
data <- readRDS('kick_full_2806.rds')

# Отобранные поля для модели
selected_fields <- c(
  # Текстовые поля
  "story", "risks", "name", "blurb",
  # Числовые поля
  "goal", 
  # Категориальные поля
  "country", "currency", "category",
  # Временные метки
  "deadline", "created_at", "launched_at", 
  # Бинарные/статусные поля
  "staff_pick", "spotlight",
  # Дополнительные поля
  "static_usd_rate",
  # Целевая переменная
  "state"
)

# Создание нового датафрейма с отобранными полями
model_data <- data[, selected_fields]

# Feature Engineering

# 1. Преобразование временных меток из unix timestamp в POSIXct
model_data$deadline_date <- as.POSIXct(model_data$deadline, origin="1970-01-01")
model_data$created_date <- as.POSIXct(model_data$created_at, origin="1970-01-01")
model_data$launched_date <- as.POSIXct(model_data$launched_at, origin="1970-01-01")

# 2. Расчет временных характеристик
model_data$campaign_duration_days <- as.numeric(difftime(model_data$deadline_date, 
                                               model_data$launched_date, 
                                               units="days"))
model_data$preparation_time_days <- as.numeric(difftime(model_data$launched_date, 
                                             model_data$created_date, 
                                             units="days"))

# 3. Преобразование целевой суммы в USD
model_data$goal_usd <- model_data$goal * model_data$static_usd_rate

# 4. Извлечение простых текстовых характеристик
model_data$story_length <- nchar(as.character(model_data$story))
model_data$blurb_length <- nchar(as.character(model_data$blurb))
model_data$name_length <- nchar(as.character(model_data$name))
model_data$risks_length <- nchar(as.character(model_data$risks))

# 5. Бинаризация целевой переменной
model_data$target <- ifelse(model_data$state == "successful", 1, 0)

# 6. Удаление избыточных полей после создания производных признаков
model_data$deadline <- NULL
model_data$created_at <- NULL
model_data$launched_at <- NULL
model_data$static_usd_rate <- NULL  # Уже использовали для расчета goal_usd

# Проверка структуры полученного датафрейма
str(model_data)

# Проверка баланса классов
table(model_data$state)
table(model_data$target)

# Сохранение подготовленного датафрейма
saveRDS(model_data, 'kick_model_data.rds')
write.csv(model_data, 'kick_model_data.csv', row.names = FALSE)

# Вывод размера датафрейма
print(paste("Исходное количество строк:", nrow(data)))
print(paste("Количество строк в подготовленном датафрейме:", nrow(model_data)))
print(paste("Количество полей в подготовленном датафрейме:", ncol(model_data))) 