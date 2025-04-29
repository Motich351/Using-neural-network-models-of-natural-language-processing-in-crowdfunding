# Установка необходимых пакетов для анализа данных и построения моделей

# Установка зеркала CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Список необходимых пакетов
packages <- c(
  "caret",       # Для моделирования и предобработки
  "glmnet",      # Для логистической регрессии с регуляризацией
  "randomForest", # Для моделей случайного леса
  "doParallel",  # Для параллельных вычислений
  "recipes",     # Для предобработки данных
  "dplyr",       # Для манипуляций с данными
  "tidyr",       # Для работы с данными в "аккуратном" формате
  "ggplot2"      # Для визуализации
)

# Проверка и установка недостающих пакетов
missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  cat("Устанавливаем недостающие пакеты:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages, dependencies=TRUE)
  cat("Установка завершена.\n")
} else {
  cat("Все необходимые пакеты уже установлены.\n")
}

# Загрузка всех пакетов
for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    cat("Ошибка загрузки пакета:", pkg, "\n")
  } else {
    cat("Пакет", pkg, "успешно загружен\n")
  }
}

cat("Подготовка завершена. Теперь вы можете запустить скрипт train_model.R\n") 