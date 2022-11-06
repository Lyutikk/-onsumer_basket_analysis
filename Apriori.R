TZ = 'GMT+3'
Sys.getlocale()
setwd()

#    Imports
# ============================================

install.packages("arules")
install.packages("arulesViz")

# Подключение пакетов
library(arules)
library(arulesViz)


#    info
# ============================================

# Считали данные из файла groceries.csv в разреженную матрицу
groceries <- read.transactions("groceries.csv", sep = ",")
# Вывели сводную информацию по транзакциям в матрице
summary(groceries)
# Вывели первые 5 транзакций
inspect(groceries[1:5])
# Вывели информацию о частоте встречаемости первых 3 (по алфавиту)
# продуктов в транзакциях
itemFrequency(groceries[, 1:3])


#    visualization
# ============================================

# Построение частотной диаграммы
# Вывели на график ВСЕ продукты (в алфавитном порядке), у которых  
# поддержка больше 10% = присутствуют минимум в 10% всех транзакций
itemFrequencyPlot(groceries, support = 0.1)
# Вывели на график 20 продуктов, наиболее часто встречающихся в транзакциях
itemFrequencyPlot(groceries, topN = 20)

#    search
# ============================================

# Пример №1. Поиск конкретного товара по критериям.
# Поиск продукта на 25 месте по убыванию частоты встречаемости в транзакциях.
itemFrequencyPlot(groceries, topN = 25) # Построение графика для 25 самых частопокупаемых товаров.
itemFrequency(groceries[, 25]) # Данное решение будет неправильным т.к. товары идут в алфавитном порядке.
# Следовательно - их нужно отсортировать.
sort(itemFrequency(groceries), decreasing = TRUE)[25]

# Построение разреженной матрицы
# Вывели первые 5 транзакций
image(groceries[1:5])
# Вывели 100 случайных транзакций из общего набора
image(sample(groceries, 100))


#    learning random set
# ============================================

# Пример №2. Детальное изучение случайного набора
# Фиксация seed для создаваемой выборки транзакций 
set.seed(10, kind = "Mersenne-Twister", normal.kind = "Inversion")
image(sample(groceries, 100))
image(sample(groceries[ ,122:127], 100))
itemFrequency(groceries[, 122:127])

# Создание модели ассоциативных правил
# Запуск функции с параметрами по умолчанию (Поддержка 0,1 и достоверность 0,8)
apriori(groceries)
# Создаем набор правил для более реальных параметров
# Всего 9835 транзакций за месяц. Сколько из них нас устроит?
# 2 покупки в день = 60 за месяц. 60/9835 - это примерно  0,006.
# Определение уровня достоверности - методом проб и ошибок. 
# (Поддержка 0,006 и достоверность 0,25)
groceryrules <- apriori(groceries, parameter = 
    list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules

# Вывод первых 3 правил
inspect(groceryrules[1:3])
# Вывод 5 лучших правил в порядке убывания параметра "лифт"
inspect(sort(groceryrules, by = "lift")[1:5])

# Виджет с таблицей всех правил
rules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
rules                
redundant <- is.redundant(rules)
rules <- rules[!redundant]
table <- inspectDT(rules)
htmlwidgets::saveWidget(table, "arules.html", selfcontained = FALSE)
browseURL("arules.html")

# Создание подмножеств для конкретных товаров
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)


#    search for rules with specific criteria
# ============================================

# Пример №3. Поиск правил с конкретными критериями.
#
# поддержка - 0.006
# достоверность - 0.299
# лифт - 2.850
find_grocery <- function(rules, supp, conf, lft) 
  {
    s <- subset(
      rules,
      subset = (lift > (lft - 0.001) & lift < (lft + 0.001) &
                  support > (supp - 0.001) & support < (supp + 0.001) &
                  confidence > (conf - 0.001) & confidence < (conf + 0.001)))
    inspect(s)
  }
find_grocery(groceryrules, supp = 0.006, conf = 0.299, lft = 2.850)

# Сохранение набора правил в файл формата CSV
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# Сохранение набора правил в датафрейм
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)