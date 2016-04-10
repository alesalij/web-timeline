# Упражнение 1 -----------------------------------------------------------------

# Вариант #12 (http://www.rambler.ru) ===========================================

# Загрузка пакетов
install.packages('RCurl')
install.packages('XML')
library('RCurl')
library('XML') 

# Устанавливаем временные границы поиска
time.limits <- seq(as.numeric(2015), as.numeric(2040))
# Инициализируем переменные
years <- vector(mode = "numeric", length = 0)         # годы
headers <- vector(mode = "character", length = 0)     # заголовки
sources <- vector(mode = "character", length = 0)     # источники на странице
URLs <- vector(mode = "character", length = 0)        # ссылки на источник
# Устанавливаем текст запроса
request <- array(c("Вымирание животных ", "Вымирание* животных ", "Вымирание животных* ","Вымирание зверей* ","Вымирание* зверей"), dim =c(5))
# Обрабатываем каждый запрос в цикле
for(i in seq(1,length(request))) {
  # URL поисковой системы rambler.ru c параметрами поискового запроса
  request.URL = "http://nova.rambler.ru/search?&query="
  # Преобразуем текст запроса в соответствии с синтаксисом GET запросов URL
  request[i] <- paste(request[i], collapse ='+')
  # Добавляем запрос к строке URL
  request.URL <- paste(c(request.URL, request), collapse = '')
  # Цикл по годам
  for(j in time.limits) {
    # Загружаем текст html-страницы
    html <- getURL(paste(c(request.URL, as.character(j)), collapse ='+'))
    # Разбираем как html
    doc <- htmlTreeParse(html, useInternalNodes = T)
    # Корневой элемент
    rootNode <- xmlRoot(doc)
    # Выбираем все заголовки результатов запроса
    headers <- c(headers, xpathSApply(rootNode,'//a[@class="b-serp-item__link"]',xmlValue))
    # Создаём столбец лет
    years <- c(years, rep(j, 10))
    # Получаем полные ссылки на источники
    URLs <- c(URLs, xpathSApply(rootNode, '//a[@class="b-serp-item__link"]',xmlGetAttr, 'href'))
    # Получаем указанные на странице источники
    sources <- c(sources, xpathSApply(rootNode, '//span[@class="b-serp-item__info"]/span', xmlValue))
  }
}

# Заносим данные во фрейм
data <- data.frame(Year = years, Header = headers, Source = sources, URL = URLs)
# Записываем фрейм в файл Timeline.csv
write.csv(data, file = './Timeline.csv', append = T, row.names = F)

