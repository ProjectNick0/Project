####### read in data #####
data2 <- read.csv("tabl_it.csv", sep=";", header=TRUE, stringsAsFactors=TRUE)
head(data2)


library(lmerTest)
library(lmerTest)
library(lme4)
library(ggplot2)

data2$yearCit<-as.numeric(data2$yearCit)
data2$year<-as.numeric(data2$year)
data2$link<-as.factor(data2$link)
data2$author<-as.factor(data2$author)
summary(data2$author)
summary(data2$yearCit )
data2$citations<-as.numeric(data2$citations)


fm1 <- lmer(citations ~ yearCit*author+
            (1|link), data = data2)
summary(fm1 )

options(max.print = 10000)
sink("output.txt")
summary(fm1)
sink()
cat(readLines("output.txt"))
###################### Cредние значения цитирований по keyword:
mean_citations <- aggregate(citations ~ keyword, data = data2, FUN = mean)

# Результаты:
print(mean_citations)
##############Модель для определения тенденций цитирования ключевых слов
new_model2 <- lmer(citations ~ yearCit * factor(keyword) + (1|link),
                  subset = keyword %in% c("variable temperature", "parallel restoration", 
                                          "online parameter identification","neural network",
                                          "lithium-ion battery", "energy transactions",
                                          "coordinated control",
                                          "adaptive cubature Kalman filter",
                                          "black-start"),
                  data = data2)
summary(new_model2)


summary(new_model2)
#нарисовать график с прогнозируемыми трендами
# Создать диаграмму рассеивания для прогнозируемых средних значений цитирований и года публикации по каждому ключевому слову:
keywords <- c("variable temperature", "parallel restoration", 
              "online parameter identification","neural network",
              "lithium-ion battery", "energy transactions",
              "coordinated control",
              "adaptive cubature Kalman filter",
              "black-start")
years <- seq(min(2017), max(2024))

new_data <- expand.grid(keyword = keywords, yearCit = years)

#  Вставка столбца 'keyword'  к новому данным-фрейму
new_data$keyword <- factor(new_data$keyword)

# Удаление из модели случайного фактор 'link'
model_formula_without_link<-as.formula(paste(deparse(substitute(citations ~ yearCit * factor(keyword) +
                                                                  (1| keyword))),sep=''))
#  Используование функцию predict() с дополнительным параметром re.form = ~ -1
preds <- predict(lmer(model_formula_without_link , data = data2), 
                 new_data, re.form = NA)

# Объединение данных new_data с предсказанными значениями
result <- cbind(new_data, preds)

############9 Графиков
ggplot(result %>% filter(!is.na(preds)), aes(x = yearCit)) + 
  geom_line(aes(y = preds)) +
  labs(title = "Прогнозируемые средние цитаты по году публикации",
       x = "Годы цитирования",
       y = "Среднее количество цитат") + 
  facet_wrap(~ keyword) + 
  theme(legend.position="bottom")

###############
ggplot(result %>% filter(!is.na(preds)), aes(x = yearCit)) + 
  geom_line(aes(y = preds, color = keyword)) +
  labs(title = "Прогнозируемое среднее число цитат по ключевым словам во времени",
       x = "Годы цитирования",
       y = "Среднее количество цитат") + 
  theme(legend.position="bottom")


#  Диаграмма рассеивания для цитат и года публикации по каждому ключевому слову:
install.packages("ggplot2")
library(ggplot2)

plot(data2 %>% 
       filter(keyword %in% c("variable temperature", "parallel restoration", 
                             "online parameter identification","neural network",
                             "lithium-ion battery", "energy transactions",
                             "coordinated control")) %>%
       ggplot(aes(x = yearCit, y = citations, color = keyword)) +
       geom_line() + 
       labs(title = "Цитаты по году публикации по ключевым словам",
            x = "Годы цитирования",
            y = "Количество цитат"))
     

#################################
# Выберем только определенных авторов, с ниабольшим числом цитат:
author_list <- c("Huawen Wang", "Xin Zhang", "Yong Tian")
new_model <- glm(citations ~ yearCit + factor(author)[author_list] , data = data2)
summary(new_model)
#########
# Создадим новый фактор для только тех авторов, которых мы хотим включить:
new_author <- factor(ifelse(data2$author %in% c("Huawen Wang", "Xin Zhang", "Yong Tian"),
                            data2$author,
                            NA))
new_model <- glm(citations ~ yearCit + new_author , data = data2)
summary(new_model)
summary(data2$new_author)
###########
# Выберем коэффициенты для определенных авторов:
coef(fmg0)[grep("Huawen Wang", colnames(coef(fmg0)))]
coef(fmg0)[grep("Xin Zhang", colnames(coef(fmg0)))]
coef(fmg0)[grep("Yong Tian", colnames(coef(fmg0)))]

# Создадим новую модель с только теми коэффициентами:
new_model <-  glm(citations ~ yearCit * factor(author) ,
                  subset = author %in% c("Huawen Wang", "Xin Zhang", "Yong Tian") ,
                  data = data2)
summary(new_model)
##############
new_model <- lmer(citations ~ yearCit * factor(author) + (1|link),
                  subset = author %in% c("Huawen Wang", "Xin Zhang", "Yong Tian"),
                  data = data2)
summary(new_model)
################################
# Создаем диаграмму рассеивания для цитат и года публикации:
install.packages("dplyr")
library(dplyr)
plot(citations ~ yearCit, data = data2 %>% filter(yearCit >= 2015 & yearCit <= 2025),
     main = "Цитирования авторов по годам",
     xlab = "Годы цитирования",
     ylab = "Количество цитат")



# Добавляем точки для каждого автора:
points(data2$yearCit[data2$author == "Huawen Wang"],
       data2$citations[data2$author == "Huawen Wang"],
       col = "red", pch = 19)
points(data2$yearCit[data2$author == "Xin Zhang"],
       data2$citations[data2$author == "Xin Zhang"],
       col = "green", pch = 19)
points(data2$yearCit[data2$author == "Yong Tian"],
       data2$citations[data2$author == "Yong Tian"],
       col = "blue", pch = 19)

# Добавьляем линию регрессии для каждой группы авторов:
abline(lm(citations ~ yearCit,
          subset = author %in% c("Huawen Wang"),
          data = data2), col="red")
abline(lm(citations ~ yearCit,
          subset = author %in% c("Xin Zhang"),
          data=data2),col="green")
abline(lm(citations ~ yearCit,
          subset= author %in% c("Yong Tian"),data=data2),col="blue")

# Легенды к диаграмме:
legend("topright",
       legend=c("Хуавэнь Ван","Синь Чжан","Ионг Тянь"),
       lty=1,col=c("red","green","blue"))
# 
legend("topright", bty = "n",
       legend=c("Хуавэнь Ван","Синь Чжан","Ионг Тянь"),
       lty=1,col=c("red","green","blue"))
# 
legend(0.1, 0.8, bty = "n",
       legend=c("Хуавэнь Ван","Синь Чжан","Ионг Тянь"),
       lty=1,col=c("red","green","blue"))


plot(citations ~ yearCit, data=data2 ,
     main=paste ("Цитаты по году публикации: ", min_year,"-",max_year),
     xlab=paste ("Год цитирования: ", yearCit,"-",yearCit),
     ylab="Количество цитат")
###############добавить визуализацию случайных эффектов в модели LMM
library(ggplot)
ggplot(new_model$data.frame, aes(x=factor(author))) + 
  geom_bar(aes(y=mean(new_model$fitted.values)), stat='identity') + 
  theme_classic() +
  labs(title="Случайные эффекты в модели LMM") +
  theme(plot.title.position='frame')

###################Исключить влияние авторского вклада
##############Исключить влияние авторского вклада
new_model3<- lmer(citations ~ yearCit * factor(keyword) + (1|author),
                  subset = keyword %in% c("variable temperature", "parallel restoration", 
                                          "online parameter identification","neural network",
                                          "lithium-ion battery", "energy transactions",
                                          "coordinated control",
                                          "adaptive cubature Kalman filter",
                                          "black-start"),
                  data = data2)


l