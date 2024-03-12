<h2 align = center>REAL ESTATE AGENT APP</h2>
<p align = center><a href="https://git.io/typing-svg"><img src="https://readme-typing-svg.demolab.com?font=Pixelify+Sans&pause=1000&random=false&width=600&lines=%D0%A1%D0%B5%D1%80%D0%B2%D0%B8%D1%81+%D0%B4%D0%BB%D1%8F+%D0%BF%D1%80%D0%B5%D0%B4%D1%81%D0%BA%D0%B0%D0%B7%D0%B0%D0%BD%D0%B8%D1%8F+%D1%86%D0%B5%D0%BD%D1%8B+%D0%BF%D1%80%D0%BE%D0%B4%D0%B0%D0%B6%D0%B8+%D0%BA%D0%B2%D0%B0%D1%80%D1%82%D0%B8%D1%80%D1%8B" alt="Typing SVG" /></a></p>
<div align="justify">Личный риэлтор – сервис, формирующий цену, по которой пользователь может выгодно продать свою квартиру, не прибегая к услугам агента по продаже жилья. На вход принимает характеристики квартиры, а выдает предсказанную стоимость и карту с квартирами, имеющими похожие параметры. Таким образом, пользователь экономит на оценщике.
  
В разработке продукта принимали участие: <a href = "https://github.com/jinAntonik"> Антон Алексеев</a>, Валерия Винниченко</a>,Дмитрий Жучков, <a href = "https://github.com/veronikavinnichenko">Вероника Винниченко</a>.</div>

### Ссылка на сервис: 
https://data8group.shinyapps.io/best_app/


### 🔧 Технологии

* ![R](https://img.shields.io/badge/R-276DC3?style=plastic&logo=r&logoColor=white)
* ![Python](https://img.shields.io/badge/Python%20-%20white?style=plastic&logo=python&logoColor=yellow&color=%234682B4)
* ![Shiny](https://img.shields.io/badge/Shiny-8A2BE2)


### 📂 Структура проекта

```sh
└── data_parser.ipynb
├── data
   └── data_final.xlsx
   ├── partions of data
      ├── part1
      ├── part2
      ├── part3
      ├── part4
      └── part5
├── model
      └── models.R
└── app
      ├── server.R
      └── ui.R
```
|    |   Feature         | Description |
|----|-------------------|---------------------------------------------------------------|
| 📄 | data_parser    | Файл для парсинга данных с сайта cian.ru
| 📔 | data  |Директория, содержащая как исходные (partitions of data), так и предобработанные данные (data_final) с харакетристками квартир  |
| 📔 | model  | Директория, содержащая модель машинного обучения|
| 📔 | app |  Директория, содержащая файлы с кодом интервеса и серверной части приложения |


### 💽 Входные данные для работы сервиса

* Количество комнат
* Общая площадь
* Жилая площадь
* Площадь кухни
* Тип ремонта квартиры
* Наличие балкона
* Вид санузла (совмещенный/раздельный)
* Количество санузлов
* Этаж
* Год постройки дома
* Тип дома
* Наличие лифта
* Район расположения
* Ближайшая станция метро
* Расстояние до метро пешком (мин)


### Выходные данные:

* 💵 Предсказанный интервал цены
* 🗺️ Карта с рекомендацией квартир со схожими храктеристиками

### ⚙️ Машинное обучение

<ol>
  <li>Применение обучения с “учителем” для обучения таких алгоритмов ML, как линейная регрессия, RIDGE-регрессия, LASSO-регрессия, случайный лес</li>
  <li>Оформление: ансамбль</li>
  <li>Структура ансамбля: итоговое предсказание является суммой взвешенных предсказания отдельно работающих моделей</li>
  <li>Входные данные для обучения моделей: square, room, kitchen_square, living_square, year, toilet_quantity, type_toilet, if_lift, if_balcony</li>
</ol>

### 🔝 Рекомендательная система
<ol>
  <li>Тип: content-based</li>
  <li>Вход: 15 характерисик квартир</li>
  <li>Вывод: 5 наиболее похожих квартир</li>
  <li>Формат вывода: карта г. Санкт-Петербурга с похожими квартирами и их характеристикмми</li>
</ol>

```r
output$mymap = renderLeaflet({
    vector1 = c(1, 0)
    vector2 = c("есть", "нет")
    
    recommend = recommend()
    M = leaflet(recommend) %>% setView(lng = 30.3141, lat = 59.9386, zoom = 10)
    M %>% addTiles() %>%
      addMarkers(~dolgota, ~shirota, popup = paste("<br>Адрес:", recommend$addr,
                                                   "<br>Район:", recommend$district,
                                                   "<br>Цена:", paste0(formatC(as.numeric(recommend$price), format="f", digits=0, big.mark="'"), " ₽"),
                                                   "<br>Кол-во комнат:", recommend$rooms,
                                                   "<br>Площадь (кв.м):", recommend$square,
                                                   "<br>Жилая площадь (кв.м):", recommend$living_square,
                                                   "<br>Площадь кухни (кв.м):", recommend$kitchen_square,
                                                   "<br>Этаж:", recommend$floor,
                                                   "<br>Год постройки:", recommend$year,
                                                   "<br>Тип дома:", recommend$housetype,
                                                   "<br>Балкон:", recommend$if_balcony,
                                                   "<br>Наличие лифта:", recommend$if_lift,
                                                   "<br>Кол-во санузлов:", recommend$toilet_quantity,
                                                   "<br>Тип туалета:", recommend$type_toilet,
                                                   "<br>Ремонт:", recommend$remont,
                                                   "<br>Станция метро:", recommend$subway_TRUE,
                                                   "<br>Пешком до метро (мин):", recommend$subway_dist_peshkom_TRUE),
                 label = paste0(formatC(as.numeric(recommend$price), format="f", digits=0, big.mark="'"), " ₽"))
  })
```

## 👩‍💻 Пример использования интерфеса

### Карта с похожими квартирами
![IMG_8667](https://github.com/veronikavinnichenko/bum-s-bucket/assets/127980580/e2d36a00-ffa1-42eb-bea3-5d4d7d81f4fc)

### Предсказанная цена
![IMG_8668](https://github.com/veronikavinnichenko/bum-s-bucket/assets/127980580/62a72b8a-d0dd-44d6-8a2c-2bb354c5191b)

