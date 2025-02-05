# introducción al análisis de datos con R para principiantes
# nivel 2: Manipulación de bases de datos en R
# clase 1

# cargar datos ----

# instalar paquetes
install.packages("readxl")

# cargar un paquete
library("readxl")

# cargar un archivo excel
datos <- read_excel("datos/campamentos_chile_2024.xlsx")
# recordemos que para que la carga sea exitosa, debemos estar trabajando en el proyecto de R apropiado

# ver los datos
datos

# cargar un archivo csv: opción 1
read.csv2("datos/presidenciales_2021_comuna.csv")

# install.packages("readr") # instalar
library("readr")

# cargar un archivo csv: opción 2 (preferible)
eleccion <- read_csv2("datos/presidenciales_2021_comuna.csv")

eleccion

# install.packages("haven") # instalar
library("haven")

# cargar archivos spss
haven::read_sav("datos/cep_base_92_20112024.sav")

# cargar archivos desde stata
# haven::read_dta("datos/cep_base_92_20112024.dta")


# install.packages("dplyr") # instalar
library("dplyr")

# crear una tabla: opción a
tibble(a = 1:10,
       b = 1:10,
       c = 11:20)

# crear una tabla: opción b
tribble(~animal,  ~patas, ~color,
        "perro",   4,     "negro",
        "mapache", 4,     "gris",
        "ganso",   2,     "blanco"
        )


# exploración de datos ----

# ver datos
eleccion

# ver las primeras filas de la tabla
head(eleccion, n = 20)

# ver las últimas filas de la tabla
tail(eleccion, n = 20)

# ver la cantidad que queramos de filas de una tabla
print(eleccion, n = 100)
# print(eleccion, n = Inf) # imprimir todas las filas (puede tomar un tiempo si es una tabla muy grande)

# revisar datos con las columnas hacia abajo
glimpse(eleccion)

# ver datos como planilla en una nueva pestaña
View(eleccion)

# extraer una de las columnas de la tabla como vector
eleccion$votos

# dejar sólo las observaciones únicas (no repetidas) de un vector
unique(eleccion$region)

## seleccionar ----

# seleccionar columnas de una tabla
select(eleccion, candidatura, votos)

# usar pipe o conector para encadenar operaciones
eleccion |> select(candidatura, votos)
# control + shift + M en Windows
# comando + shift + M en Mac

# el conector puede ser %>% o |>

# encadenar dos selecciones usando un conector
eleccion |> select(candidatura, votos, comuna) |> select(-comuna)

# también se puede escribir hacia abajo
eleccion |> 
  select(candidatura, votos, comuna) |> 
  select(-comuna)

# selección negativa (excluir columna)
eleccion |> select(-cut_comuna)

# seleccionar columnas que empiecen con... 
eleccion |> select(starts_with("votos"))

# seleccionar columnas que terminen con...
eleccion |> select(ends_with("comuna"))

# combinar métodos de seleccion 
eleccion |> select(comuna, region, candidatura, 
                   starts_with("votos"))

# seleccionar columnas por su tipo
eleccion |> select(comuna, region, candidatura, 
                   where(is.numeric))

# des-seleccionar columnas por su tipo
eleccion |> select(-where(is.numeric))

eleccion |> select(-where(is.character))

# seleccionar columnas que contengan un nombre en su texto
eleccion |> select(contains("mu"))

# crear un vector de texto y usarlo para seleccionar columnas
variables_elecciones <- c("candidatura", "votos_comuna", "animales")

eleccion |> select(any_of(variables_elecciones))

eleccion |> select(all_of(variables_elecciones))


## seleccionar filas ----

# ver la fila numero 1000 de la tabla
eleccion |> 
  slice(1000)

# ver desde la mil a la mil diez
eleccion |> 
  slice(1000:1010)

# usar un vector para realizar la selección
fila <- 500
eleccion |> 
  slice(fila:(fila-10))

# seleccionar la fila donde se tenga el máximo de una columna
eleccion |> 
  slice_max(votos)

# encadenar dos instrucciones
eleccion |> 
  slice_max(votos) |> 
  select(comuna, candidatura) 

# seleccionar 10 filas con mayor cantidad de una columna
eleccion |> 
  slice_max(votos, n = 10)

# seleccionar mínimos
eleccion |> 
  slice_min(votos, n = 10)

# seleccionar una muestra aleatoria de filas
eleccion |> 
  slice_sample(n = 10)


## filtros ----
# filtrar observaciones donde el valor de una columna supere x
eleccion |> 
  filter(votos > 100000)

eleccion |> 
  filter(votos == 4567)

# filtrar observaciones donde las observaciones equivalgan a un valor
eleccion |> 
  filter(comuna == "La Florida")

eleccion$eleccion |> unique()

#  filtrar observaciones donde las observaciones no coincidan con un valor
eleccion |> 
  filter(eleccion != "Primera vuelta")

# encadenar múltiples filtros consecutivamente
eleccion |> 
  filter(eleccion == "Primera vuelta") |> 
  filter(region == "Valparaíso") |> 
  filter(candidatura == "Gabriel Boric") |> 
  select(comuna, votos, ganador_comuna) |> 
  print(n=Inf)

# extraer comunas con mayor cantidad de votos
comunas_mayor_n <- eleccion |> 
  slice_max(votos, n = 10) |> 
  pull(comuna) # convierte una columna en un vector

comunas_mayor_n

# filtrar datos dejando solo comunas con mayor cantidad de votos
eleccion |> 
  filter(comuna %in% comunas_mayor_n)

# usando filter:

datos |> 
  filter(hogares == 1001)

# filtrar usando un vector
numero <- 1004
datos |> 
  filter(hogares == numero)

# filtrar observaciones que coincidan con el máximo
datos |> 
  filter(hogares == max(hogares))

# filtrar observaciones mayores al promedio
datos |> 
  filter(hogares > mean(hogares))

# filtrar observaciones mayores a un cuantil
datos |> 
  filter(hogares > quantile(hogares, 0.7))

# filtrar observaciones mayores al doble del promedio
datos |> 
  filter(hogares > mean(hogares)*2)

# filtrar observaciones mayores al 40% del máximo
datos |> 
  filter(hogares >= max(hogares)*0.4)



## observaciones distintas ----

# ya vimos que con unique() podemos quedar con las observaciones únicas del vector (eliminar las repetidas)
eleccion$region
unique(eleccion$region)

#  observaciones únicas de una columna (eliminar filas con valores repetidos)
eleccion |> distinct(region)

#  observaciones únicas de una columna, pero dejando el resto de columnas
eleccion |> distinct(region, .keep_all = TRUE)

#  observaciones únicas de la combinación de dos columnas
eleccion |> distinct(region, comuna)

eleccion |> distinct(candidatura)

eleccion |> 
  select(1:5) |> 
  distinct()

eleccion


# cargar otro archivo excel
datos <- read_excel("datos/campamentos_chile_2024.xlsx")


## ordenar observaciones ----

# de menor a mayor
datos |> arrange(hogares)

# de mayor a menor
datos |> arrange(desc(hogares))

# ordenar por dos columnas a la vez (primero ordena por la primera, luego por la segunda dentro del orden de la primera
datos |> 
  arrange(region, desc(hogares)) |> 
  print(n = 200)

## reubicar columnas
# mover una columna a una posición
datos |> 
  relocate(nombre, .after = comuna)

# mover las columnas que empiecen con x al final de la tabla
datos |> 
  relocate(starts_with("cut"), .after = 0)
# se pueden usar las mismas formas de seleccion que se usan en select()

# reordenar con select
datos |> select(nombre, region)
datos |> select(region, nombre)
datos |> 
  select(region, nombre, everything())

## renombrar ----
# cambiar el nombre de una columna
datos |> 
  rename(campamento = nombre)

# cambiar el nombre de varias columnas según su posición
datos |> 
  rename(a = 1,
         b = 2,
         c = 3)

# renombrar usando select
datos |> 
  select(campamento = nombre, comuna, region)


# mutate ----

# crear una variable que contenga un solo valor, repetido
datos |> 
  select(nombre, region, comuna,
         hogares:area) |> 
  mutate(prueba = "a")

# nueva variable en base a operaciones matemáticas
datos |> 
  select(nombre, region, comuna, hogares:area) |> 
  mutate(prueba = area/1000)

datos |> 
  select(nombre, region, comuna, hogares:area) |> 
  mutate(prueba = area/1000*1000^3)

# aplicar una función a una variable existente para crear una nueva
datos |> 
  select(nombre, region, comuna, hogares:area) |> 
  mutate(hectareas2 = round(hectareas, 1))

# aplicar una función a una variable existente para modificar la variable existente
datos |> 
  select(nombre, region, comuna, hogares:area) |> 
  mutate(hectareas = round(hectareas, 1))

# crear nuevos textos a partir del texto de una variable
datos |> 
  select(nombre, region, comuna, hogares:area) |> 
  mutate(nombre = paste("Campamento", nombre))

# guardar una operación sobre un dataframe como un nuevo objeto
datos2 <- datos |> select(nombre, region, comuna, hogares:area)

## ifelse ----
# crear variables usando ifelse()
datos2 |> 
  mutate(tamaño_hect = ifelse(hectareas > 4, 
                              yes = "grande",
                              no = "chico"))

# crear dos variables consecutivas
datos2 |> 
  mutate(tamaño_hect = ifelse(hectareas > 4, 
                         yes = "grande",
                         no = "chico")) |> 
  mutate(tamaño_hog = ifelse(hogares > 60, 
                          yes = "grande",
                          no = "chico"))

# crear dos variables consecutivas, luego filtrar
datos2 |> 
  mutate(tamaño_hect = ifelse(hectareas > 4, 
                              yes = "grande",
                              no = "chico")) |> 
  mutate(tamaño_hog = ifelse(hogares > 60, 
                             yes = "grande",
                             no = "chico")) |> 
  filter(tamaño_hog == "grande") |> 
  filter(tamaño_hect == "chico")


## recodificar ----

# cambiar el contenido de una variable (recodificar) usando ifelse
datos2 |> 
  distinct(region, .keep_all = T) |> 
  mutate(region = ifelse(region == "Valparaíso", 
                         "Región de...",
                         region))

# usando recode
datos2 |> 
  distinct(region, .keep_all = T) |> 
  mutate(region = recode(region,
                         "Valparaíso" = "Región de..."))

# obtener una muestra aleatoria de los datos
datos_muestra <- datos2 |> 
  slice_sample(n = 10)

## case_when ----
# permite crear nuevas variables usando múltiples expresiones que definirán las observaciones que obtienen el nuevo nivel
datos_muestra |> 
  mutate(prueba = case_when(hogares > 100 ~ "alto",
                            hogares > 50 ~ "medio",
                            hogares > 10 ~ "bajo",
                            .default = "otros"))

datos_muestra |> 
  mutate(prueba = case_when(hogares > 100 ~ "alto",
                            hogares > 50 ~ "medio"))

# usar dos condiciones para definir un nivel de la variable
datos_muestra |> 
  mutate(prueba = case_when(hogares > 100 ~ "alto",
                            hogares > 50 & hogares < 100 ~ "medio"))

# usar dos condiciones para definir un nivel de la variable
datos_muestra |> 
  mutate(prueba = case_when(hogares > 100 ~ "alto",
                            hogares > 50 ~ "medio",
                            hogares > 10 & region == "Antofagasta" ~ "bajo 2",
                            hogares > 10 ~ "bajo",
                            .default = "otros"))
  
## porcentaje  ----
datos_muestra |> 
  mutate(total = sum(hogares),
         porcentaje = hogares/total*100)

# obtener nueva muestra de los datos
datos_muestra2 <- datos2 |> 
  filter(region == "Valparaíso" | region == "Antofagasta") |> 
  slice_sample(n = 10)

# calcular porcentaje
datos_muestra2 |> 
  mutate(total = sum(hogares),
         porcentaje = hogares/total*100)

## operaciones agrupadas ----

# la misma operación que antes, pero agrupada por una variable
# esto provoca que el cálculo se haga por grupos, resultando en
# distintos porcentajes calculados para cada región
datos_muestra2 |> 
  group_by(region) |> 
  mutate(total = sum(hogares),
         porcentaje = hogares/total*100)

# obtener promedio general
datos_muestra2 |> 
  mutate(promedio = mean(hogares))

# obtener promedios de cada región
datos_muestra2 |> 
  group_by(region) |> 
  mutate(promedio = mean(hogares))

# resumir ----
# convierte una tabla en una nueva tabla con datos resumidos, con una fila por cada
# combinación de la variable de agrupación, que contienen el estadístico que elegimos para el resumen

# agrupar por región, resumir para obtener los promedios
datos_muestra2 |> 
  group_by(region) |> 
  summarise(promedio = mean(hogares))

# aplicar lo mismo pero a la tabla original con todas las observaciones
datos |> 
  group_by(region) |> 
  summarise(promedio = mean(hogares))

# agrupar por región, y calcular múltiples estadísticos para cada región
datos |> 
  group_by(region) |> 
  summarise(promedio = mean(hogares),
            maximo = max(hogares),
            minimo = min(hogares),
            cantidad = n() ) |> 
  arrange(desc(cantidad))

# lo mismo que antes, pero con un cambio de la variable de agrupación, para
# que los estadísticos se obtengan por comuna
datos |> 
  group_by(comuna) |> 
  summarise(promedio = mean(hogares),
            maximo = max(hogares),
            minimo = min(hogares),
            cantidad = n() ) |> 
  arrange(desc(cantidad))
