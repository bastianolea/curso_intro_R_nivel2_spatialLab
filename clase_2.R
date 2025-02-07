# cargar un paquete
library("readxl")
library("dplyr")

# cargar un archivo excel con datos de campamentos
datos <- read_excel("datos/campamentos_chile_2024.xlsx")

# ver observaciones únicas
datos |> distinct(region)


# conteos ----

# contar cantidad de observaciones en la variable
datos |> count(region, sort = TRUE)

# contar observaciones a través de dos variables y ordenarla por frecuencia
datos |> count(region, comuna, 
               sort = TRUE)

# limpiar datos ----

# cargar paquete que facilita la limpieza de datos
# install.packages("janitor")
library("janitor")

# cargar datos que no vienen limpios, sobre población en situación de pobreza
pobreza <- read_excel("datos/pobreza_comunas_2022.xlsx")
# read_excel("datos/pobreza_comunas_2022.xlsx", skip = 1) #cargar excel saltándose filas

# especificar que los nombres de columna vienen en otra fila
pobreza1 <- pobreza |> 
  row_to_names(2)

# corregir nombres de columnas, porque los originales son muy largos
pobreza2 <- pobreza1 |> 
  clean_names() |> 
  rename(personas_n = 4,
         pobreza_n = 5,
         pobreza_p = 6)

# ver nombres de columnas
pobreza2 |> names()

# ver nombres de columnas, con previsualización de observaciones
pobreza2 |> glimpse()


# mutate ----

# convertir columna a numérica
pobreza2 |> 
  mutate(personas_n = as.numeric(personas_n))

# estando en formato numérico, es posible realizar operaciones matemáticas
pobreza2 |> 
  select(nombre_comuna, personas_n) |> 
  mutate(personas_n = as.numeric(personas_n)) |> 
  mutate(miles_de_personas = personas_n/1000)

# pero el dataframe tiene varias columnas que requieren esta transformación
pobreza2 |> 
  select(4:8)

## mutate across ----

# modificar múltiples columnas al mismo tiempo, seleccionando las columnas por su posición
pobreza2 |> 
  mutate(across(4:8, as.numeric))

# modificar múltiples columnas al mismo tiempo, seleccionando las columnas por su nombre
pobreza2 |> 
  mutate(across(c(personas_n, pobreza_n, pobreza_p, limite_inferior, limite_superior, codigo), 
                as.numeric))

# modificar múltiples columnas al mismo tiempo, en base a que empiezan con el mismo texto
pobreza2 |> 
  mutate(across(starts_with("pobreza"), 
                as.numeric))

# modificar múltiples columnas al mismo tiempo, en base a que empiezan con el mismo texto, y realizar operación matemática
pobreza2 |> 
  mutate(across(starts_with("pobreza"), 
                as.numeric)) |> 
  mutate(across(where(is.numeric),
                ~.x/1000))
# aquí, ".x" opera como si fuera las columnas que vamos a modificar. Significa que cada una de las columnas que seleccionamos en across() van a tomar la posición de ".x", y ejecutar dicho cálculo

# modificar columnas pero explicitando las columnas seleccionadas en la operación
pobreza2 |> mutate(across(4:8, ~as.numeric(.x)))
# es equivalente a pobreza2 |> mutate(across(4:8, as.numeric))

# otro ejemplo, con otras columnas y otra operación
pobreza2 |> 
  mutate(across(starts_with("limite"),
                as.numeric)) |> 
  # redondear cifras
  mutate(across(starts_with("limite"),
                ~round(.x, digits = 2)))

# siempre se pueden poner varias funciones dentro de funciones
pobreza2 |> 
  mutate(across(starts_with("limite"),
                ~round(as.numeric(.x), digits = 2)))

# o, en lugar de ponerlas una dentro de otra, se pueden usar conectores para encadenar las operaciones
pobreza2 |> 
  mutate(across(starts_with("limite"),
                ~as.numeric(.x) |> round(digits = 2)))


# cargar datos sobre casos de corrupción en Chile
datos <- read_excel("datos/casos_corrupcion_chile.xlsx") |> 
  clean_names() |> 
  rename(año = 3)

# conteo
datos |> 
  count(año) |> 
  print(n = Inf)

datos |> count(posicion)
datos |> select(posicion) |> print(n=30)
# variable que tiene muchos datos perdidos

# datos perdidos ----

## filtrar datos perdidos ----
# dejar sólo observaciones de la columna que tengan datos perdidos
datos |> 
  filter(is.na(posicion))

# excluir datos perdidos en la columna
datos |> 
  filter(!is.na(posicion))

# paquete que facilita la transformación de datos
# install.packages("tidyr")
library("tidyr")

# excluir datos perdidos en la columna
datos |> 
  drop_na(posicion)

datos |> 
  count(partido)

## recodificar datos perdidos ----
# usando ifelse para definir el valor que tendrá el dato perdido
datos |> 
  select(1:3, posicion) |> 
  mutate(posicion = ifelse(is.na(posicion),
                           yes = "Sin información",
                           no = posicion))
# si son numéricos, podría ser cero

# usando ifelse, pero definiendo que el caso perdido se reemplace por el valor que tiene la observación en otra columna
datos |> 
  select(1:3, posicion) |> 
  mutate(posicion = ifelse(is.na(posicion),
                           yes = caso,
                           no = posicion))

# usando una función de tidyr qeue simplifica el reemplazo de datos perdidos
datos |> 
  select(1:3, posicion) |> 
  mutate(posicion = replace_na(posicion, "Sin datos"))

# variable numérica con datos perdidos
datos |> 
  select(1:4, starts_with("ano")) |> 
  arrange(ano_fin) |> 
  print(n=30)

# rellenar datos ----

# rellenar datos perdidos con el valor que tiene la observación de arriba
datos |> 
  select(1:4, starts_with("ano")) |> 
  arrange(ano_fin) |> 
  fill(ano_inicio) |> 
  print(n=30)

# rellenar de abajo hacia arriba
datos |> 
  fill(comuna, .direction = "up")


datos |> 
  select(1:4, starts_with("ano")) |> 
  arrange(año) |> 
  print(n=30)

## rellenar con el promedio ----
datos |> 
  select(1:4, starts_with("ano")) |> 
  arrange(año) |> 
  mutate(ano_inicio = ifelse(is.na(ano_inicio),
                             mean(ano_inicio, na.rm = TRUE),
                             ano_inicio)) |> 
  print(n=30)

## rellenar con la mediana ----
datos |> 
  select(1:4, starts_with("ano")) |> 
  arrange(año) |> 
  mutate(ano_inicio = ifelse(is.na(ano_inicio),
                             median(ano_inicio, na.rm = TRUE),
                             ano_inicio)) |> 
  print(n=30)


datos |> 
  count(año)

# completar ----

# completar una secuencia de valores
datos |> 
  count(año) |> 
  tidyr::complete(año = 1989:2025) |>
  mutate(n = replace_na(n, 0)) |> 
  print(n=Inf)

# cargar datos de elecciones presidenciales
elecciones <- readr::read_csv2("datos/presidenciales_2021_comuna.csv")

# separar una columna en varias columnas distintas, definiendo el caracter que indica la separación entre las columnas
elecciones |> 
  select(1:2, candidatura) |> 
  separate(candidatura,
           into = c("nombre", "apellido", "segundo apellido"),
           sep = " ")

# otro ejemplo
elecciones |> 
  select(1:2, candidatura) |> 
  separate(eleccion,
           into = c("cantidad", "tipo"),
           sep = " ")


# pivotes ----

# cargar datos de proyecciones de población
censo <- read_excel("datos/estimaciones-y-proyecciones-2002-2035-comunas.xlsx") |> 
  clean_names() |> 
  rename(sexo = 7)

# este dataset tiene sus datos en decenas de columnas hacia el lado
censo
glimpse(censo)

library("tidyr")

## pivotar hacia largo ----

# transformar la tabla a formato largo
censo_largo <- censo |> 
  select(nombre_comuna, sexo, edad, 
         starts_with("poblacion")) |> 
  pivot_longer(cols = starts_with("poblacion"),
               names_to = "año", values_to = "poblacion")

censo_largo
# de esta forma, los datos pasan de estar en varias columnas, a estar en dos columnas con sus valores hacia abajo

# usar separate() para limpiar la columna resultante desde el nombre de las columnas anteriores
censo_largo2 <- censo_largo |> 
  filter(edad == 40) |> # filtro para hacer la operación más liviana
  separate(año, into = c("variable", "año"), sep = "_")

# probar el nuevo dataset
censo_largo2 |> 
  mutate(año = as.numeric(año)) |> 
  filter(año == 2024,
         sexo == 2) |> 
  filter(nombre_comuna == "La Florida")


## pivotar hacia ancho ----

# probar un resumen de datos general
elecciones |> 
  filter(eleccion == "Primera vuelta") |> 
  group_by(candidatura) |> 
  summarize(total = sum(votos))

# complejizar el resumen de datos incluyendo otra variable más
elecciones |> 
  filter(eleccion == "Primera vuelta") |> 
  group_by(region, candidatura) |> 
  summarize(total = sum(votos))

# pivotar el resumen de datos para que los valores estén en varias columnas
elecciones_ancho <- elecciones |> 
  filter(eleccion == "Primera vuelta") |> 
  group_by(region, candidatura) |> 
  summarize(total = sum(votos)) |> 
  pivot_wider(names_from = candidatura, 
              values_from = total)
# facilita la lectura de los datos

# guardar los datos en un nuevo archivo Excel
library("writexl")

write_xlsx(elecciones_ancho,
           "resultados_presidenciales.xlsx")


# —----

# crear dataframe con resultados por comuna
elecciones_comuna <- elecciones |> 
  group_by(cut_comuna, comuna,
           candidatura) |> 
  summarise(total_votos = sum(votos)) |> 
  ungroup() |> 
  group_by(cut_comuna, comuna) |>
  slice_max(total_votos) |> 
  ungroup()

elecciones_comuna

# pivotar censo a formato largo
censo2 <- censo |> 
  select(nombre_comuna, 
         cut_comuna = comuna,
         sexo, edad, 
         starts_with("poblacion")) |> 
  pivot_longer(cols = starts_with("poblacion"),
               names_to = "año", values_to = "poblacion")

# resumir datos del censo sumando población por comuna y año
censo3 <- censo2 |> 
  group_by(nombre_comuna, cut_comuna,
           año) |> 
  summarize(poblacion = sum(poblacion))

# separar columna del censo 
censo_comunas <- censo3 |> 
  separate(año, into = c("variable", "año"), sep = "_") |> 
  mutate(año = as.numeric(año)) |> 
  ungroup()


# left join ----

# tenemos dos tablas de datos que contienen distinta información, pero que tienen una columna en común
elecciones_comuna
censo_comunas

# unir las dos tablas
left_join(censo_comunas,
          elecciones_comuna) |> 
  filter(año == 2021)

# unir las dos tablas explicitando la variable de unión o "llave"
# unir las dos tablas
left_join(censo_comunas,
          elecciones_comuna,
          by = join_by(cut_comuna))

# realizar la unión, pero ajustando los datos justo al momento de la unión
elecciones_poblacion <- left_join(censo_comunas |> filter(año == 2021),
                                  elecciones_comuna |> select(-comuna)
                                  )

# porcentajes ----

# calcular una nueva columna gracias a la información unida 
elecciones_poblacion |> 
  mutate(porcentaje = total_votos/poblacion)

# porcentaje 
elecciones_poblacion |> 
  mutate(porcentaje = (total_votos/poblacion)*100)

# formatear porcentaje
elecciones_poblacion |> 
  mutate(porcentaje = scales::percent(total_votos/poblacion, 
                                      accuracy = 0.1, decimal.mark = ","))

# formatear porcentaje manualmente
elecciones_poblacion |> 
  mutate(porcentaje = 
           paste(
             round(
               (total_votos/poblacion)*100, 
               1), 
             "%", sep = "")
  )

# formatear poorcentaje manualmente, pero usando conectores
elecciones_poblacion |> 
  mutate(porcentaje = ((total_votos/poblacion)*100) |> round(1) |> paste("%", sep = ""))

# definir una función para formatear el porcentaje
porcentaje <- function(x, y, simbolo) {
  ((x/y)*100) |> round(1) |> paste(simbolo, sep = "")
}

# usar la función dentro de mutate
elecciones_poblacion |> 
  mutate(porcentaje = porcentaje(total_votos, poblacion, "$"))
