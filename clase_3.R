# introducción al análisis de datos con R para principiantes
# nivel 2: Manipulación de bases de datos en R
# clase 3
# contacto: Bastián Olea Herrera - baolea@uc.cl - https://bastianolea.rbind.io

# cargar paquetes
library("readxl")
library("dplyr")


# texto ----

# install.packages("stringr")
library("stringr")
# paquete para trabajar con datos en formato texto

# crear un vector de textos
nombres <- c("juan", "maria", "raul", "pedro", "josefa", "marisol")
class(nombres) # consultar el tipo del vector

nombres # ver contenidos del vector

# consultar si el vector tiene una equivalencia exacta con un dato
nombres == "maria"

# consultar si se detecta el texto entre los elementos del vector
str_detect(nombres, "maria")

# esto permite consultar coincidencias parciales
str_detect(nombres, "ma") # evaluar si algún elemento de vector contiene las letras "ma"


## filtrar texto ----
# cargar un archivo excel con datos de campamentos
datos <- read_excel("datos/campamentos_chile_2024.xlsx")

# usar str_detect() para filtrar los datos por una coincidencia parcial de texto
datos |> 
  filter(str_detect(nombre, "Nueva"))
# observaciones que contienen la palabra "Nueva"

# filtrar regiones que contienen "Metro"
datos |> 
  filter(str_detect(region, "Metro"))


## eliminar texto ---- 
# modificar una columna de texto para remover letras, símbolos o palabras
datos |> 
  mutate(nombre = str_remove(nombre, "Los"))
# eliminar las letras "Los" de la columna

# eliminar "Los" y luego "Las"
datos |> 
  mutate(nombre = str_remove(nombre, "Los"),
         nombre = str_remove(nombre, "Las"))

# eliminar "Los" y "Las" con el operador "o" (|)
datos |> 
  mutate(nombre = str_remove(nombre, "Los|Las"))
# busca la palabra "Los", y si no la encuentra, busca la palabra "Las"

# eliminar texto, pero tomando en consideración el espacio que queda después de la palabra
datos |> 
  mutate(nombre = str_remove(nombre, "Los |los ")) |> 
  print(n=40)
# así borras la palabra y el espacio que le sigue


## reemplazar texto ----

# con str_replace(), defines la variable que vas a ocupar, luego el texto que quieres coincidir,
# y luego el texto que quieres reemplazar
datos |> 
  count(region) |> 
  mutate(region = str_replace(region, "General", "G."))
# entonces todos los textos dentro de las obseravaciones que dicen "General" cambiarán a "G"


## mayúsculas ----

# cambiar letras a mayúsculas, y luego a minúsculas
datos |> 
  count(region) |> 
  mutate(region = str_to_upper(region))

datos |> 
  count(region) |> 
  mutate(region = str_to_lower(region))

# convertir a titulares (mayúscula en cada palabra)
datos |> 
  count(region) |> 
  mutate(region = str_to_upper(region)) |> 
  mutate(region = str_to_title(region))

# convertir a oración (mayúsculas solo al principio de la línea
datos |> 
  count(region) |> 
  mutate(region = str_to_upper(region)) |> 
  mutate(region = str_to_sentence(region))

datos |> 
  select(observaciones) |> 
  mutate(observaciones = str_to_sentence(observaciones))


# convertir a titulares, y luego reemplazar palabras específicas
# para que palabras que no corresponde que vayan en mayúsculas se corrijan
datos |> 
  count(region) |> 
  mutate(region = str_to_upper(region)) |> 
  mutate(region = str_to_title(region)) |> 
  mutate(region = str_replace(region, " Y ", " y "),
         region = str_replace(region, " De ", " de "),
         region = str_replace(region, " La ", " la "))


## extraer números desde texto ----

# formas de limpiar una columna para extraer los números que estén entre textos

datos |> 
  select(año) |> 
  distinct()

# opción 1: eliminando el texto para dejar el número
datos |> 
  select(año) |> 
  distinct() |> 
  mutate(año = str_remove(año, "CATASTRO_"))
# pero este método solo sirve si sabemos de antemano el texto que hay que borrar

# opción 2: usar str_extract() para extraer los años
# pero hay que saber qué año queremos extraer
datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "2019"))

# opción 2.1: usar str_extract() pero con el operador "o" para extraer varios años
datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "2011|2012|2013|2014|2019"))

# opción 2.2: usar str_extract() pero creando un vector de todos los años usando el operador "o" para extraer todos los años posibles
años <- 1900:2025 # crear secuencia de números
paste(años, collapse = ", ") # ejemplo de unir los años con comas
años_extraer <- paste(años, collapse = "|") # crear texto uniendo con "|"

# usar el vector para extraer
datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, años_extraer))

### regex ----
# las expresiones regulares (regex) son formas de detrectar, coincidir, o extraer texto usando operadores especiales
vignette("regular-expressions")

# usar el operador "\\d+" para extraer cualquier cifra
datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "\\d+")) # extraer números
# el problema es que, al extraer cualquier cifra, extrae también otras que no son años

# usar el operador "\\d{4}" para extraer cifras de 4 dígitos
datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "\\d{4}")) # extraer números de 4 dígitos

# usar el operador "$" para indicar que la cifra tiene que estar pegada al final de la línea
datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "\\d{4}$")) # extraer números de 4 dígitos al final del texto
# para buscar al principio del texto: "^" antes del texto a coincidir


# explorar datos ----

## listas de espera ----
# cargar datos de listas de espera 
lista <- read_excel("datos/datos_estudiantes/Base de datos Lista de Espera.xlsx") |> 
  janitor::clean_names()

# conteos
lista |> count(region)

lista |> count(sexo)

lista |> count(linea_programatica)

# explorar
lista |> glimpse()

lista |> count(nombre)

# conteo por dos variables
lista |> 
  count(nombre, sexo) |> 
  print(n=Inf)
# detectamos un problema: hay una columna que contiene dos variables: la línea programatica y el nombre del establecimiento, por lo que si queremos hacer un conteo por establecimiento, no podemos, porque el establecimiento está separados por sus líneas programáticas

# filtrar el texto parcialmente, para filtrar todas las observaciones de un establecimiento, y luego hacerle un conteo a esas observaciones
lista |> 
  filter(str_detect(nombre, "COPIAPÓ")) |> 
  count(sexo)

# la solución completa sería separar en dos columnas la columna que contiene dos variables
library(tidyr)

# separar
lista2 <- lista |> 
  separate(nombre, into = c("linea", "nombre"), sep = " - ") 
# guardar el resultado en un dataframe nuevo

# probar resultado
lista2 |> 
  count(nombre, sexo)

# pivotar a ancho para ver resultados
lista2 |> 
  count(nombre, sexo) |> 
  pivot_wider(names_from = sexo, values_from = n)

# calcular promedio
lista2 |> 
  group_by(region, nombre) |> 
  summarise(mean(permanencia_meses))

# calcular promedio
lista2 |> 
  group_by(region) |> 
  summarise(promedio = mean(permanencia_meses))

# calcular promedio y ordenar
lista2 |> 
  group_by(linea_programatica) |> 
  summarise(promedio = mean(permanencia_meses)) |> 
  arrange(desc(promedio))

# calcular estadísticos descriptivos: promedio, mínimo, percentil 25, 50 y 75, y máximo
lista2 |> 
  group_by(linea_programatica) |> 
  summarise(promedio = mean(permanencia_meses),
            minimo = min(permanencia_meses),
            percentil_25 = quantile(permanencia_meses, 0.25), # percentil
            percentil_50 = median(permanencia_meses),
            percentil_75 = quantile(permanencia_meses, 0.75), # percentil
            maximo = max(permanencia_meses),
  )

# reutilizar el código anterior, cambiando la variable que queremos describir
lista2 |> 
  group_by(linea_programatica) |> 
  summarise(promedio = mean(promedio_linea_programatica),
            minimo = min(promedio_linea_programatica),
            percentil_25 = quantile(promedio_linea_programatica, 0.25), # percentil
            percentil_50 = median(promedio_linea_programatica),
            percentil_75 = quantile(promedio_linea_programatica, 0.75), # percentil
            maximo = max(promedio_linea_programatica),
  )

# tip: si queremos explorar más rápido los datos, primero creamos una variable que se llame "variable"
# que sea igual a la variable que queremos explorar, y luego hacemos que todos los estadísticos se calculen
# sobre "variable"; de esta forma, si queremos cambiar la variable, la cambiamos una sola vez en ves de 6 veces
lista2 |> 
  group_by(linea_programatica) |> 
  mutate(variable = promedio_linea_programatica) |> # cambiar aquí la variable
  summarise(promedio = mean(variable),
            minimo = min(variable),
            percentil_25 = quantile(variable, 0.25), # percentil
            percentil_50 = median(variable),
            percentil_75 = quantile(variable, 0.75), # percentil
            maximo = max(variable),
  )

### fechas ----
# para trabajar con fechas
library("lubridate")

# este dataframe contiene columnas en formato de fecha/hora
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha"))

# convertir fecha/hora a fecha
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(fecha_ingreso = as_date(fecha_ingreso_le))

# restar fechas para obtener los días de diferencia
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(dias = as_date(fecha_egreso_le) - as_date(fecha_ingreso_le))

# funciones para obtener fechas de hoy, y fecha de hoy con hora
today()
now()

# usar la fecha de hoy para calcular los días entre la fecha de los datos y hoy
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(dias = today() - as_date(fecha_ingreso_le))

# extraer años y meses desde las fechas
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(año_ingreso = year(fecha_ingreso_le),
         mes_ingreso = month(fecha_ingreso_le))

# filtrar una fecha por su año
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  filter(year(fecha_ingreso_le) == 2021)

# filtrar datos de fecha para dejar observaciones más recientes que 6 meses
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  filter(fecha_egreso_le > today() - months(6))

# filtrar datos de fecha para dejar observaciones más recientes que 2 días
lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  filter(fecha_egreso_le > today() - days(2))

# fecha escrita en texto (tipo texto)
"2025-02-11" |> class()

# convertir un texto a fecha
as_date("2025-02-11") |> class()

# convertir a fecha, extraer los días
as_date("2025-02-11") |> day()

# si la fecha viene escrita como día/mes/año, no funciona
as_date("11/02/2025")

# en su lugar, usar la función dmy() para día/mes/año
dmy("11/02/2025")

ymd("2025/02/11")



## datos de texto ----

# cargar datos de texto
citas <- read_excel("datos/datos_estudiantes/Citas sobre género - Ecosistema libertario en X Uruguay.xlsx") |>
  rename(id = 1, cita = 2, codigos = 3)

# explorar
citas |> glimpse()

# filtrar textos que contienen la palabra
citas |> 
  select(cita) |> 
  filter(str_detect(cita, "genero"))

# filtrar textos que contienen una palabra escrita de dos formas
citas |> 
  select(cita) |> 
  filter(str_detect(cita, "género|genero"))

# filtrar textos que contienen una palabra, donde caracteres individuales pueden escribirse de más de una forma
citas |> 
  select(cita) |> 
  filter(str_detect(cita, "(g|G)(é|e)nero"))
# coindice con género, Género, Genero, genero

# crear una columna nueva que indica si una observación contiene la/las palabra/s
citas |> 
  mutate(genero = str_detect(cita, "(g|G)(é|e)nero"))

# crear una columna nueva que indica si una observación contiene múltiples palabra/s
citas |> 
  mutate(genero = str_detect(cita, "(g|G)(é|e)nero|mujer|feminismo|sexo"))

# crear una variable que clasifica los textos en varios temas dependiendo de si coinciden varias palabras
citas |> 
  select(-codigos) |> 
  mutate(tema = case_when(str_detect(cita, "g.nero|mujer|feminis") ~ "Género",
                          str_detect(cita, "economia|precios|dólar") ~ "Economía",
                          str_detect(cita, "odio|rabia|violen") ~ "Violencia",
                          str_detect(cita, "pol.tica|gobierno|presidente") ~ "Política")) |> 
  count(tema)

# truncar textos muy largos
citas |> 
  mutate(cita = str_trunc(cita, 30)) |> 
  select(id, cita, codigos)

# revisar columna de códigos
citas |> 
  select(codigos)

# separar columnas
paste0("codigo_", 1:8) #crear un vector que crea una secuencia de palabras con numeros

# separar las columnas de códigos
citas3 <- citas |> 
  mutate(cita = str_trunc(cita, 30)) |> 
  select(id, cita, codigos) |> 
  separate(codigos, sep = "\n", into = paste0("codigo_", 1:8))

# luego pivotar a largo
citas4 <- citas3 |> 
  pivot_longer(cols = starts_with("codigo_"), values_to = "codigo") |> 
  filter(!is.na(codigo)) |> 
  select(-name)

# ahora podemos filtrar las observaciones que contengan un código
citas4 |> 
  filter(codigo == "Queja")

# filtrar varios códigos
citas4 |> 
  filter(codigo %in% c("Queja", "Plata")) |> 
  print(n=Inf)

# crear una secuencia de ids de las citas que contienen un código
id_quejas <- citas4 |> filter(codigo == "Queja") |> pull(id)

id_quejas

# ejemplo de crear un id, si no existiera
citas |> mutate(id = 1:n())

# usar el vector de ids para filtrar las observaciones que contienen quejas, y luego filtrar además por otro código
citas4 |> 
  filter(id %in% id_quejas) |> 
  filter(codigo == "Izquierda") |> 
  print(n=20)

# hacer lo mismo que antes, pero con str_detect(): filtrar las que tienen queda, y luego, entre esas, las que tienen otro tipo de queja
citas |> 
  select(id, codigos) |> 
  filter(str_detect(codigos, "Queja")) |> 
  filter(str_detect(codigos, "Izquierda"))

citas


## directorio de escuelas ----
library(readr)
escuelas <- read_csv2("datos/datos_estudiantes/20240912_Directorio_Oficial_EE_2024_20240430_WEB.csv") |> 
  janitor::clean_names()

# conteo
escuelas |> 
  count(agno)

# explorar
escuelas |> glimpse()

# seleccionar columnas
escuelas |> 
  select(nom_rbd, starts_with("ens"))
# son 11 columnas que contienen una cantidad desconocida de códigos que refieren a su una escuela ofrece un nivel de enseñanza específico
# estos códigos son números donde el primer dígito indica una agrupación mayor de enseñanza (parvularia, basica, media, etc)

# resolver este problema
escuelas |> 
  # seleccionar columnas
  select(nom_rbd, starts_with("ens")) |> 
  # pivotar a largo las columnas, para dejar los códigos en una sola columna
  pivot_longer(starts_with("ens")) |> 
  # extraer el primer número de las cifras usando el regex para extraer un solo caracter
  mutate(codigo = str_extract(value, ".")) |> 
  # crear columna con el conteo de dígitos en las cifras
  mutate(n_car = nchar(value)) |> 
  # crear la nueva variable de clasificación a partir de los primeros dígitos de cada cifra,
  # incluyendo las cifras de menos de tres dígitos (porque parvularia es "1x" y básica es "1xx"; ambas empiezan con 1 pero la diferencia es la cantidad de dígitos)
  mutate(enseñanza = case_when(n_car == 2 & codigo == 1 ~ "Parvularia",
                               n_car > 2 & codigo == 1 ~ "Básica",
                               n_car > 2 & codigo == 3 ~ "Media")) |> 
  print(n=50)


## beneficiarios fonasa ----

# cargar datos
read_csv("datos/datos_estudiantes/Beneficiarios 2023.csv")
# este dataset viene con una codificación poco común, lo que provoca que se cargue con los tildes y otras cifras incorrectamente

# cargar dataser con la codificación correcta
beneficiarios <- read_csv("datos/datos_estudiantes/Beneficiarios 2023.csv", 
                          locale = locale(encoding = "latin1")) |> 
  janitor::clean_names()

# conteo
beneficiarios |> 
  count(tramo_fonasa)

# sumar datos de una columna en base a otra
beneficiarios |> 
  group_by(tramo_fonasa) |> 
  summarise(sum(cuenta_beneficiarios))

# contar observaciones según una columna
beneficiarios |> 
  group_by(tramo_fonasa) |> 
  summarise(n())

beneficiarios |> 
  count(tramo_fonasa)

# agregar columna con un conteo
beneficiarios |> 
  add_count(tramo_fonasa)

# lo anterior es equivalente a lo siguiente
beneficiarios |> 
  group_by(tramo_fonasa) |> 
  mutate(n_tramo = n())

# crear versión más pequeña del dataset
beneficiarios2 <- beneficiarios |> 
  select(2, 3, 4, 5) |> 
  tail()

beneficiarios2

# agregar una fila manualmente a los datos
beneficiarios2 |> 
  add_row(titular_carga = "Titular", tramo_fonasa = "B", sexo = "Mujer", edad_tramo = "99 años")

# crear una tabla nueva
extras <- tibble(titular_carga = c("Titular", "Titular"),
                 tramo_fonasa = c("B", "D"), 
                 sexo = c("Mujer", "Hombre"), 
                 edad_tramo = c("99 años", "20 a 29 años"))

extras

# agregar esta tabla debajo de los datos
beneficiarios2 |> 
  bind_rows(extras)

# agregarla arriba de los datos
bind_rows(extras, beneficiarios2)

# agregarla a los datos originales
bind_rows(extras, beneficiarios)
# las variables que no están definidas quedan como NA

# crear una columna
extra2 <- tibble(altural = c(170, 180, 150, 145, 156, 187))

# agregar la columna a la tabla pequeñs
beneficiarios2 |> 
  bind_cols(extra2)
