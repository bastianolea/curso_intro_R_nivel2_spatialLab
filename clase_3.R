# cargar un paquete
library("readxl")
library("dplyr")

# cargar un archivo excel con datos de campamentos
datos <- read_excel("datos/campamentos_chile_2024.xlsx")

# install.packages("stringr")


# texto ----

library("stringr")


nombres <- c("juan", "maria", "raul", "pedro", "josefa")
class(nombres)
nombres

nombres == "maria"

str_detect(nombres, "maria")

str_detect(nombres, "a")

datos |> 
  filter(str_detect(nombre, "Nueva"))

datos |> 
  filter(str_detect(region, "Metro"))

datos |> 
  mutate(nombre = str_remove(nombre, "Los"))

datos |> 
  mutate(nombre = str_remove(nombre, "Los"),
         nombre = str_remove(nombre, "Las"))

datos |> 
  mutate(nombre = str_remove(nombre, "Los|Las"))

datos |> 
  mutate(nombre = str_remove(nombre, "Los |los ")) |> 
  print(n=40)

datos |> 
  count(region) |> 
  mutate(region = str_replace(region, "General", "G."))

datos |> 
  count(region) |> 
  mutate(region = str_to_upper(region)) |> 
  mutate(region = str_to_title(region)) |> 
  mutate(region = str_replace(region, " Y ", " y "),
         region = str_replace(region, " De ", " de "),
         region = str_replace(region, " La ", " la "))

datos |> 
  count(region) |> 
  mutate(region = str_to_upper(region)) |> 
  mutate(region = str_to_sentence(region))

datos |> 
  mutate(observaciones = str_to_sentence(observaciones))


datos |> 
  select(año) |> 
  distinct()

datos |> 
  select(año) |> 
  distinct() |> 
  mutate(año = str_remove(año, "CATASTRO_"))


datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "2011|2012|2013|2014|2019"))


años <- 1900:2025
paste(años, collapse = ", ")
años_extraer <- paste(años, collapse = "|")

datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, años_extraer))

# regex
vignette("regular-expressions")

datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "\\d+")) # extraer números

datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "\\d{4}")) # extraer números de 4 dígitos

datos |> 
  select(nombre, año) |> 
  mutate(año = paste(nombre, año)) |> 
  slice_sample(n = 20) |> 
  mutate(año2 = str_extract(año, "\\d{4}$")) # extraer números de 4 dígitos al final del texto
# para buscar al principio del texto: ^



# cargar datos de listas de espera ----

lista <- read_excel("datos/datos_estudiantes/Base de datos Lista de Espera.xlsx") |> 
  janitor::clean_names()

lista |> count(region)

lista |> count(sexo)

lista |> count(linea_programatica)

lista |> glimpse()

lista |> count(nombre)

lista |> 
  count(nombre, sexo) |> 
  print(n=Inf)

lista |> 
  filter(str_detect(nombre, "COPIAPÓ")) |> 
  count(sexo)

library(tidyr)

lista2 <- lista |> 
  separate(nombre, into = c("linea", "nombre"), sep = " - ") 

lista2 |> 
  count(nombre, sexo)

lista2 |> 
  count(nombre, sexo) |> 
  pivot_wider(names_from = sexo, values_from = n)

lista2 |> 
  group_by(region, nombre) |> 
  summarise(mean(permanencia_meses))

lista2 |> 
  group_by(region) |> 
  summarise(promedio = mean(permanencia_meses))

lista2 |> 
  group_by(linea_programatica) |> 
  summarise(promedio = mean(permanencia_meses)) |> 
  arrange(desc(promedio))

lista2 |> 
  group_by(linea_programatica) |> 
  summarise(promedio = mean(permanencia_meses),
            minimo = min(permanencia_meses),
            percentil_25 = quantile(permanencia_meses, 0.25), # percentil
            percentil_50 = median(permanencia_meses),
            percentil_75 = quantile(permanencia_meses, 0.75), # percentil
            maximo = max(permanencia_meses),
  )

lista2 |> 
  group_by(linea_programatica) |> 
  summarise(promedio = mean(promedio_linea_programatica),
            minimo = min(promedio_linea_programatica),
            percentil_25 = quantile(promedio_linea_programatica, 0.25), # percentil
            percentil_50 = median(promedio_linea_programatica),
            percentil_75 = quantile(promedio_linea_programatica, 0.75), # percentil
            maximo = max(promedio_linea_programatica),
  )


lista2 |> 
  group_by(linea_programatica) |> 
  mutate(variable = promedio_linea_programatica) |> 
  summarise(promedio = mean(variable),
            minimo = min(variable),
            percentil_25 = quantile(variable, 0.25), # percentil
            percentil_50 = median(variable),
            percentil_75 = quantile(variable, 0.75), # percentil
            maximo = max(variable),
  )


# para trabajar con fechas
library("lubridate")

lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(fecha_ingreso = as_date(fecha_ingreso_le))

lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(dias = as_date(fecha_egreso_le) - as_date(fecha_ingreso_le))

today()
now()

lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(dias = today() - as_date(fecha_ingreso_le))


lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  mutate(año_ingreso = year(fecha_ingreso_le),
         mes_ingreso = month(fecha_ingreso_le))

lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  filter(year(fecha_ingreso_le) == 2021)

lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  filter(fecha_egreso_le > today() - months(6))

lista2 |> 
  select(region, nombre, nombres, sexo, starts_with("fecha")) |> 
  filter(fecha_egreso_le > today() - days(2))


"2025-02-11" |> class()

as_date("2025-02-11") |> class()

as_date("2025-02-11") |> day()

as_date("11/02/2025")

dmy("11/02/2025")
ymd("2025/02/11")



# datos de texto ----

citas <- read_excel("datos/datos_estudiantes/Citas sobre género - Ecosistema libertario en X Uruguay.xlsx") |> 
  rename(id = 1, cita = 2, codigos = 3)

citas |> glimpse()

citas |> 
  select(cita) |> 
  filter(str_detect(cita, "genero"))

citas |> 
  select(cita) |> 
  filter(str_detect(cita, "género|genero"))

citas |> 
  select(cita) |> 
  filter(str_detect(cita, "(g|G)(é|e)nero"))

citas |> 
  mutate(genero = str_detect(cita, "(g|G)(é|e)nero"))

citas |> 
  mutate(genero = str_detect(cita, "(g|G)(é|e)nero|mujer|feminismo|sexo"))

citas |> 
  select(-codigos) |> 
  mutate(tema = case_when(str_detect(cita, "g.nero|mujer|feminis") ~ "Género",
                          str_detect(cita, "economia|precios|dólar") ~ "Economía",
                          str_detect(cita, "odio|rabia|violen") ~ "Violencia",
                          str_detect(cita, "pol.tica|gobierno|presidente") ~ "Política")) |> 
  count(tema)


citas |> 
  mutate(cita = str_trunc(cita, 30)) |> 
  select(id, cita, codigos)


"codigo1"

# separar columnas
citas3 <- citas |> 
  mutate(cita = str_trunc(cita, 30)) |> 
  select(id, cita, codigos) |> 
  separate(codigos, sep = "\n", into = c(paste0("codigo_", 1:8)))

citas4 <- citas3 |> 
  pivot_longer(cols = starts_with("codigo_"), values_to = "codigo") |> 
  filter(!is.na(codigo)) |> 
  select(-name)

citas4 |> 
  filter(codigo == "Queja")

citas4 |> 
  filter(codigo %in% c("Queja", "Plata")) |> 
  print(n=Inf)


id_quejas <- citas4 |> filter(codigo == "Queja") |> pull(id)

id_quejas

citas |> mutate(id = 1:n())

citas4 |> 
  filter(id %in% id_quejas) |> 
  filter(codigo == "Izquierda") |> 
  print(n=20)

citas |> 
  select(id, codigos) |> 
  filter(str_detect(codigos, "Queja")) |> 
  filter(str_detect(codigos, "Izquierda"))

citas

library(readr)
escuelas <- read_csv2("datos/datos_estudiantes/20240912_Directorio_Oficial_EE_2024_20240430_WEB.csv") |> 
  janitor::clean_names()

escuelas |> 
  count(agno)

escuelas |> glimpse()

escuelas |> 
  select(nom_rbd, starts_with("ens"))

escuelas |> 
  select(nom_rbd, starts_with("ens")) |> 
  pivot_longer(starts_with("ens")) |> 
  mutate(codigo = str_extract(value, ".")) |> 
  mutate(n_car = nchar(value)) |> 
  mutate(enseñanza = case_when(n_car == 2 & codigo == 1 ~ "Parvularia",
                               n_car > 2 & codigo == 1 ~ "Básica",
                               n_car > 2 & codigo == 3 ~ "Media")) |> 
  print(n=50)


read_csv("datos/datos_estudiantes/Beneficiarios 2023.csv")


beneficiarios <- read_csv("datos/datos_estudiantes/Beneficiarios 2023.csv", 
                          locale = locale(encoding = "latin1")) |> 
  janitor::clean_names()

beneficiarios |> 
  count(tramo_fonasa)

beneficiarios |> 
  group_by(tramo_fonasa) |> 
  summarise(sum(cuenta_beneficiarios))


beneficiarios |> 
  group_by(tramo_fonasa) |> 
  summarise(n())

beneficiarios |> 
  count(tramo_fonasa)

beneficiarios |> 
  add_count(tramo_fonasa)

beneficiarios |> 
  group_by(tramo_fonasa) |> 
  mutate(n_tramo = n())


beneficiarios2 <- beneficiarios |> 
  select(2, 3, 4, 5) |> 
  tail()


beneficiarios2 |> 
  add_row(titular_carga = "Titular", tramo_fonasa = "B", sexo = "Mujer", edad_tramo = "99 años")

extras <- tibble(titular_carga = c("Titular", "Titular"),
                 tramo_fonasa = c("B", "D"), 
                 sexo = c("Mujer", "Hombre"), 
                 edad_tramo = c("99 años", "20 a 29 años"))

extras

beneficiarios2 |> 
  bind_rows(extras)

 bind_rows(extras, beneficiarios2)
 
 bind_rows(extras, beneficiarios)

 
extra2 <- tibble(altural = c(170, 180, 150, 145, 156, 187))
 
beneficiarios2 |> 
  bind_cols(extra2)
 