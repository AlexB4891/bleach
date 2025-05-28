
library(bleach) # Cambia por el nombre de tu paquete si es diferente

test_that("missing_evaluation.character clasifica correctamente", {
  x <- c("", "    ", NA_character_, "data")
  res <- missing_evaluation(x)
  expect_equal(res$original, c(NA_character_, NA_character_, NA_character_, "data"))
  expect_equal(res$modificada, c("texto_vacio", "texto_vacio", "texto_na", NA_character_))
})

test_that("missing_evaluation.numeric clasifica correctamente", {
  x <- c("1", "2", "NULL", "4")
  class(x) <- "numeric_raw"
  res <- missing_evaluation(x)
  expect_equal(res$original, c(1, 2, NA_real_, 4))
  expect_equal(res$modificada, c(NA_character_, NA_character_, "no_numero", NA_character_))
})



test_that("asignar_clase asigna clase character", {
  df <- data.frame(a = c("1", "2"), stringsAsFactors = FALSE)
  clases <- c("character")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "character")
})

test_that("asignar_clase asigna clase integer", {
  df <- data.frame(a = c("1", "2"), stringsAsFactors = FALSE)
  clases <- c("integer")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "integer")
})

test_that("asignar_clase asigna clase numeric", {
  df <- data.frame(a = c("1.5", "2.5"), stringsAsFactors = FALSE)
  clases <- c("numeric")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "numeric")
})

test_that("asignar_clase asigna clase logical", {
  df <- data.frame(a = c("TRUE", "FALSE"), stringsAsFactors = FALSE)
  clases <- c("logical")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "logical")
})

test_that("asignar_clase asigna clase Date", {
  df <- data.frame(a = c("2023-01-01", "2023-01-02"), stringsAsFactors = FALSE)
  clases <- c("fecha")
  res <- asignar_clase(df, clases)
  expect_s3_class(res$a, "fecha")
})

test_that("asignar_clase asigna clase POSIXct", {
  df <- data.frame(a = c("2023-01-01 12:00:00", "2023-01-02 13:00:00"), stringsAsFactors = FALSE)
  clases <- c("fecha_hora")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "fecha_hora")
})

test_that("asignar_clase funciona con varias columnas y clases", {
  df <- data.frame(
    a = c("1", "2"),
    b = c("2023-01-01", "2023-01-02"),
    c = c("TRUE", "FALSE"),
    stringsAsFactors = FALSE
  )
  clases <- c("integer", "fecha", "logical")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "integer")
  expect_equal(class(res$b), "fecha")
  expect_equal(class(res$c), "logical")
})



# Funciones de perfilamiento ----------------------------------------------

library(tibble)

datos_simulados <- tibble(
  upm  = c("001", "002", "", "004", NA, "006", "\t", "008", "009", "010"),
  fexp = c(1.2, NA, 0.8, "", 2.5, 1.0, "\t", 3.1, NA, 2.0),
  p45  = c(5, "", NA, 10, 3, "\t", 7, 4, 6, NA),
  p11  = c("2023-01-01", "", "2023-01-03", NA, "\t", "2023-01-06", "2023-01-07", "", "2023-01-09", "2023-01-10")
)

instrucciones <- tribble(
  ~variable, ~descripcion_de_la_variable, ~tipo_de_variable,         ~tipo_perfilamiento,  ~formato,
  "upm",     "Unidad Primaria de Muestreo", "Identificador",         "cadena",             NA_character_,
  "fexp",    "Factor de expansion",         "Numérico",              "numero",             NA_character_,
  "p45",     "Cuantos años trabaja",        "Cuantitativa discreta", "entero",             NA_character_,
  "p11",     "Fecha de levantamiento",      "Fecha",                 "fecha",              "%Y-%m-%d"
)




