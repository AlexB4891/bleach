
library(bleach)
library(RPostgres)# Cambia por el nombre de tu paquete si es diferente

test_that("missing_evaluation.character clasifica correctamente", {
  x <- c("", "    ", NA_character_, "data")
  class(x) <- "character_raw"
  res <- missing_evaluation(x)
  expect_equal(res$original, c(NA_character_, NA_character_, NA_character_, "data"))
  expect_equal(res$modificada, c("texto_vacio", "texto_vacio", "texto_na", NA_character_))
})

test_that("missing_evaluation.numeric clasifica correctamente", {
  x <- c("1", "2", "NULL", "4")
  class(x) <- "numeric_raw"
  res <- missing_evaluation(x)
  expect_equal(res$original, c("1", "2", NA_character_, "4"))
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
  clases <- c("entero")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "entero")
})

test_that("asignar_clase asigna clase numeric", {
  df <- data.frame(a = c("1.5", "2.5"), stringsAsFactors = FALSE)
  clases <- c("numero")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "numero")
})

test_that("asignar_clase asigna clase logical", {
  df <- data.frame(a = c("TRUE", "FALSE"), stringsAsFactors = FALSE)
  clases <- c("logico")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "logico")
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
  clases <- c("entero", "fecha", "logico")
  res <- asignar_clase(df, clases)
  expect_equal(class(res$a), "entero")
  expect_equal(class(res$b), "fecha")
  expect_equal(class(res$c), "logico")
})



# Funciones de perfilamiento ----------------------------------------------

test_that("perfilamiento.decimal respeta el atributo de separador decimal", {
  x <- c("1,23", "4,56", "7,89", NA, "1000,01", "999999")
  class(x) <- "decimal" # Asignar clase para simular datos de tipo decimal

  attr(x, "argumentos") <- 'dec=","' # separador decimal es la coma

  attr(x, "valores_na") <- "999999" # separador decimal es la coma

  resultado <- perfilamiento(x)

  expect_equal(as.numeric(resultado), c(1.23, 4.56, 7.89, NA_real_, 1000.01, NA_real_), tolerance = 1e-6)
  expect_true(!is.null(attr(resultado, "count_nas")))
})


test_that("perfilamiento.decimal respeta el atributo de separador decimal bien", {
  x <- c("1,23", "4,56", "7,89", NA, "1000,01", "999999")
  class(x) <- "decimal" # Asignar clase para simular datos de tipo decimal

  attr(x, "argumentos") <- 'dec=","' # separador decimal es la coma

  attr(x, "valores_na") <- "999999" # separador decimal es la coma

  resultado <- bleach::perfilamiento(x)

  expect_equal(as.numeric(resultado), c(1.23, 4.56, 7.89, NA_real_, 1000.01, NA_real_), tolerance = 1e-6)
  expect_true(!is.null(attr(resultado, "count_nas")))
})


test_that("perfilamiento.fecha respeta el atributo formato", {
  x <- c("2023-01-01", "2023-02-15", NA, "2023-12-31")
  attr(x, "argumentos") <- '%Y-%m-%d'
  class(x) <- "fecha" # Asignar clase para simular datos de tipo fecha
  resultado <- perfilamiento(x)
  expect_s3_class(resultado, "Date")
  expect_equal(resultado[1], as.Date("2023-01-01"))
  expect_true(is.na(resultado[3]))
  expect_true(!is.null(attr(resultado, "count_nas")))
})

test_that("perfilamiento.fecha_hora respeta el atributo formato", {
  x <- c("2023-01-01 12:00:00", "2023-02-15 23:59:59", NA, "2023-12-31 00:00:00")
  attr(x, "argumentos") <- 'formato = "%Y-%m-%d %H:%M:%S"'
  class(x) <- "fecha_hora" # Asignar clase para simular datos de tipo fecha_hora
  resultado <- perfilamiento(x)
  expect_s3_class(resultado, "POSIXct")
  expect_equal(format(resultado[1], "%Y-%m-%d %H:%M:%S"), "2023-01-01 12:00:00")
  expect_true(is.na(resultado[3]))
  expect_true(!is.null(attr(resultado, "count_nas")))
})


test_that("perfilamiento.entero transforma correctamente", {
  x <- c("1", "2", NA, "abc", "5","999999","2.5", "s")
  class(x) <- "entero" # Asignar clase para simular datos de tipo entero
  attr(x, "valores_na") <- "999999" # separador decimal es la coma
  resultado <- perfilamiento(x)
  expect_type(resultado, "integer")
  expect_equal(as.integer(resultado), c(1L, 2L, NA_integer_, NA_integer_, 5L, NA_integer_,25, NA_integer_))
  expect_true(!is.null(attr(resultado, "count_nas")))
})

test_that("perfilamiento.logico transforma correctamente", {
  x <- c("TRUE", "FALSE", NA, "T", "F", "otro")
  class(x) <- "logico" # Asignar clase para simular datos de tipo logico
  resultado <- perfilamiento(x)
  expect_type(resultado, "logical")
  expect_equal(as.logical(resultado), c(TRUE, FALSE, NA, TRUE, FALSE, NA))
  expect_true(!is.null(attr(resultado, "count_nas")))
})

test_that("perfilamiento.cadena transforma correctamente", {
  x <- c("a", NA, "b", 1, TRUE)
  class(x) <- "cadena" # Asignar clase para simular datos de tipo cadena
  resultado <- perfilamiento(x)
  expect_type(resultado, "character")
  expect_equal(as.character(resultado), c("a", NA, "b", "1", "TRUE"))
  expect_true(!is.null(attr(resultado, "count_nas")))
})


test_that("perfilamiento.categorica funciona con diccionario", {
  x <- c("A", "B", "A", "C", NA)
  diccionario <- c("A" = "Alfa", "B" = "Beta", "C" = "Gamma")
  attr(x, "diccionario") <- diccionario

  class(x) <- "categorica"

  resultado <- perfilamiento(x)

  expect_equal(as.character(resultado), c("Alfa", "Beta", "Alfa", "Gamma", "Sin categoría"))
  expect_type(resultado, "character")
})

test_that("perfilamiento.categorica maneja valores no encontrados", {
  x <- c("A", "D", "B")
  diccionario <- c("A" = "Alfa", "B" = "Beta")
  attr(x, "diccionario") <- diccionario

  class(x) <- "categorica"

  resultado <- perfilamiento(x)

  expect_equal(as.character(resultado), c("Alfa", "Sin categoría", "Beta"))
})

library(testthat)
library(DBI)

con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

crear_esquema_tablas_in_test <- function(con) {

  # Tabla tablas_in (incluye fecha_datos porque la función hace EXTRACT(YEAR FROM ti.fecha_datos))
  DBI::dbExecute(con, "
    CREATE TABLE tablas_in (
      id_insumo   INTEGER PRIMARY KEY,
      nombre      TEXT NOT NULL,
      fecha_datos DATE
    )
  ")

  # SQLite no maneja schemas como Postgres; creamos una vista con el nombre esperado.
  DBI::dbExecute(con, "
    CREATE VIEW 'public.tablas_in' AS
    SELECT * FROM tablas_in
  ")
}

insertar_datos_tablas_in_test <- function(con) {

  DBI::dbExecute(con, "
    INSERT INTO tablas_in (id_insumo, nombre, fecha_datos)
    VALUES
      (1, 'encuesta_hogares',  '2021-03-15'),
      (2, 'encuesta_personas', '2022-07-22'),
      (3, 'censo_poblacion',   '2023-11-05')
  ")
}

crear_esquema_tablas_in_test(con2)
insertar_datos_tablas_in_test(con2)


testthat::test_that("f_valida_anio: tibble SIN anio  agrega anio desde BD y process_anio = no por que falta insumo" , {

  # No necesitamos esquema para este caso, porque la función no debería consultar nada

  tabla_in <- tibble::tibble(
    id_insumo = c(1L, 2L),
    categoria = c("A", "B")
  )

  out <- f_valida_anio(
    tabla = tabla_in,
    conexion = con2,
    insumo = 999
  )

  testthat::expect_identical(attr(out, "process_anio"), "no")
})

testthat::test_that("f_valida_anio: tibble SIN anio agrega anio desde BD y process_anio = yes por que si hay insumo" , {

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")


  # No necesitamos esquema para este caso, porque la función no debería consultar nada

  tabla_in <- tibble::tibble(
    id_insumo = c(1L, 2L),
    categoria = c("A", "B")
  )

  out <- f_valida_anio(
    tabla = tabla_in,
    conexion = con2,
    insumo = 1L
  )

  testthat::expect_identical(attr(out, "process_anio"), "yes")
})

testthat::test_that("f_valida_anio: tibble SIN anio  agrega anio desde BD y process_anio = no por que falta insumo" , {

  # No necesitamos esquema para este caso, porque la función no debería consultar nada

  tabla_in <- tibble::tibble(
    id_insumo = c(1L, 2L),
    categoria = c("A", "B")
  )

  out <- f_valida_anio(
    tabla = tabla_in,
    conexion = con2,
    insumo = 999
  )

  testthat::expect_identical(attr(out, "process_anio"), "no")
})

testthat::test_that("f_valida_anio: tibble CON anio mantiene anio y process_anio = no" , {

  # No necesitamos esquema para este caso, porque la función no debería consultar nada

  tabla_in <- tibble::tibble(
    id_insumo = c(1L, 2L),
    categoria = c("A", "B"),
    anio = c(2020L, 2021L)
  )

  out <- f_valida_anio(
    tabla = tabla_in,
    conexion = con2,
    insumo = 1L
  )

  testthat::expect_identical(attr(out, "process_anio"), "no")
})

DBI::dbDisconnect(con2)


test_that("perfilamiento.ruc identifica rucs con 13 digitos", {
  x <- c("1723293849001", "1723293849", NA, "1717171717001")

  class(x) <- "ruc" # Asignar clase para simular datos de tipo fecha_hora
  resultado <- perfilamiento(x)
  expect_type(resultado, "character")
  expect_equal(as.character(resultado), c("1723293849001", NA, NA, "1717171717001"))

})
