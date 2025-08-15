
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
  x <- c("1.23", "4.56", "7.89", NA, "1000.01", "999999")
  class(x) <- "decimal" # Asignar clase para simular datos de tipo decimal

  attr(x, "argumentos") <- 'dec="\\."' # separador decimal es la coma

  attr(x, "valores_na") <- "999999" # separador decimal es la coma

  resultado <- bleach::perfilamiento(x)

  expect_equal(as.numeric(resultado), c(1.23, 4.56, 7.89, NA_real_, 1000.01, NA_real_), tolerance = 1e-6)
  expect_true(!is.null(attr(resultado, "count_nas")))
})


test_that("perfilamiento.fecha respeta el atributo formato", {
  x <- c("2023-01-01", "2023-02-15", NA, "2023-12-31")
  attr(x, "argumentos") <- 'formato = "%Y-%m-%d"'
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
  x <- c("1", "2", NA, "abc", "5","999999")
  class(x) <- "entero" # Asignar clase para simular datos de tipo entero
  attr(x, "valores_na") <- "999999" # separador decimal es la coma
  resultado <- perfilamiento(x)
  expect_type(resultado, "integer")
  expect_equal(as.integer(resultado), c(1L, 2L, NA_integer_, NA_integer_, 5L, NA_integer_))
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

  expect_equal(as.character(resultado), c("Alfa", "Beta", "Alfa", "Gamma", NA))
  expect_type(resultado, "character")
})

test_that("perfilamiento.categorica maneja valores no encontrados", {
  x <- c("A", "D", "B")
  diccionario <- c("A" = "Alfa", "B" = "Beta")
  attr(x, "diccionario") <- diccionario

  class(x) <- "categorica"

  resultado <- perfilamiento(x)

  expect_equal(as.character(resultado), c("Alfa", NA, "Beta"))
})

library(testthat)
library(DBI)
library(RSQLite)

# Función auxiliar para crear el esquema completo
crear_esquema_test <- function(con) {

  # Tabla tablas_in
  dbExecute(con, "
    CREATE TABLE tablas_in (
      id_insumo INTEGER PRIMARY KEY,
      nombre TEXT NOT NULL
    )")

  # Tabla variables
  dbExecute(con, "
    CREATE TABLE variables (
      id_var INTEGER PRIMARY KEY,
      id_insumo INTEGER,
      variable_in TEXT NOT NULL,
      tipo TEXT NOT NULL,
      FOREIGN KEY (id_insumo) REFERENCES tablas_in(id_insumo)
    )")

  # Tabla categorias_out (catálogo de categorías de salida)
  dbExecute(con, "
    CREATE TABLE categorias_out (
      id_cat_out INTEGER PRIMARY KEY,
      categoria_out TEXT NOT NULL,
      categoria_desc TEXT NOT NULL
    )")

  # Tabla categorias_in (mapeo de categorías de entrada a salida)
  dbExecute(con, "
    CREATE TABLE categorias_in (
      id_cat_in INTEGER PRIMARY KEY,
      id_var INTEGER,
      categoria_in TEXT NOT NULL,
      id_cat_out INTEGER,
      FOREIGN KEY (id_var) REFERENCES variables(id_var),
      FOREIGN KEY (id_cat_out) REFERENCES categorias_out(id_cat_out)
    )")
}

# Función auxiliar para insertar datos de prueba
insertar_datos_test <- function(con) {

  # Insertar tabla de entrada
  dbExecute(con, "INSERT INTO tablas_in VALUES (1, 'encuesta_hogares')")
  dbExecute(con, "INSERT INTO tablas_in VALUES (2, 'encuesta_personas')")

  # Insertar variables
  dbExecute(con, "INSERT INTO variables VALUES (1, 1, 'estado_civil', 'categórica')")
  dbExecute(con, "INSERT INTO variables VALUES (2, 1, 'nivel_educativo', 'categórica')")
  dbExecute(con, "INSERT INTO variables VALUES (3, 2, 'ocupacion', 'categórica')")

  # Insertar categorías de salida (catálogo estándar)
  dbExecute(con, "INSERT INTO categorias_out VALUES (1, 'S', 'Soltero')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (2, 'C', 'Casado')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (3, 'D', 'Divorciado')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (4, 'V', 'Viudo')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (5, 'PRIM', 'Primaria')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (6, 'SEC', 'Secundaria')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (7, 'UNIV', 'Universitaria')")

  # Insertar mapeo de categorías de entrada a salida
  # Para estado_civil
  dbExecute(con, "INSERT INTO categorias_in VALUES (1, 1, 'Soltero', 1)")
  dbExecute(con, "INSERT INTO categorias_in VALUES (2, 1, 'Casado', 2)")
  dbExecute(con, "INSERT INTO categorias_in VALUES (3, 1, 'Separado', 3)")
  dbExecute(con, "INSERT INTO categorias_in VALUES (4, 1, 'Viudo', 4)")

  # Para nivel_educativo
  dbExecute(con, "INSERT INTO categorias_in VALUES (5, 2, 'Básica', 5)")
  dbExecute(con, "INSERT INTO categorias_in VALUES (6, 2, 'Media', 6)")
  dbExecute(con, "INSERT INTO categorias_in VALUES (7, 2, 'Superior', 7)")
}

# Tests completos
test_that("f_cat_homolog funciona con etiqueta=FALSE (códigos)", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar la función
  resultado <- f_cat_homolog("encuesta_hogares", "estado_civil", etiqueta = FALSE, con)

  expect_equal(resultado, c("Soltero" = "S", "Casado" = "C", "Separado" = "D", "Viudo" = "V"))
  expect_type(resultado, "character")
  expect_equal(length(resultado), 4)
})

test_that("f_cat_homolog funciona con etiqueta=TRUE (descripciones)", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar la función
  resultado <- f_cat_homolog("encuesta_hogares", "estado_civil", etiqueta = TRUE, con)

  expect_equal(resultado, c("Soltero" = "Soltero", "Casado" = "Casado", "Separado" = "Divorciado", "Viudo" = "Viudo"))
  expect_type(resultado, "character")
  expect_equal(length(resultado), 4)
})

test_that("f_cat_homolog funciona con diferente variable", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar con nivel_educativo
  resultado <- f_cat_homolog("encuesta_hogares", "nivel_educativo", etiqueta = FALSE, con)

  expect_equal(resultado, c("Básica" = "PRIM", "Media" = "SEC", "Superior" = "UNIV"))
  expect_type(resultado, "character")
  expect_equal(length(resultado), 3)
})

test_that("f_cat_homolog maneja tabla/variable inexistente", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar con tabla inexistente
  resultado <- f_cat_homolog("tabla_inexistente", "estado_civil", etiqueta = FALSE, con)

  expect_equal(length(resultado), 0)
  expect_type(resultado, "character")
})
