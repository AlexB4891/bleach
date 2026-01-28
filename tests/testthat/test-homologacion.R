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
  dbExecute(con, "INSERT INTO categorias_out VALUES (1, 1, 'Soltero')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (2, 2, 'Casado')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (3, 3, 'Separado')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (4, 4, 'Viudo')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (5, 1, 'Primaria')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (6, 2, 'Secundaria')")
  dbExecute(con, "INSERT INTO categorias_out VALUES (7, 3, 'Universitaria')")

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

# Tests para f_cat_homolog (alias) - copiados desde test-perfilamiento.R
test_that("f_cat_homolog funciona correctamente", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar la función
  resultado <- f_cat_homolog("encuesta_hogares", "estado_civil", etiqueta = FALSE, con)

  expect_equal(resultado, c("Soltero" = "1", "Casado" = "2", "Separado" = "3", "Viudo" = "4"))
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

  expect_equal(resultado, c("Básica" = "1", "Media" = "2", "Superior" = "3"))
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

# Tests para f_cat_etiquetas (función principal)
test_that("f_cat_etiquetas funciona correctamente", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar la función
  resultado <- f_cat_etiquetas("encuesta_hogares", "estado_civil", con)

  expect_equal(resultado, c("Soltero" = "1", "Casado" = "2", "Separado" = "3", "Viudo" = "4"))
  expect_type(resultado, "character")
  expect_equal(length(resultado), 4)
})

test_that("f_cat_etiquetas funciona con diferente variable", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar con nivel_educativo
  resultado <- f_cat_etiquetas("encuesta_hogares", "nivel_educativo", con)

  expect_equal(resultado, c("Primaria" = "1", "Secundaria" = "2", "Universitaria" = "3"))
  expect_type(resultado, "character")
  expect_equal(length(resultado), 3)
})

test_that("f_cat_etiquetas maneja tabla/variable inexistente", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Crear esquema e insertar datos
  crear_esquema_test(con)
  insertar_datos_test(con)

  # Probar con tabla inexistente
  resultado <- f_cat_etiquetas("tabla_inexistente", "estado_civil", con)

  expect_equal(length(resultado), 0)
  expect_type(resultado, "character")
})
