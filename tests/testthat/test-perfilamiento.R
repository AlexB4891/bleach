
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
