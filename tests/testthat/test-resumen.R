test_that("sumarizado agrega sumas por grupo sin pesos", {
  df <- tibble::tibble(
    g1 = c("A","A","B"),
    g2 = c(1, 1, 2),
    x  = c(10, NA, 3),
    y  = c(1,  2,  3)
  )

  out <- sumarizado(df, grupo = c("g1","g2"), variables = c("x","y"))

  expect_s3_class(out, "tbl_df")
  expect_false(dplyr::is_grouped_df(out))

  # A-1: x = 10 + NA => 10 ; y = 1 + 2 => 3
  a1 <- out |> dplyr::filter(g1 == "A", g2 == 1)
  expect_equal(a1$x, 10)
  expect_equal(a1$y, 3)

  # B-2: x = 3 ; y = 3
  b2 <- out |> dplyr::filter(g1 == "B", g2 == 2)
  expect_equal(b2$x, 3)
  expect_equal(b2$y, 3)
})

test_that("sumarizado agrega sumas ponderadas por grupo cuando peso no es NULL", {
  df <- tibble::tibble(
    g1 = c("A","A","B"),
    g2 = c(1, 1, 2),
    x  = c(10, NA, 3),
    y  = c(1,  2,  3),
    w  = c(2,  1,  5)
  )

  out <- sumarizado(df, grupo = c("g1","g2"), variables = c("x","y"), peso = "w")

  # A-1: x*w = 10*2 + NA*1 => 20 ; y*w = 1*2 + 2*1 => 4
  a1 <- out |> dplyr::filter(g1 == "A", g2 == 1)
  expect_equal(a1$x, 20)
  expect_equal(a1$y, 4)

  # B-2: x*w = 3*5 => 15 ; y*w = 3*5 => 15
  b2 <- out |> dplyr::filter(g1 == "B", g2 == 2)
  expect_equal(b2$x, 15)
  expect_equal(b2$y, 15)
})

test_that("sumarizado permite grupo vacío (agrega todo)", {
  df <- tibble::tibble(
    x = c(1,2,NA),
    y = c(10, 0, 5)
  )

  out <- sumarizado(df, grupo = character(0), variables = c("x","y"))

  expect_equal(nrow(out), 1)
  expect_equal(out$x, 3)
  expect_equal(out$y, 15)
})

test_that("cubo incluye el nivel más granular y coincide con sumarizado", {
  df <- tibble::tibble(
    prov = c("A","A","B","B"),
    cant = c("X","Y","X","Y"),
    sexo = c("M","F","M","F"),
    valor = c(1,2,3,4)
  )

  grupo <- c("prov","cant","sexo")
  out_cubo <- cubo(df, grupo = grupo, agregacion = c("valor"))
  out_full <- sumarizado(df, grupo = grupo, variables = c("valor"))

  # El cubo debe contener filas donde están presentes prov,cant,sexo (nivel granular)
  out_cubo_full <- out_cubo |>
    dplyr::filter(!is.na(prov), !is.na(cant), !is.na(sexo)) |>
    dplyr::arrange(prov, cant, sexo)

  out_full <- out_full |>
    dplyr::arrange(prov, cant, sexo)

  # Comparamos "valor" en nivel granular
  expect_equal(out_cubo_full$valor, out_full$valor)
})

test_that("cubo ponderado coincide en el primer nivel con sumarizado ponderado", {
  df <- tibble::tibble(
    prov = c("A","A","B","B"),
    cant = c("X","Y","X","Y"),
    sexo = c("M","F","M","F"),
    valor = c(1,2,3,4),
    w = c(2,1,2,1)
  )

  grupo <- c("prov","cant","sexo")

  out_cubo <- cubo(df, grupo = grupo, agregacion = c("valor"), peso = "w")
  out_full_w <- sumarizado(df, grupo = grupo, variables = c("valor"), peso = "w")

  out_cubo_full <- out_cubo |>
    dplyr::filter(!is.na(prov), !is.na(cant), !is.na(sexo)) |>
    dplyr::arrange(prov, cant, sexo)

  out_full_w <- out_full_w |>
    dplyr::arrange(prov, cant, sexo)

  expect_equal(out_cubo_full$valor, out_full_w$valor)
})

