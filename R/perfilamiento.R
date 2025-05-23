#' Evaluate Missing Values
#'
#' Generic function to evaluate missing values in an object.
#'
#' @param x An object to be evaluated for missing values.
#' @return The result of the missing value evaluation. The specific return value depends on the method implemented for the class of \code{x}.
#' @export
missing_evaluation <- function(x){

  UseMethod("missing_evaluation")
}

#' Evaluate and categorize missing values in a character vector
#'
#' This function takes a character vector and evaluates whether the elements correspond to common missing values,
#' such as empty strings, blank spaces, or the text "NULL". It returns a tibble with two columns:
#' \describe{
#'   \item{original}{The original value, replacing missing values with \code{NA_character_}.}
#'   \item{modificada}{A descriptive label for the type of missing value detected, or \code{NA_character_} if not applicable.}
#' }
#'
#' @param x A character vector to evaluate.
#'
#' @return A tibble with two columns: \code{original} and \code{modificada}.
#'
#' @examples
#' missing_evaluation.character(c("", " ", "NULL", "data"))
#'
#' @export
missing_evaluation.character <- function(x){

  tibble::tibble(
    original =  dplyr::case_when(
      x == "" ~ NA_character_,
      x == " " ~ NA_character_,
      x == "NULL" ~ NA_character_,
      TRUE ~ x
    ),
    modificada = dplyr::case_when(
      x == "" ~ "texto_vacio",
      x == " " ~ "texto_espacio_vacio",
      x == "NULL" ~ "texto_nulo",
      TRUE ~ NA_character_
    )
  )

}


#' Evaluate and Label Missing Numeric Values
#'
#' This function checks a numeric vector for values equal to the string "NULL" and replaces them with `NA_real_` in the `original` column and with the label `"numero_nulo"` in the `modificada` column. All other values are returned as-is in the `original` column and as `NA_character_` in the `modificada` column.
#'
#' @param x A numeric vector to be evaluated for missing values represented as the string "NULL".
#'
#' @return A tibble with two columns:
#'   \describe{
#'     \item{original}{The original numeric values, with "NULL" replaced by `NA_real_`.}
#'     \item{modificada}{A character column labeling "NULL" values as `"numero_nulo"`, and all other values as `NA_character_`.}
#'   }
#'
#' @examples
#' missing_evaluation.numeric(c(1, 2, "NULL", 4))
#'
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
missing_evaluation.numeric <- function(x){

  tibble::tibble(
    original =  dplyr::case_when(
      x == "NULL" ~ NA_real_,
      TRUE ~ x
    ),
    modificada = dplyr::case_when(
      x == "NULL" ~ "numero_nulo",
      TRUE ~ NA_character_
    )
  )

}




#' Asigna clases a columnas de un data frame
#'
#' Esta función toma un data frame y un vector de clases, y asigna la clase correspondiente a cada columna del data frame.
#'
#' @param data Un data frame cuyas columnas se desean reclasificar.
#' @param clases Un vector de caracteres indicando la clase a asignar a cada columna de \code{data}.
#'
#' @return Un data frame con las columnas reclasificadas según las clases especificadas.
#'
#' @importFrom purrr pmap_dfc
#'
#' @examples
#' library(purrr)
#' df <- data.frame(a = c("1", "2"), b = c("3.5", "4.5"))
#' clases <- c("integer", "numeric")
#' asignar_clase(df, clases)
#'
#' @export
asignar_clase <- function(data, clases){
  purrr::pmap_dfc(.l = list(
    data,
    clases
  ),
  .f = function(vector, clase){
    class(vector) <- clase
    return(vector)
  }
  )
}




iris_2 <- asignar_clase(iris,
                        c("numeric", "numeric", "numeric", "numeric", "character")
)

#' missing_treatment
#'
#' Esta función evalúa y trata los valores faltantes en un data.frame, permitiendo limpiar o generar reportes según los argumentos proporcionados.
#'
#' @param data Un data.frame que contiene los datos a evaluar.
#' @param clean Lógico. Si es TRUE, retorna solo las columnas originales sin sufijo "_original". Por defecto es FALSE.
#' @param reporte Lógico. Si es TRUE, retorna solo las columnas modificadas sin sufijo "_modificada". Por defecto es FALSE.
#'
#' @return Un data.frame con el tratamiento de valores faltantes, dependiendo de los argumentos \code{clean} y \code{reporte}.
#'
#' @details
#' Utiliza \code{purrr::imap_dfc}, \code{dplyr::rename_with}, \code{dplyr::select}, \code{dplyr::mutate}, \code{stringr::str_c}, y \code{stringr::str_remove}.
#' La función interna \code{missing_evaluation} debe estar definida en el entorno.
#'
#' @importFrom purrr imap_dfc
#' @importFrom dplyr rename_with select
#' @importFrom stringr str_c str_remove
#'
#' @examples
#' \dontrun{
#' missing_treatment(mi_data)
#' missing_treatment(mi_data, clean = TRUE)
#' missing_treatment(mi_data, reporte = TRUE)
#' }
missing_treatment <- function(data, clean = FALSE, reporte = FALSE){

  tabla <- data %>%
    purrr::imap_dfc(~{

      nombre <- .y

      missing_evaluation(.x) %>%
        dplyr::rename_with(.cols = dplyr::everything(), ~ stringr::str_c(nombre, .x, sep = "_"))

    })

  if(isTRUE(clean)){

    tabla <- tabla %>%
      dplyr::select(dplyr::matches("original")) %>%
      dplyr::rename_with(.cols = dplyr::everything(), ~ stringr::str_remove(.x, "_original"))

  }

  if(isTRUE(reporte)){

    tabla <- tabla %>%
      dplyr::select(dplyr::matches("modificada")) %>%
      dplyr::rename_with(.cols = dplyr::everything(), ~ stringr::str_remove(.x, "_modificada"))

  }

  return(tabla)

}
