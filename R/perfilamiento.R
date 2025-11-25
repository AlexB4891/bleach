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
#' @method missing_evaluation character
#' @examples
#' \dontrun{
#' missing_evaluation.character(c("", " ", "NULL", "data"))
#'  }
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @export
missing_evaluation.character_raw <- function(x){


  tibble::tibble(
    original =  dplyr::case_when(
      is.na(x) ~ NA_character_,
      stringr::str_detect(x,"^[:blank:]*$") ~ NA_character_,
      TRUE ~ as.character(x)
    ),
    modificada = dplyr::case_when(
      is.na(x)  ~ "texto_na",
      stringr::str_detect(x,"^[:blank:]*$") ~ "texto_vacio",
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
#' @method missing_evaluation numeric_raw
#' @examples
#' \dontrun{
#' missing_evaluation.numeric(c(1, 2, "NULL", 4))
#' }
#' @import tibble tibble
#' @importFrom dplyr case_when
#' @export
missing_evaluation.numeric_raw <- function(x){


  tibble::tibble(
    original =  dplyr::case_when(
      is.na(x) ~ NA_character_,
      stringr::str_detect(x,"^[:blank:]*$") ~ NA_character_,
      stringr::str_detect(x,"[[:digit:]\\.,]", negate = T) ~ NA_character_,
      TRUE ~ as.character(x)
    ),
    modificada = dplyr::case_when(
      is.na(x) ~ "numero_na",
      stringr::str_detect(x,"^[:blank:]*$") ~ "numero_vacio",
      stringr::str_detect(x,"[[:digit:]\\.,]", negate = T) ~ "no_numero",
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
#' \dontrun{
#' library(purrr)
#' df <- data.frame(a = c("1", "2"), b = c("3.5", "4.5"))
#' clases <- c("integer", "numeric")
#' asignar_clase(df, clases)
#' }
#' @export
asignar_clase <- function(data, clases){
  purrr::pmap_dfc(.l = list(
    data,
    clases
  ),
  .f = function(vector, clase){

    vector <- structure(vector, class = clase)

    return(vector)
  }
  )
}




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
#' @importFrom magrittr %>%
#'
#'
#' @examples
#' \dontrun{
#' missing_treatment(mi_data)
#' missing_treatment(mi_data, clean = TRUE)
#' missing_treatment(mi_data, reporte = TRUE)
#' }
#'
#' @export
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


#' Asignar los atributos para una variable de acuerdo a una lista
#'
#' Ocupando la matriz de perfilamiento se asignan los atributos a los vectores que componen un data frame
#'
#' @param x Un objeto al que se le asignarán atributos.
#' @param atributos Lista nombrada con los atributos a asignar al objeto \code{x}. Cada elemento de la lista debe tener un nombre que será el nombre del atributo.
#'
#' @returns El objeto \code{x} con los atributos asignados.
#' @export
#'
asignar_atributos <- function(x, atributos) {
  for (nombre in names(atributos)) {
    attr(x, nombre) <- atributos[[nombre]]
  }
  return(x)
}


#' Perfilamiento genérico
#'
#' Esta función genérica define el método \code{perfilamiento} para su uso con diferentes clases de objetos.
#'
#' @param x Un objeto sobre el que se aplicará el método de perfilamiento.
#' @return El resultado dependerá del método específico implementado para la clase de \code{x}.
#' @export
#'
perfilamiento <- function(x) {

  UseMethod("perfilamiento")

}


#' Perfilamiento de fechas
#' Esta función evalúa y transforma un objeto de tipo fecha, aplicando un formato específico definido en los atributos del objeto.
#'
#' @param x Un objeto de tipo fecha que se desea transformar.
#' @return Un objeto de tipo fecha transformado según el tipo especificado en la clase.
#' @method perfilamiento fecha
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @export
perfilamiento.fecha <- function(x) {

  arg <- attr(x, "argumentos")

  # Expresión regular para extraer el formato después de "format = "
  formato <- stringr::str_match(arg, 'formato\\s*=\\s*"([^"]+)"')[,2]


  y <- dplyr::case_when(
    is.na(x) ~ NA_Date_,
    TRUE ~ as.Date(as.character(x), format = formato)
  )

  attr(y, "count_nas") <- tibble(original = x, transformada = y) %>%
    dplyr::mutate(dplyr::across(everything(), is.na)) %>%
    dplyr::count(dplyr::across(everything()))

  return(y)

}

#' Perfilamiento de objetos de tipo fecha y hora
#'
#' Esta función evalúa y transforma un objeto de tipo fecha y hora, aplicando un formato específico definido en los atributos del objeto.
#'
#' @param x Un objeto de tipo fecha y hora que se desea transformar.
#' @return Un objeto de tipo fecha y hora transformado según el tipo especificado en la clase.
#' @method perfilamiento fecha_hora
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @export
#'

perfilamiento.fecha_hora <- function(x) {

  arg <- attr(x, "argumentos")

  # Expresión regular para extraer el formato después de "format = "
  formato <- stringr::str_match(arg, 'formato\\s*=\\s*"([^"]+)"')[,2]

  y <- dplyr::case_when(
    is.na(x) ~ NA_POSIXct_,
    TRUE ~ as.POSIXct(as.character(x), format = formato, tz = "UTC")
  )

  attr(y, "count_nas") <- tibble(original = x, transformada = y) %>%
    dplyr::mutate(dplyr::across(everything(), is.na)) %>%
    dplyr::count(dplyr::across(everything()))

  return(y)

}

#' Perfilamiento de objetos de tipo numérico
#' Esta función evalúa y transforma un objeto de tipo numérico.
#' @param x Un objeto de tipo numérico que se desea transformar.
#' @return Un objeto de tipo numérico transformado según el tipo especificado en la clase.
#' @method perfilamiento decimal
#' @import dplyr
#' @import tibble
#' @export

perfilamiento.decimal <- function(x) {

  arg <- attr(x, "argumentos")

  na_values <- attr(x, "valores_na")

  sep_dec <- stringr::str_match(arg, 'dec\\s*=\\s*\\"(.*?)\\"')[,2]


  regex <- stringr::str_c("[^0-9", sep_dec, "]+")

  x <- stringr::str_remove_all(x, regex)

  if(sep_dec == "."){
    sep_dec <- "\\."
  }

  x <- stringr::str_replace(x,
                            pattern = sep_dec,
                            replacement = "\\.")

  y <- dplyr::case_when(
    is.na(x) | x == na_values  ~ NA_real_,
    TRUE ~ as.numeric(x)
  )

  attr(y, "count_nas") <- tibble(original = x, transformada = y) %>%
    dplyr::mutate(dplyr::across(everything(), is.na)) %>%
    dplyr::count(dplyr::across(everything()))

  return(y)

}

#' Perfilamiento de objetos de tipo entero
#'
#' Esta función evalúa y transforma un objeto de tipo entero.
#'
#' @param x Un objeto de tipo entero que se desea transformar.
#' @return Un objeto de tipo entero transformado según el tipo especificado en la clase.
#' @method perfilamiento entero
#' @import dplyr
#' @import tibble
#' @export
#'

perfilamiento.entero <- function(x) {


  na_values <- attr(x, "valores_na")

  y <- dplyr::case_when(
    is.na(x) | x == na_values ~ NA_integer_,
    TRUE ~ as.integer(stringr::str_remove_all(x,"[^0-9]"))
  )

  attr(y, "count_nas") <- tibble(original = x, transformada = y) %>%
    dplyr::mutate(dplyr::across(everything(), is.na)) %>%
    dplyr::count(dplyr::across(everything()))

  return(y)

}

#' Perfilamiento de objetos de tipo lógico
#' Esta función evalúa y transforma un objeto de tipo lógico.
#' @param x Un objeto de tipo lógico que se desea transformar.
#' @return Un objeto de tipo lógico transformado según el tipo especificado en la clase.
#' @method perfilamiento logico
#' @import dplyr
#' @import tibble
#' @export
#'

perfilamiento.logico <- function(x) {

  y <- dplyr::case_when(
    is.na(x) ~ NA,
    TRUE ~ as.logical(x)
  )

  attr(y, "count_nas") <- tibble(original = x, transformada = y) %>%
    dplyr::mutate(dplyr::across(everything(), is.na)) %>%
    dplyr::count(dplyr::across(everything()))

  return(y)

}

#' Perfilamiento de objetos tipo character
#'
#' Esta función evalúa y transforma un objeto de tipo character.
#' @param x Un objeto de tipo character que se desea transformar.
#' @return Un objeto de tipo character transformado según el tipo especificado en la clase.
#' @method perfilamiento cadena
#' @import dplyr
#' @import tibble
#' @export
#'

perfilamiento.cadena <- function(x) {

  y <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    TRUE ~ as.character(x)
  )

  attr(y, "count_nas") <- tibble(original = x, transformada = y) %>%
    dplyr::mutate(dplyr::across(everything(), is.na)) %>%
    dplyr::count(dplyr::across(everything()))

  return(y)

}


#' Perfilamiento de variable categórica
#'
#' Esta función toma un vector categórico y devuelve su representación según un diccionario asociado como atributo.
#'
#' @param x Un vector categórico con un atributo \code{diccionario} que mapea los valores de \code{x} a sus descripciones.
#' @method perfilamiento categorica
#' @return Un vector con las descripciones correspondientes a los valores de \code{x} según el diccionario.
#'
#'
#' @export
perfilamiento.categorica <- function(x){

  diccionario <- attr(x, "diccionario")

  y <- diccionario[x]

  if_else(is.na(y), "Sin categoría", as.character(y))
}




#' Homologación de categorías de una variable categórica
#'
#' Esta función obtiene un vector de homologación entre las categorías de entrada y salida
#' para una variable categórica específica de una tabla, utilizando una conexión a base de datos.
#'
#' @param tbl_nombre Character. Nombre de la tabla de entrada.
#' @param var Character. Nombre de la variable categórica a homologar.
#' @param etiqueta Logical. Si es TRUE, utiliza la descripción de la categoría; si es FALSE, utiliza el código de salida. Por defecto es FALSE.
#' @param conexion DBIConnection. Conexión activa a la base de datos (por ejemplo, generada con \code{RPostgres::dbConnect}).
#'
#' @return Named character vector. Vector de homologación donde los nombres corresponden a las categorías de entrada y los valores a las categorías homologadas.
#'
#' @importFrom dplyr if_else pull
#' @importFrom glue glue_sql
#' @importFrom purrr set_names
#' @importFrom RPostgres dbGetQuery
#'
#' @export
f_cat_homolog <- function(tbl_nombre,
                        var,
                        etiqueta=FALSE,
                        conexion){

  var_homologacion <- dplyr::if_else(condition = isTRUE(etiqueta),
                                     true = "categoria_desc",
                                     false = "categoria_out")

  qry_cat_hom <-
    glue::glue_sql("
WITH variable AS (
  SELECT id_var FROM variables
  WHERE id_insumo=(SELECT id_insumo FROM tablas_in WHERE nombre={tbl_nombre}) AND
  variable_in={var} AND
  tipo='categórica')
SELECT cin.categoria_in, cout.{`var_homologacion`}
FROM categorias_out cout, categorias_in cin
WHERE cin.id_var=(SELECT id_var FROM variable) AND
cin.id_cat_out=cout.id_cat_out
         ",.con=conexion)

  tbl_homol <- RPostgres::dbGetQuery(conexion,qry_cat_hom)

  vector_homologacion <- purrr::set_names(x = dplyr::pull(tbl_homol,var_homologacion),
                                          nm = dplyr::pull(tbl_homol,"categoria_in"))
  vector_homologacion
}

