#' Homologación de categorías de una variable categórica
#'
#' Esta función obtiene un vector de homologación entre las categorías de entrada y salida
#' para una variable categórica específica de una tabla, utilizando una conexión a base de datos.
#' El vector resultante permite mapear categorías de entrada a sus correspondientes categorías
#' estandarizadas de salida.
#'
#' @param tbl_nombre Character. Nombre de la tabla de entrada en el esquema de metadatos.
#' @param var Character. Nombre de la variable categórica para la cual se requiere homologación.
#' @param conexion DBIConnection. Conexión activa a la base de datos
#'   (por ejemplo, generada con \code{DBI::dbConnect} o \code{RPostgres::dbConnect}).
#'
#' @return Named character vector. Vector de homologación donde los nombres corresponden
#'   a las descripciones de las categorías de salida y los valores a los códigos
#'   de las categorías de salida.
#'
#' @examples
#' \dontrun{
#' # Obtener vector de homologación
#' homolog <- f_cat_etiquetas("encuesta_hogares", "estado_civil", conexion = mi_con)
#' }
#'
#' @importFrom dplyr pull
#' @importFrom glue glue_sql
#' @importFrom purrr set_names
#' @importFrom DBI dbGetQuery
#'
#' @export
f_cat_etiquetas <- function(tbl_nombre,
                          var,
                          conexion){



  qry_cat_hom <-
    glue::glue_sql("
WITH variable AS (
  SELECT id_var FROM variables
  WHERE id_insumo=(SELECT id_insumo FROM tablas_in WHERE nombre={tbl_nombre}) AND
  variable_in={var} AND
  tipo='categórica')
SELECT cout.categoria_out, cout.categoria_desc
FROM categorias_out cout, categorias_in cin
WHERE cin.id_var=(SELECT id_var FROM variable) AND
cin.id_cat_out=cout.id_cat_out
         ",.con=conexion)

  tbl_homol <- DBI::dbGetQuery(conexion,qry_cat_hom)

  vector_homologacion <- purrr::set_names(x = dplyr::pull(tbl_homol,categoria_out),
                                          nm = dplyr::pull(tbl_homol,categoria_desc))
  vector_homologacion
}
