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
                          conexion = NULL,
                          diccionario = NULL){



  if(!is.null(conexion)){

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

  } else {
    tbl_homol <- diccionario %>%
      filter(variable_out == var)
  }




  vector_homologacion <- purrr::set_names(x = dplyr::pull(tbl_homol,categoria_out),
                                          nm = dplyr::pull(tbl_homol,categoria_desc))
  vector_homologacion
}

#' Aplicar homologación de categorías a un vector
#'
#' Esta función aplica un vector de homologación a un vector de categorías de entrada,
#' mapeando cada categoría a su correspondiente categoría estandarizada de salida.
#' Si una categoría de entrada no tiene una correspondencia en el vector de homologación,
#' se asigna la categoría "Sin categoría".
#'
#' @param x Vector de categorías de entrada (puede ser factor, character, etc.).
#' @param homologacion Named character vector. Vector de homologación donde los nombres
#'  corresponden a las categorías de entrada y los valores a las categorías de salida.
#'
#'  @return Vector con las categorías homologadas.
#'  @export


f_apply_homologacion <- function(x, homologacion){

  homologacion_revert <- names(homologacion)
  names(homologacion_revert) <- homologacion

  resultado <- homologacion_revert[x]
  resultado[is.na(resultado)] <- "Sin categoría"
  unname(resultado)

}



#' Función para asignar nombres finales a las columas de un dataframe
#'
#' Asignar nuevos nombres a las columnas de un data.frame
#' @params df data.frame al cual se le asignarán nuevos nombres de columnas
#' @params vector nombrado, el valor es el nombre vigente en la tabla, el nombre es la etiqueta que se
#' convertira en el nuevo nombre de la columna
#'
#' @return data.frame con los nuevos nombres de columnas asignados
#' @export
#'
asignar_nombres_columnas <- function(df, nuevos_nombres) {

  nombres_actuales <- names(df)

  nombres_nuevos <- sapply(nombres_actuales, function(nombre) {
    if (nombre %in% nuevos_nombres) {
      names(nuevos_nombres)[which(nuevos_nombres == nombre)]
    } else {
      nombre
    }
  })
  names(df) <- nombres_nuevos
  return(df)
}


