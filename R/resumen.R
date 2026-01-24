library(magrittr)
library(dplyr)
library(purrr)

#' Resumir (agregar) variables por grupo, con o sin ponderación
#'
#' Calcula sumas por grupo para un conjunto de variables numéricas. Si se especifica
#' `peso`, la suma se calcula como \eqn{\sum x_i w_i}. La función retorna una tabla
#' con una fila por combinación de niveles en `grupo` y una columna por cada variable
#' en `variables` (con el mismo nombre original).
#'
#' @param tabla Un `data.frame`/`tibble` con las columnas de agrupación (`grupo`),
#'   las variables numéricas a agregar (`variables`) y opcionalmente la columna de
#'   pesos (`peso`).
#' @param grupo Character vector con los nombres de las columnas usadas para agrupar.
#'   Debe contener nombres existentes en `tabla`. Puede ser `character(0)` (agrega
#'   toda la tabla a un solo registro).
#' @param variables Character vector con los nombres de las columnas numéricas a
#'   sumar. Deben existir en `tabla`.
#' @param peso `NULL` (default) o `character(1)` con el nombre de la columna de
#'   pesos. Si se especifica, la suma se calcula como `sum(.x * tabla[[peso]], na.rm = TRUE)`.
#'
#' @return Un `tibble` con columnas de grupo y columnas agregadas para `variables`.
#'   La salida está desagrupada (`.groups = "drop"`).
#'
#' @details
#' - Con `peso = NULL`, para cada variable `v` en `variables` se calcula:
#'   `sum(tabla[[v]], na.rm = TRUE)` por grupo.
#' - Con `peso` no nulo, para cada variable `v` se calcula:
#'   `sum(tabla[[v]] * tabla[[peso]], na.rm = TRUE)` por grupo.
#' @importFrom dplyr group_by summarise across all_of
#' @importFrom magrittr %>%
#' @export
#'
sumarizado <- function(tabla,grupo,variables,peso=NULL){


  tabla=tabla %>% dplyr::group_by(dplyr::across(dplyr::all_of(grupo)))



  tabla=
    if (is.null(peso)) {
      tabla %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(variables),
                         #  list(suma =
                         ~sum(.x,na.rm = TRUE)
                         # ,
                         #     media = ~mean(.x,na.rm = TRUE)
        ),
        .groups="drop")
    }else{
      tabla %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(variables),
                         #  list(suma =
                         ~sum(.x*.data[[peso]],na.rm = TRUE)
                         #  ,
                         #       media = ~weighted.mean(x = .x, w = .data[[peso]],na.rm = TRUE))
        ),
        .groups="drop")
    }


  return(tabla)

}


#' Cubo de agregaciones (power set de grupos) para variables numéricas
#'
#' Construye un "cubo" de resúmenes generando agregaciones para todas las
#' combinaciones posibles de variables de agrupación (`grupo`), usando el conjunto
#' potencia. Para cada subconjunto de `grupo`, calcula sumas (o sumas ponderadas si
#' `peso` se especifica) sobre `agregacion`, y devuelve la unión (row-bind) de todos
#' los niveles de agregación en una sola tabla larga.
#'
#' La función optimiza el cálculo reutilizando resultados: en lugar de resumir desde
#' `tabla` para cada subconjunto, resume desde el resultado previo "más granular"
#' cuando es posible.
#'
#' @param tabla Un `data.frame`/`tibble` con las columnas de agrupación (`grupo`),
#'   variables numéricas a agregar (`agregacion`) y opcionalmente `peso`.
#' @param grupo Character vector con los nombres de columnas de agrupación base.
#' @param agregacion Character vector con los nombres de columnas numéricas a agregar.
#' @param peso `NULL` (default) o `character(1)` con el nombre de la columna de pesos.
#'   Si se especifica, solo se aplica en el primer nivel de cálculo (el más granular);
#'   los niveles siguientes se obtienen por re-agregación de sumas ya ponderadas.
#'
#' @return Un `tibble` con las sumas por cada subconjunto del conjunto potencia de `grupo`.
#'   La salida es "larga" en el sentido de que contiene filas de distintos niveles de
#'   agregación; las columnas de grupo presentes dependerán del subconjunto (las que
#'   no aplican no existen/son omitidas en ese bloque). El resultado final se obtiene
#'   con `list_rbind(lista_resumen)`.
#'
#' @section Dependencias:
#' - Requiere `rje::powerSet()` para generar el conjunto potencia de `grupo`.
#' - Requiere `list_rbind()` (por ejemplo, `purrr::list_rbind`) para concatenar resultados.
#' @importFrom purrr map list_c list_rbind
#' @importFrom rje powerSet
#' @importFrom magrittr %>%
#' @examples
#' library(dplyr)
#' df <- tibble::tibble(
#'   prov = c("A","A","B","B"),
#'   cant = c("X","Y","X","Y"),
#'   sexo = c("M","F","M","F"),
#'   valor = c(1,2,3,4),
#'   w = c(1,1,2,2)
#' )
#'
#' # Cubo sin ponderación sobre 'valor'
#' cubo(df, grupo = c("prov","cant","sexo"), agregacion = c("valor"))
#'
#' # Cubo ponderado (primer nivel): sum(valor*w)
#' cubo(df, grupo = c("prov","cant","sexo"), agregacion = c("valor"), peso = "w")
#'
#' @export
cubo <-
  function(tabla,grupo,agregacion,peso=NULL){

    # se obtiene el conjunto potencia para el cubo
    gr_potencia=rje::powerSet(grupo,rev = TRUE)
    plan_largo=purrr::map(gr_potencia,length) %>% purrr::list_c()
    n_plan=length(plan_largo)

    # me dice qué agrupación del conjunto potencia se obtendrá de qué resultado,
    # así recorro muchas menos filas de la parte pesada que es el cálculo.
    plan_ejecucion=vector(mode = "integer", length = n_plan)
    if (n_plan>2) {
      for (x in 2:(n_plan-1)) {
        plan_ejecucion[x] = max(which(plan_largo[1:x]>plan_largo[x]))
      }
    }

    lista_resumen = list()

    for (i in seq(n_plan)) {
      if (plan_ejecucion[i]==0) {
        lista_resumen[[i]]=
          sumarizado(
            tabla,
            gr_potencia[[i]],
            agregacion,
            peso)
      }else{
        lista_resumen[[i]]=
          sumarizado(
            lista_resumen[[plan_ejecucion[i]]],
            gr_potencia[[i]],
            agregacion)
      }
    }

    return(list_rbind(lista_resumen))
  }
