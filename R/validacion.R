

#' Valida y genera la variable año a partir de metadatos del insumo
#'
#' Esta función verifica si la tabla de entrada contiene una columna llamada
#' \code{anio}. En caso de no existir, consulta la base de datos para extraer
#' el año desde la fecha asociada al insumo y lo integra a la tabla.
#' Además, registra mediante un atributo si el procesamiento del año fue realizado.
#'
#' @param tabla data.frame o tibble que contiene los datos a procesar.
#' @param conexion Objeto de conexión a base de datos compatible con \code{DBI}.
#' @param insumo Identificador del insumo (\code{id_insumo}) utilizado para filtrar la consulta.
#'
#' @return Un objeto de tipo data.frame o tibble con la columna \code{anio} añadida
#'   si corresponde. Se incluye el atributo \code{process_anio} con valores
#'   \code{"yes"} o \code{"no"} indicando si se realizó el proceso.
#'
#' @details
#' Si la tabla de entrada ya contiene una variable llamada \code{anio},
#' no se realiza ninguna consulta ni modificación estructural.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr left_join
#' @importFrom stringr str_detect
#' @importFrom glue glue_sql
#' @importFrom lubridate year
#'
#' @export

f_valida_anio <- function(tabla,
                          conexion,
                          insumo){

  var_group <- names(tabla)
  # 8. Generar información de años (integración de lógica generar_anio)
  anios_data <- if(!any(stringr::str_detect(var_group, "^anio$"))) {
    # Caso 1: No existe columna anio en las variables categóricas
    query <- glue::glue_sql("
    SELECT DISTINCT
           id_insumo,
           fecha_datos
    FROM tablas_in
    WHERE id_insumo = {insumo}
  ", .con = conexion)

    aux <- DBI::dbGetQuery(conexion, query)

    aux$anio <-  as.integer(lubridate::year(aux$fecha_datos))

    aux
  } else {
    data.frame()
  }

  if(nrow(anios_data) != 0){

    tabla <- tabla %>%
      dplyr::left_join(anios_data, by = "id_insumo")

    attr(tabla, "process_anio") <- "yes"
  } else {
    attr(tabla, "process_anio") <- "no"
  }

  return(tabla)

}

#' Valida la presencia y consistencia de niveles DPA en una tabla
#'
#' Esta función verifica si una tabla contiene las variables de niveles
#' administrativos (DPA) requeridas según un insumo específico. Se asegura de que
#' la presencia de niveles inferiores (canton, parroquia) esté acompañada por
#' los niveles superiores correspondientes (provincia, canton).
#' Además, proporciona mensajes informativos sobre la presencia o ausencia
#' de estos niveles en la tabla y en el insumo.
#'
#' @param tabla Un `data.frame` o `tibble` que contiene las columnas a validar.
#' @param conexion Objeto de conexión a base de datos compatible con `DBI`.
#' @param insumo Identificador del insumo (`id_insumo`) utilizado para filtrar la consulta.
#' @return Invisiblemente, `NULL`. La función imprime mensajes informativos
#'  sobre la validación de los niveles DPA.
#'  @importFrom DBI dbGetQuery
#'  @importFrom glue glue
#'  @export
#'
valida_dpa <- function(tabla,
                       conexion,
                       insumo){

  # Como minimo debe hacer provincia en las variables asociadas al insumo
  # Luego si hay canton debe haber canton y provincia, si solo hay canton se debe generar
  # provincia con los dos primeros digitos de canton homologado
  # primer verificamos las variables en la tabla variables en la columna
  # variable_out

  var_group <- names(tabla)

  query <- glue::glue("select variables_out from variables where id_insumo = {insumo}")

  vars_insumo <- DBI::dbGetQuery(conexion, query)$variables_out

  expected <- c("provincia","canton","parroquia")

  vars_present_tabla <- intersect(expected, var_group)

  vars_present_insumo <- intersect(expected, vars_insumo)

 #si vars present tabla es vacio devolver el mensaje: ningun dpa presente en tabla sumado al mensjae si esta o no en
  # vars insumo

  tiene_dpa <- TRUE

  if(length(vars_present_tabla) == 0){
    msg <- "Ningún nivel de DPA presente en la tabla."
    if(length(vars_present_insumo) == 0){
      msg <- paste0(msg, " Tampoco se encuentran niveles de DPA asociados al insumo.")
    } else {
      msg <- paste0(msg, " Sin embargo, se encontraron niveles de DPA asociados al insumo: ",
                    paste(vars_present_insumo, collapse = ", "), ".")
    }

    tiene_dpa <- FALSE
  }

  completo_dpa <- TRUE
  # luego verifica y provoca un mensaje de si faltan los niveles inferiores, en canton y parroquia
  if(tiene_dpa){
    if("canton" %in% vars_present_tabla & !("provincia" %in% vars_present_tabla)){
     msg_2 <- "La variable 'canton' está presente en la tabla, pero falta la variable 'provincia'. Se requiere 'provincia' para agregar 'canton'."
     completo_dpa <- FALSE
    }

    if("parroquia" %in% vars_present_tabla){
      if(!("canton" %in% vars_present_tabla) | !("provincia" %in% vars_present_tabla)){
        msg_2 <- "La variable 'parroquia' está presente en la tabla, pero faltan las variables 'canton' y/o 'provincia'. Se requieren ambas para agregar 'parroquia'."
        completo_dpa <- FALSE
      }
    }
  }

  if(!tiene_dpa){
    print(msg)
  }

  if(!completo_dpa){
    print(msg_2)
  }

  flags_dpa <- list(
    tiene_dpa = tiene_dpa,
    completo_dpa = completo_dpa
  )

  return(flags_dpa)

}

