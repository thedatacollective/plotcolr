#' Mothers and babies in the Queensland Health System
#'
#' Mothers and babies by individual hospital of birth, Queensland, 1995 to 2013 calendar years.
#'
#' @format a data.frame with 1500 rows and 7 variables:
#' \describe{
#'   \item{id}{a numeric id used to link the count of mothers to babies for each location and calendar year}}
#'   \item{year}{the calendar year in which the births took place}
#'   \item{health_service}{a text description of the health service region}
#'   \item{facility_id}{an id for the specific hospital facility}
#'   \item{facitlity_name}{a text name of the hospital}
#'   \item{type}{a descriptor indicating whether the count is of mothers or babies}
#'   \item{count}{a numeric count of mothes or babies.}
#' }
#' @source Queensland Health Data - https://www.data.qld.gov.au/dataset/mothers-and-babies-queensland-calendar-years-1995-to-2013/resource/8079d988-8a1f-4b72-b4bf-eb7e07d6897c
#' @return data.frame
"qld_births"