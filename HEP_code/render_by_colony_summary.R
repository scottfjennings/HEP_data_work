


library(tidyverse)
library(here)



#' render_by_colony_summary
#' 
#' function to output colony summary
#'
#' @param file 
#' @param zyear 
#' @param zcode 
#' @param zcol.name 
#'
#' @return
#' @export
#'
#' @examples
render_by_colony_summary <- function(file = here("code/observer_observer_summary.Rmd"), zcode, zcol.name) {
  zcode.sub = gsub("\\.", "_", zcode)
  rmarkdown::render(file, params = list(
    zcode = zcode
  ), envir = new.env(),
  output_file = here(paste0("figures_output/colony_summaries/", zcode.sub, "_", zcol.name, ".html", sep = ""))
  )
}


colonies <- readRDS(here("HEP_data/HEP_site_names_nums_utm")) %>% 
  distinct(code, site.name)

pmap(.l = list(file = here("HEP_rmds/by_colony_summary.Rmd"), zcode = 22, zcol.name = "HiddenCove"), .f = render_by_colony_summary)

render_list <- pmap(.l = list(file = here("HEP_rmds/by_colony_summary.Rmd"), zcode = colonies$code, zcol.name = colonies$site.name), .f = safely(render_by_colony_summary))
