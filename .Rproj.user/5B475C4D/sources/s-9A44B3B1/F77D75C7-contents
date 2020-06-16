#----- Libraries -----

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("googlesheets4")
install.packages("rlang")
install.packages("rematch2")
install.packages("devtools")
install.packages("fs")
library(googlesheets4)
devtools::install_github("r-lib/gargle") # Necessary to read correctly Spanish accents
library(installr)
library(gmodels)
library(tidytext)
library(tm)


#----- Loading headers data ----

# link_afpo2020 <- "https://www.superfinanciera.gov.co/descargas/institucional/pubFile1044614/afpo2020.xls"
# It doesn't work, R didn't read the link

pensionados_2020_03_headers <- readxl::read_xls(path = "afpo2020.xls",
                                        range = "Pensionados !A38:AS40",
                                        n_max = 3,
                                        col_names = FALSE)


View(t(pensionados_2020_03_headers))



#---- Function fills missing data ----

fillHeaders <- function(header_table) {
  for (row in 2:nrow(header_table)) {
    this_row <- header_table[row, ]
    last_row <- header_table[row-1, ]
    new_row <- ifelse(is.na(this_row), last_row, this_row)
    header_table[row, ] <- new_row
  }
  header_table
}

is.na(pensionados_2020_03_headers[1, ])



#---- Function test ----

function_test_fill <- pensionados_2020_03_headers %>% 
  t() %>% # transposes the data and turns it into a matrix
  as_tibble() %>% # turn it back as a tibble
  fillHeaders()


View(function_test_fill)



#---- Creating new table headers ----

new_names <- pensionados_2020_03_headers %>% 
  t() %>% # transposes the data and turns it into a matrix
  as_tibble() %>% # turn it back as a tibble
  fillHeaders() %>% 
  mutate(name = paste(V1, V2, V3, sep = "_")) %>% 
  pull(name)

View(new_names)


#---- Loading data ----

pensionados_2020_03_data <- readxl::read_xls(path = "afpo2020.xls",
                                             range = "Pensionados !A42:AS46",
                                             col_names = new_names)

View(pensionados_2020_03_data)



#---- Transforming data to long format ----

pensionados_2020_03_data_long <- pensionados_2020_03_data %>% 
  # add row IDs if each row doesn't already have uniquely identifying column(s)
  mutate(identificador = row_number()) %>% 
  # pivot longer "lengthens" data, increasing the number of rows and decreasing the number of columns.
  # The inverse transformation is pivot_wider()
  pivot_longer(cols = 2:45, names_to = "var", values_to = "val") %>% 
  # split the variable names into their three component parts
  separate(var, c("tipo_pension", "modo_pension", "sexo"), sep = "_")

View(pensionados_2020_03_data_long)


#---- Resuming data ----


# By modo_pension

pensionados_2020_03_data_long %>% summarise(cant_total = sum(val))

cant_pensionados_rais_modo <- pensionados_2020_03_data_long %>% group_by(modo_pension) %>% 
  summarise(cantidad_pensionados = sum(val))

write_csv2(cant_pensionados_rais_modo, "cant_pensionados_rais_modo.csv")


# By modo_pension and sexo

cant_pensionados_rais_modo_sexo <- pensionados_2020_03_data_long %>% 
  group_by(modo_pension, sexo) %>% 
  summarise(cantidad_pensionados = sum(val))

write_csv2(cant_pensionados_rais_modo_sexo, "cant_pensionados_rais_modo_sexo.csv")

