{ 
  # Check if the packages that we need are installed
  want = c("tidyverse", "data.table", "readr", "stringr", "zoo", "plyr", "dplyr", "COVID19", "lubridate", "tidyr")
  have = want %in% rownames(installed.packages())
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  # Remove the objects we created
  rm(have, want, junk)
} # 0.0 Import packages'

{
  # Import lat and lng references
  
  # Declare the download link 
  url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR80hqppkNzASq8iaUE9hl9NRQmZitrXfrjm1VG5gBVHUJ4ScJ_c8Zw8531iefVx5J9LpJ872sDu6ma/pub?gid=537880276&single=true&output=csv"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the file
  download.file(url,tmp)
  
  # Import database
  cod <- read_csv(tmp, 
                  locale = locale(decimal_mark = ",", grouping_mark = "."))
  
  
  ## Import COVID cities database
  
  # Declare the download link 
  url = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19 =   read_csv(gzfile(tmp),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                        encoding = "UTF-8"))
  
  # Create dcovidcity database deleting the latest data
  dcovid19citya = dcovid19 %>% filter(place_type == "city") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(date > as.Date("2020-03-30")) %>%
    arrange(date, city) %>%
    slice(-which.max(date)) %>%
    arrange(desc(date)) %>%
    mutate(select = paste0(lubridate::day(date), "/", lubridate::month(date), "/", lubridate::year(date)))
  
  # Create dcovidcity database with the latest data
  dcovid19cityb = dcovid19 %>% filter(place_type == "city") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(date > as.Date("2020-03-30") & is_last == TRUE) %>%
    arrange(date, city) %>% 
    mutate(select = "?ltimo")
  
  # bind databases
  dcovid19city = dcovid19cityb %>% bind_rows(dcovid19citya) %>% arrange(desc(date))
  
  
  # Include the state and region name
  dcovid19city = dcovid19city %>% mutate(statename = case_when(state =="DF" ~ "Distrito Federal",
                                                               state =="GO" ~ "Goi?s",
                                                               state =="MT" ~ "Mato Grosso",
                                                               state =="MS" ~ "Mato Grosso do Sul",
                                                               state =="AL" ~ "Alagoas",
                                                               state =="BA" ~ "Bahia",
                                                               state =="CE" ~ "Cear?",
                                                               state =="MA" ~ "Maranh?o",
                                                               state =="PB" ~ "Para?ba",
                                                               state =="PE" ~ "Pernambuco",
                                                               state =="PI" ~ "Piau?",
                                                               state =="RN" ~ "Rio Grande do Norte",
                                                               state =="SE" ~ "Sergipe",
                                                               state =="AC" ~ "Acre",
                                                               state =="AP" ~ "Amap?",
                                                               state =="AM" ~ "Amazonas",
                                                               state =="PA" ~ "Par?",
                                                               state =="RO" ~ "Rond?nia",
                                                               state =="RR" ~ "Roraima",
                                                               state =="TO" ~ "Tocantins",
                                                               state =="ES" ~ "Esp?rito Santo",
                                                               state =="MG" ~ "Minas Gerais",
                                                               state =="RJ" ~ "Rio de Janeiro",
                                                               state =="SP" ~ "S?o Paulo",
                                                               state =="PR" ~ ".Paran?",
                                                               state =="RS" ~ "Rio Grande do Sul",
                                                               state =="SC" ~ "Santa Catarina",
                                                               TRUE ~ ""),
                                         region =  case_when(state =="DF" ~ "Centro Oeste",
                                                             state =="GO" ~ "Centro Oeste",
                                                             state =="MT" ~ "Centro Oeste",
                                                             state =="MS" ~ "Centro Oeste",
                                                             state =="AL" ~ "Nordeste",
                                                             state =="BA" ~ "Nordeste",
                                                             state =="CE" ~ "Nordeste",
                                                             state =="MA" ~ "Nordeste",
                                                             state =="PB" ~ "Nordeste",
                                                             state =="PE" ~ "Nordeste",
                                                             state =="PI" ~ "Nordeste",
                                                             state =="RN" ~ "Nordeste",
                                                             state =="SE" ~ "Nordeste",
                                                             state =="AC" ~ "Norte",
                                                             state =="AP" ~ "Norte",
                                                             state =="AM" ~ "Norte",
                                                             state =="PA" ~ "Norte",
                                                             state =="RO" ~ "Norte",
                                                             state =="RR" ~ "Norte",
                                                             state =="TO" ~ "Norte",
                                                             state =="ES" ~ "Sudeste",
                                                             state =="MG" ~ "Sudeste",
                                                             state =="RJ" ~ "Sudeste",
                                                             state =="SP" ~ "Sudeste",
                                                             state =="PR" ~ "Sul",
                                                             state =="RS" ~ "Sul",
                                                             state =="SC" ~ "Sul",
                                                             TRUE ~ ""))
  
  
  
  # Join lat and lng information into the database
  dcovid19city = dcovid19city %>% left_join(cod, by = c("city_ibge_code" = "cod")) 
  
  # Tranform database for 100 thousand people
  dcovid19city = dcovid19city %>% mutate (pop = estimated_population_2019) %>%
    mutate(death100 = (deaths/pop)*100000,
           confirmed100 = (confirmed/pop)*100000)
  
  # Separate deaths
  death = dcovid19city %>% mutate(occurrences = deaths,
                                  group = "Mortes") %>%
    select(date, city, occurrences, select, lat, lng, group, statename, region, pop)
  
  # Separate death100
  death100 = dcovid19city %>% mutate(occurrences = death100,
                                     group = "Mortes por 100 mil hab.") %>%
    select(date, city, occurrences, select, lat, lng, group, statename, region, pop)
  
  # Separate confirmed
  confirmed = dcovid19city %>% mutate(occurrences = confirmed,
                                      group = "Casos confirmados") %>%
    select(date, city, occurrences, select, lat, lng, group, statename, region, pop)
  
  # Separate confirmed100
  confirmed100 = dcovid19city %>% mutate(occurrences = confirmed100,
                                         group = "Casos por 100 mil hab.") %>%
    select(date, city, occurrences, select, lat, lng, group, statename, region, pop) 
  
  # Join all databases
  dcovid19city = death %>% 
    bind_rows(death100) %>%
    bind_rows(confirmed) %>%
    bind_rows(confirmed100) %>% arrange(desc(date))
  
  # Drop unnecessary databases
  remove(list = c("cod", "dcovid19", "dcovid19citya", "dcovid19cityb", "tmp", "url",
                  "death", "death100", "confirmed", "confirmed100"))
  
} # Import and organize Brazil database by city

{
  ## Import COVID state database
  
  # Declare the download link 
  url = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19 =   read_csv(gzfile(tmp),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                        encoding = "UTF-8"))
  
  # Create dcovidcity database deleting the latest data
  dcovid19statea = dcovid19 %>% filter(place_type == "state") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(date >= as.Date("2020-03-17") & is_last == FALSE) %>%
    arrange(date, city) %>%
    slice(-which.max(date)) %>%
    mutate(select = paste0(lubridate::day(date), "/", lubridate::month(date), "/", lubridate::year(date)))
  
  # Create dcovidcity database with the latest data
  dcovid19stateb = dcovid19 %>% filter(place_type == "state") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(date >= as.Date("2020-03-17") & is_last == TRUE) %>%
    arrange(date, city) %>% 
    mutate(select = "?ltimo")
  
  # bind databases
  dcovid19state = dcovid19statea %>% bind_rows(dcovid19stateb)
  
  
  # Include the state and region name
  dcovid19staten = dcovid19state %>%
    mutate(pop = estimated_population_2019) %>%
    select(date, state, confirmed, deaths, is_last, pop) %>% 
    group_by(state, date) %>%
    filter(deaths > 0) %>%
    arrange(state, date) %>%
    group_by(state) %>% dplyr::mutate(id = sequence(n()), 
                                      date = as.Date(date, format = "%Y-%m-%d"),
                                      base = "Normal") %>%
    tidyr::gather(key = "variable", value = "occurrences", deaths, confirmed) %>%
    mutate(variable = case_when(variable == "deaths" ~ "Mortes",
                                variable == "confirmed" ~ "Casos confirmados",
                                TRUE ~ "")) %>%
    mutate(statename = case_when(state =="DF" ~ "Distrito Federal",
                                 state =="GO" ~ "Goi?s",
                                 state =="MT" ~ "Mato Grosso",
                                 state =="MS" ~ "Mato Grosso do Sul",
                                 state =="AL" ~ "Alagoas",
                                 state =="BA" ~ "Bahia",
                                 state =="CE" ~ "Cear?",
                                 state =="MA" ~ "Maranh?o",
                                 state =="PB" ~ "Para?ba",
                                 state =="PE" ~ "Pernambuco",
                                 state =="PI" ~ "Piau?",
                                 state =="RN" ~ "Rio Grande do Norte",
                                 state =="SE" ~ "Sergipe",
                                 state =="AC" ~ "Acre",
                                 state =="AP" ~ "Amap?",
                                 state =="AM" ~ "Amazonas",
                                 state =="PA" ~ "Par?",
                                 state =="RO" ~ "Rond?nia",
                                 state =="RR" ~ "Roraima",
                                 state =="TO" ~ "Tocantins",
                                 state =="ES" ~ "Esp?rito Santo",
                                 state =="MG" ~ "Minas Gerais",
                                 state =="RJ" ~ "Rio de Janeiro",
                                 state =="SP" ~ "S?o Paulo",
                                 state =="PR" ~ ".Paran?",
                                 state =="RS" ~ "Rio Grande do Sul",
                                 state =="SC" ~ "Santa Catarina",
                                 TRUE ~ ""),
           region =  case_when(state =="DF" ~ "Centro Oeste",
                               state =="GO" ~ "Centro Oeste",
                               state =="MT" ~ "Centro Oeste",
                               state =="MS" ~ "Centro Oeste",
                               state =="AL" ~ "Nordeste",
                               state =="BA" ~ "Nordeste",
                               state =="CE" ~ "Nordeste",
                               state =="MA" ~ "Nordeste",
                               state =="PB" ~ "Nordeste",
                               state =="PE" ~ "Nordeste",
                               state =="PI" ~ "Nordeste",
                               state =="RN" ~ "Nordeste",
                               state =="SE" ~ "Nordeste",
                               state =="AC" ~ "Norte",
                               state =="AP" ~ "Norte",
                               state =="AM" ~ "Norte",
                               state =="PA" ~ "Norte",
                               state =="RO" ~ "Norte",
                               state =="RR" ~ "Norte",
                               state =="TO" ~ "Norte",
                               state =="ES" ~ "Sudeste",
                               state =="MG" ~ "Sudeste",
                               state =="RJ" ~ "Sudeste",
                               state =="SP" ~ "Sudeste",
                               state =="PR" ~ "Sul",
                               state =="RS" ~ "Sul",
                               state =="SC" ~ "Sul",
                               TRUE ~ "")) 
  
  # Execute log transformation
  dcovid19statel = dcovid19staten %>% mutate(occurrences = log(occurrences),
                                             base = "Em log" )
  
  # Convert the database by 100 thousand people
  dcovid19statep = dcovid19staten %>% mutate(occurrences = (occurrences/pop)*100000,
                                             base = "Por 100 mil hab." )
  
  # Join all databases
  dcovid19state = dcovid19staten %>% 
    bind_rows(dcovid19statel) %>%
    bind_rows(dcovid19statep) %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    arrange(state, base, variable, id)
  
  # Drop unnecessary databases
  remove(list = c("dcovid19", "dcovid19statea", "dcovid19stateb",
                  "dcovid19statel", "dcovid19staten","dcovid19statep", "tmp", "url"))
  
  
} # Import and organize Brazil database by state

{
  ## Import COVID state database
  
  # Declare the download link 
  url = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19 =   read_csv(gzfile(tmp),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                        encoding = "UTF-8"))
  
  # Create dcovidcity database deleting the latest data
  dcovid19statea = dcovid19 %>% filter(place_type == "state") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(is_last == FALSE) %>%
    arrange(date, city) %>%
    slice(-which.max(date))
  
  # Create dcovidcity database with the latest data
  dcovid19stateb = dcovid19 %>% filter(place_type == "state") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(is_last == TRUE) %>%
    arrange(date, city)
  
  # bind databases
  dcovid19state = dcovid19statea %>% bind_rows(dcovid19stateb)
  
  # Include a time lag variables
  setDT(dcovid19state)[, deaths_1 := shift(deaths, fill=0), by = state]
  setDT(dcovid19state)[, confirmed_1 := shift(confirmed, fill=0), by = state]  
  
  # Organize database  
  dcovid19state = dcovid19state %>% 
    mutate(deaths_new = deaths - deaths_1,
           confirmed_new = confirmed - confirmed_1) %>%
    mutate(drop = case_when(date == max(date) & confirmed_new <= 0 ~ TRUE,
                            TRUE ~ FALSE)) %>%
    filter(drop == FALSE) %>%
    select(date, deaths, deaths_new, confirmed, confirmed_new, is_last, state) %>%
    tidyr::gather(key = "variable", value = "occurrences", deaths, deaths_new, confirmed, confirmed_new) %>%
    mutate(variable = case_when(variable == "deaths" ~ "Mortes",
                                variable == "deaths_new" ~"Novas mortes",
                                variable == "confirmed" ~ "Casos confirmados",
                                variable == "confirmed_new" ~ "Novas confirma??es",
                                TRUE ~ "")) %>%
    mutate(occurrences = case_when(occurrences < 0 ~ 0,
                                   TRUE ~ occurrences)) 
  
  # Include the state and region name
  dcovid19staten = dcovid19state %>%
    group_by(state, variable) %>% dplyr::mutate(id = sequence(n()), 
                                                date = as.Date(date, format = "%Y-%m-%d"),
                                                base = "Normal") %>%
    mutate(statename = case_when(state =="DF" ~ "Distrito Federal",
                                 state =="GO" ~ "Goi?s",
                                 state =="MT" ~ "Mato Grosso",
                                 state =="MS" ~ "Mato Grosso do Sul",
                                 state =="AL" ~ "Alagoas",
                                 state =="BA" ~ "Bahia",
                                 state =="CE" ~ "Cear?",
                                 state =="MA" ~ "Maranh?o",
                                 state =="PB" ~ "Para?ba",
                                 state =="PE" ~ "Pernambuco",
                                 state =="PI" ~ "Piau?",
                                 state =="RN" ~ "Rio Grande do Norte",
                                 state =="SE" ~ "Sergipe",
                                 state =="AC" ~ "Acre",
                                 state =="AP" ~ "Amap?",
                                 state =="AM" ~ "Amazonas",
                                 state =="PA" ~ "Par?",
                                 state =="RO" ~ "Rond?nia",
                                 state =="RR" ~ "Roraima",
                                 state =="TO" ~ "Tocantins",
                                 state =="ES" ~ "Esp?rito Santo",
                                 state =="MG" ~ "Minas Gerais",
                                 state =="RJ" ~ "Rio de Janeiro",
                                 state =="SP" ~ "S?o Paulo",
                                 state =="PR" ~ ".Paran?",
                                 state =="RS" ~ "Rio Grande do Sul",
                                 state =="SC" ~ "Santa Catarina",
                                 TRUE ~ ""),
           region =  case_when(state =="DF" ~ "Centro Oeste",
                               state =="GO" ~ "Centro Oeste",
                               state =="MT" ~ "Centro Oeste",
                               state =="MS" ~ "Centro Oeste",
                               state =="AL" ~ "Nordeste",
                               state =="BA" ~ "Nordeste",
                               state =="CE" ~ "Nordeste",
                               state =="MA" ~ "Nordeste",
                               state =="PB" ~ "Nordeste",
                               state =="PE" ~ "Nordeste",
                               state =="PI" ~ "Nordeste",
                               state =="RN" ~ "Nordeste",
                               state =="SE" ~ "Nordeste",
                               state =="AC" ~ "Norte",
                               state =="AP" ~ "Norte",
                               state =="AM" ~ "Norte",
                               state =="PA" ~ "Norte",
                               state =="RO" ~ "Norte",
                               state =="RR" ~ "Norte",
                               state =="TO" ~ "Norte",
                               state =="ES" ~ "Sudeste",
                               state =="MG" ~ "Sudeste",
                               state =="RJ" ~ "Sudeste",
                               state =="SP" ~ "Sudeste",
                               state =="PR" ~ "Sul",
                               state =="RS" ~ "Sul",
                               state =="SC" ~ "Sul",
                               TRUE ~ "")) 
  
  # Execute log transformation
  dcovid19statel = dcovid19staten %>% mutate(occurrences = log(occurrences),
                                             base = "Em log" )
  
  # Transform -inf to 0
  dcovid19statel$occurrences[which(!is.finite(dcovid19statel$occurrences))] <- 0
  
  # Join all databases
  dcovid19state = dcovid19staten %>% 
    bind_rows(dcovid19statel) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    arrange(state, base, variable, id)
  
  # Drop unnecessary databases
  remove(list = c("dcovid19", "dcovid19statea", "dcovid19stateb",
                  "dcovid19statel", "dcovid19staten", "tmp", "url"))
  
  
} # Import and organize Brazil database by state (daily variation for graph)

{
  ## Import COVID state database
  
  # Declare the download link 
  url = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19 =   read_csv(gzfile(tmp),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                        encoding = "UTF-8"))
  
  # Create dcovidcity database deleting the latest data
  dcovid19statea = dcovid19 %>% filter(place_type == "state") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(is_last == FALSE) %>%
    arrange(date, city) %>%
    slice(-which.max(date))
  
  # Create dcovidcity database with the latest data
  dcovid19stateb = dcovid19 %>% filter(place_type == "state") %>%
    select(date, city, confirmed, deaths, is_last, estimated_population_2019, city_ibge_code, state) %>%
    filter(is_last == TRUE) %>%
    arrange(date, city)
  
  # bind databases
  dcovid19state = dcovid19statea %>% bind_rows(dcovid19stateb)
  
  # Include a time lag variables
  setDT(dcovid19state)[, deaths_1 := shift(deaths, fill=0), by = state]
  setDT(dcovid19state)[, confirmed_1 := shift(confirmed, fill=0), by = state]  
  
  # Organize database  
  dcovid19state = dcovid19state %>% 
    mutate(deaths_new = deaths - deaths_1,
           confirmed_new = confirmed - confirmed_1) %>%
    mutate(drop = case_when(date == max(date) & confirmed_new < 0 ~ TRUE,
                            TRUE ~ FALSE)) %>%
    filter(drop == FALSE) %>%
    select(date, deaths, deaths_new, confirmed, confirmed_new, state) %>%
    mutate(statename = case_when(state =="DF" ~ "Distrito Federal",
                                 state =="GO" ~ "Goi?s",
                                 state =="MT" ~ "Mato Grosso",
                                 state =="MS" ~ "Mato Grosso do Sul",
                                 state =="AL" ~ "Alagoas",
                                 state =="BA" ~ "Bahia",
                                 state =="CE" ~ "Cear?",
                                 state =="MA" ~ "Maranh?o",
                                 state =="PB" ~ "Para?ba",
                                 state =="PE" ~ "Pernambuco",
                                 state =="PI" ~ "Piau?",
                                 state =="RN" ~ "Rio Grande do Norte",
                                 state =="SE" ~ "Sergipe",
                                 state =="AC" ~ "Acre",
                                 state =="AP" ~ "Amap?",
                                 state =="AM" ~ "Amazonas",
                                 state =="PA" ~ "Par?",
                                 state =="RO" ~ "Rond?nia",
                                 state =="RR" ~ "Roraima",
                                 state =="TO" ~ "Tocantins",
                                 state =="ES" ~ "Esp?rito Santo",
                                 state =="MG" ~ "Minas Gerais",
                                 state =="RJ" ~ "Rio de Janeiro",
                                 state =="SP" ~ "S?o Paulo",
                                 state =="PR" ~ "Paran?",
                                 state =="RS" ~ "Rio Grande do Sul",
                                 state =="SC" ~ "Santa Catarina",
                                 TRUE ~ ""),
           region =  case_when(state =="DF" ~ "Centro Oeste",
                               state =="GO" ~ "Centro Oeste",
                               state =="MT" ~ "Centro Oeste",
                               state =="MS" ~ "Centro Oeste",
                               state =="AL" ~ "Nordeste",
                               state =="BA" ~ "Nordeste",
                               state =="CE" ~ "Nordeste",
                               state =="MA" ~ "Nordeste",
                               state =="PB" ~ "Nordeste",
                               state =="PE" ~ "Nordeste",
                               state =="PI" ~ "Nordeste",
                               state =="RN" ~ "Nordeste",
                               state =="SE" ~ "Nordeste",
                               state =="AC" ~ "Norte",
                               state =="AP" ~ "Norte",
                               state =="AM" ~ "Norte",
                               state =="PA" ~ "Norte",
                               state =="RO" ~ "Norte",
                               state =="RR" ~ "Norte",
                               state =="TO" ~ "Norte",
                               state =="ES" ~ "Sudeste",
                               state =="MG" ~ "Sudeste",
                               state =="RJ" ~ "Sudeste",
                               state =="SP" ~ "Sudeste",
                               state =="PR" ~ "Sul",
                               state =="RS" ~ "Sul",
                               state =="SC" ~ "Sul",
                               TRUE ~ "")) 
  
  # Performing a variable transposition (column to rows)
  #dcovid19state = dcovid19state %>% tidyr::gather(key = "variable", value = "occurrences", deaths, deaths_new, confirmed, confirmed_new)
  
  # Separating the database of deaths
  death = dcovid19state %>% select(date, deaths, deaths_new, state, statename, region) %>% 
    mutate(Dados = "Mortes",
           Total = deaths, 
           Nodia = deaths_new) %>%
    select(date, state, statename, region, Dados, Total, Nodia)
  
  # Separating the database of deaths
  confirmed = dcovid19state %>% select(date, confirmed, confirmed_new, state, statename, region) %>% 
    mutate(Dados = "Casos confirmados",
           Total = confirmed, 
           Nodia = confirmed_new) %>%
    select(date, state, statename, region, Dados, Total, Nodia)
  
  # Join confirmed and death databases
  dcovid19state = confirmed %>% bind_rows(death)
  
  # Drop unnecessary databases
  remove(list = c("dcovid19", "dcovid19statea", "dcovid19stateb", "tmp", "url", "death", "confirmed"))
  
  
} # Import and organize Brazil database by state (daily variation for table)


library(tidyr)
library(dplyr)
library(prophet)
library(walrasthetics2)
library(ggthemes)

dcovid19city2 = dcovid19city %>%
  mutate(ds = date) %>%
  filter(group != "Mortes por 100 mil hab.") %>%
  filter(group != "Casos por 100 mil hab.") %>%
  mutate(yhat_lower = NULL, yhat_upper = NULL, floor = 0) %>%
  arrange(ds)


dcovid19city3 = dcovid19city2  %>% 
  group_by(city, group) %>%
  mutate(y = occurrences)

data = dcovid19city3[!duplicated(dcovid19city3[c("date","city", "group")]),]
data =  subset(data, data$pop > 25000)
limite = as.data.frame(table(data$city) > 10)
limite = subset(limite, limite$`table(data$city) > 10` == TRUE)
data = subset(data, data$city %in% rownames(limite))

data2 = data %>%  
  group_by(city, group) %>%
  do(predict(prophet(., daily.seasonality=TRUE, interval.width = 0.99999), make_future_dataframe(prophet(., daily.seasonality=TRUE, interval.width = 0.99999), periods = 7))) %>%
  select(ds, city, group, yhat, yhat_lower, yhat_upper) %>%
  mutate(yhat = round(yhat, 0), yhat_lower = round(yhat_lower, 0), yhat_upper = round(yhat_upper, 0), ds = as.Date(ds)) %>%
  filter(ds > max(data$ds)-1)

teste = bind_rows(data, data2)
teste = teste  %>% mutate(y = c(NA, diff(y)))
teste = teste  %>% mutate(yhat = c(NA, diff(yhat)))

teste[teste<0] <- 0
teste = teste %>% select(ds, city, group, y, yhat, yhat_lower, yhat_upper) 
teste = teste %>% filter_at(.vars = vars(y, yhat), .vars_predicate = any_vars(!is.na(.)))
teste = subset(teste, teste$ds > "2020-05-21")


library(mongolite)
my_collection = mongo(collection = "Forecasted_data", db = "NasaHacka", url = "mongodb+srv://thiago:dYMxx3LzY0dTUUWV@maincluster-hdkzh.gcp.mongodb.net/test?retryWrites=true&w=majority") # create connection, database and collectio
my_collection$find()




SP = subset(teste, teste$city == "São Paulo")
#cwb = subset(curitiba, curitiba$group == "Casos confirmados")

ggplot(SP, aes(x = ds)) +
  geom_line(aes(y = y), color = walras_cols(4), size = 2) +
  facet_wrap(~group) + 
  geom_line(aes(y = yhat), color = walras_cols(6), linetype = "dotted", size = 2) +
  theme_fivethirtyeight() +
  facet_wrap(group~., scales = "free", nrow =2) + 
  theme(panel.background = element_rect(fill = NA, color = "black"))+ 
  theme(strip.text.x = element_text(size = 20)) +
    scale_color_discrete(name = "Y series", labels = c("Y2", "Y1"))
