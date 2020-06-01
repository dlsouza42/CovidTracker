library(simstudy)
library(tidyverse)
library(mongolite)
library(googledrive)
library(openxlsx)

setwd("C:/Users/diego/OneDrive/Área de Trabalho/covid space apps")

leitos <- read.csv("leitos.csv", sep = ";")


 
my_collection = mongo(collection = "Simulated_data", db = "NasaHacka", url = "mongodb+srv://thiago:dYMxx3LzY0dTUUWV@maincluster-hdkzh.gcp.mongodb.net/test?retryWrites=true&w=majority") # create connection, database and collectio

my_collection$iterate()$one()

data <-  my_collection$find('{}')

my_collection = mongo(collection = "Forecasted_data", db = "NasaHacka", url = "mongodb+srv://thiago:dYMxx3LzY0dTUUWV@maincluster-hdkzh.gcp.mongodb.net/test?retryWrites=true&w=majority")

forecast <-  my_collection$find('{}')

simulated_answers <- data %>%
  mutate(Comorb = C_cardiovascular + C_diabetes + C_pressao + C_imunosupress + C_respiratorios +
           C_gestante + C_transplante,
         Sintomas_fortes =  S_febre_4_dias + S_respiracao,
         Sintomas_leves = S_coriza + S_febre + S_garganta + S_hipotensao + S_cansaco,
         risk_level = ifelse((Comorb > 0| age >=60) & (Sintomas_fortes > 0 | Sintomas_leves > 0), "High",
                             ifelse(Comorb == 0 & Sintomas_fortes > 0, "Medium", "Low")))


setwd("D:/Dados/Pessoal/Data Science/Projects/Olist/Relative Thinking")

olist_geolocation_dataset <- read.csv("olist_geolocation_dataset.csv")

olist_geolocation_dataset <- olist_geolocation_dataset %>%
  group_by(geolocation_zip_code_prefix) %>%
  summarise(geolocation_lat = mean(geolocation_lat), 
            geolocation_lng = mean(geolocation_lng))

position <- rbind(olist_geolocation_dataset, olist_geolocation_dataset, olist_geolocation_dataset, olist_geolocation_dataset)

position <- sample_n(position, nrow(simulated_answers))




simulated_answers <- simulated_answers %>%
  select(-lat, -lng)
  

simulated_answers <- simulated_answers %>%
  mutate(risk_score = (C_cardiovascular * 0.317073171) + 
           (C_diabetes * 0.404255319) + 
           (C_fumante * 0.315789474) + 
           (C_pressao * 0.257142857) +
           (C_respiratorios * 0.310344828) + 
           (S_coriza * 0.207317073) + 
           (S_febre * 0.188191882),
         risk_score = ifelse(age >= 65, risk_score + 0.3333,
                             ifelse(age < 65 & age >= 41, risk_score + 0.140449438, 
                                    ifelse(age >= 20 & age <= 40, risk_score + 0.029411765, risk_score))))

simulated_answers <- simulated_answers %>%
  mutate(risk_level = ifelse(risk_score <= 0.75, "low",
                             ifelse(risk_score >= 1.5, "high", "medium")))

colnames(simulated_answers)[c(1:18)] <- c("idnum", "C_cardiovascular", "C_diabetes", "C_smoker", "C_health_professional",
                                 "C_pressure", "C_respiratory", "C_immunosuppression", "C_pregnant", "C_transplant",
                                "C_kidney", "S_cough", "S_fever", "S_fever_4_days", "S_throat", "S_breath", "S_hypotension",
                                "S_fatique")

simulated_answers$UserID <- seq(1:60250)

simulated_answers <- cbind(simulated_answers, position)

simulated_answers <- simulated_answers %>%
  mutate(UserID = paste("U", UserID, sep = ""))



colnames(simulated_answers) <- str_replace(colnames(simulated_answers), "C_", "Comoborbity: ")
colnames(simulated_answers) <- str_replace(colnames(simulated_answers), "S_", "Symptoms: ")
colnames(simulated_answers) <- str_replace(colnames(simulated_answers), "throat", "Throat pain")
colnames(simulated_answers) <- str_replace(colnames(simulated_answers), "fever_4_days", "fever (more than 4 days)")


for (i in 2:18){
  
  simulated_answers[,i] <- ifelse( simulated_answers[,i] == 1, "yes", "no")
  
}

agg_simulated_answers <- simulated_answers[,c(2:18, 20, 26, 30)] %>%
  gather(key = "symptoms", value = "value", -c(city, risk_level, risk_score)) %>%
  group_by(city, symptoms) %>%
  summarise(values = sum(value),
            risk_score = sum(risk_score)) 


agg_simulated_answers$symptoms <- str_replace(agg_simulated_answers$symptoms, "C_", "Comoborbity: ")
agg_simulated_answers$symptoms <- str_replace(agg_simulated_answers$symptoms, "S_", "Symptoms: ")
agg_simulated_answers$symptoms <- str_replace(agg_simulated_answers$symptoms, "throat", "Throat pain")
agg_simulated_answers$symptoms <- str_replace(agg_simulated_answers$symptoms, "fever_4_days", "fever (more than 4 days)")

agg_simulated_answers <- agg_simulated_answers %>%
  mutate(Type = ifelse(grepl("Comoborbity", symptoms) == TRUE, "Comoborbity", "Symptoms"))



forecast <- forecast %>%
  mutate(ds = ymd(ds))

forecast <- forecast %>%
  mutate(group = str_replace(group, "Casos confirmados", "confirmed"),
         group = str_replace(group, "Mortes", "death"))

leitos <- leitos %>%
  mutate(Taxa = str_replace(Taxa, ",", "."),
         Taxa = as.numeric(Taxa),
         city = cidade, beds = leitos)


leitos <- leitos[,c(5,6,7)]



forecast <- forecast %>%
  left_join(leitos)

leitos <- forecast %>%
  filter(group == "confirmed") %>%
  mutate(beds_available = beds - (Taxa * beds),
         total = ifelse(is.na(y), yhat, y),
         beds_available = beds_available - total,
         occupation_rate = 1 - (beds_available/beds),
         occupation_rate = ifelse(occupation_rate > 1, 1, occupation_rate ))


  

setwd("C:/Users/diego/OneDrive/Área de Trabalho/covid space apps")
write.xlsx(simulated_answers, 'simulated_answers.xlsx')
drive_upload('simulated_answers.xlsx', type='spreadsheet')

write.xlsx(agg_simulated_answers, 'agg_simulated_answers.xlsx')
drive_upload('agg_simulated_answers.xlsx', type='spreadsheet')


write.xlsx(forecast, "forecast.xlsx")
drive_upload("forecast.xlsx", type='spreadsheet')

write.xlsx(leitos, "leitos.xlsx")
drive_upload("leitos.xlsx", type='spreadsheet')


