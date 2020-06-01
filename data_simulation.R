library(simstudy)
library(tidyverse)
library(mongolite)


setwd("C:/Users/diego/OneDrive/Área de Trabalho/covid space apps")



def <- defData(varname = "random", dist = "nonrandom", formula = 7, id = "idnum")
def <- defData(def, varname = "xCat", formula = "0.3;0.2;0.5", dist = "categorical")
def <- defData(def, varname = "C_cardiovascular", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_diabetes", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_fumante", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_profissional_saude", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_pressao", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_respiratorios", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_imunosupress", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_gestante", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_transplante", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "C_renais", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_coriza", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_febre", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_febre_4_dias", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_garganta", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_respiracao", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_hipotensao", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "S_cansaco", dist = "binary", formula = "-3 + xCat", link = "logit")
def <- defData(def, varname = "age", dist = "uniform", formula = "8;80")



simulated_answers <- genData(10000, def)

simulated_answers$age <- floor(simulated_answers$age)



pop <- readRDS("POP.rds")

simulated_pop <- simulated_answers[1,] %>%
  mutate(city = "Curitiba", codigo = 4106902)


cities <- pop %>%
  filter(ANO == 2019)

codigos <- cities$CODIGO


for (codigo in codigos){
  
  data <- pop %>%
    filter(CODIGO == codigo, ANO == 2019)
  
  sim <- sample_n(simulated_answers, floor(0.0003 * data$POP), replace = TRUE)
  
  sim <- sim %>%
    mutate(city = data$NOME, codigo = data$CODIGO)
  
  simulated_pop <- rbind(simulated_pop, sim)
  
  
  
}

simulated_data <- simulated_pop %>%
  left_join(pop %>%
              filter(ANO == 2019), by = c("codigo" = "CODIGO", "city" = "NOME")) %>%
  select(-random, -xCat)




simulated_data$lat_sim <- runif(nrow(simulated_data), -0.001,0.001)
simulated_data$long_sim <- runif(nrow(simulated_data), -0.001,0.001)

simulated_data <- simulated_data %>%
  mutate(lat = lat + lat_sim,
         lng = lng + long_sim) %>%
  select(-lat_sim, -long_sim, - ANO) 


my_collection = mongo(collection = "Simulated_data", db = "NasaHacka", url = "mongodb+srv://thiago:dYMxx3LzY0dTUUWV@maincluster-hdkzh.gcp.mongodb.net/test?retryWrites=true&w=majority") # create connection, database and collectio

my_collection$insert(simulated_data)



