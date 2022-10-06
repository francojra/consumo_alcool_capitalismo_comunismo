
# Consumo de álcool em países capitalistas e comunistas ------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 05/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/alcohol-consumption -------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### O álcool tem historicamente, e continua a ter, um papel importante no engajamento
### social e criação de laços para muitos. Beber álcool socialmente e moderadamente
### para muitos é prazeroso.

### Entretanto, o consumo do álcool - principalmente em excesso - é ligado a um numeroso
### resultado negativo: risco para doenças e impactos na saúde, criminalidade, acidentes
### de trânsito, e para alguns, dependência. Globalmente, o consumo de álcool causa
### 2,8 milhões de mortes prematuras por ano.

### Esse registro mostra os dados sobre padrões globais do consumo de álcool, padrões
### de consumo, tipos de bebidas, prevalência de alcolismo; e as consequências do consumo,
### incluindo crime, mortalidade e acidentes de trânsito.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

cons_alc <- read.csv("total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv")
view(cons_alc)
names(cons_alc)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

cons_alc <- cons_alc %>%
  select(-Code) %>%
  rename(consumo = Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.) %>%
  view()

cons_alc1 <- cons_alc %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(consumo),
            sd = sd(consumo), n = n(), 
            se = sd/sqrt(n)) %>%
  view()

