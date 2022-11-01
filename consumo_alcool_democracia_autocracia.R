
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
library(ggthemes)

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

cons_alc13 <- cons_alc %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(cons_alc1, aes(x = fct_reorder(Entity, media), y = media, 
                      fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Coreia do Norte", "Cuba", "China",
                              "Japão", "Estados Unidos", "Alemanha")) +
  labs(x = "Países", y = "Consumo de álcool por pessoa (litros)") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

c4a("dark2", 2)

ggplot(cons_alc13, aes(x = Year, y = consumo, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Consumo de álcool por pessoa (litros)", 
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
