library(tidyverse)


# READING ---------------------------------------------------------------------------


Minerva_RdM_2018_2022 <- read_csv("C:/Users/rodol/Downloads/JBS and Minerva Exports Panjiva - 2. Minerva RdM 2018-2022.csv") %>% 
  janitor::clean_names()
JBS_PV_SMdG_Vilh_2018_2022 <- read_csv("C:/Users/rodol/Downloads/JBS and Minerva Exports Panjiva - 1. JBS PV, SMdG, Vilh, 2018-2022.csv") %>% 
  janitor::clean_names()

# CRIA DESTINO-ORIGEM
Minerva_RdM_2018_2022 %>% 
  group_by(municipality,shipment_destination_country) %>% 
  summarise(total=sum(gross_weight_t)) %>% 
  mutate(empresa="minerva") -> minerva

JBS_PV_SMdG_Vilh_2018_2022 %>% 
  group_by(municipality,shipment_destination_country) %>% 
  summarise(total=sum(gross_weight_t)) %>% 
  mutate(empresa="jbs") -> jbs

rbind(minerva,jbs) %>% 
  rename(Name = shipment_destination_country) -> jbsminerva
jbsminerva %>% mutate(Name=case_when(total<3000 ~ "Outros", 
                                     TRUE ~ as.character(Name))) %>% clipr::write_clip()

# DADOS PRA MAPA
world <- map_data("world")
Untitled_spreadsheet_latlon <- read_csv("C:/Users/rodol/Downloads/Untitled spreadsheet - latlon.csv")

full_join(Untitled_spreadsheet_latlon, jbsminerva, 'Name') -> geo_jbsminerva

# MAPA
# Reagrupa pra nao diferenciar empresa
geo_jbsminerva %>% 
  group_by(Name, country, latitude, longitude) %>% 
  summarise(total = sum(total)) -> geo_jbsminerva

# DF de linhas
geo_jbsminerva 

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = geo_jbsminerva,
    aes(longitude, latitude, size=total),
    alpha = 0.7
  ) +
  coord_map("gilbert")
  theme_minimal()
