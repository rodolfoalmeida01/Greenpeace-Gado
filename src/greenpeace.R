library(tidyverse)


# READING ---------------------------------------------------------------------------

Minerva_RdM_2018_2022 <- read_csv("C:/Users/rodol/OneDrive/Desktop/Projetos/Greenpeace Gado/data/JBS and Minerva Exports Panjiva - 2. Minerva RdM 2018-2022.csv") %>% 
  janitor::clean_names()
JBS_PV_SMdG_Vilh_2018_2022 <- read_csv("C:/Users/rodol/OneDrive/Desktop/Projetos/Greenpeace Gado/data/JBS and Minerva Exports Panjiva - 1. JBS PV, SMdG, Vilh, 2018-2022.csv") %>% 
  janitor::clean_names()


# WRANGLING -------------------------------------------------------------------------

# CRIA DESTINO-ORIGEM MINERVA
Minerva_RdM_2018_2022 %>% 
  group_by(municipality,shipment_destination_country) %>% 
  summarise(total=sum(gross_weight_t)) %>% 
  mutate(empresa="minerva") -> minerva

# CRIA DESTINO-ORIGEM JBS
JBS_PV_SMdG_Vilh_2018_2022 %>% 
  group_by(municipality,shipment_destination_country) %>% 
  summarise(total=sum(gross_weight_t)) %>% 
  mutate(empresa="jbs") -> jbs

# JUNTA MINERVA+JBS E CRIA CLASSE OUTROS PARA GADO < 3000
rbind(minerva,jbs) %>% 
  rename(Name = shipment_destination_country) -> jbsminerva
jbsminerva %>% mutate(Name=case_when(total<3000 ~ "Outros", 
                                     TRUE ~ as.character(Name))) %>% clipr::write_clip()

# CARREGA LAT LONS DE PAÍSES PARA MAPA DE EXPORTAÇÃO
world <- map_data("world")
Untitled_spreadsheet_latlon <- read_csv("C:/Users/rodol/OneDrive/Desktop/Projetos/Greenpeace Gado/data/latlon.csv")
full_join(Untitled_spreadsheet_latlon, jbsminerva, 'Name') -> geo_jbsminerva

# REAGRUPA PARA NÃO DIFERENCIAR EMPRESA
geo_jbsminerva %>% 
  group_by(Name, country, latitude, longitude) %>% 
  summarise(total = sum(total)) -> geo_jbsminerva

# PLOTTING --------------------------------------------------------------------------

# PLOTA MAPA
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
