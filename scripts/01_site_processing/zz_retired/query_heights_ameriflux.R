



mountains <- read_csv("data/03_compiled/mountains.csv")

sonoran <- filter(mountains, site == "AZ")
unique(sonoran$year)


ameriflux <- read_csv("data/BASE_MeasurementHeight_20190902.csv")
glimpse(ameriflux)
sort(unique(ameriflux$Height))


## 2. Query heights ############

US_SCd <- filter(ameriflux, Site_ID == "US-SCD")
US_Vcp <- filter(ameriflux, Site_ID == "US-Vcp")
filter(US_Vcp, Variable == "TA")$Height
US_Vcm <- filter(ameriflux, Site_ID == "US-Vcm")
filter(US_Vcm, Variable == "TA")$Height

ggplot(filter(ameriflux, Height > 0 & Height < 10), aes(Height)) +
  geom_histogram(bins = 100)

surface <- filter(ameriflux, Height > 0 & Height < 10)

mode <- surface %>% 
  group_by(Height) %>% 
  count()

arrange(mode, -n) # 2m is the most common height for surface

soil <- filter(ameriflux, Height < 0 )

mode <- soil %>% 
  group_by(Height) %>% 
  count()

arrange(mode, -n) # -0.1m is the most common depth for soil
