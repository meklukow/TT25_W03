library(tidyverse)
library(dplyr)
library(maps)


peaks_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv')

#check to see if I should plot individual peaks or just mountain ranges
length(unique(peaks_tidy$PEAKID)) 
#480: too many
length(unique(peaks_tidy$HIMAL))
#20
ranges <- unique(peaks_tidy$HIMAL_FACTOR)
#[1] "Khumbu"                    "Annapurna"                
#[3] "Api/Byas Risi/Guras"       "Manaslu/Mansiri"          
#[5] "Langtang"                  "Damodar"                  
#[7] "Nalakankar/Chandi/Changla" "Ganesh/Shringi"           
#[9] "Peri"                      "Rolwaling"                
#[11] "Dhaulagiri"                "Kangchenjunga/Simhalila"  
#[13] "Mukut/Mustang"             "Jugal"                    
#[15] "Jongsang"                  "Saipal"                   
#[17] "Kanjiroba"                 "Makalu"                   
#[19] "Janak/Ohmi Kangri"         "Kanti/Palchung"  

#create tibble with each range's max 
mxpks <- peaks_tidy %>%
  group_by(HIMAL_FACTOR) %>%
  mutate(MAX = max(HEIGHTM)) %>%
  filter(MAX == HEIGHTM)

#list the tallest peak in each range
tallestpeaks <- unique(mxpks$PKNAME)
#[1] "Annapurna I"     "Api Main"        "Dhaulagiri I"    "Everest"        
#[5] "Ganesh I"        "Gaurishankar"    "Jongsang"        "Kangchenjunga"  
#[9] "Kanjiroba South" "Langtang Lirung" "Leonpo Gang"     "Makalu"         
#[13] "Manaslu"         "Nemjung"         "Saipal"          "Sita Chuchura"  
#[17] "Kanti Himal"     "Janak Chuli"     "Lachama Chuli"   "Lugula" 

#add GPS coordinates
pkcoord <- read_csv("pkcoord.csv")
mxpks <- merge(mxpks,pkcoord)

#plot tallest peak in each range across latitude
skylinep <- ggplot(mxpks, aes(LAT, HEIGHTM)) +
  geom_point(aes(color = OPEN)) +
  scale_colour_manual(values = c("TRUE" = "green", 
                                 "FALSE" = "red"))
skylinep
 #this one doesn't say much, becuase all of these are open

#build map
countries <- map_data("world", region = c("Nepal","India","China"))
peaksmap <- ggplot(data = countries, 
                   aes(x = long, 
                       y = lat,
                       group = group,
                       fill = region)) +
  geom_polygon(colour = "gray", alpha = .5) +
  scale_fill_manual(values = c("India" = "lightblue",
                    "Nepal" = "lightgray",
                    "China" = "lightblue1"))+
  coord_cartesian(ylim=c(26.5, 30.5), xlim=c(80,89))+
  geom_point(data = mxpks, 
             aes(x = LONG, 
                 y = LAT,
                 color = HEIGHTM,
                 label = PKNAME),
                 inherit.aes = FALSE) +
  scale_colour_gradient(low = "black", high = "red")+
  labs(color = "Peak Height (m)",
       fill = "Country",
       title = "Giants of the Himalayas", 
       subtitle = "The Tallest Peaks in Each of the 20 Himalayan Ranges") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank())
peaksmap

###plot each peak with how dangerous it is
peaks <- unique(mxpks$PEAKID)
# [1] "ANN1" "APIM" "DHA1" "EVER" "GAN1" "GAUR" "JANK" "JONG" "KANG" "KJRS" "KANT"
#[12] "LCHA" "LANG" "LEON" "LUGU" "MAKA" "MANA" "NEMJ" "SAIP" "SITA"

exped_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv')

expeddat <- exped_tidy %>%
  filter(PEAKID %in% peaks) 

#plot mortality by peak
mortdat <- expeddat %>% 
  filter(MDEATHS > 0) %>%
  group_by(PEAKID) %>%
  summarise(TOTMORT = sum(MDEATHS))

mortpeaks <- unique(mortdat$PEAKID)
mortpkid <- mxpks %>%
  filter(PEAKID %in% mortpeaks) %>%
  select(PEAKID, PKNAME,MAX)
mortdat <- merge(mortpkid,mortdat)

mortp <- ggplot(data = mortdat, 
                aes(x = reorder(PKNAME, -MAX),
                    y = TOTMORT)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_text(aes(label = TOTMORT),vjust = -.1, color = "black")+
  labs(title = "Most Dangerous Peaks", 
       subtitle = "Total Number of Deaths from 2020-2024",
       caption = "Source data from The Himalaya Database, compiled by ME Klukow",
       y = "Number of Climber Deaths",
       x = "Peak, in Descending Elevation")+
  theme_classic()
mortp

#plot by peak, group by termination reason
termreasons <- c(4,5,6,7,8)
termdat <- expeddat %>%
  filter(TERMREASON %in% termreasons)

termpeaks <- unique(termdat$PEAKID)
termpkht <- mxpks %>%
  filter(PEAKID %in% termpeaks) %>%
  select(PEAKID, PKNAME, MAX)
termdat <- merge(termpkht,termdat)

termp <- ggplot(data = termdat,
                aes(x = reorder(PKNAME, -MAX)))+
  geom_bar(position = "dodge", fill = "darkblue") +
  theme_classic() +
  labs(title = "Most Difficult Peaks", 
       subtitle = "Number of Climbs Ended Early from 2020-2024",
       caption = "Source data from The Himalaya Database, compiled by ME Klukow",
       y = "Number of Terminated Climbs",
       x = "Peak, in Descending Elevation")
termp