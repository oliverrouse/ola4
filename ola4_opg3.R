library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)

# Liste alle logfiler fra mappen
log_files <- list.files("log", pattern = "\\.log", full.names = TRUE)

# Læs og kombiner alle logfiler
all_logs <- lapply(log_files, readLines)

# Regulært udtryk - mønster nr 1. (specifikt)
pattern <- '^([0-9\\.]+) - - \\[([^\\]]+)\\] "(GET|POST|PUT|DELETE|HEAD|OPTIONS|PATCH) ([^"]+) HTTP/[^"]+" (\\d{3}) (\\d+) "([^"]*)" "([^"]*)"$'

#opretter tom dataframe 
logs.sammen <- as.data.frame(matrix(data=NA, nrow = 0, ncol = 9)) 

# Anvend på log data
for (i in 1:16){
  templog <- str_match(all_logs[[i]], pattern)
  logs.sammen <- rbind(logs.sammen, templog)
}

#når man bruger str_match returner den hele udtrykket i første kolonne

#laver colnames
regex.grupper <- c("Samlet", "IP", "Tid", "Metode", "URL-sti", "Statuskode", "Str", "Reference", "UserAgent")
colnames(logs.sammen) <- regex.grupper

#opdeler tidskolonne i dato og tid
logs.sammen <- logs.sammen %>%
  separate(Tid, into = c("Dato", "Tid"), sep = ":", extra = "merge")

#########ANALYSE
################################ Optælling af aktive IP-adresser pr. døgn - 2 metoder ##############
# n(): Tæller alle forespørgsler for hver dato, uanset om IP'en er den samme. 
ialt.ip <- logs.sammen %>%
  group_by(Dato) %>%
  summarise(Antal_Forespørgsler = n())

ialt.ip$Dato <- as.Date(ialt.ip$Dato, format = "%d/%b/%Y") #datokolonne i dato-format

#fjerner række 8 som outlier - fejl at den er i 2022
ialt.ip<- ialt.ip[-8,]

ialt.ip <- ialt.ip %>%
  arrange(desc(Dato))

# n_distinct(IP): tæller alle unikke forespøgelser - så ikke samme IP adresse
aktiv.ip.døgn <- logs.sammen %>%
  group_by(Dato) %>%
  summarise(Aktive_IP = n_distinct(IP))

aktiv.ip.døgn$Dato <- as.Date(aktiv.ip.døgn$Dato, format = "%d/%b/%Y")

aktiv.ip.døgn<- aktiv.ip.døgn[-8,]

aktiv.ip.døgn <- aktiv.ip.døgn %>%
  arrange(desc(Dato))

# Kombiner de to datarammer på 'Dato'
combined_data <- left_join(ialt.ip, aktiv.ip.døgn, by = "Dato")

# Plot 
ggplot(combined_data, aes(x = Dato)) +
  geom_line(aes(y = Antal_Forespørgsler, color = "Antal forespørgsler (samlet)"), size = 1) +
  geom_line(aes(y = Aktive_IP, color = "Antal aktive IP-adresser"), size = 1) +
  scale_x_date(date_labels = "%d/%m", date_breaks = "1 day") +
  labs(
    title = "Aktiviteten genereres af gentagne forespørgsler fra de samme IP-adresser",
    x = "Dato",
    y = "Antal forespørgsler"
  ) +
  scale_color_manual(
    values = c("Antal forespørgsler (samlet)" = "blue", 
               "Antal aktive IP-adresser" = "red"), # Ændret navnene
    name = "" # Fjerner "colour" fra legend
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85), # Placerer legend
    plot.title = element_text(face = "bold") # Gør overskriften fed
  )


############################ Mest aktive IP-adresser #########################
mest_aktive_ip <- logs.sammen %>%
  count(IP, name = "Antal") %>%
  arrange(desc(Antal))


# Plot de mest aktive IP-adresser barplot
library(RColorBrewer)
farver.barplot <- brewer.pal(10, "RdBu")

ggplot(mest_aktive_ip[1:10, ], aes(x = reorder(IP, Antal), y = Antal, fill = IP)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Stor forskel i de mest aktive IP-adressers antal forespørgsler", x = "IP-adresse", y = "Antal forespørgsler") +
  scale_fill_manual(values = farver.barplot) +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))


#Plot af de 5 mest aktive ip adresser over tid
top.aktive.ip <- mest_aktive_ip[1:5,1]
print(top.aktive.ip)

# dataframe af de mest aktive IP'er + antal forespørgsler
aktive_ip_data <- logs.sammen %>%
  filter(IP %in% top.aktive.ip) %>%  # Filtrer for de 5 mest aktive IP-adresser
  group_by(Dato, IP) %>%             # Gruppér efter Dato og IP
  summarise(Antal_Forespørgsler = n(), .groups = "drop")  # tæl forespørgsler i ny kolonne

aktive_ip_data$Dato <- as.Date(aktive_ip_data$Dato, format = "%d/%b/%Y") #datokolonne i dato-format

#fjerner række 19 som outlier - fejl at den er i 2022???
aktive_ip_data <- aktive_ip_data[-19,]

#vender rækkefølgen af datoerne i dataframen
aktive_ip_data <- aktive_ip_data %>%
  arrange(desc(Dato))

#Plotter 5 mest aktive over tid
# pænere farver på grafen
library(RColorBrewer)
farver <- brewer.pal(10, "Set1")

ggplot(aktive_ip_data, aes(x = Dato, y = Antal_Forespørgsler, color = IP, group = IP)) +
  geom_line() +
  labs(title = "Den mest aktive IP-adresse har 288 forespørgsler pr. dag",
       x = "Dato", y = "Antal forespørgsler", color = "IP-adresse") +
  scale_x_date(date_labels = "%d/%m", date_breaks = "1 day") + #dato akse
  theme(axis.text.x = element_text(angle = 45, hjust = 1),) +
  geom_text(aes(label = Antal_Forespørgsler), vjust = -0.5, size = 3) + #antal forespørgsler ved punkt
  theme_light()+
  theme(plot.title = element_text(face = "bold"))+
  scale_color_manual(values = farver) 


#!! her kan det ses at den mest aktive ikke 'spiker' på bestemt dato, men at den har 288 forsøg hver dag


##########whois-info på den mest aktive. 
mest_aktive_ip_adresse <- mest_aktive_ip$IP[1]

# Whois-opslag
whois_info <- system(paste("whois", mest_aktive_ip_adresse), intern = TRUE)
cat(whois_info)

#bruger hele tekststrengen da mit R ikke kan downloade whois pakken
whois_info <- "% IANA WHOIS server % for more information on IANA, visit http://www.iana.org % This query returned 1 object  refer:        whois.arin.net  inetnum:      192.0.0.0 - 192.255.255.255 organisation: Administered by ARIN status:       LEGACY  remarks:      192.0.2.0/24  reserved for TEST-NET-1 [RFC5737]. remarks:      Complete registration details for 192.0.2.0/24 are found remarks:      iniana-ipv4-special-registry.192.88.99.0/24 reserved for remarks:      6to4 Relay Anycast [RFC7526]Complete registration remarks:      details for 192.88.99.0/24 are found remarks:      iniana-ipv4-special-registry.192.88.99.2/32 reserved for remarks:      6a44 Relay Anycast [RFC6751](possibly collocated with remarks:      6to4 Relay at 192.88.99.1/32 - see remarks:      [RFC7526])192.168.0.0/16 reserved for Private-Use remarks:      Networks [RFC1918]. Complete registration details for remarks:      192.168.0.0/16 are found iniana-ipv4-special-registry. remarks:      192.0.0.0/24 reserved for IANA IPv4 Special Purpose remarks:      Address Registry [RFC5736]. Complete registration remarks:      details for 192.0.0.0/24 are found remarks:      iniana-ipv4-special-registry.  whois:        whois.arin.net  changed:      1993-05 source:       IANA  # whois.arin.net  NetRange:       192.0.64.0 - 192.0.127.255 CIDR:           192.0.64.0/18 NetName:        AUTOMATTIC NetHandle:      NET-192-0-64-0-1 Parent:         NET192 (NET-192-0-0-0-0) NetType:        Direct Allocation OriginAS:       AS2635 Organization:   Automattic, Inc (AUTOM-93) RegDate:        2012-11-20 Updated:        2024-05-21 Comment:        Geofeed https://as2635.network/geofeed.csv Ref:            https://rdap.arin.net/registry/ip/192.0.64.0   OrgName:        Automattic, Inc OrgId:          AUTOM-93 Address:        60 29th Street #343 City:           San Francisco StateProv:      CA PostalCode:     94110 Country:        US RegDate:        2011-10-05 Updated:        2023-08-11 Ref:            https://rdap.arin.net/registry/entity/AUTOM-93   OrgNOCHandle: NOC12276-ARIN OrgNOCName:   NOC OrgNOCPhone:  +1-877-273-8550  OrgNOCEmail:  ipadmin@automattic.com OrgNOCRef:    https://rdap.arin.net/registry/entity/NOC12276-ARIN  OrgAbuseHandle: ABUSE3970-ARIN OrgAbuseName:   Abuse OrgAbusePhone:  +1-877-273-8550  OrgAbuseEmail:  abuse@automattic.com OrgAbuseRef:    https://rdap.arin.net/registry/entity/ABUSE3970-ARIN  OrgTechHandle: NOC12276-ARIN OrgTechName:   NOC OrgTechPhone:  +1-877-273-8550  OrgTechEmail:  ipadmin@automattic.com OrgTechRef:    https://rdap.arin.net/registry/entity/NOC12276-ARIN"

# Udtræk organisation
organisation <- str_extract(whois_info, "Organization:\\s*(.*?)\\s*(?=\\()")

# Udtræk netværksområde (IP Range)
ip_range <- str_extract(whois_info, "NetRange:\\s*(\\d+\\.\\d+\\.\\d+\\.\\d+\\s*-\\s*\\d+\\.\\d+\\.\\d+\\.\\d+)")

# Udtræk land (Country)
country <- str_extract(whois_info, "Country:\\s*([A-Za-z]+)")

# Udtræk adresse
address <- str_extract(whois_info, "Address:\\s*(.*?)\\n")

# Udtræk abuse kontakt e-mail
abuse_contact <- str_extract(whois_info, "OrgAbuseEmail:\\s*(\\S+)")

# Udskriv resultaterne
cat("Organisation:", organisation, "\n")
cat("IP Range:", ip_range, "\n")
cat("Country:", country, "\n")
cat("Address:", address, "\n")
cat("Abuse Contact Email:", abuse_contact, "\n")


#Resultat: who is information på mest aktive IP adresse

#Organisation: Automattic, Inc
#IP Range: 192.0.64.0 - 192.0.127.255
#Country: US
#Address: 60 29th Street #343
#Abuse Contact Email: abuse@automattic.com

library(dplyr)
#hvad prøver den mest aktive på mht metode - 
logs.sammen %>%
  filter(IP == "192.0.102.40") %>% 
  pull(Metode) %>%                      
  unique() 

# "HEAD"

#den mest aktive ip adresse bruger kun HEAD

# Filtrer for den specifikke IP og grupper efter statuskode
status_counts <- logs.sammen %>%
  filter(IP == "192.0.102.40") %>%    # Filtrer efter IP
  group_by(Statuskode) %>%             # Gruppér efter statuskode
  summarise(count = n(), .groups = 'drop')  # Tæl forekomsten af hver statuskode

# Statuskode count
# 200         4143
# 404            2

# Vis resultaterne
print(status_counts)




#########Gruppering på 404, herunder en beskrivelse af ”mistænksomme” requests.
# Filtrer efter 404-statuskoder
logs_404 <- logs.sammen %>%
  filter(Statuskode == 404)

logs_404_grouped <- logs_404 %>%
  group_by(IP, Dato) %>% #grupperer efter IP adresse og dato
  summarise(Antal_404 = n())

# Tæl mistænkelige requests pr. sti (url vej)
mistænkelige_stier <- logs_404 %>%
  count(`URL-sti`, name = "Antal_Forespørgsler") %>%
  arrange(desc(Antal_Forespørgsler))

print(mistænkelige_stier)

# identificere mistænksomme mønstre - algoritme
#1. IP'er, der har over 30 404-fejl pr. dag 
mistænkelige_ips <- logs_404_grouped %>%
  filter(Antal_404 > 30)

#2. anmodninger om sjældne ressourcer - forsøg på adgang til admin-sider
mistænkelige_login_requests <- logs_404 %>%
  filter(grepl("admin|config|login", `URL-sti`))

#3. Find mistænkelige user-agents
mistænkelige_agents <- logs_404 %>%
  filter(grepl("curl|python|bot|wget", UserAgent, ignore.case = TRUE))

#4. # hvis 25% af http-metoder er head er den mistænksom
# Beregn andelen af hver metode pr. IP
metode.andel <- logs.sammen %>%
  group_by(IP, Metode) %>%          
  summarise(count = n(), .groups = "drop") %>%  # Tæl forekomsten af hver metode pr. IP
  group_by(IP) %>%                  # Gruppér igen kun efter IP
  mutate(total_requests = sum(count),         # Beregn totalt antal anmodninger pr. IP
         percentage = count / total_requests * 100) %>%  # Andel af hver metode
  ungroup()

# Filtrér for IP'er, hvor 'HEAD' udgør over 25% af anmodningerne
head.metode <- metode.andel %>%
  filter(Metode == "HEAD" & percentage > 25)


# samler alle mistnkelige ip adresser i en dataframe
#vil kun have ip adresser med over 10 forespørgsler: 

# Filtrer IP'er med mere end 10 forespørgsler
aktive.ip <- mest_aktive_ip %>%
  filter(Antal > 10) %>%
  select(IP) %>%
  distinct() %>%
  mutate(mest_aktiv = TRUE)

library(tidyr)
# Saml unikke IP'er fra hver kriterie
ips_kriterie1 <- mistænkelige_ips %>% select(IP) %>% distinct() %>% mutate(kriterie1 = TRUE)
ips_kriterie2 <- mistænkelige_login_requests %>% select(IP) %>% distinct() %>% mutate(kriterie2 = TRUE)
ips_kriterie3 <- mistænkelige_agents %>% select(IP) %>% distinct() %>% mutate(kriterie3 = TRUE)
ips_kriterie4 <- head.metode %>% select(IP) %>% distinct() %>% mutate(kriterie4 = TRUE)


# Kombiner dataene i én tabel
alle_ips <- full_join(ips_kriterie1, ips_kriterie2, by = "IP") %>%
  full_join(ips_kriterie3, by = "IP") %>%
  full_join(ips_kriterie4, by = "IP") %>%
  full_join(aktive.ip, by = "IP") %>% # Tilføj de mest aktive IP'er
  replace_na(list(kriterie1 = FALSE, kriterie2 = FALSE, kriterie3 = FALSE, kriterie4 = FALSE, mest_aktiv = FALSE))

# Tilføj en kolonne for antal opfyldte kriterier
alle_ips <- alle_ips %>%
  ungroup() %>%
  mutate(across(kriterie1:kriterie4, as.logical)) %>% # Sikr korrekt datatype
  mutate(antal_kriterier_opfyldt = rowSums(select(., kriterie1:kriterie4) == TRUE)) %>%
  filter(mest_aktiv == TRUE) # Behold kun IP'er, der er blandt de mest aktive


# Vis IP-adresser, der opfylder mere end ét kriterie
alle.mistænkelige.ip <- alle_ips %>%
  filter(antal_kriterier_opfyldt >= 1)

#visualisering af kriterier opfyldt per ip adresse
library(ggplot2)
library(reshape2)

# Transformér data til langt format
mistænkelige_long <- alle.mistænkelige.ip %>%
  select(IP, kriterie1:kriterie4) %>%
  melt(id.vars = "IP", variable.name = "Kriterium", value.name = "Opfyldt")
library(dplyr)

# Ændre navnene på kriterierne i langt format
mistænkelige_long <- mistænkelige_long %>%
  mutate(Kriterium = recode(Kriterium,
                            "kriterie1" = "Over 30 404-fejl pr. dag",
                            "kriterie2" = "Adgangsforsøg - admin/login/config ",
                            "kriterie3" = "Mistænkelige user agents",
                            "kriterie4" = "Høj andel af HEAD http-metode"))

# Plot heatmap med de nye navne
ggplot(mistænkelige_long, aes(x = Kriterium, y = IP, fill = Opfyldt)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightgrey")) +
  theme_minimal() +
  labs(
    title = "Variation i adfærd blandt mistænkelige IP-adresser",
    x = "Kriterier for mistænksomt adfærd",
    y = "IP-adresser",
    fill = "Opfyldt"
  )+
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Justér størrelse og fed tekst
  )



#########################geo visualisering


install.packages("httr")
library(httr)
library(dplyr)

# Funktion til at hente data fra ipapi
get_ip_info <- function(ip) {
  tryCatch({
    res <- httr::GET(paste0("http://ip-api.com/json/", ip))
    content(res, as = "parsed")
  }, error = function(e) {
    # Returnér en tom liste, hvis der opstår en fejl
    list(status = "error", message = as.character(e))
  })
}

# Hent geo data for alle mistænkelige
alle_ips_geo <- alle.mistænkelige.ip %>%
  filter(antal_kriterier_opfyldt > 0) %>%
  rowwise() %>%
  mutate(geo_info = list(get_ip_info(IP))) %>%
  unnest_wider(geo_info)

# Se resultater
head(alle_ips_geo)

#visualiserer geografisk

library(ggplot2)
library(maps) # Indeholder kortdata
library(leaflet)

# Interaktivt kort
leaflet(alle_ips_geo) %>%
  addTiles() %>% # Tilføj baggrundskort
  addCircleMarkers(~lon, ~lat, 
                   label = ~paste0("IP: ", IP, "<br>Land: ", country),
                   color = "red", radius = 5) %>%
  setView(lng = mean(alle_ips_geo$lon, na.rm = TRUE), 
          lat = mean(alle_ips_geo$lat, na.rm = TRUE), 
          zoom = 2)


library(ggplot2)
library(tidyr)

# Data fordeling efter land
ggplot(alle_ips_geo, aes(x = reorder(country, country, function(x) length(x)))) +
  geom_bar(fill = "lightblue") +
  theme_minimal() +
  labs( title = "Flest mistænklige IP-adresser i UK og US",
        x = "Land",
        y = "Antal mistænklige IP-adresser"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Justér størrelse og fed tekst
  )



##############################status 200 kode - filtrerer alle mistænkelige ip-adresser fra den samlede data - får logdata på alle mistænkelige IP-adresser
mistænkelige_logdata <- logs.sammen %>%
  filter(IP %in% alle.mistænkelige.ip$IP)  # Filtrer for mistænkelige IP-adresser

#3. tjekker om mistænkelige ip-adresser har returneret 200
mistænkelige_200 <- mistænkelige_logdata %>%
  filter(Statuskode == 200)

#4. grupperer på IP med antallet af 200-statuskoder per IP
mistænkelige_200_summary <- mistænkelige_200 %>%
  group_by(IP) %>%
  summarise(Antal_200 = n())%>%
  arrange(desc(Antal_200))

# Vis opsummeringen
head(mistænkelige_200_summary)

# 5.179.80.205          1408

#ex har ovenstående IP adresse returneret 200(statuskode) 1408, og er på liste over mistænkelige forsøg



