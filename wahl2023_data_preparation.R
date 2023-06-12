library(tidyverse)
library(rvest)

zuteilen_nach_hoechstzahlen <- function(N, Stimmen, divisorfolge="saintlague", ersterdivisor=NA, output="Mandate") {
  Stimmen <- Stimmen/sum(Stimmen)
  divisoren <- switch(divisorfolge, 
                      saintlague= (1:N)-0.5,
                      hillhuntington= sqrt((0:(N-1)) * (1:N)), 
                      dhondt=1:N, 
                      adams=0:N)
  if (is.numeric(ersterdivisor)) {divisoren[1] = ersterdivisor}
  hoechstzahlen <- purrr::as_vector(purrr::map(Stimmen, function(x) x / divisoren))
  partei_labels <- rep(1:length(Stimmen), each=N)
  mandate <- partei_labels[order(hoechstzahlen, decreasing = TRUE)[1:N]]
  switch(output, 
         Mandate =	tabulate(mandate, nbins = length(Stimmen)), # Hauptoutput für Mandatsverteilung
         Letztmandat = 1:length(Stimmen)==mandate[N],
         Zweitletztmandat = 1:length(Stimmen)==mandate[N-1],
         Hoechstzahl_Letztmandat = hoechstzahlen[order(hoechstzahlen, decreasing = TRUE)][N],
         Hoechstzahl_Zweitletztmandat = hoechstzahlen[order(hoechstzahlen, decreasing = TRUE)][N-1]
  )
}

if (!file.exists("bhv2023.csv")) {
  format_df <- function(html_tabs) html_tabs[[3]] |> 
    filter(!(`Name, Vorname` %in% c("Partei + Personenstimmen","Personenstimmen", "Name, Vorname"))) |> 
    mutate(`Nr.` = as.numeric(`Nr.`) |> replace_na(0), Stimmen = Stimmen |> str_remove("\\.") |> as.numeric(),
           ParteiID = cumsum(`Nr.` == 0)) |> left_join( # Als nächstes die Parteinamen
             html_tabs[[2]] |> select(1) |> 
               filter(!(Partei %in% c("","Wahlberechtigte", "Wähler:innen", "Ungültige Stimmzettel","Gültige Stimmen"))) |> 
               mutate(ParteiID = 1:n()), by = join_by(ParteiID)
           ) |> 
    select(Partei, Listenplatz = `Nr.`, Name = `Name, Vorname`, Stimmen)
  read_html("https://www.wahlen-bremen.de/Wahlen/2023_05_14/ergebnisse_gemeinde_04012000.html") |> 
    html_elements(css = ".col-xl-3-of-6") |> html_table() |> format_df() |> write_csv("bhv2023.csv")
  bre <- read_html("https://www.wahlen-bremen.de/Wahlen/2023_05_14/ergebnisse_gemeinde_04011000.html") |> 
    html_elements(css = ".col-xl-3-of-6") |> html_table() |> format_df() |> write_csv("bre2023.csv")
}  
bre <- read_csv("bre2023.csv")
bhv <- read_csv("bhv2023.csv")
  
# Schritt 1: Parteimandate
schritt1 <- function(df, Sitze) df |> group_by(Partei) |> 
  summarize(Stimmen = sum(Stimmen)) |> 
  mutate(Anteil = Stimmen/sum(Stimmen)) |> 
  filter(Anteil >= 0.05) |> 
  mutate(Ideal_Mandate = Stimmen/sum(Stimmen) * Sitze, 
         Mandate = zuteilen_nach_hoechstzahlen(Sitze, Stimmen))
bre |> schritt1(72)
bhv |> schritt1(15)

# Schritt 2: Listen- und Personenbank
schritt2 <- function(df, Sitze) df |> 
  mutate(Stimmenart = if_else(Listenplatz==0, "Listenbank", "Personenbank")) |> 
  group_by(Partei, Stimmenart) |> 
  summarize(Stimmen = sum(Stimmen), .groups = "drop") |> 
  left_join(schritt1(df, Sitze) |> select(Partei,Parteimandate = Mandate), by = join_by(Partei)) |> 
  filter(!is.na(Parteimandate)) |> 
  group_by(Partei) |> 
  mutate(Ideal_Mandate = Stimmen/sum(Stimmen) * max(Parteimandate),
         Mandate = zuteilen_nach_hoechstzahlen(max(Parteimandate), Stimmen)) |> 
  group_by()
bre2 <- bre |> schritt2(72)
bhv2 <- bhv |> schritt2(15)

# Schritt 3: Personen
n_lowest <- function(v, n) ifelse(length(sort(v)[n])==0,0,sort(v)[n])
schritt3 <- function(df,Sitze) df |> filter(Name != "Parteistimmen") |> group_by(Partei) |> 
  mutate(Stimmenrang = n() + 1 - rank(Stimmen, ties.method = "max")) |> 
  left_join(schritt2(df,Sitze) |> select(Partei, Mandate, Stimmenart) |> 
              pivot_wider(names_from = Stimmenart, values_from = Mandate), 
            by = join_by(Partei)) |> 
  mutate(Mandat = if_else(Stimmenrang <= Personenbank, "Personenbank", ""), 
         RestListenplatz = if_else(Mandat == "Personenbank", NA, Listenplatz), 
         Mandat = if_else(Mandat != "Personenbank" & Listenplatz <= n_lowest(RestListenplatz, max(Listenbank)), 
                          "Listenbank", Mandat)) |> 
  mutate(Mandat_vor_2019 = if_else(Listenplatz <= Listenbank, "Listenbank", ""), 
         RestStimmenrang = if_else(Mandat_vor_2019 == "Listenbank", NA, Stimmenrang), 
         Mandat_vor_2019 = if_else(Mandat_vor_2019 != "Listenbank" & Stimmenrang <= n_lowest(RestStimmenrang, max(Personenbank)), 
                          "Personenbank", Mandat_vor_2019)) |> 
  mutate(Mandat_vor_2011 = if_else(Listenplatz <= Listenbank + Personenbank, "Listenbank", "")) |> 
  mutate(Mandat_Stimmenrang = if_else(Stimmenrang <= Listenbank + Personenbank, "Personenbank", "")) |> 
  select(-RestListenplatz, -RestStimmenrang) |> group_by() |> 
  mutate(across(c(Mandat,Mandat_vor_2011, Mandat_vor_2019, Mandat_Stimmenrang), \(x) str_remove(x, "bank")))
bre3 <- bre |> schritt3(72)  
bhv3 <- bhv |> schritt3(15)  


