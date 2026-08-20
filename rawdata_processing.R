# The data for all voting ballots have to be gathered from 
#     Landesamt für Statistik Bremen 
# It can only be provided by them when a non-disclosure agreement has been signed
# This script documents the data processing but the data is not in the public repopsitory!

library(tidyverse)
library(glue)
library(readxl)
library(arrow)

datapath <- "/home/janlo/Documents/data/landeswahlleiterbremen/"

# Kandidaten
BüW2011WvBre <- read_xlsx(glue("{datapath}BüW 2011 Wahlvorschläge Bremen.xlsx")) |> select(-Wohnort, -Geburtsort, -PLZ)
BüW2011WvBhv <- read_csv(glue("{datapath}BüW 2011 Wahlvorschläge Bremerhaven.csv")) |> select(-district, -year)
BüW2015WvBre <- read_xlsx(glue("{datapath}BüW 2015 Wahlvorschläge Bremen.xlsx")) |> select(-Wohnort, -Geburtsort, -PLZ)
BüW2015WvBhv <- read_xlsx(glue("{datapath}BüW 2015 Wahlvorschläge Bremerhaven.xlsx")) |> select(-Wohnort, -Geburtsort, -PLZ)
BüW2019WvBre <- read_xlsx(glue("{datapath}BüW 2019 Wahlvorschläge Bremen.xlsx")) |> select(-Wohnort, -Geburtsort, -PLZ)
BüW2019WvBhv <- read_xlsx(glue("{datapath}BüW 2019 Wahlvorschläge Bremerhaven.xlsx")) |> select(-Wohnort, -Geburtsort, -PLZ)
BüW2023WvBre <- read_xlsx(glue("{datapath}BüW 2023 Wahlvorschläge Bremen GeschlechtGeschätzt.xlsx")) |> 
  select(-`Liste Nr.`) |> 
  rename(`Name Partei/Wählervereinigung` = Partei, Kurzform = Kurz, `Stadt- oder Ortsteil` = `Stadt-/Ortsteil`) |> 
  mutate(Name = paste0(if_else(is.na(Namensvorsatz),"",paste0(Namensvorsatz," ")), Name), Listenplatz = as.numeric(Listenplatz)) |> select(-Namensvorsatz)
BüW2023WvBhv <- read_xlsx(glue("{datapath}BüW 2023 Wahlvorschläge Bremerhaven GeschlechtGeschätzt.xlsx")) |> 
  select(-`Liste Nr.`) |> 
  rename(`Name Partei/Wählervereinigung` = Partei, Kurzform = Kurz, `Stadt- oder Ortsteil` = `Stadt-/Ortsteil`) |> 
  mutate(Name = paste0(if_else(is.na(Namensvorsatz),"",paste0(Namensvorsatz," ")), Name), Listenplatz = as.numeric(Listenplatz)) |> select(-Namensvorsatz)
bind_rows(
  BüW2011WvBre |> mutate(Jahr = 2011, Wahlbezirk = "Bremen"),
  BüW2011WvBhv |> mutate(Jahr = 2011, Wahlbezirk = "Bremerhaven"),
  BüW2015WvBre |> mutate(Jahr = 2015, Wahlbezirk = "Bremen"),
  BüW2015WvBhv |> mutate(Jahr = 2015, Wahlbezirk = "Bremerhaven"),
  BüW2019WvBre |> mutate(Jahr = 2019, Wahlbezirk = "Bremen"),
  BüW2019WvBhv |> mutate(Jahr = 2019, Wahlbezirk = "Bremerhaven"),
  BüW2023WvBre |> mutate(Jahr = 2023, Wahlbezirk = "Bremen"),
  BüW2023WvBhv |> mutate(Jahr = 2023, Wahlbezirk = "Bremerhaven")
) |> 
  mutate(Geschlecht = str_replace(Geschlecht, "F", "W")) |> 
  write_parquet("data/Kandidaten.parquet")

# Listen
read_csv(glue("{datapath}Parteilisten_all.csv")) |> write_parquet("data/Listen.parquet")

# Stimmzettel
bind_rows(
  read_xlsx(glue("{datapath}Stimmzettel 2011 Wahlbereich Bremen Vertrag.xlsx"), skip = 1) |> select(1:5) |> 
    count(`Stimme 1`, `Stimme 2`, `Stimme 3`, `Stimme 4`, `Stimme 5`) |> mutate(Jahr = 2011, Wahlbezirk = "Bremen") |> 
    setNames(c(paste0("Stimme",1:5), "n", "Jahr", "Wahlbezirk")),
  read_xlsx(glue("{datapath}Stimmzettel 2011 Wahlbereich Bremerhaven Vertrag.xlsx"), skip = 1) |> select(1:5) |> 
    count(`Stimme 1`, `Stimme 2`, `Stimme 3`, `Stimme 4`, `Stimme 5`) |> mutate(Jahr = 2011, Wahlbezirk = "Bremerhaven") |> 
    setNames(c(paste0("Stimme",1:5), "n", "Jahr", "Wahlbezirk")),
  read_xlsx(glue("{datapath}Stimmzettel 2015 Wahlbereich Bremen Vertrag.xlsx"), skip = 1) |> select(1:5) |> 
    count(`Stimme 1`, `Stimme 2`, `Stimme 3`, `Stimme 4`, `Stimme 5`) |> mutate(Jahr = 2015, Wahlbezirk = "Bremen") |> 
    setNames(c(paste0("Stimme",1:5), "n", "Jahr", "Wahlbezirk")),
  read_xlsx(glue("{datapath}Stimmzettel 2015 Wahlbereich Bremerhaven Vertrag.xlsx"), skip = 1) |> select(1:5) |> 
    count(`Stimme 1`, `Stimme 2`, `Stimme 3`, `Stimme 4`, `Stimme 5`) |> mutate(Jahr = 2015, Wahlbezirk = "Bremerhaven") |> 
    setNames(c(paste0("Stimme",1:5), "n", "Jahr", "Wahlbezirk")),
  read_xlsx(glue("{datapath}Stimmzettel 2019 Wahlbereich Bremen Vertrag.xlsx"), skip = 0) |> select(1:5) |> 
    count(`Stimme1`, `Stimme2`, `Stimme3`, `Stimme4`, `Stimme5`) |> mutate(Jahr = 2019, Wahlbezirk = "Bremen"),
  read_xlsx(glue("{datapath}Stimmzettel 2019 Wahlbereich Bremerhaven Vertrag.xlsx"), skip = 0) |> select(1:5) |> 
    count(`Stimme1`, `Stimme2`, `Stimme3`, `Stimme4`, `Stimme5`) |> mutate(Jahr = 2019, Wahlbezirk = "Bremerhaven"),
  read_xlsx(glue("{datapath}Stimmzettel 2023 Wahlbereich Bremen Vertrag.xlsx"), skip = 1) |> select(1:5) |> 
    count(`Stimme 1`, `Stimme 2`, `Stimme 3`, `Stimme 4`, `Stimme 5`) |> mutate(Jahr = 2023, Wahlbezirk = "Bremen") |> 
    setNames(c(paste0("Stimme",1:5), "n", "Jahr", "Wahlbezirk")),
  read_xlsx(glue("{datapath}/Stimmzettel 2023 Wahlbereich Bremerhaven Vertrag.xlsx"), skip = 1) |> select(1:5) |> 
    count(`Stimme 1`, `Stimme 2`, `Stimme 3`, `Stimme 4`, `Stimme 5`) |> mutate(Jahr = 2023, Wahlbezirk = "Bremerhaven") |> 
    setNames(c(paste0("Stimme",1:5), "n", "Jahr", "Wahlbezirk")) |> 
    mutate(across(starts_with("Stimme"), as.numeric))
) |> write_parquet("data/Stimmzettel.parquet")
