library(tidyverse)
library(glue)
library(arrow)
library(writexl)
library(readxl)

# BASICS 

## Parteifarben 
Parteifarben <- tibble(
  Partei = c("SPD","CDU","GRÜNE","FDP","DIE LINKE","BIW","AfD"), 
  Farbe = c("#ff0000", "#000000", "#009933","#ced121","#990033", "#ff9900", "#0099ff" )
)
write_parquet(Parteifarben, "data/Parteifarben.parquet")

## Load prepared raw data
Stimmzettel <- read_parquet("data/Stimmzettel.parquet")  
Kandidaten <- read_parquet("data/Kandidaten.parquet")
Listen <- read_parquet("data/Listen.parquet")

## Stimmzettel (=ballots) with numbers stripped down to nurParteien (=only parties)
## e.g. 101 -> 100, 234 -> 200, ...
Stimmzettel_nurParteien <- Stimmzettel |> 
  mutate(across(starts_with("Stimme"), \(x) 100 * (x %/% 100))) |> 
  mutate(ID = 1:n()) |> 
  pivot_longer(starts_with("Stimme")) |> 
  mutate(value = sort(value, na.last = TRUE), .by = ID) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  select(-ID) |> 
  summarize(n = sum(n), .by = c(starts_with("Stimme"), Jahr, Wahlbezirk)) 
Stimmzettel_nurParteien |> write_parquet("data/Stimmzettel_nurParteien.parquet")

# STATS

## Stimmzettel 
getmode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  ta <- tabulate(match(v, uniqv))
  ifelse(length(ta) == 1 | diff(sort(ta, decreasing = TRUE))[1] < 0, uniqv[which.max(ta)], NA)
}
Stimmzettel <- Stimmzettel |> mutate(ID = 1:n()) 
Stimmzettel_Stats <- Stimmzettel |> 
  pivot_longer(Stimme1:Stimme5, values_to = "Stimme") |> 
  summarize(num_valid = sum(!is.na(Stimme)),
            num_unique = sum(!is.na(unique(Stimme))),
            num_most = sort(tabulate(match(Stimme[!is.na(Stimme)], unique(Stimme[!is.na(Stimme)]))), decreasing = TRUE)[1],
            num_parties = sum(!is.na(unique(floor(Stimme/100)))),
            main_vote = getmode(Stimme),
            main_party = getmode(floor(Stimme/100)),
            full_cum = num_unique == 1,
            one_party = num_parties == 1, 
            only_party = all((Stimme %% 100) == 0, na.rm = TRUE), 
            only_person = all((Stimme %% 100) > 0, na.rm = TRUE), 
            only_lead = all((Stimme %% 100) == 1, na.rm = TRUE),
            no_lead = all((Stimme %% 100) > 1 , na.rm = TRUE),
            .by = "ID") |> 
  right_join(Stimmzettel, by = "ID") |> 
  select(Jahr, Wahlbezirk, n, Stimme1:Stimme5, num_valid, main_vote, main_party, everything(), -ID)
Stimmzettel_Stats |> write_parquet("data/Stimmzettel_Stats.parquet")

## Kandidaten und Listen
Auszählung <- Stimmzettel |> 
  mutate(ID = 1:n(), .by = c("Jahr", "Wahlbezirk")) |> 
  pivot_longer(starts_with("Stimme"), names_to = "Stimme", values_to = "Kennnummer") |>
  filter(!is.na(Kennnummer)) |> 
  summarize(Stimmen = sum(n), # collecting all votes per voter 
            Wähler = max(n), 
            .by = c("Jahr","Wahlbezirk","Kennnummer","ID")) |> 
  summarize(Stimmen = sum(Stimmen), 
            Wähler = sum(Wähler), 
            .by = c("Jahr","Wahlbezirk","Kennnummer")) |> 
  mutate(Stimmen_pro_Wähler = Stimmen/Wähler) |> 
  arrange(Jahr, Wahlbezirk, Kennnummer)
Kandidaten_Stats <- Kandidaten |> 
  left_join(Listen, by = join_by(`Name Partei/Wählervereinigung`, Kurzform, Jahr, Wahlbezirk)) |>
  mutate(Kennnummer = `Liste Nr.` + Listenplatz, Typ = "Person") |> 
  left_join(Auszählung, join_by(Jahr, Wahlbezirk, Kennnummer)) |> 
  mutate(Stimmenrang = n() + 1 - rank(Stimmen, ties.method = "max"),
         Wählerrang = n() + 1 - rank(Wähler, ties.method = "max"),
         .by = c(Jahr, Wahlbezirk, Kurzform))
Kandidaten_Stats |> write_parquet("data/Kandidaten_Stats.parquet")
Listen_Stats <- Listen |> mutate(Kennnummer = `Liste Nr.`, Typ = "Liste") |> 
  left_join(Auszählung, join_by(Jahr, Wahlbezirk, Kennnummer))
Listen_Stats |> write_parquet("data/Listen_Stats.parquet")

## Wahlvorschläge = Listen und Kandidaten
Wahlvorschläge <- bind_rows(
  Kandidaten_Stats |> mutate(Name = paste0(if_else(is.na(Titel), "", paste0(Titel, " ")), Vorname, " ", Name)) |> 
    select(Jahr, Wahlbezirk, Typ, Kennnummer, Name, Partei = Kurzform, Stimmen, Wähler, Stimmen_pro_Wähler),
  Listen_Stats |> select(Jahr, Wahlbezirk, Kennnummer, Typ, Name = `Name Partei/Wählervereinigung`, Partei = Kurzform,
                         Stimmen, Wähler, Stimmen_pro_Wähler)
) |> arrange(Jahr, Wahlbezirk, Kennnummer)
Wahlvorschläge |> write_parquet("data/Wahlvorschläge.parquet")



# MANDATES / RESULTS (Mandate / Wahlergebnisse)

zuteilen_nach_hoechstzahlen <- function(N, Stimmen) {
  Stimmen <- Stimmen/sum(Stimmen)
  divisoren <- (1:N)-0.5 # divisorfolge="saintlague"
  hoechstzahlen <- purrr::as_vector(purrr::map(Stimmen, function(x) x / divisoren))
  partei_labels <- rep(1:length(Stimmen), each = N)
  mandate <- partei_labels[order(hoechstzahlen, decreasing = TRUE)[1:N]]
  tabulate(mandate, nbins = length(Stimmen))
}
# Schritt 1: Parteimandate
Sitze <- expand_grid(Jahr = seq(2011, 2023, by = 4), Wahlbezirk = c("Bremen","Bremerhaven")) |> 
  mutate(Sitze = c(68, 15, 68, 15, 69, 15, 72, 15))
Auszählung_Parteien <- Wahlvorschläge |> 
  summarize(Personenstimmen = sum(if_else(Typ == "Person", Stimmen, 0)),
            Personenwähler = sum(if_else(Typ == "Person", Wähler, 0)),
            Stimmen = sum(Stimmen),
            Wähler = sum(Wähler),
            .by = c("Jahr", "Wahlbezirk", "Partei"))
Relevanzschwellen <- Sitze |> 
  left_join(Auszählung_Parteien |> 
              summarize(across(c(Stimmen, Personenstimmen, Wähler, Personenwähler),sum),
                        .by = c("Jahr", "Wahlbezirk")),
            by = join_by(Jahr, Wahlbezirk)) |>  
  mutate(HareQuota = Stimmen/Sitze,
         # DroopQuota = floor(Stimmen/(Sitze + 1))+1,
         HareQuotaPersonenstimmen = Personenstimmen/Sitze, 
         HareQuotaPersonenwähler = Personenwähler/Sitze, 
         Relevanzschwelle = HareQuota/2/5,
         RelevanzschwellePersonenwähler = HareQuotaPersonenwähler/2/5,
         RelevanzschwellePersonenstimmen = HareQuotaPersonenstimmen/2/5)
Mandate_Parteien <- Auszählung_Parteien |> 
  mutate(Anteil = Stimmen/sum(Stimmen), .by = c("Jahr", "Wahlbezirk")) |> 
  filter(Anteil >= 0.05) |> 
  left_join(Sitze, by = join_by(Jahr, Wahlbezirk)) |> 
  mutate(Ideal_Mandate = Stimmen/sum(Stimmen) * max(Sitze),
         Mandate = zuteilen_nach_hoechstzahlen(max(Sitze), Stimmen), .by = c("Jahr", "Wahlbezirk"))
# Schritt 2: Listen- und Personenbank
Mandate_ListePersonen <- Wahlvorschläge |> 
  summarize(Stimmen = sum(Stimmen), .by = c(Jahr, Wahlbezirk, Partei, Typ)) |> 
  left_join(Mandate_Parteien |> select(Jahr, Wahlbezirk, Partei, Mandate), 
            by = join_by(Jahr, Wahlbezirk, Partei)) |> 
  filter(!is.na(Mandate)) |> 
  mutate(Ideal_Mandate = Stimmen/sum(Stimmen) * max(Mandate),
         Mandate = zuteilen_nach_hoechstzahlen(max(Mandate), Stimmen), 
         .by = c(Jahr, Wahlbezirk, Partei))
# Schritt 3: Personen
n_lowest <- function(v, n) ifelse(length(sort(v)[n])==0,0,sort(v)[n])
Mandate_Kandidaten <- Kandidaten_Stats |> 
  left_join(Relevanzschwellen |> select(Jahr, Wahlbezirk, starts_with("Relevanzschwelle")), by = join_by(Jahr, Wahlbezirk)) |> 
  # mutate(Stimmenrang = n() + 1 - rank(Stimmen, ties.method = "max"),
  #        Wählerrang = n() + 1 - rank(Wähler, ties.method = "max"),
  #        .by = c(Jahr, Wahlbezirk, Kurzform)) |>  
  left_join(Mandate_ListePersonen |> select(Jahr, Wahlbezirk, Partei, Typ, Mandate) |> 
              pivot_wider(names_from = Typ, values_from = Mandate), 
            by = join_by(Jahr, Wahlbezirk, Kurzform == Partei)) |> 
  replace_na(list(Liste = 0, Person = 0)) |> 
  mutate(
    # erstPerson
    Mandat_erstPerson = if_else(Stimmenrang <= Person, "Person", ""), 
    RestListenplatz = if_else(Mandat_erstPerson == "Person", NA, Listenplatz), 
    Mandat_erstPerson = if_else(Mandat_erstPerson != "Person" & Listenplatz <= n_lowest(RestListenplatz, max(Liste)), 
                                "Liste", Mandat_erstPerson), 
    # erstListe
    Mandat_erstListe = if_else(Listenplatz <= Liste, "Liste", ""), 
    RestStimmenrang = if_else(Mandat_erstListe == "Liste", NA, Stimmenrang), 
    Mandat_erstListe = if_else(Mandat_erstListe != "Liste" & Stimmenrang <= n_lowest(RestStimmenrang, max(Person)), 
                               "Person", Mandat_erstListe),
    # nurListe
    Mandat_nurListe = if_else(Listenplatz <= Liste + Person, "Liste", ""),
    # nur Stimmenrang
    Mandat_nurStimmenrang = if_else(Stimmenrang <= Liste + Person, "Person", ""),
    # nur Wählerrang
    Mandat_nurWählerrang = if_else(Wählerrang <= Liste + Person, "Person", ""),
    # nur StimmenrangRelevanzschwelle
    Mandat_nurStimmenrangRelevanzschwelle = if_else((Stimmenrang <= Liste + Person) & 
                                                      Stimmen >= Relevanzschwelle, "Person", ""), 
    RestListenplatz = if_else(Mandat_nurStimmenrangRelevanzschwelle == "Person", NA, Listenplatz), 
    IrrelevantStimmenrang = max(Liste) + max(Person) - sum(Mandat_nurStimmenrangRelevanzschwelle == "Person"),
    Mandat_nurStimmenrangRelevanzschwelle = 
      if_else(Mandat_nurStimmenrangRelevanzschwelle != "Person" & 
                Listenplatz <= n_lowest(RestListenplatz, max(IrrelevantStimmenrang)), 
              "Liste", Mandat_nurStimmenrangRelevanzschwelle), 
    # nur StimmenrangRelevanzschwelle1000
    Mandat_nurStimmenrangRelevanzschwelle1000 = if_else((Stimmenrang <= Liste + Person) & 
                                                      Stimmen >= 1000, "Person", ""), 
    RestListenplatz = if_else(Mandat_nurStimmenrangRelevanzschwelle1000 == "Person", NA, Listenplatz), 
    IrrelevantStimmenrang = max(Liste) + max(Person) - sum(Mandat_nurStimmenrangRelevanzschwelle1000 == "Person"),
    Mandat_nurStimmenrangRelevanzschwelle1000 = 
      if_else(Mandat_nurStimmenrangRelevanzschwelle1000 != "Person" & 
                Listenplatz <= n_lowest(RestListenplatz, max(IrrelevantStimmenrang)), 
              "Liste", Mandat_nurStimmenrangRelevanzschwelle1000), 
    # nur WählerrangRelevanzschwelle
    Mandat_nurWählerrangRelevanzschwelle = if_else((Wählerrang <= Liste + Person) & 
                                                     Wähler >= Relevanzschwelle, "Person", ""), 
    RestListenplatz = if_else(Mandat_nurWählerrangRelevanzschwelle == "Person", NA, Listenplatz), 
    IrrelevantWählerrang = max(Liste) + max(Person) - sum(Mandat_nurWählerrangRelevanzschwelle == "Person"),
    Mandat_nurWählerrangRelevanzschwelle = 
      if_else(Mandat_nurWählerrangRelevanzschwelle != "Person" & 
                Listenplatz <= n_lowest(RestListenplatz, max(IrrelevantWählerrang)), 
              "Liste", Mandat_nurWählerrangRelevanzschwelle), 
    # nur WählerrangRelevanzschwelle1000
    Mandat_nurWählerrangRelevanzschwelle1000 = if_else((Wählerrang <= Liste + Person) & 
                                                         Wähler >= 1000, "Person", ""), 
    RestListenplatz = if_else(Mandat_nurWählerrangRelevanzschwelle1000 == "Person", NA, Listenplatz), 
    IrrelevantWählerrang = max(Liste) + max(Person) - sum(Mandat_nurWählerrangRelevanzschwelle1000 == "Person"),
    Mandat_nurWählerrangRelevanzschwelle1000 = 
      if_else(Mandat_nurWählerrangRelevanzschwelle1000 != "Person" & 
                Listenplatz <= n_lowest(RestListenplatz, max(IrrelevantWählerrang)), 
              "Liste", Mandat_nurWählerrangRelevanzschwelle1000), 
    # nur WählerrangRelevanzschwelle800
    Mandat_nurWählerrangRelevanzschwelle800 = if_else((Wählerrang <= Liste + Person) & 
                                                         Wähler >= 800, "Person", ""), 
    RestListenplatz = if_else(Mandat_nurWählerrangRelevanzschwelle800 == "Person", NA, Listenplatz), 
    IrrelevantWählerrang = max(Liste) + max(Person) - sum(Mandat_nurWählerrangRelevanzschwelle800 == "Person"),
    Mandat_nurWählerrangRelevanzschwelle800 = 
      if_else(Mandat_nurWählerrangRelevanzschwelle800 != "Person" & 
                Listenplatz <= n_lowest(RestListenplatz, max(IrrelevantWählerrang)), 
              "Liste", Mandat_nurWählerrangRelevanzschwelle800), 
    # nur WählerrangRelevanzschwellePersonenstimmen
    Mandat_nurWählerrangRelevanzschwellePersonenstimmen = 
      if_else((Wählerrang <= Liste + Person) & 
                Wähler >= RelevanzschwellePersonenstimmen, "Person", ""), 
    RestListenplatz = if_else(Mandat_nurWählerrangRelevanzschwellePersonenstimmen == "Person", NA, Listenplatz), 
    IrrelevantWählerrangRelevanzschwellePersonenstimmen = max(Liste) + max(Person) - sum(Mandat_nurWählerrangRelevanzschwellePersonenstimmen == "Person"),
    Mandat_nurWählerrangRelevanzschwellePersonenstimmen = 
      if_else(Mandat_nurWählerrangRelevanzschwellePersonenstimmen != "Person" & 
                Listenplatz <= n_lowest(RestListenplatz, max(IrrelevantWählerrangRelevanzschwellePersonenstimmen)), 
              "Liste", Mandat_nurWählerrangRelevanzschwellePersonenstimmen), 
    .by = c(Jahr, Wahlbezirk, Kurzform)) |> 
  select(-RestListenplatz, -RestStimmenrang)
Relevanzschwellen |> write_parquet("data/Relevanzschwellen.parquet")
Mandate_Parteien |> write_parquet("data/Mandate_Parteien.parquet")
Mandate_ListePersonen |> write_parquet("data/Mandate_ListePersonen.parquet")
Mandate_Kandidaten |> write_parquet("data/Mandate_Kandidaten.parquet")

## Rausschreiben Excel-Tabellen mit Mandaten nach verschiedenen Auszählungen und 
## Übersicht Relevanzschwelle nach Vorschlag Grüne

Mandate_Kandidaten |> 
  mutate(Mandate_Partei = Liste + Person,
         Relevanzschwelle = ceiling(Relevanzschwelle),
         Stimmen_pro_Wähler = round(Stimmen_pro_Wähler, digits = 2), 
         Relevant_Stimmenrang = Mandate_Partei - IrrelevantStimmenrang, 
         Relevant_Wählerrang = Mandate_Partei - IrrelevantWählerrang) |> 
  select(Jahr, Wahlbezirk, Partei = Kurzform, Kennnummer, Titel, Vorname, Name, Geschlecht, Geburtsjahr, Beruf, 
         `Stadt- oder Ortsteil`, Stimmen, Wähler, Stimmen_pro_Wähler, Stimmenrang, Wählerrang,
         Mandate_Partei, Listenbank = Liste, Personenbank = Person, Relevanzschwelle, Relevant_Stimmenrang, Relevant_Wählerrang, 
         starts_with("Mandat")) |> 
  write_xlsx("data/Modellrechnung_Kandidaten.xlsx")

read_xlsx("data/Modellrechnung_Kandidaten.xlsx") |> 
  summarize(across(c(Mandate_Partei, Listenbank, Personenbank, Relevanzschwelle, Relevant_Stimmenrang, Relevant_Wählerrang), max),
            .by = c(Jahr, Wahlbezirk, Partei)) |> 
  filter(Mandate_Partei>0) |> 
  write_xlsx("data/Übersicht_PersonenBank_Relevanz.xlsx")


# CO-VOTERS

## Dissimilarity measures / Conditional probabilities of co-voting
num_covoters <- function(x,y,...) sum(x>0 & y>0) # symmertric
frac_covoters_x <- function(x,y,...) num_covoters(x,y)/sum(x>0) # non-symmetric
frac_covoters_y <- function(x,y,...) num_covoters(x,y)/sum(y>0) # non-symmetric
frac_covoters_joint <- function(x,y,...) num_covoters(x,y)/sum(x>0 | y>0) # symmetric
frac_covoters_all <- function(x,y,...) num_covoters(x,y)/length(x) # no maximum of identity
dissim_voters_x <- function(x,y,...) 1 - frac_covoters_x(x,y) # non-symmertric, identity is 0
dissim_voters_sym <- function(x,y,...) 1 - frac_covoters_joint(x,y) # symmertric, identity is zero

## Co-Voter Pairs, MDS on dissim

Stimmzettel_nurParteien_reformatted <- 
  Stimmzettel_nurParteien |>
  mutate(ID = 1:n(), .by = c(Jahr, Wahlbezirk), .after = Wahlbezirk) |> 
  select(Jahr, Wahlbezirk, ID, n, starts_with("Stimme")) |> 
  nest(.by = c(Jahr, Wahlbezirk)) |> 
  mutate(Waehler_Wahlvorschlag_Matrix = data |> 
           map(\(df) df |> 
                 pivot_longer(starts_with("Stimme")) |> 
                 select(-name) |> 
                 count(ID, n, value, name = "votes") |> 
                 pivot_wider(names_from = value, values_from = votes, values_fill = 0)
           ), 
         Wahlvorschlag_pairs = Waehler_Wahlvorschlag_Matrix |>
           # Create pairs df
           map(\(w) w |> 
                 uncount(weights = n) |> select(-ID) |>
                 summarize(across(everything(), list)) |> # nest
                 pivot_longer(everything()) |> # pivot
                 mutate(name = parse_integer(name)) |>
                 expand(nesting(name,value),
                        nesting(name2 = name,value2 = value)) |> 
                 mutate(co_voters = map2_dbl(value, value2, num_covoters, .progress = TRUE),
                        dissim_voters_x = map2_dbl(value, value2, dissim_voters_x, .progress = TRUE),
                        dissim_voters_sym = map2_dbl(value, value2, dissim_voters_sym, .progress = TRUE),
                        dissim_voters_avgxy = map2_dbl(value, value2, 
                                                       \(v1,v2) (dissim_voters_x(v1,v2)+dissim_voters_x(v2,v1))/2, 
                                                       .progress = TRUE)
                 ) |> 
                 select(-value, -value2)
               ), 
           # Append partynames 
         Wahlvorschlag_pairs = 
           pmap(list(wp = Wahlvorschlag_pairs, j = Jahr, wb = Wahlbezirk), \(wp,j,wb) wp  |> 
                  left_join(Listen_Stats |> filter(Jahr == j, Wahlbezirk == wb) |>  
                              select(name = Kennnummer, partyname = Kurzform), by = join_by(name)) |> 
                  left_join(Listen_Stats |> filter(Jahr == j, Wahlbezirk == wb) |>  
                              select(name2 = Kennnummer, partyname2 = Kurzform), by = join_by(name2))
                )
         )

tidy_cmdscale <- function(df) {
  df_matrix <- df |> 
    pivot_wider(names_from = name2)
  bind_cols(
    df_matrix |> select(name, partyname),
    df_matrix |> select(-name, -partyname) |> 
      cmdscale() |> as_tibble() |> set_names(c("MDS_x","MDS_y"))
  )
}

Stimmzettel_nurParteien_MDS <- Stimmzettel_nurParteien_reformatted |> mutate(
  MDS_dissim_voters_sym = Wahlvorschlag_pairs |> 
    map(\(wp) wp |> na.omit() |> 
          select(name, partyname, name2, value = dissim_voters_sym) |> 
          tidy_cmdscale() |> 
          rename(MDS_x_dissimsym = MDS_x, MDS_y_dissimsym = MDS_y)),
  MDS_dissim_voters_avgxy = Wahlvorschlag_pairs |> 
    map(\(wp) wp |> na.omit() |> 
          select(name, partyname, name2, value = dissim_voters_avgxy) |> 
          tidy_cmdscale() |> 
          rename(MDS_x_dissimavgxy = MDS_x, MDS_y_dissimavgxy = MDS_y)), 
  Wahlvorschlag_pairs_mainparties = Wahlvorschlag_pairs |> map(
    \(wp) wp |> filter(partyname %in% Parteifarben$Partei, partyname2 %in% Parteifarben$Partei)),
  Wahlvorschlag_pairs_mainparties_norightwing = Wahlvorschlag_pairs |> map(
    \(wp) wp |> filter(partyname %in% setdiff(Parteifarben$Partei, c("AfD","BIW")), 
                       partyname2 %in% setdiff(Parteifarben$Partei, c("AfD","BIW")))), 
  MDS_dissim_voters_avgxy_mainparties = Wahlvorschlag_pairs_mainparties |> 
    map(\(wp) wp  |> na.omit() |> 
          select(name, partyname, name2, value = dissim_voters_avgxy) |> 
          tidy_cmdscale() |> 
          rename(MDS_dissimavgxy_x_mainparties = MDS_x, MDS_dissimavgxy_y_mainparties = MDS_y)),
  MDS_dissim_voters_avgxy_mainparties_norightwing = Wahlvorschlag_pairs_mainparties_norightwing |> 
    map(\(wp) wp  |> na.omit() |> 
          select(name, partyname, name2, value = dissim_voters_avgxy) |> 
          tidy_cmdscale() |> 
          rename(MDS_dissimavgxy_x_mainparties_norightwing = MDS_x, MDS_dissimavgxy_y_mainparties_norightwing = MDS_y))
)
Stimmzettel_nurParteien_MDS$Wahlvorschlag_pairs_mainparties[[7]] |> write_parquet("CoVotersBremen2023.parquet")   

Listen_Stats_MDS <- Listen_Stats |>  
  left_join(Stimmzettel_nurParteien_MDS |> 
              select(Wahlbezirk, Jahr, MDS_dissim_voters_avgxy) |> 
              unnest(cols = c(MDS_dissim_voters_avgxy)), 
            by = join_by(Wahlbezirk, Jahr, Kurzform == partyname, Kennnummer == name)
  ) |> 
  left_join(Stimmzettel_nurParteien_MDS |> 
              select(Wahlbezirk, Jahr, MDS_dissim_voters_avgxy_mainparties) |> 
              unnest(cols = c(MDS_dissim_voters_avgxy_mainparties)), 
            by = join_by(Wahlbezirk, Jahr, Kurzform == partyname, Kennnummer == name)
  ) |> 
  left_join(Stimmzettel_nurParteien_MDS |> 
              select(Wahlbezirk, Jahr, MDS_dissim_voters_avgxy_mainparties_norightwing) |> 
              unnest(cols = c(MDS_dissim_voters_avgxy_mainparties_norightwing)), 
            by = join_by(Wahlbezirk, Jahr, Kurzform == partyname, Kennnummer == name)
  ) |> 
 # Hand-pick flip and reverses of dimensions to make MDS comparable
  left_join(
    expand_grid(Jahr = unique(Listen$Jahr), Wahlbezirk = unique(Listen$Wahlbezirk)) |> 
      mutate(new_x_MDSavgxy = c( "x",  "y", "-x", "y", "x", "y", "x", "x"), 
             new_y_MDSavgxy = c("-y", "-x", "-y", "x", "y","-x", "y","-y")), 
    by = join_by("Jahr", "Wahlbezirk")
  ) |> 
  # Apply the flip and reverse
  mutate(
    x = case_when(new_x_MDSavgxy == "x"  ~  MDS_x_dissimavgxy, 
                  new_x_MDSavgxy == "-x" ~ -MDS_x_dissimavgxy, 
                  new_x_MDSavgxy == "y"  ~  MDS_y_dissimavgxy, 
                  new_x_MDSavgxy == "-y" ~ -MDS_y_dissimavgxy),
    y = case_when(new_y_MDSavgxy == "x"  ~  MDS_x_dissimavgxy, 
                  new_y_MDSavgxy == "-x" ~ -MDS_x_dissimavgxy, 
                  new_y_MDSavgxy == "y"  ~  MDS_y_dissimavgxy, 
                  new_y_MDSavgxy == "-y" ~ -MDS_y_dissimavgxy)
  )
Listen_Stats_MDS |> write_parquet("data/Listen_Stats_MDS.parquet")


# Stimmen
Mandate_Kandidaten |> 
  summarize(Stimmen = sum(Stimmen), Wähler = sum(Wähler), .by=c(Jahr, Wahlbezirk)) |> 
  mutate(Stimmen_pro_Wähler = Stimmen/Wähler)


## Politischer Raum nur relevante Parteien

Listen_Stats_MDS |> left_join(Parteifarben, by = join_by(Kurzform == Partei)) |>
  mutate(Farbe = if_else(is.na(Farbe), "gray50", Farbe)) |> 
  ggplot() +
  aes(x = MDS_dissimavgxy_x_mainparties, 
      y = MDS_dissimavgxy_y_mainparties, 
      color = Farbe, label = Kurzform, size = Wähler) + 
  geom_point(alpha=0.8) +
  geom_text_repel(size = 2) +
  facet_grid(Wahlbezirk ~ Jahr) +
  coord_equal() + 
  scale_color_identity() +
  scale_size_area() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "lightgray", fill = NA))

## Politischer Raum nur relevante Parteien ohne rechtspopulisitische

Listen_Stats_MDS |> left_join(Parteifarben, by = join_by(Kurzform == Partei)) |>
  mutate(Farbe = if_else(is.na(Farbe), "gray50", Farbe)) |> 
  ggplot() +
  aes(x = MDS_dissimavgxy_x_mainparties_norightwing, 
      y = MDS_dissimavgxy_y_mainparties_norightwing, 
      color = Farbe, label = Kurzform, size = Wähler) + 
  geom_point(alpha=0.8) +
  geom_text_repel(size = 2) +
  facet_grid(Wahlbezirk ~ Jahr) +
  coord_equal() + 
  scale_color_identity() +
  scale_size_area() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "lightgray", fill = NA))


