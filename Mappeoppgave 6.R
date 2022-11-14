# Utfordring 6
# Oppgave 6.2.3

# Fjerner alt fra Global Envirement så ingenting kan klusse til koden senere.
rm(list = ls())

# Laster inn nødvendige pakker.
library(sf)
library(plyr)
library(httr) 
library(gdata)
library(dplyr)
library(readr)
library(tibble)
library(rjstat)
library(ggpubr)
library(rstatix)
library(cowplot)
library(janitor)
library(ggplot2)
library(tidyverse)
library(ggtextures)


# Setter lokalet til no-No for å få norsk språk (for å få øæå).
Sys.setlocale(locale="no_NO")

# Setter arbeidsplassen.
setwd("~/")

#lagrer url
data <- "https://data.ssb.no/api/v0/no/table/05111/"
data2 <- "https://data.ssb.no/api/v0/no/table/12441/"

#lagrer etterspørringen til dataene i json stil.
arbeidsledige <- '{  "query": [    {      "code": "ArbStyrkStatus",      "selection": {        "filter": "item",        "values": [          "2"        ]      }    },    {      "code": "Kjonn",      "selection": {        "filter": "item",        "values": [          "1",          "2"        ]      }    },    {      "code": "Alder",      "selection": {        "filter": "item",        "values": [          "15-74",          "20-64",          "20-66",          "15-24",          "25-54",          "55-74"        ]      }    },    {      "code": "ContentsCode",      "selection": {        "filter": "item",        "values": [          "Personer"        ]      }    },    {      "code": "Tid",      "selection": {        "filter": "item",        "values": [          "2005",          "2006",          "2007",          "2008",          "2009",          "2010",          "2011",          "2012",          "2013",          "2014",          "2015",          "2016",          "2017",          "2018",          "2019"        ]      }    }  ],  "response": {    "format": "json-stat2"  }}'
sykefraver <- '{  "query": [    {      "code": "Kjonn",      "selection": {        "filter": "item",        "values": [          "1",          "2"        ]      }    },    {      "code": "NACE2007",      "selection": {        "filter": "item",        "values": [          "00-99"        ]      }    },    {      "code": "Sykefraver2",      "selection": {        "filter": "item",        "values": [          "Alt"        ]      }    },    {      "code": "Tid",      "selection": {        "filter": "item",        "values": [          "2005",          "2006",          "2007",          "2008",          "2009",          "2010",          "2011",          "2012",          "2013",          "2014",          "2015",          "2016",          "2017",          "2018",          "2019"        ]      }    }  ],  "response": {    "format": "json-stat2"  }}'

# Lagre disse i en ny variabel.
arbeidsledige <- data %>%
  POST(body = arbeidsledige, encode = "json")

sykefraver <- data2 %>%
  POST(body = sykefraver, encode = "json")

# Lagre i tibble format.
arbeidsledige <-  arbeidsledige %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()

sykefraver <-  sykefraver %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()

# Fjerner brukte "values".
rm(data, data2)

# "Binder" datasettene sammen.
kjonn <- bind_rows(arbeidsledige, sykefraver)

# Omgjør NA i alder raden til Sykefraver.
kjonn$alder <- kjonn$alder %>% replace_na("Sykefraver") 

# Lager en ny kolonne kallt sykefraver som inneholder sykefraveret.
kjonn <- kjonn %>% 
  mutate(sykefraver = if_else(alder == 'Sykefraver', value, value- value))

# Setter alle 0 til NA.
kjonn[kjonn == 0] <- NA

# Filtrerer for Kvinner.
kjonn_Kvinner <- kjonn %>% 
  filter(kjønn == "Kvinner")

# Filter for Menn.
kjonn_Menn <- kjonn %>% 
  filter(kjønn == "Menn")


# Plotter for Kvinner.
ggplot(kjonn_Kvinner, aes(x = år)) +
  geom_bar(aes(y=value, fill = alder), 
           stat="identity", width=.5, position = "dodge") +
  geom_line(aes(y= sykefraver, group=1), size=1, color="red") +
  scale_y_continuous(name = "Arbeidsledige i prosent %",
                     limits = c(0,80),
                     breaks = c(0,20,40,60,80),
                     sec.axis = sec_axis(~., 
                     name="Sykefravær i prosent %")) +
  labs(title = "Graf over arbeidsledige og sykefravær i prosent % - Kvinner") +
  theme_bw() 


# Plotter for Menn.
ggplot(kjonn_Menn, aes(x = år)) +
  geom_bar(aes(y=value, fill = alder), 
           stat="identity", width=.5, position = "dodge") +
  geom_line(aes(y= sykefraver, group=1), size=1, color="red") +
  scale_y_continuous(name = "Arbeidsledige i prosent %",
                     limits = c(0,80),
                     breaks = c(0,20,40,60,80),
                     sec.axis = sec_axis(~., 
                     name="Sykefravær i prosent %")) +
  labs(title = "Graf over arbeidsledige og sykefravær i prosent % - Menn") +
  theme_bw() 

