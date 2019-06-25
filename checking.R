rank_data.pop <- rank_data1 %>%
  group_by(Year, Population.type) %>%
  mutate(ordering = rank(-Ref.per.pop)) %>%
  ungroup()


rank_data.pop <- rank_data1 %>%
    filter( !(is.na(Ref.per.pop)) & Population.type %in% c("Refugees (incl. refugee-like situations)")) %>%
    group_by(Year, Population.type) %>%
    mutate(ordering = rank(-Ref.per.pop)) %>%
    ungroup() %>%
    select(Country, Year, Ref.per.pop, ordering, Value2)

rank_data.pop <- rank_data.pop[rank_data.pop$ordering <= 10  , ]
#write.csv(rank_data.pop, "rank_datapop.csv", row.names = FALSE)



rank_data.pop$Country <- as.character(rank_data.pop$Country)

# levels(as.factor(rank_data.pop$ordering))
# View(rank_data.pop[ rank_data.pop$ordering == "9.5" &
#                       rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)"), ])
#
# View(rank_data.pop[  rank_data.pop$Year == "1972"&
#                        rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)"), ])

## Need to fix manually issue when ex-aequo rank


## In 1964
rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1964" &
                         rank_data.pop$Country == "Austria" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 9

rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1964" &
                         rank_data.pop$Country == "Switzerland" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 10


## In 1967
rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1967" &
                         rank_data.pop$Country == "France" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 8

rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1967" &
                         rank_data.pop$Country == "Belgium" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 9

rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1967" &
                         rank_data.pop$Country == "Sudan" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 10

rank_data.pop[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1967" &
                         rank_data.pop$Country == "	Switzerland" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- NULL

## In 1972
rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1972" &
                         rank_data.pop$Country == "Switzerland" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 9

rank_data.pop$ordering[rank_data.pop$ordering == 9.5 &
                         rank_data.pop$Year == "1972" &
                         rank_data.pop$Country == "	Zambia" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 10



## In 1973
rank_data.pop$ordering[rank_data.pop$ordering == 6.5 &
                         rank_data.pop$Year == "1973" &
                         rank_data.pop$Country == "Zambia" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 6

rank_data.pop$ordering[rank_data.pop$ordering == 6.5 &
                         rank_data.pop$Year == "1973" &
                         rank_data.pop$Country == "United Arab Emirates" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 7


## In 2003
rank_data.pop$ordering[rank_data.pop$ordering == 5.5 &
                         rank_data.pop$Year == "2003" &
                         rank_data.pop$Country == "Guinea" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 6

rank_data.pop$ordering[rank_data.pop$ordering == 5.5 &
                         rank_data.pop$Year == "2003" &
                         rank_data.pop$Country == "Zambia" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 5


## In 2009
rank_data.pop$ordering[rank_data.pop$ordering == 6.5 &
                         rank_data.pop$Year == "2009" &
                         rank_data.pop$Country == "Iran (Islamic Rep. of)" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 6

rank_data.pop$ordering[rank_data.pop$ordering == 6.5 &
                         rank_data.pop$Year == "2009" &
                         rank_data.pop$Country == "Djibouti" &
                         rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)")] <- 7

## Regnerate factors modality -
rank_data.pop$Country <- as.factor(as.character(rank_data.pop$Country))



#check <- as.data.frame(unique(rank_data.pop[ ,c("Country","Year", "ordering")]))
## check our value for rank -- Note that there are different tie method
levels(as.factor(rank_data.pop$ordering))

View(rank_data.pop[ rank_data.pop$ordering == "5.5" &
              rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)"), ])

View(rank_data.pop[  rank_data.pop$Year == "2003"&
                       rank_data.pop$Population.type %in% c("Refugees (incl. refugee-like situations)"), ])


  rank_data.gdp <- rank_data1 %>%
  group_by(Year, Population.type) %>%
  mutate(ordering = rank(-Ref.per.gdp)) %>%
  ungroup() %>%
  select(Country, Year, Ref.per.gdp, ordering, Value2)
rank_data.gdp <- rank_data.gdp[rank_data.gdp$ordering <= 10, ]

rank_data.cap <- rank_data1 %>%
  group_by(Year, Population.type) %>%
  mutate(ordering = rank(-Ref.per.gdp.cap)) %>%
  ungroup() %>%
  select(Country, Year, Ref.per.gdp.cap, ordering, Value2)
rank_data.cap <- rank_data.cap[rank_data.cap$ordering <= 10, ]
