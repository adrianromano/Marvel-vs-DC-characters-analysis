## Load the datasets
setwd("/Users/adrianromano/Downloads/")
info <- read.csv("/Users/adrianromano/Downloads/superhero-set/heroes_information.csv", na.strings = c("-", "-99"))
stats <- read.csv("/Users/adrianromano/Downloads/superhero-set/characters_stats.csv", na.strings = "")
powers <- read.csv("/Users/adrianromano/Downloads/superhero-set/super_hero_powers.csv")

head(info)
head(stats)
head(powers)

library(reshape2)
library(plyr)
library(dplyr)

## Data Preparation
colnames(info)[colnames(info) == "name"] <- "Name"
marvel_dc_info <- info[(info$Publisher == "Marvel Comics" | info$Publisher == "DC Comics"), ]
marvel_dc_info = marvel_dc_info[!duplicated(marvel_dc_info$Name), ]
marvel_dc_info <- marvel_dc_info %>%
    select(Name, Gender, Race, Publisher) %>%
head(marvel_dc_info)

marvel_dc_stats_info <- join(marvel_dc_info, stats, by = "Name", type = "inner")
head(marvel_dc_stats_info)

colnames(powers)[colnames(powers) == "hero_names"] <- "Name"
full_marvel_dc <- join(marvel_dc_stats_info, powers, by = "Name", type = "inner")
head(full_marvel_dc)

marvel_dc <- melt(full_marvel_dc, id = c("Name", "Gender", "Race", "Publisher", "Alignment", "Intelligence", "Strength",
                                          "Speed", "Durability", "Power", "Combat", "Total"))
colnames(marvel_dc)[colnames(marvel_dc) == "variable"] <- "Super.Power"

marvel_dc <- marvel_dc %>%
    filter(value == "True") %>%
    select(-value) 
head(marvel_dc)

colSums(is.na(marvel_dc))

## Data Visualization
library(ggplot2)
library(gridExtra)
library(wesanderson)
marvel_dc$Name <- as.factor(marvel_dc$Name)
marvel_dc$Gender <- as.factor(marvel_dc$Gender)
marvel_dc$Race <- as.factor(marvel_dc$Race)
marvel_dc$Publisher <- as.factor(marvel_dc$Publisher)
marvel_dc$Alignment <- as.factor(marvel_dc$Alignment)
marvel_dc$Super.Power <- as.factor(marvel_dc$Super.Power)

ggplot(marvel_dc, aes(x = Publisher, fill = Publisher)) + 
    geom_bar(stat = "count", aes(fill = Publisher), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Marvel vs DC") +
    theme_classic()

## Gender
marvel_dc_gender <- marvel_dc %>%
    filter(!is.na(Gender)) %>%
    group_by(Gender) %>%
    dplyr::count(Publisher) %>%
    select(Gender, Publisher, Count = n)

ggplot(marvel_dc_gender, aes(x = Gender, y = Count)) +
    geom_bar(stat = "identity", aes(fill = Gender), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Characters", title = "Marvel vs DC Gender Comparison") +
    facet_wrap(~Publisher) +
    theme_bw()

## Race
marvel_race <- marvel_dc %>%
        filter(!is.na(Race)) %>%
        filter(Publisher == "Marvel Comics") %>%
        group_by(Race) %>%
        dplyr::count(Race) %>%
        select(Race, Count = n) %>%
        arrange(-Count)

marvel_race <- ggplot(marvel_race[1:10, ], aes(x = reorder(Race, Count), y = Count)) + 
    geom_bar(stat = "identity", aes(fill = Race), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 10 Marvel Races") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

dc_race <- marvel_dc %>%
    filter(!is.na(Race)) %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Race) %>%
    dplyr::count(Race) %>%
    select(Race, Count = n) %>%
    arrange(-Count)

dc_race <- ggplot(dc_race[1:10, ], aes(x = reorder(Race, Count), y = Count)) + 
    geom_bar(stat = "identity", aes(fill = Race), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 10 DC Races") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()
grid.arrange(marvel_race, dc_race, ncol = 2)

## Alignment
marvel_dc_alignment <- marvel_dc %>%
    filter(!is.na(Alignment)) %>%
    group_by(Alignment) %>%
    dplyr::count(Publisher) %>%
    select(Alignment, Publisher, Count = n)

ggplot(marvel_dc_alignment, aes(x = Alignment, y = Count)) +
    geom_bar(stat = "identity", aes(fill = Alignment), col = "black", alpha = 0.8) +
    labs(x = "", y = "Number of Characters", title = "Heroes vs Villains Comparison") +
    facet_wrap(~Publisher) +
    theme_bw()

## Intelligence
marvel_dc_intelligence <- ggplot(marvel_dc, aes(x = Publisher, y = Intelligence, fill = Publisher)) + 
    geom_boxplot(alpha = 0.5) +
    labs(x = "", title = "Boxplot Comparison of Intelligence") +
    scale_fill_brewer(palette="Spectral") + 
    #guides(fill = FALSE) +
    theme_minimal()

## Strength
marvel_dc_strength <- ggplot(marvel_dc, aes(x = Publisher, y = Strength, fill = Publisher)) + 
    geom_boxplot(alpha = 0.5) +
    labs(x = "", title = "Boxplot Comparison of Strength") +
    scale_fill_manual(values=wes_palette(n=2, name= "Royal1")) + 
    #guides(fill = FALSE) +
    theme_minimal()

## Speed
marvel_dc_speed <- ggplot(marvel_dc, aes(x = Publisher, y = Speed, fill = Publisher)) + 
    geom_boxplot(alpha = 0.5) +
    labs(x = "", title = "Boxplot Comparison of Speed") +
    scale_fill_manual(values=wes_palette(n=2, name= "Moonrise3")) + 
    #guides(fill = FALSE) +
    theme_minimal()

## Durability
marvel_dc_durability <- ggplot(marvel_dc, aes(x = Publisher, y = Durability, fill = Publisher)) + 
    geom_boxplot(alpha = 0.5) +
    labs(x = "", title = "Boxplot Comparison of Durability") +
    scale_fill_brewer(palette="PuOR") + 
    #guides(fill = FALSE) +
    theme_minimal()

## Power
marvel_dc_power <- ggplot(marvel_dc, aes(x = Publisher, y = Power, fill = Publisher)) + 
    geom_boxplot(alpha = 0.5) +
    labs(x = "", title = "Boxplot Comparison of Power") +
    scale_fill_brewer(palette="Dark2") + 
    #guides(fill = FALSE) +
    theme_minimal() 

## Combat
marvel_dc_combat <- ggplot(marvel_dc, aes(x = Publisher, y = Combat, fill = Publisher)) + 
    geom_boxplot(alpha = 0.5) +
    labs(x = "", title = "Boxplot Comparison of Combat Skills") +
    scale_fill_manual(values=wes_palette(n=2, name= "Rushmore")) + 
    #guides(fill = FALSE) +
    theme_minimal() 

grid.arrange(marvel_dc_intelligence, marvel_dc_strength, marvel_dc_speed, 
             marvel_dc_durability, marvel_dc_power, marvel_dc_combat,  ncol = 2)

## Intelligence
marvel_intel <- marvel_dc %>%
    filter(Publisher == "Marvel Comics") %>%
    group_by(Name) %>%
    distinct(Intelligence) %>%
    select(Name, Intelligence) %>%
    arrange(-Intelligence)

marvel_intel <- ggplot(marvel_intel[1:15, ], aes(x = reorder(Name, Intelligence), y = Intelligence)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    ylim(0, 150) +
    labs(x = "", y = "", title = "Top 15 Marvel Characters with the Highest Intelligence") +
    coord_flip() +
    theme_bw()

dc_intel <- marvel_dc %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Name) %>%
    distinct(Intelligence) %>%
    select(Name, Intelligence) %>%
    arrange(-Intelligence)

dc_intel <- ggplot(dc_intel[1:15, ], aes(x = reorder(Name, Intelligence), y = Intelligence)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    ylim(0, 150) +
    labs(x = "", y = "", title = "Top 15 DC Characters with the Highest Intelligence") +
    coord_flip() +
    theme_bw()

grid.arrange(marvel_intel, dc_intel, ncol = 2)

## Strength
marvel_str <- marvel_dc %>%
    filter(Publisher == "Marvel Comics") %>%
    group_by(Name) %>%
    distinct(Strength) %>%
    select(Name, Strength) %>%
    arrange(-Strength)

marvel_str <- ggplot(marvel_str[1:15, ], aes(x = reorder(Name, Strength), y = Strength)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    ylim(0, 150) +
    labs(x = "", y = "", title = "Top 15 Marvel Characters with the Highest Strength") +
    coord_flip() +
    theme_bw()

dc_str <- marvel_dc %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Name) %>%
    distinct(Strength) %>%
    select(Name, Strength) %>%
    arrange(-Strength)

dc_str <- ggplot(dc_str[1:15, ], aes(x = reorder(Name, Strength), y = Strength)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    ylim(0, 150) +
    labs(x = "", y = "", title = "Top 15 DC Characters with the Highest Strength") +
    coord_flip() +
    theme_bw()

grid.arrange(marvel_str, dc_str, ncol = 2)

## Speed
marvel_spd <- marvel_dc %>%
    filter(Publisher == "Marvel Comics") %>%
    group_by(Name) %>%
    distinct(Speed) %>%
    select(Name, Speed) %>%
    arrange(-Speed)

marvel_spd <- ggplot(marvel_spd[1:15, ], aes(x = reorder(Name, Speed), y = Speed)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    ylim(0, 150) +
    labs(x = "", y = "", title = "Top 15 Marvel Characters with the Highest Speed") +
    coord_flip() +
    theme_bw()

dc_spd <- marvel_dc %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Name) %>%
    distinct(Speed) %>%
    select(Name, Speed) %>%
    arrange(-Speed)

dc_spd <- ggplot(dc_spd[1:15, ], aes(x = reorder(Name, Speed), y = Speed)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    ylim(0, 150) +
    labs(x = "", y = "", title = "Top 15 DC Characters with the Highest Speed") +
    coord_flip() +
    theme_bw()

grid.arrange(marvel_spd, dc_spd, ncol = 2)

## Total
ggplot(marvel_dc, aes(x = Publisher, y = Total, fill = Publisher)) + 
    geom_boxplot() +
    labs(x = "", title = "Boxplot Comparison of Overall Stats") +
    scale_fill_brewer(palette="Dark2") + 
    theme_minimal() 

marvel_total <- marvel_dc %>%
    filter(Publisher == "Marvel Comics") %>%
    group_by(Name) %>%
    distinct(Total) %>%
    select(Name, Total) %>%
    arrange(-Total)

marvel_total <- ggplot(marvel_total[1:20, ], aes(x = reorder(Name, Total), y = Total)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 20 Strongest Marvel Characters") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

dc_total <- marvel_dc %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Name) %>%
    distinct(Total) %>%
    select(Name, Total) %>%
    arrange(-Total)

dc_total <- ggplot(dc_total[1:20, ], aes(x = reorder(Name, Total), y = Total)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 20 Strongest DC Characters") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

grid.arrange(marvel_total, dc_total, ncol = 2)

## Super Power
marvel_super <- marvel_dc %>%
    filter(Publisher == "Marvel Comics") %>%
    group_by(Super.Power) %>%
    dplyr::count(Super.Power) %>%
    select(Super.Power, Count = n) %>%
    arrange(-Count)

marvel_super <- ggplot(marvel_super[1:15, ], aes(x = reorder(Super.Power, Count), y = Count)) + 
    geom_bar(stat = "identity", aes(fill = Super.Power), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 15 Marvel Super Powers") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

dc_super <- marvel_dc %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Super.Power) %>%
    dplyr::count(Super.Power) %>%
    select(Super.Power, Count = n) %>%
    arrange(-Count)

dc_super <- ggplot(dc_super[1:15, ], aes(x = reorder(Super.Power, Count), y = Count)) + 
    geom_bar(stat = "identity", aes(fill = Super.Power), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 15 DC Super Powers") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

grid.arrange(marvel_super, dc_super, ncol = 2)

## Number of Super Powers
marvel_power <- marvel_dc %>%
    filter(Publisher == "Marvel Comics") %>%
    group_by(Name) %>%
    dplyr::count(Name) %>%
    select(Name, Count = n) %>%
    arrange(-Count)
    
marvel_power <- ggplot(marvel_power[1:20, ], aes(x = reorder(Name, Count), y = Count)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 20 Marvel Characters with the Highest Number of Super Powers") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

dc_power <- marvel_dc %>%
    filter(Publisher == "DC Comics") %>%
    group_by(Name) %>%
    dplyr::count(Name) %>%
    select(Name, Count = n) %>%
    arrange(-Count)

dc_power <- ggplot(dc_power[1:20, ], aes(x = reorder(Name, Count), y = Count)) + 
    geom_bar(stat = "identity", aes(fill = Name), col = "black", alpha = 0.8) +
    labs(x = "", y = "", title = "Top 20 DC Characters with the Highest Number of Super Powers") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE) +
    theme_bw()

grid.arrange(marvel_power, dc_power, ncol = 2)


