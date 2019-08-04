# Load data
## https://www.kaggle.com/jmpark746/pga-tour-data-2010-2018
pgaTourData <- read_csv("data/pgaTourData.csv")
##  https://data.world/neilgallen/official-world-golf-ranking
OWGR_Historical <- read_csv("data/OWGR_Historical.csv")

# Add column for year to ranking data
OWGR_Historical <- OWGR_Historical %>% 
  mutate(Year = year(Date))

# Get average ranking for each player and each year
avgRank <- OWGR_Historical %>% 
  group_by(Player, Year) %>% 
  summarise(`Average OWGR` = round(mean(OWGR), 2))

# Get final ranking for each player and each year
finalRank <- OWGR_Historical %>% 
  group_by(Player, Year) %>% 
  filter(Date == max(Date))

# Join dataframes with both rankings
ranks <- inner_join(finalRank, avgRank, by = c("Player", "Year"))

# Get matching columns to join two datasets on
colnames(pgaTourData)[1] <- "Player"

# Convert winnings to numeric
pgaTourData <- pgaTourData %>% 
  mutate(Money = parse_number(Money)/1000000)

# Join rankings and tour data
tourDataCombined <- inner_join(
  pgaTourData %>% mutate(Player = toupper(Player)), ranks, by = c("Player", "Year")
)

# Create columns for just player last name to use as labels
tourDataCombined$LName <- str_split_fixed(tourDataCombined$Player, " ", 2)[,2]

# Convert some NA's to 0's as appropriate
tourDataCombined$Wins[is.na(tourDataCombined$Wins)] <- 0
tourDataCombined$`Top 10`[is.na(tourDataCombined$`Top 10`)] <- 0
tourDataCombined$Points[is.na(tourDataCombined$Points)] <- 0
tourDataCombined$Money[is.na(tourDataCombined$Money)] <- 0

# Drop rows where rounds played is 'NA' and statistics are missing
# tourDataCombined <- tourDataCombined[!is.na(tourDataCombined$Rounds),]
tourDataCombined <- tourDataCombined %>% 
  filter(!is.na(Rounds))

# Rename a few columns
colnames(tourDataCombined)[which(names(tourDataCombined) == "Avg Distance")] <- "Average Driving Distance"
colnames(tourDataCombined)[which(names(tourDataCombined) == "gir")] <- "GIR"
colnames(tourDataCombined)[which(names(tourDataCombined) == "OWGR")] <- "Final OWGR"

# Remove spaces in column names
tourDataCombined <- tourDataCombined %>% dplyr::rename_all(list(~make.names(.)))
