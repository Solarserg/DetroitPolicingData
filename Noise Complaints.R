# We will be working with data from the City of Detroit open data portal.
# February 2021 for WSU Center for Behavioral Health and Justice.
# -Sergio Brilanti-Martinez, Student Assistant


library(tidyverse)
sc_original <- read_csv("911_Calls_For_Service.csv")

# Drop unnecessary columns
cols_drop <- c("X","Y","agency","callcode","category","respondingunit","intaketime","neighborhood","block_id","oid","council_district")
sc_master1 <- select(sc_original, -one_of(cols_drop))

# Cleaning the data. Getting rid of:
# NA, extreme values

# Drop rows with NA's in important columns that should always have a data entry
vars_dropna <- c("precinct_sqa","totaltime","calldescription","priority","longtitude","latitude","officerinitiated","incident_id")
sc_master <- drop_na(sc_master1, any_of(vars_dropna))

# Check for any irregular entries in officerinitiated; none found
officerinit <- sc_master %>% 
  group_by(officerinitiated) %>% 
  summarise(count = n())

# Drop rows with negative total time
sc_master <- subset(sc_master, !totaltime < 0)
sc_master <- subset(sc_master, totaltime < 240)

# Separate call_timestamp into year, month, day, day of week, 6 buckets by time of day
sc_master <- separate(sc_master, "call_timestamp", c("date", "time"), sep = " ") %>%
  separate(sc_master, "date", c('year','month','day'), sep = '/')

# longtitude range: -84.13221 | -82.71078, no outliers
# latitude range: 42.08298 | 42.68528, no outliers

# Add all dirty rows to sc_dirty for later analysis and quality assurance
sc_dirty <- anti_join(sc_master1, sc_master, by = 'incident_id')

# Create df with noise complaints
nc <- sc_master[sc_master$calldescription == 'NOISE COMPLAINT',]

# Save file
write_csv(nc, "noise_complaints.csv")
