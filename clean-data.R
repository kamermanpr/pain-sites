############################################################
#                                                          #
#              Clean pain sites project data               #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(skimr)
library(knitr)

# Set skimr outputs
skim <- skim_with(factor = sfl(ordered = NULL),
                  numeric = sfl(hist = NULL))

# Make data-cleaned directory
if(!dir.exists('data-cleaned')) {
    dir.create('data-cleaned')
}

############################################################
#                                                          #
#                  De-identification step                  #
#                                                          #
############################################################
# In some cases DOB was given, this needed to be converted to 
# age in years for de-identification purposes
# 
# # Load packages
# library(tidyverse)
# library(lubridate)
#
# # Import data
# ## data-original.csv is not available to outside individuals or organizations 
# data <- read_csv('data-original/data-original.csv')
# 
# # Calculate age to years (part of de-identification step)
# data <- data %>%
#     mutate(Age2 = round(interval(`Date of birth`, `Date of visit`)/years(1))) %>% 
#     mutate(Age = ifelse(is.na(Age),
#                         yes = Age2,
#                         no = Age)) %>% 
#     select(-Age2, -`Date of birth`, -`Date of visit`)
# 
# # Write data
# write_csv(data, path = 'data-original/data-deidentified.csv')

############################################################
#                                                          #
#                 Clean-up of all columns                  #
#                                                          #
############################################################
# Import data 
data <- read_csv('data-original/data-deidentified.csv')

# Remove all (choice=No) columns
data <- data %>% 
    select(-ends_with('No)'))

# Rename Record ID
data <- data %>% 
    rename(ID = `Record ID`)

# Extract unique IDs
data <- data %>% 
    distinct(ID, .keep_all = TRUE) %>% 
    filter(!is.na(ID))

# Clean head/face data
data <- data %>% 
    mutate(Head = case_when(
        `Head (choice=Yes)` == 'Checked' |
            `Head (choice=Front)` == 'Checked' |
            `Head (choice=Back)` == 'Checked' |
            `Head (choice=Left)` == 'Checked' |
            `Head (choice=Right)` == 'Checked' |
            `Face (choice=Yes)` == 'Checked' |
            `Face (choice=Front)` == 'Checked' |
            `Face (choice=Left)` == 'Checked' |
            `Face (choice=Right)` == 'Checked' |
            `Face (choice=Back)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-Record, -starts_with('Head ('), -starts_with('Face (')) %>% 
    select(ID, Head, everything())

# Clean neck and cervical spine
data <- data %>% 
    mutate(Throat = case_when(
        `Neck (choice=Yes)` == 'Checked' |
            `Neck (choice=Yes)` == 'Checked' |
            `Neck (choice=Front)` == 'Checked' |
            `Neck (choice=Left)` == 'Checked' |
            `Neck (choice=Right)` == 'Checked' |
            `cervical (choice=Front)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Cervical_spine = case_when(
        `cervical (choice=Yes)` == 'Checked' |
            `cervical (choice=Back)` == 'Checked' |
            `cervical (choice=Left)` == 'Checked' |
            `cervical (choice=Right)` == 'Checked' |
            `Neck (choice=Back)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Neck ('), -starts_with('cervical (')) %>% 
    select(ID, Head, Throat, Cervical_spine, everything())

# Clean shoulder
data <- data %>% 
    mutate(Shoulder = case_when(
        `Shoulder (choice=Yes)` == 'Checked' |
            `Shoulder (choice=Front)` == 'Checked' |
            `Shoulder (choice=Back)` == 'Checked' |
            `Shoulder (choice=Left)` == 'Checked' |
            `Shoulder (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Shoulder_bilateral = case_when(
        `Shoulder (choice=Left)` == 'Checked' &
            `Shoulder (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Shoulder (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, everything())

# Clean Arm/Forearm
## Group arm and forearm into one site
data <- data %>% 
    mutate(Arms = case_when(
        `Arm (choice=Yes)` == 'Checked' |
            `Arm (choice=Front)` == 'Checked' |
            `Arm (choice=Back)` == 'Checked' |
            `Arm (choice=Left)` == 'Checked' |
            `Arm (choice=Right)` == 'Checked' |
            `Forearm (choice=Front)` == 'Checked' |
            `Forearm (choice=Back)` == 'Checked' |
            `Forearm (choice=Left)` == 'Checked' |
            `Forearm (choice=Right)` == 'Checked' |
            `Forearm (choice=Yes)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Arm ('), -starts_with('Forearm (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, Arms, everything())

# Clean elbow
data <- data %>% 
    mutate(Elbows = case_when(
        `Elbow (choice=Yes)` == 'Checked' |
            `Elbow (choice=Front)` == 'Checked' |
            `Elbow (choice=Back)` == 'Checked' |
            `Elbow (choice=Left)` == 'Checked' |
            `Elbow (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Elbows_bilateral = case_when(
        `Elbow (choice=Left)` == 'Checked' &
            `Elbow (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Elbow (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, Arms, Elbows, Elbows_bilateral, everything())

# Clean wrists/hand
data <- data %>% 
    mutate(Wrists.Hands = case_when(
        `Wrist and Hand (choice=Yes)` == 'Checked' |
            `Wrist and Hand (choice=Front)` == 'Checked' |
            `Wrist and Hand (choice=Back)` == 'Checked' |
            `Wrist and Hand (choice=Left)` == 'Checked' |
            `Wrist and Hand (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Wrists.Hands_bilateral = case_when(
        `Wrist and Hand (choice=Left)` == 'Checked' &
            `Wrist and Hand (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Wrist and Hand (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, Arms, Elbows, Elbows_bilateral, Wrists.Hands, 
           Wrists.Hands_bilateral, everything())

# Clean chest and upper back (excluding thoracic spine)
## Group chest/sternum, thorax(choice=Front)
data <- data %>% 
    mutate(Chest = case_when(
        `Sternum/Chest (choice=Yes)` == 'Checked' |
            `Sternum/Chest (choice=Front)` == 'Checked' |
            `Sternum/Chest (choice=Left)` == 'Checked' |
            `Sternum/Chest (choice=Right)` == 'Checked' |
            `Thorax (choice=Front)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Upper_back = case_when(
        `Thorax (choice=Back)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Sternum/Chest ('), -starts_with('Thorax (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, Arms, Elbows, Elbows_bilateral, Wrists.Hands, 
           Wrists.Hands_bilateral, Chest, Upper_back, everything())

# Clean lower back (excluding lumbar spine)
data <- data %>% 
    mutate(Lower_back = case_when(
        `Flank (choice=Yes)` == 'Checked' |
            `Flank (choice=Back)` == 'Checked' |
            `Flank (choice=Left)` == 'Checked' |
            `Flank (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Flank (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, Arms, Elbows, Elbows_bilateral, Wrists.Hands, 
           Wrists.Hands_bilateral, Chest, Upper_back, Lower_back, everything())

# Abdomen
data <- data %>% 
    mutate(Abdomen = case_when(
        `Abdomen (choice=Yes)` == 'Checked' |
            `Abdomen (choice=Front)` == 'Checked' |
            `Abdomen (choice=Left)` == 'Checked' |
            `Abdomen (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Abdomen (')) %>% 
    select(ID, Head, Throat, Cervical_spine, Shoulder, 
           Shoulder_bilateral, Arms, Elbows, Elbows_bilateral, Wrists.Hands,
           Wrists.Hands_bilateral, Chest, Upper_back, Lower_back, Abdomen, 
           everything()) 

# Spine
data <- data %>% 
    mutate(Thoracic_spine = case_when(
        `spine (choice=Yes)` == 'Checked' |
            `spine (choice=Back)` == 'Checked' |
            `spine (choice=Left)` == 'Checked' |
            `spine (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Lumbosacral_spine = case_when(
        `Lumber (choice=Yes)` == 'Checked' |
            `Lumber (choice=Back)` == 'Checked' |
            `Lumber (choice=Left)` == 'Checked' |
            `Lumber (choice=Right)` == 'Checked' |
            `Sacral (choice=Yes)` == 'Checked' |
            `Sacral (choice=Back)` == 'Checked' |
            `Sacral (choice=Left)` == 'Checked' |
            `Sacral (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    select(-starts_with('Spine ('), -starts_with('Lumber ('), 
           -starts_with('Sacral (')) %>% 
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, 
           Thoracic_spine, Lumbosacral_spine, everything())

# Groin
data <- data %>% 
    mutate(Groin = case_when(
        `Groin (choice=Yes)` == 'Checked' |
            `Groin (choice=Front)` == 'Checked' |
            `Groin (choice=Left)` == 'Checked' |
            `Groin (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    select(-starts_with('Groin (')) %>% 
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, Thoracic_spine, 
           Lumbosacral_spine, Groin, everything())

# Hips
data <- data %>% 
    mutate(Hips = case_when(
        `Hip (choice=Yes)` == 'Checked' |
            `Hip (choice=Front)` == 'Checked' |
            `Hip (choice=Back)` == 'Checked' |
            `Hip (choice=Left)` == 'Checked' |
            `Hip (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    mutate(Hips_bilateral = case_when(
            `Hip (choice=Left)` == 'Checked' &
            `Hip (choice=Right)` == 'Checked'  ~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    select(-starts_with('Hip (')) %>% 
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, Thoracic_spine, 
           Lumbosacral_spine, Groin, Hips, Hips_bilateral, everything())

# Legs
## Combine thigh and leg
data <- data %>% 
    mutate(Legs = case_when(
        `Thigh (choice=Yes)` == 'Checked' |
            `Thigh (choice=Front)` == 'Checked' |
            `Thigh (choice=Back)` == 'Checked' |
            `Thigh (choice=Left)` == 'Checked' |
            `Thigh (choice=Right)` == 'Checked' |
            `Leg (choice=Yes)` == 'Checked' |
            `Leg (choice=Front)` == 'Checked' |
            `Leg (choice=Back)` == 'Checked' |
            `Leg (choice=Left)` == 'Checked' |
            `Leg (choice=Right)` == 'Checked'~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    select(-starts_with('Thigh (')) %>% 
    select(-starts_with('Leg (')) %>% 
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, Thoracic_spine, 
           Lumbosacral_spine, Groin, Hips, Hips_bilateral, Legs, everything())

# Knees
data <- data %>% 
    mutate(Knees = case_when(
        `Knees (choice=Yes)` == 'Checked' |
            `Knees (choice=Front)` == 'Checked' |
            `Knees (choice=Back)` == 'Checked' |
            `Knees (choice=Left)` == 'Checked' |
            `Knees (choice=Right)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    mutate(Knees_bilateral = case_when(
        `Knees (choice=Left)` == 'Checked' &
            `Knees (choice=Right)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Knees (')) %>%
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, Thoracic_spine, 
           Lumbosacral_spine, Groin, Hips, Hips_bilateral, Legs, Knees, 
           Knees_bilateral, everything())

# Ankles and feet
data <- data %>% 
    mutate(Ankles.Feet = case_when(
        `Ankle (choice=Yes)` == 'Checked' |
            `Ankle (choice=Front)` == 'Checked' |
            `Ankle (choice=Back)` == 'Checked' |
            `Ankle (choice=Left)` == 'Checked' |
            `Ankle (choice=Right)` == 'Checked' |
            `Feet (choice=Yes)` == 'Checked' |
            `Feet (choice=Front)` == 'Checked' |
            `Feet (choice=Back)` == 'Checked' |
            `Feet (choice=Left)` == 'Checked' |
            `Feet (choice=Right)` == 'Checked'~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    mutate(Ankles_bilateral = case_when(
        `Ankle (choice=Left)` == 'Checked' &
            `Ankle (choice=Right)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Feet_bilateral = case_when(
        `Feet (choice=Left)` == 'Checked' &
            `Feet (choice=Right)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    mutate(Ankles.Feet_bilateral = case_when(
        `Feet_bilateral` == 'Checked' |
            `Ankles_bilateral` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>% 
    select(-starts_with('Ankle (')) %>%
    select(-starts_with('Feet (')) %>%
    select(-Ankles_bilateral) %>%
    select(-Feet_bilateral) %>%
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, Thoracic_spine, 
           Lumbosacral_spine, Groin, Hips, Hips_bilateral, Legs, Knees, 
           Knees_bilateral, Ankles.Feet, Ankles.Feet_bilateral, everything())

# Buttocks
data <- data %>% 
    mutate(Buttocks = case_when(
        `Buttock/Gluteus (choice=Yes)` == 'Checked' |
            `Buttock/Gluteus (choice=Front)` == 'Checked' |
            `Buttock/Gluteus (choice=Back)` == 'Checked' |
            `Buttock/Gluteus (choice=Left)` == 'Checked' |
            `Buttock/Gluteus (choice=Right)` == 'Checked' ~ 'Yes',
        TRUE ~ 'No'
    )) %>%
    select(-starts_with('Buttock/Gluteus (')) %>%
    select(ID, Head, Throat, Shoulder, Shoulder_bilateral,
           Arms, Elbows, Elbows_bilateral, Wrists.Hands, Wrists.Hands_bilateral,
           Chest, Upper_back, Lower_back, Abdomen, Cervical_spine, Thoracic_spine, 
           Lumbosacral_spine, Groin, Hips, Hips_bilateral, Legs, Knees, 
           Knees_bilateral, Ankles.Feet, Ankles.Feet_bilateral, Buttocks,
           everything())

# Remove whole body / whole arm / whole leg
data <- data %>% 
    select(-starts_with('whole body')) %>% 
    select(-starts_with('Whole leg')) %>% 
    select(-starts_with('Whole arm'))

# Rename CD4+ (recent) / Employment status / ART columns / pain columns
data <- data %>% 
    rename(CD4_diagnosed = `CD4+ diagnosed`,
           CD4_recent = `CD4+ (recent)`,
           ART_currently = `Currently on ART`,
           ART_D4T.now = `Current ARV includes Stavudine (d4T)`,
           ART_D4T.previous = `Previous regimen include stavudine(d4T)`,
           Employment_status = `Employment Status`,
           Pain_week = `Pain rating time-scale (choice=in the last week)`,
           Pain_month = `Pain rating time-scale (choice=in the last month)`,
           Pain_worst = `Worst pain`,
           Pain_least = `Least pain`,
           Pain_now = `Pain now`)

## Convert 'Missing' to <NA>
data[data == 'Missing'] <- NA

## Convert 'No details' to <NA>
data[data == 'no details'] <- NA

## Convert -99 to <NA>
data[data == '-99'] <- NA
data[data == -99] <- NA

# Fix CD4_diagnosed
data$CD4_diagnosed <- as.numeric(data$CD4_diagnosed)

# Fix Age
data <- data %>% 
    mutate(Age = ifelse(Age < 18,
                        yes = NA,
                        no = Age))

# Fix employment status
data <- data %>% 
    mutate(Employment_status = case_when(
        Employment_status == 'Employed part time/ Piece work' ~ 'Part-time work',
        Employment_status == 'Employment full-time' ~ 'Full-time work',
        Employment_status == 'Other' ~ 'Other',
        Employment_status == 'Unemployed' ~ 'Unemployed'
    )) 

# Fix education
temp <- data %>% 
    select(ID, starts_with('Education')) %>% 
    mutate(Primary = ifelse(`Education (choice=Primary ( grades 1-7))` == 'Unchecked',
                            yes = NA,
                            no = 'Primary')) %>% 
    mutate(Secondary = ifelse(`Education (choice=secondary (grades 8-12))` == 'Unchecked',
                              yes = NA,
                              no = 'Secondary')) %>%
    mutate(Tertiary = ifelse(`Education (choice=Tertiary (any post-school))` == 'Unchecked',
                             yes = NA,
                             no = 'Tertiary')) %>%
    mutate(Missing = ifelse(`Education (choice=Missing)` == 'Unchecked',
                            yes = NA,
                            no = 'Missing')) %>% 
    select(-starts_with('Education')) %>% 
    pivot_longer(cols = -ID,
                 names_to = 'Education',
                 values_to = 'Value') %>%
    mutate(Value = ifelse(Value == 'Missing',
                          yes = NA,
                          no = Value)) %>% 
    filter(!is.na(Value)) %>% 
    # Check original records for DD12, RPA15, STIG10, STIG40, STIG47
    # Stop-gap until original data fixed
    distinct(ID, .keep_all = TRUE) %>% 
    select(ID, Value) %>% 
    rename(Education = Value)

data <- data %>% 
    select(-starts_with('Education')) %>% 
    left_join(temp) 

# Fix pain record time span
timing_temp <- data %>% 
    select(ID, Pain_week, Pain_month) %>% 
    pivot_longer(cols = -ID,
                 names_to = 'Rating_period',
                 values_to = 'Values') %>% 
    filter(Values == 'Checked') %>% 
    select(-Values) %>%
    mutate(Rating_period = ifelse(Rating_period == 'Pain_week',
                                  yes = 'Weekly',
                                  no = 'Monthly'))
    
data <- data %>% 
    left_join(timing_temp)

# Remove pain_week and pain_month
data <- data %>% 
    select(-Pain_week, -Pain_month)

# Remove Complete
data <- data %>% 
    select(-`Complete?`)

# Add BDI
bdi <- read_csv('data-original/bdi-data.csv')
data <- data %>% 
    left_join(bdi)

## Check data and remove variables with less than 80% completeness
data %>%
    mutate_if(is.character, factor) %>% 
    skim()

#####################
# Reviewer requests #
#####################
# Keep BDI despite only been
# Keep diabetes
# Keep TB

### Remove CD4_recent values > 1500 cells/mm3
data <- data %>% 
    filter(is.na(CD4_recent) | CD4_recent <= 1500)

### Remove Urban / rural
data <- data %>% 
    select(-`Urban or rural`)
    
### Remove ancestry
data <- data %>% 
    select(-Ancestry)

### Remove CD4_diagnosed
data <- data %>% 
    select(-CD4_diagnosed)

### Remove language 
### (>80% complete, but I cannot see a use for the variable)
data <- data %>% 
    select(-Language)

### Remove TB treatment
data <- data %>% 
    select(-`currently on TB treatment`)

### Remove Shingles
data <- data %>% 
    select(-starts_with('Shingles'))

### Remove currently on D4T treatment 
data <- data %>% 
    select(-ART_D4T.now)

### Remove previous exposure to D4T
data <- data %>% 
    select(-ART_D4T.previous)

# Add study site column
data <- data %>% 
    # Clean-up ID column to keep only study site information
    mutate(Site = str_remove(ID, 
                             pattern = '[0-9][0-9]?[0-9]?[0-9]?')) %>% 
    # Fix RP? (all from the same site, just different group allocation)
    mutate(Site = ifelse(Site == 'RPA' | Site == 'RPB' | Site == 'RPC' | Site == 'RPD',
                         yes = 'RP',
                         no = Site))

# Check sample size per site
data %>% 
    # Select Site
    select(Site) %>% 
    # Group data 
    group_by(Site) %>% 
    summarise(Count = n()) %>% 
    kable(caption = 'Sample size by study site')

# Fix two mis-coded sites (RISI => RESI, and RBP => RP)
data <- data %>% 
    mutate(Site = str_replace(Site, 
                            pattern = 'RISI',
                            replacement = 'RESI'),
           Site = str_replace(Site, 
                            pattern = 'RBP',
                            replacement = 'RP'))

############################################################
#                                                          #
#                         All data                         #
#                                                          #
############################################################
# Write to file
write_csv(data, file = 'data-cleaned/data-all.csv')
write_rds(data, file = 'data-cleaned/data-all.rds')

############################################################
#                                                          #
#                     Pain sites only                      #
#                                                          #
############################################################
# Extract data from complete dataset
names(data)

sites <- data %>% 
    select(-ends_with('bilateral'), -c(27:40))

# Write to file
write_csv(sites, file = 'data-cleaned/data-pain-sites.csv')
write_rds(sites, file = 'data-cleaned/data-pain-sites.rds')

############################################################
#                                                          #
#                    Demographics only                     #
#                                                          #
############################################################
# Extract data from complete dataset
demo <- data %>% 
    select(ID, Site, 27:33, 37, 39)

# Write to file
write_csv(demo, file = 'data-cleaned/data-demographics.csv')
write_rds(demo, file = 'data-cleaned/data-demographics.rds')

############################################################
#                                                          #
#                   Pain intensity only                    #
#                                                          #
############################################################
# Extract data from complete dataset
pain <- data %>% 
    select(ID, Site, 34:36, 38)

# Write to file
write_csv(pain, file = 'data-cleaned/data-pain-intensity.csv')
write_rds(pain, file = 'data-cleaned/data-pain-intensity.rds')

# Session Information
sessionInfo()
