getwd()
#setwd("C:/Users/mathi/OneDrive/Bureau")
setwd("C:/Users/Juan David Alonso/Documents/M2/Dimitri")
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

# Read the CSV file
smartphones <- read.csv('smartphones.csv')
# Create a copy of the DataFrame
df <- smartphones
# We remove duplicate values on the dataframe
df <- df %>% distinct()
# Replace blanks with NA in "user_rating" column
df <- df %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
# Check for missing values
colSums(is.na(df))
#We will drop rows with missing values in specific columns
df <- df %>% filter(!is.na(processor) & !is.na(front_cameras) & !is.na(operating_system))
# Fill missing values in additional_features column
df <- df %>% mutate(additional_features = ifelse(is.na(additional_features), "No add features", additional_features))
# Drop irrelevant columns as the link of the review
df <- df %>% select(-review, -review_link)
# Split user_rating into avg_user_rating and num_ratings
df <- df %>%
  mutate(
    avg_user_rating = str_extract(user_rating, "\\d+\\.\\d+"),
    num_ratings = str_extract(user_rating, "\\d+[KLM]?")
  )
# Extract processor brand and number of cores
df <- df %>%
  mutate(
    processor_brand = str_extract(processor, "^[A-Za-z]+"),
    num_cores = str_extract(processor, "Single|Dual|Quad|Octa|Tru-Octa|Hexa|Deca|Nona")
  )
# We proceed to Convert number of scores into numeric
core_mapping <- c(
  "Single" = 1,
  "Dual" = 2,
  "Quad" = 4,
  "Hexa" = 6,
  "Octa" = 8,
  "Tru-Octa" = 8,
  "Deca" = 10,
  "Nona" = 9
)
#We attached to the dataset
df <- df %>%
  mutate(num_cores = core_mapping[num_cores])
# Handle rear and front cameras
df <- df %>%
  mutate(
    num_rear_cameras = str_count(rear_cameras, "\\+") + 1,
    main_rear_camera = as.numeric(str_extract(rear_cameras, "\\d+\\.?\\d*")),
    num_front_cameras = str_count(front_cameras, "\\+") + 1,
    main_front_camera = as.numeric(str_extract(front_cameras, "\\d+\\.?\\d*"))
  )
# We extract display size, refresh rate, and type
df <- df %>%
  mutate(
    display_size = as.numeric(str_extract(display, "\\d+\\.?\\d*")),
    refresh_rate = str_extract(display, "\\d+Hz"),
    display_type = str_extract(display, "IPS LCD|Super AMOLED|AMOLED|OLED|TFT LCD|PLS LCD|Dynamic AMOLED|P-OLED|LTPO AMOLED|Optic AMOLED|S-LCD|LTPS LCD|Flexible AMOLED|Fluid AMOLED")
  )
#######################################################
###################### Display##########################
########################################################

# Extracting display size
df <- df %>%
  mutate(display_size = str_extract(display, "\\d+\\.?\\d*"))

# Extracting refresh rate
df <- df %>%
  mutate(refresh_rate = str_extract(display, "\\d+Hz"))

# Extracting display type
df <- df %>%
  mutate(display_type = str_extract(display, "(IPS LCD|AMOLED|Super AMOLED|OLED|P-OLED|TFT LCD)"))

# We proceed to You can categorize the display_type using a custom function
display_type_categorize <- function(display_type) {
  if (is.na(display_type)) {
    return(NA) # Handle NA values explicitly
  } else if (str_detect(display_type, "IPS LCD")) {
    return("IPS LCD")
  } else if (str_detect(display_type, "TFT LCD")) {
    return("TFT LCD")
  } else if (str_detect(display_type, "P-OLED")) {
    return("P-OLED")
  } else if (str_detect(display_type, "AMOLED") && !str_detect(display_type, "Super AMOLED")) {
    return("AMOLED")
  } else if (str_detect(display_type, "Super AMOLED")) {
    return("Super AMOLED")
  } else if (str_detect(display_type, "OLED")) {
    return("OLED")
  } else {
    return("IPS LCD") # Default fallback
  }
}


df <- df %>%
  mutate(display_type = sapply(display_type, display_type_categorize))

################################################################################
################################### RAM and Storage#############################
############################################################################
#Splitting RAM and Storage
df <- df %>%
  mutate(ram = str_extract(ram_internal_memory, "\\d+ (GB|MB) RAM"),
         storage = str_extract(ram_internal_memory, "\\d+ (GB|MB|TB) Storage"))

# Convert Storage to GB
convert_to_gb <- function(storage) {
  if (str_detect(storage, "TB")) {
    return(as.numeric(str_extract(storage, "\\d+")) * 1024)
  } else if (str_detect(storage, "GB")) {
    return(as.numeric(str_extract(storage, "\\d+")))
  } else if (str_detect(storage, "MB")) {
    return(as.numeric(str_extract(storage, "\\d+")) / 1024)
  } else {
    return(0)
  }
}

df <- df %>%
  mutate(storage_gb = sapply(storage, convert_to_gb))

convert_to_gb <- function(storage) {
  if (str_detect(storage, "TB")) {
    return(as.numeric(str_extract(storage, "\\d+")) * 1024)
  } else if (str_detect(storage, "GB")) {
    return(as.numeric(str_extract(storage, "\\d+")))
  } else if (str_detect(storage, "MB")) {
    return(as.numeric(str_extract(storage, "\\d+")) / 1024)
  } else {
    return(0)
  }
}

df <- df %>%
  mutate(storage_gb = sapply(storage, convert_to_gb))

## RAM

df <- df %>%
  mutate(ram_gb = ram,
         ram_gb = as.numeric(str_extract(ram, "\\d+")),
         ram_gb = ifelse(ram == "512 MB RAM" | ram == "768 MB RAM", ram_gb/1024, ram_gb))

#############################################################################
#########################Battery############################################
############################################################################

df <- df %>%
  mutate(
    # Ensure no empty values cause problems
    battery = ifelse(is.na(battery), "", battery)
  ) %>%
  separate(
    battery,
    into = c("battery_capacity", "charging"),
    sep = "\\|",
    remove = FALSE,
    fill = "right" # Ensures missing values are filled with NA
  ) %>%
  mutate(
    battery_capacity = str_extract(battery_capacity, "\\d+"), # Extract numeric part for capacity
    charging = str_extract(charging, "\\d+W")                # Extract numeric part for charging
  )

####################################################################################
################################# Exctracting Additional features#################
###################################################################################
#Converting the string features into boolean
add_feature_extract <- function(value) {
  has_fingerprint_sensor = str_detect(value, "Fingerprint Sensor")
  has_5g = str_detect(value, "5G")
  has_nfc = str_detect(value, "NFC")
  
  return(data.frame(has_fingerprint = has_fingerprint_sensor, has_5g = has_5g, has_nfc = has_nfc))
}

df <- df %>%
  bind_cols(add_feature_extract(df$additional_features))
##################################################################################
############################### Brand ############################################
##################################################################################
df <- df %>%
  mutate(brand = str_extract(model_name, "^[^ ]+"))

##################################################################################
############################### Operating System ####################################
##################################################################################

df <- df %>%
  separate(
    operating_system,
    into = c("os_type", "os_version"),
    sep = " ", 
    remove = FALSE,
    extra = "drop" # Discards additional pieces without warnings
  ) %>%
  mutate(
    os_type = ifelse(os_type %in% c("Android", "iOS"), os_type, "Other"),
    os_version = str_extract(os_version, "v?[\\d\\.]+") # Extract numeric version info
  )



##################################################################################
############################### Validity ####################################
##################################################################################
############################### Price ####################################

#The prices were scrapped from an Indian website, the prices were indicated
#in rupees, we convert them in euros here. Jan 2025 : exchange rate -> 1 Indian Rupee = 0.01106 Euros. 

df <- df %>%
  mutate(price = str_replace_all(price, ",", "")) %>%
  mutate(price = as.numeric(price)*0.01106)

#Inputation median of number of cores
df <- df %>%
  mutate(num_cores = ifelse(is.na(num_cores), median(num_cores, na.rm = TRUE), num_cores))

#High resolution cameras
handle_high_res_cameras <- function(cameras) {
  megapixels <- as.numeric(str_extract_all(cameras, "\\d+\\.?\\d*")[[1]])
  mean_mp <- mean(megapixels)
  num_high_resolution_cameras <- sum(megapixels >= mean_mp)
  
  return(num_high_resolution_cameras)
}

df <- df %>%
  mutate(num_high_resolution_cameras = sapply(rear_cameras, handle_high_res_cameras))
##########################
################## Dropping columns extracted

df <- df[, !(names(df) %in% c("user_rating", "rear_cameras", "front_cameras", 
                              "display", "ram_internal_memory", "battery", 
                              "operating_system", "additional_features"))]

# Check for missing values
colSums(is.na(df))


#Refresh rate as the mean
# Remove the "Hz" text and convert refresh_rate to numeric
df$refresh_rate <- as.numeric(gsub("Hz", "", df$refresh_rate))

# Calculate the median of refresh_rate excluding NA values
median_refresh_rate <- median(df$refresh_rate, na.rm = TRUE)

# Replace NA values in refresh_rate with the median
df$refresh_rate[is.na(df$refresh_rate)] <- median_refresh_rate


# Check for missing values
colSums(is.na(df))

# Drop the expert_rating column and num ratings
df <- df[, !names(df) %in% "expert_rating"]
df <- df[, !names(df) %in% "num_ratings"]
df <- df[, !names(df) %in% "charging"]
# Check for missing values
colSums(is.na(df))
# Remove rows where avg_user_rating is NA
df <- df[!is.na(df$avg_user_rating), ]


##############################
#### Variable as numeric #####
##############################

df$avg_user_rating <- as.numeric(df$avg_user_rating)
df$display_size <- as.numeric(df$display_size)
df$display_type <- as.factor(df$display_type)
df$battery_capacity <- as.numeric(df$battery_capacity)


df <- df %>%
  filter(!is.na(display_type)) %>%
  filter(!is.na(os_version))


colSums(is.na(df))
View(head(df,10))

write.csv(df, file = "data.csv", row.names = FALSE)
