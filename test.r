library(readr)
# DigitalDefensesV1 <- read_csv("~/data_test_graham.csv")
DigitalDefensesV1 <- read_csv("~/data.csv")

safe_numeric_conversion <- function(x) {
  converted <- suppressWarnings(as.numeric(x))
  ifelse(is.na(converted), x, converted)
}

#list_of_cols <- c("LSI_repression2")
#DigitalDefensesV1$new_column <- rowSums(DigitalDefensesV1[, list_of_cols], na.rm = TRUE)


#Define "primitive" and "mature" defense mechanism columns
primitive_cols <- c("LSI_denial1", "LSI_denial2", "LSI_displacement1", "LSI_displacement2", "LSI_regression1", "LSI_regression2", "LSI_projection1", "LSI_projection2")
mature_cols <- c("LSI_intellectualization1", "LSI_intellectualization2", "LSI_compensation1", "LSI_compensation2", "LSI_reactionformation1", "LSI_reactionformation2", "LSI_repression1", "LSI_repression2")


#Defense intensity calculation
DigitalDefensesV1$primitive_intensity <- rowSums(DigitalDefensesV1[, primitive_cols], na.rm = TRUE) / length(primitive_cols)
DigitalDefensesV1$mature_intensity <- rowSums(DigitalDefensesV1[, mature_cols], na.rm = TRUE) / length(mature_cols)

#Defense type define
DigitalDefensesV1$defense_type <- ifelse(DigitalDefensesV1$primitive_intensity > DigitalDefensesV1$mature_intensity, "primitive", "mature")

print(DigitalDefensesV1$defense_type)

# Count the occurrences of each defense type
defense_type_counts <- table(DigitalDefensesV1$defense_type)

# Create a pie chart
pie(defense_type_counts, labels = names(defense_type_counts), main = "Defense Type Distribution")

# Define the sub-question weights
sub_q_weights <- c(ignore = 1, like = 2, bookmark = 3, brief = 4, follow = 5, respond = 6, indepth = 7, repost = 8, report = 9)

# Identify the columns for neutral and provocative questions
neutral_cols <- grep("neutral*", names(DigitalDefensesV1), value=TRUE)  
provocative_cols <- grep("provoke*", names(DigitalDefensesV1), value=TRUE)

# Function to calculate engagement score
calculate_engagement_score <- function(response_cols, weights) {
  scores <- sapply(response_cols, function(col) {
    sub_q_name <- gsub(".*_", "", col)  # Extract sub-question name (eg "ignore", "like", "bookmark", etc)
    print(sub_q_name)
    response <- as.numeric(DigitalDefensesV1[[col]])  # Convert response to numeric
    print(response)
    response * weights[sub_q_name]  # Multiply response by weight
    #print(weights[sub_q_name])
  })
  rowMeans(scores, na.rm = TRUE)  # Avg scores for each participant
}

# Calculate engagement scores for each participant
DigitalDefensesV1$neutral_engagement_score <- calculate_engagement_score(neutral_cols, sub_q_weights)
DigitalDefensesV1$provocative_engagement_score <- calculate_engagement_score(provocative_cols, sub_q_weights)

#print(DigitalDefensesV1$provocative_engagement_score)



