library(readr)
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

# Function to calculate and add total engagement scores for each main question
calculate_and_add_total_engagement_scores <- function(response_cols, weights, data_frame) {
  # Extract unique main question names
  main_questions <- unique(sub("_[^_]+$", "", response_cols))
  
  for (main_q in main_questions) {
    # Find all columns related to this main question
    related_cols <- grep(paste0("^", main_q, "_"), response_cols, value = TRUE)
    
    # Calculate engagement scores for each related column
    scores <- sapply(related_cols, function(col) {
      sub_q_name <- gsub(".*_", "", col)  # Extract sub-question name
      response <- as.numeric(data_frame[[col]])  # Convert response to numeric
      response * weights[sub_q_name]  # Multiply response by weight
    })
    
    # Calculate the total score for the main question
    main_q_total_score <- rowSums(scores, na.rm = TRUE)
    
    # Add the new column to the data frame
    new_col_name <- paste(main_q, "total_engagement_score", sep = "_")
    data_frame[[new_col_name]] <- main_q_total_score
  }
  return(data_frame)
}

# Apply the function to your data frame
DigitalDefensesV1 <- calculate_and_add_total_engagement_scores(neutral_cols, sub_q_weights, DigitalDefensesV1)
DigitalDefensesV1 <- calculate_and_add_total_engagement_scores(provocative_cols, sub_q_weights, DigitalDefensesV1)

# Calculate the average of all neutral question scores
neutral_question_scores <- grep("neutral_.*total_engagement_score$", names(DigitalDefensesV1), value = TRUE)
DigitalDefensesV1$neutral_engagement_score <- rowMeans(DigitalDefensesV1[, neutral_question_scores], na.rm = TRUE)

# Calculate the average of all provocative question scores
provocative_question_scores <- grep("provoke_.*total_engagement_score$", names(DigitalDefensesV1), value = TRUE)
DigitalDefensesV1$provocative_engagement_score <- rowMeans(DigitalDefensesV1[, provocative_question_scores], na.rm = TRUE)

