
#eligible_str <- "web eligibility suvey_3.19.25.csv"

#calend_str <- "Calendly_phone consult_events-export_6.4.24.csv"

#screen_str <- "HS2100716BodourSalhi-ScreeningSummary_DATA_LABELS_2025-03-19_1804.csv"

#enroll_str <- "HS2100716BodourSalhi-EnrollmentSummary_DATA_LABELS_2025-03-19_1804.csv"

#enroll_str1 <- "HS2100716BodourSalhi-EnrollmentSummaryFol_DATA_LABELS_2025-03-19_1805.csv"


#master_str <- "MasterList.csv"

#dx_str <- "HS2100716BodourSalhi-BaselineTimeFromDxTo_DATA_2025-03-19_1803.csv"

#dx_strFull <- "HS2100716BodourSalhi-BaselineTimeFromDxTo_DATA_2025-03-19_1803 (1).csv"


demographics_str <- "DemographicsRaceEduc_variable labels_3.19.24.csv"



find_latest_file <- function(base_pattern) {
  # Get all CSV files in the current directory
  all_files <- list.files(path = "input", pattern = "\\.csv$", full.names = TRUE)
  
  # Filter files based on the base pattern
  matching_files <- grep(base_pattern, all_files, value = TRUE)
  
  # If no matching files, return NULL
  if (length(matching_files) == 0) {
    return(NULL)
  }
  
  # If only one file, return it
  if (length(matching_files) == 1) {
    return(matching_files)
  }
  
  # Get file info for matching files
  file_info <- file.info(matching_files)
  
  # Return the most recently modified file
  return(rownames(file_info)[which.max(file_info$mtime)])
}

calend_str <- find_latest_file("Calendly_phone consult_events-export")

# Apply to your specific files
eligible_str <- find_latest_file("Web Eligibility Survey")
openSpecimen <-find_latest_file("OpenSpecimen_collection")

dx_str <- find_latest_file("BaselineTimeFromDx")
dx_strFull <- find_latest_file("BaselineFTimeFromDx")
screen_str <- find_latest_file("ScreeningSummary")
enroll_str <- find_latest_file("EnrollmentSummary_DATA")
enroll_str1 <- find_latest_file("EnrollmentSummaryFol")

master_str <- find_latest_file("MasterList")

# Show results
cat("Eligible file:", eligible_str, "\n")
cat("Calendar file:", calend_str, "\n")
cat("Screening file:", screen_str, "\n")
cat("Enrollment file:", enroll_str, "\n")
cat("Enrollment follow-up file:", enroll_str1, "\n")
cat("Master file:", master_str, "\n")
cat("Dx file:", dx_str, "\n")
cat("DxF file:", dx_strFull, "\n")
cat("OpSp file:", openSpecimen, "\n")
