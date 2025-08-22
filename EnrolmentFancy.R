getLineChartPublicationReady <- function(df.enroll, 
                                         df.DxDate, 
                                         enrollField, 
                                         startDate = "2022-05", 
                                         endDate = format(Sys.Date(), "%Y-%m"),
                                         plot_title = NULL,
                                         x_label = "Month",
                                         y_label = "Enrollment Count",
                                         color_palette = "viridis",
                                         custom_colors = NULL,
                                         date_format = "%b %Y",
                                         x_interval = 3,
                                         line_size = 1.2,
                                         point_size = 3,
                                         legend_position = "right",
                                         font_family = "sans",
                                         output_file = NULL,
                                         width = 10,
                                         height = 6,
                                         dpi = 600,
                                         output_format = "png") {
  
  # Load required libraries
  require(dplyr)
  require(ggplot2)
  require(scales)
  require(viridis)
  require(lubridate)
  
  # Error handling
  if (!is.data.frame(df.enroll) || !is.data.frame(df.DxDate)) {
    stop("Input arguments must be data frames")
  }
  
  if (!enrollField %in% names(df.enroll)) {
    stop(paste("Field", enrollField, "not found in enrollment data frame"))
  }
  
  # Merge the dataframes
  df.merged <- tryCatch({
    inner_join(df.enroll, df.DxDate, by = c("ID" = "bcsbusername"))
  }, error = function(e) {
    stop("Error merging dataframes: ", e$message)
  })
  
  # Convert 'EarliestConsentDate' to date format
  df.merged$EarliestConsentDate <- as.Date(df.merged$EarliestConsentDate)
  
  # Extract the year and month from 'EarliestConsentDate'
  df.merged$YearMonth <- format(df.merged$EarliestConsentDate, "%Y-%m")
  
  # Generate proper date objects for sorting
  df.merged$DateObj <- as.Date(paste0(df.merged$YearMonth, "-01"))
  
  # Consolidate multiple race entries
  if (enrollField == "Race") {
    df.merged[[enrollField]] <- ifelse(grepl(",", df.merged[[enrollField]]), "Multiple", df.merged[[enrollField]])
    df.merged <- df.merged[df.merged[[enrollField]] != "No race indicated", ]
  }
  
  # Filter the data based on the specified date range
  df.merged <- df.merged %>%
    filter(YearMonth >= startDate & YearMonth <= endDate)
  
  # Create a complete set of month-group combinations
  all_months <- seq(as.Date(paste0(startDate, "-01")), as.Date(paste0(endDate, "-01")), by = "month")
  month_labels <- format(all_months, "%Y-%m")
  
  # Group ordering for better legend presentation
  groups_ordered <- df.merged %>%
    group_by(!!sym(enrollField)) %>%
    summarise(total = n()) %>%
    arrange(desc(total)) %>%
    pull(!!sym(enrollField))
  
  all_groups <- unique(groups_ordered)
  
  # Create complete grid for all combinations
  complete_grid <- expand.grid(
    YearMonth = month_labels,
    Group = all_groups,
    stringsAsFactors = FALSE
  )
  names(complete_grid)[2] <- enrollField
  
  # Add date objects to the grid for proper sorting
  complete_grid$DateObj <- as.Date(paste0(complete_grid$YearMonth, "-01"))
  
  # Count enrollments by YearMonth and the specified field
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n(), .groups = "drop") %>%
    right_join(complete_grid, by = c("YearMonth", enrollField)) %>%
    replace_na(list(Enrollment = 0)) %>%
    # Sort by date and group
    arrange(DateObj, !!sym(enrollField))
  
  # Convert grouping field to factor with ordered levels for consistent coloring
  df.counts[[enrollField]] <- factor(df.counts[[enrollField]], levels = all_groups)
  
  # Create subset of x-axis labels to avoid overcrowding
  month_indices <- seq(1, length(all_months), by = x_interval)
  selected_months <- all_months[month_indices]
  
  # Format month labels according to specified format
  formatted_labels <- format(selected_months, date_format)
  label_positions <- format(selected_months, "%Y-%m")
  
  # Set up default title if not provided
  if (is.null(plot_title)) {
    plot_title <- paste0("Enrollment by ", enrollField)
  }
  
  # Color setup
  if (is.null(custom_colors)) {
    if (color_palette == "viridis") {
      color_scale <- scale_color_viridis_d(option = "D", end = 0.9)
    } else if (color_palette == "brewer") {
      color_scale <- scale_color_brewer(palette = "Set1")
    } else if (color_palette == "grey") {
      color_scale <- scale_color_grey(start = 0.2, end = 0.8)
    } else {
      color_scale <- scale_color_viridis_d(option = "D", end = 0.9)  # Default
    }
  } else {
    if (length(custom_colors) != length(all_groups)) {
      warning("Number of custom colors doesn't match number of groups. Using default colors.")
      color_scale <- scale_color_viridis_d(option = "D", end = 0.9)
    } else {
      color_scale <- scale_color_manual(values = custom_colors)
    }
  }
  
  # Create the line chart
  lineChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, 
                                     color = !!sym(enrollField), 
                                     group = !!sym(enrollField))) +
    geom_line(size = line_size) +
    geom_point(size = point_size) +
    
    # Handle x-axis with selected labels to avoid overcrowding
    scale_x_discrete(
      name = x_label,
      breaks = label_positions,
      labels = formatted_labels
    ) +
    
    # Y-axis formatting with appropriate breaks
    scale_y_continuous(
      name = y_label,
      breaks = function(x) pretty(x, n = 8),
      expand = expansion(mult = c(0, 0.1)),
      labels = comma_format()
    ) +
    
    # Apply color scale
    color_scale +
    
    # Title and labels
    labs(
      title = plot_title,
      subtitle = paste0("Data from ", format(as.Date(paste0(startDate, "-01")), date_format), 
                        " to ", format(as.Date(paste0(endDate, "-01")), date_format)),
      color = enrollField,
      caption = paste0("Generated on ", format(Sys.Date(), "%B %d, %Y"))
    ) +
    
    # Apply theme settings
    theme_minimal(base_family = font_family) +
    theme(
      # Text elements
      text = element_text(family = font_family),
      
      # Title elements
      plot.title = element_text(
        face = "bold", 
        size = 16, 
        hjust = 0, 
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12, 
        color = "gray30",
        margin = margin(b = 20)
      ),
      plot.caption = element_text(
        size = 9, 
        color = "gray50", 
        hjust = 1,
        margin = margin(t = 10)
      ),
      
      # Axis titles
      axis.title.x = element_text(
        size = 13, 
        face = "bold", 
        margin = margin(t = 15)
      ),
      axis.title.y = element_text(
        size = 13, 
        face = "bold", 
        margin = margin(r = 15)
      ),
      
      # Axis text
      axis.text.x = element_text(
        size = 10, 
        angle = 45, 
        hjust = 1,
        margin = margin(t = 5)
      ),
      axis.text.y = element_text(
        size = 10
      ),
      
      # Legend elements
      legend.position = legend_position,
      legend.title = element_text(
        face = "bold", 
        size = 12
      ),
      legend.text = element_text(
        size = 11
      ),
      legend.key.size = unit(1, "cm"),
      legend.margin = margin(t = 10, l = 10),
      
      # Grid elements
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      
      # Add subtle axis lines
      axis.line = element_line(color = "gray20", size = 0.5),
      
      # Plot margins
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    # Create directory if it doesn't exist
    dir_path <- dirname(output_file)
    if (!dir.exists(dir_path) && dir_path != ".") {
      dir.create(dir_path, recursive = TRUE)
    }
    
    # Save with appropriate extension
    if (output_format == "png") {
      ggsave(paste0(output_file, ".png"), lineChart, width = width, height = height, dpi = dpi)
    } else if (output_format == "pdf") {
      ggsave(paste0(output_file, ".pdf"), lineChart, width = width, height = height)
    } else if (output_format == "svg") {
      ggsave(paste0(output_file, ".svg"), lineChart, width = width, height = height)
    } else if (output_format == "tiff") {
      ggsave(paste0(output_file, ".tiff"), lineChart, width = width, height = height, dpi = dpi)
    } else {
      warning("Unsupported output format. Saving as PNG.")
      ggsave(paste0(output_file, ".png"), lineChart, width = width, height = height, dpi = dpi)
    }
  }
  
  return(lineChart)
}
df.enroll <- df.enroll %>%
  mutate(location = ifelse( location == "Web", location, "Clinic"))
(getLineChartPublicationReady(
  df.enroll = df.enroll,
  df.DxDate = df.DxDate,
  enrollField = "location",
  startDate = "2022-05",
  endDate = "2025-05",
  plot_title = "Monthly Enrollment by Location",
  color_palette = "viridis",
  x_interval = 2,                    # Show every 2nd month on x-axis
  legend_position = "right",
  output_file = "plots/enrollment_by_race",
  output_format = "png",
  width = 10,
  height = 6
))
# Example usage:
# getLineChartPublicationReady(
#   df.enroll = enrollment_data, 
#   df.DxDate = diagnosis_data, 
#   enrollField = "Race",
#   startDate = "2022-05", 
#   endDate = "2023-12",
#   plot_title = "Monthly Enrollment by Race",
#   color_palette = "viridis", 
#   x_interval = 2,
#   legend_position = "right",
#   output_file = "plots/enrollment_by_race",
#   output_format = "pdf"
# )