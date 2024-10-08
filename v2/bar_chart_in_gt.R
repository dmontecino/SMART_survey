bar_chart <- function(label, height = "30px", fill = "red", background = "white", line_color = "black") {
  # Define the 50% line
  line <- glue::glue("<div style='position:absolute;width:0;height:{height};border-left:2px solid {line_color};left:50%;top:0;'></div>")
  
  # Define the bar
  bar <- glue::glue("<div style='background:{fill};width:{label}%;height:{height};'></div>")
  
  # Combine the line and the bar into a chart
  chart <- glue::glue("<div style='position:relative;flex-grow:1;margin-left:8px;background:{background};'>{line}{bar}</div>")
  
  # Return the HTML content for the bar chart with the 50% line
  glue::glue("<div style='display:flex;align-items:left;'>{chart}</div>") %>%
    gt::html()
}


