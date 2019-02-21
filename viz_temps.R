# The starting point for data analysis done in this module is
# file livermore_15yr_temps.csv.

# Preprocessing and summarizing data
library(dplyr)

# Visualization development
library(ggplot2)

# For text graphical object (to add text annotation)
library(grid)
source("resizeTextGrob.R")

viztemps <- function (start_year,
                      hide_hilows,
                      hide_avgs) {
  # Load 15 years temperature data
  all_temps_15yrs <- read.csv("merced_15yr_temp.csv", stringsAsFactors=FALSE, sep=",")
  
  # Clean the data: drop records with invalid temp values, and missing or invalid
  #  measurement, quality or source flags.
  #
  #  NOTES: 1. The raw CSV file shows white space for Tmax and Tmin Measurement flags
  #            but when read into the dataframe those flags read NA. The NA values are
  #            treated as Normal in this analysis
  #         2. The raw CSV file shows white space for Tmax and Tmin Quality flags but
  #            when read into the dataframe the Tmax Quality flags show up as NA, while Tmin
  #            Quality flags show up as " " (white space), as expected. 
  
  all_temps_x_years <- all_temps_15yrs %>%
    filter(tmax != -9999 &   
             tmin != -9999 &
             is.na(measurement.flag.7) & 
             is.na(measurement.flag.8) & 
             is.na(quality.flag.7) & 
             source.flag.7 != " " & 
             !is.na(source.flag.7) & 
             source.flag.8 != " " & 
             !is.na(source.flag.8))
  
  # Keep data from only the start_year
  all_temps_x_years <- all_temps_x_years %>%
    group_by(year, month) %>%
    filter(year >= start_year) %>%
    ungroup()
  
  # Compute the average per year min and max temps
  avg_temps_each_year <- all_temps_x_years %>%
    group_by(year) %>%
    summarise(avg_min = mean(tmin),
              avg_max = mean(tmax)) %>%
    ungroup()
  
  avg_max_per_year <- arrange(avg_temps_each_year, desc(avg_max))
  avg_min_per_year <- arrange(avg_temps_each_year, desc(avg_min))
  
  print(avg_max_per_year[,c("year","avg_max")])
  print(avg_min_per_year[,c("year","avg_min")])
  
  # create a dataframe that represents 2014 temperature data
  Present <- all_temps_x_years %>%
    group_by(year, month) %>%
    arrange(day) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(newDay = seq(1, length(day))) %>%    # label days as 1:365 (will represent x-axis)
    ungroup() %>%
    filter(year == 2014)                    # filter out all years except 2014 data
  
  
  # Initialize number of record high and low temp days in 2014
  n2014highs <- 0
  n2014lows <- 0
  
  if(start_year != 2014) {
    # create a dataframe that represents 14 years of historical temp data from 2000-2013
    Past <- all_temps_x_years %>%
      group_by(year, month) %>%
      arrange(day) %>%
      ungroup() %>%
      group_by(year) %>%
      mutate(newDay = seq(1, length(day))) %>%    # label days as 1:365 (will represent x-axis)
      ungroup() %>%
      filter(year != 2014) %>%                # filter out 2014 data
      group_by(newDay) %>%
      mutate(upper = max(tmax),              # identify same day highest max temp from all years
             lower = min(tmin),              # identify same day lowest min temp from all years
             avg_upper = mean(tmax),         # compute same day average max temp from all years
             avg_lower = mean(tmin)) %>%     # compute same day average min temp from all years
      ungroup()
    
    # create dataframe that represents the lowest same-day temperature from years 2000-2013
    PastLows <- Past %>%
      group_by(newDay) %>%
      summarise(Pastlow = min(tmin)) # identify lowest same-day temp between 2000 and 2013
    
    # create dataframe that represents the highesit same-day temperature from years 2000-2013
    PastHighs <- Past %>%
      group_by(newDay) %>%
      summarize(Pasthigh = max(tmax)) # identify highest same-day temps between 2000 and 2013
    
    # create dataframe that identifies days in 2014 when temps were lower than in all previous 14 years
    PresentLows <- Present %>%
      left_join(PastLows) %>%         # merge historical lows to 2014 low temp data
      mutate(record = ifelse(tmin<Pastlow, "Y", "N")) %>% # current year was a record low?
      filter(record == "Y")           # filter for 2014 record low days
    
    # create dataframe that identifies days in 2014 when temps were higher than in all previous 14 years
    PresentHighs <- Present %>%
      left_join(PastHighs) %>%        # merge historical lows to 2014 low temp data
      mutate(record = ifelse(tmax>Pasthigh, "Y", "N")) %>% # current year was a record high?
      filter(record == "Y")           # filter for 2014 record high days
    
    n2014highs <- nrow(PresentHighs)
    print(n2014highs)
    
    n2014lows <- nrow(PresentLows)
    print(n2014lows)
    
    print(dim(PresentHighs))
    print(dim(PresentLows))
  }
  
  #   **** Prepare to do Visualization ****
  
  # prepare y-axis details
  
  # function: Turn y-axis labels into values with a degree superscript
  degree_format <- function(x, ...) {
    parse(text = paste(x, "*degree", sep=""))
  }
  
  # create y-axis variable
  yaxis_temps <- degree_format(seq(0, 120, by=10))
  
  #   **** Visualization Steps ****
  # Create the canvas for the plot.
  p <- ggplot(Present, aes(newDay, tmax)) +
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "seashell2"),
          axis.ticks = element_blank(),
          #axis.text = element_blank(),  
          axis.title = element_blank())
  
  # If start year is not 2014, plot background temps for years upto, but not including, 2014 
  if(start_year != 2014) {
    
    # If feature is not disabled, plot highest and lowest daily temps of past years.
    # This is the first 'background' plot
    if(hide_hilows != TRUE) {
      p <- p + geom_linerange(Past,
                              mapping=aes(x=newDay, ymin=lower, ymax=upper),
                              size=0.8, colour = "#CAA586", alpha=.6)
    }
    
    # If feature is not disabled, plot average daily low and high temps from past years.
    # This plot overlays the first background plot.
    if(hide_avgs != TRUE) {
      p <- p + geom_linerange(Past,
                              mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper),
                              size=0.8, colour = "#A57E69")
    }
  }
  
  # Plot 2014 high and low temps
  p <- p + geom_linerange(Present,
                          mapping=aes(x=newDay, ymin=tmin, ymax=tmax),
                          size=0.8, colour = "#4A2123")
  
  # Add the y-axis border and the x-axis grid lines
  p <- p + 
    geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1) +
    geom_hline(yintercept = 0, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 10, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 20, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 30, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 40, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 50, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 60, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 70, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 80, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 90, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 100, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 110, colour = "ivory2", linetype=1, size=.1) +
    geom_hline(yintercept = 120, colour = "ivory2", linetype=1, size=.1)
  
  
  # Add vertical gridlines to mark end of each month
  p <- p + 
    geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.4) +
    geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.4) 
  
  # Add labels to the x and y axes
  p <- p +
    coord_cartesian(ylim = c(0,120)) +
    scale_y_continuous(breaks = seq(0,120, by=10), labels = yaxis_temps) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                       labels = c("January", "February", "March", "April",
                                  "May", "June", "July", "August", "September",
                                  "October", "November", "December"))
  
  # If start year is not 2014 show temp points that were record highs or lows (relative to
  # previous years)
  if(start_year != 2014) {
    if(n2014highs != 0) {
      # Add points to mark 2014 record high temps
      p <- p + geom_point(data=PresentHighs, aes(x=newDay, y=tmax), colour="firebrick3")
      
      # Add annotation for points representing 2014 record high temperatures
      if(n2014highs == 1) {
        nhighs <- paste("In 2014 there was 1 day\n",
                        "that was hottest since ", start_year, "\n", sep="")
      } else {
        nhighs <- paste("In 2014 there were ", n2014highs," days\n",
                        "that were hottest since ", start_year, "\n", sep="")
      }
      grob3 = textGrob(nhighs, x=0.72, y=0.9, hjust=0,
                       gp=gpar(col="firebrick3", fontsize=11))
      p <- p + annotation_custom(grob3)
      p <- p + annotate("segment", x=257, xend=263, y=99, yend=108, colour="firebrick3") 
    }
    
    if(n2014lows != 0) {
      # Add points to mark 2014 record low temps
      p <- p + geom_point(data=PresentLows, aes(x=newDay, y=tmin), colour="blue3")
      
      # Add annotation for points representing 2014 record low temperatures
      if(n2014lows == 1) {
        nlows <- paste("In 2014 there was 1 day\n",
                       "that was coldest since ", start_year, "\n", sep="")
      } else {
        nlows <- paste("In 2014 there were ", n2014lows," days\n",
                       "that were coldest since ", start_year, "\n", sep="")
      }
      grob4 = textGrob(nlows, x=0.105, y=0.13, hjust=0,
                       gp=gpar(col="blue3", fontsize=11))
      p <- p + annotation_custom(grob4)
      p <- p + annotate("segment", x=33, xend=38, y=30, yend=22, colour="blue3") 
    }
  }
  
  # Add title to plot
  #p <- p +
  #     ggtitle("Dublin (California) Weather in 2014") +
  #     theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="gray30",size=18))
  
  grob1 = grobTree(textGrob("Temperature\n",
                            x=0.02, y=0.92, hjust=0,
                            gp=gpar(col="gray30", fontsize=rel(10.8), fontface="bold")),
                   textGrob(paste("Bars represent range between\n",
                                  "daily high and low temperatures.\n",
                                  sep=""),
                            x=0.02, y=0.85, hjust=0,
                            gp=gpar(col="gray30", fontsize=12.5)))
  
  p <- p + annotation_custom(grob1)
  
  # *** Add legend to explain difference between the different data point layers
  
  # Draw legend for 'background' highest and lowest temps
  if((start_year != 2014) && (hide_hilows != TRUE)) {
    
    # Position of rectangular (grob) color bar
    rect_x <- unit(0.5, "npc")
    rect_y <- unit(0.15, "npc")
    
    # Position of text 'Record High' and 'Record Low'
    textup_x   <- rect_x - unit(0.009,"npc")
    textup_y   <- rect_y + unit(0.095, "npc")
    textdown_x <- textup_x
    textdown_y <- rect_y - unit(0.095, "npc")
    
    # Show color bar (legend) for 'background' highest and lowest temps
    grob_RecordLegend <- grobTree(
      rectGrob(x = rect_x, y = rect_y, width = unit(0.010, "npc"), height = unit(0.20, "npc"),
               just = "centre", hjust = NULL, vjust = NULL, default.units = "npc", name = NULL,
               gp=gpar(col="#CAA586", fill="#CAA586"), vp = NULL),
      resizingTextGrob("RECORD HIGH", x=textup_x, y=textup_y, just="right", hjust=NULL,
                       gp=gpar(col="gray30")),
      resizingTextGrob("RECORD LOW",  x=textdown_x, y=textdown_y, just="right", hjust=NULL,
                       gp=gpar(col="gray30"))
    )
    p <- p + annotation_custom(grob_RecordLegend)
  }
  
  # Draw legend for 'average' (normal) high and low temps
  if((start_year != 2014) && (hide_avgs != TRUE)) {
    
    # Position of rectangular (grob) color bar
    rect_x <- unit(0.5, "npc")
    rect_y <- unit(0.15, "npc")
    
    # Position of upper horizontal line segment for 'Normal Range'
    segup_x0 <- rect_x - unit(0.011, "npc")
    segup_y0 <- unit(0.21, "npc")
    segup_x1 <- segup_x0 + unit(0.003, "npc")
    segup_y1 <- segup_y0
    
    # Position of lower horizontal line segment for 'Normal Range'
    segdown_x0 <- segup_x0
    segdown_y0 <- unit(0.09, "npc")
    segdown_x1 <- segup_x1
    segdown_y1 <- segdown_y0
    
    # Position of vertical line segment for 'Normal Range'
    vsegtop_x    <- segup_x0
    vsegtop_y    <- segup_y0
    vsegbottom_x <- segdown_x0
    vsegbottom_y <- segdown_y0
    
    # Position of text 'Normal Range'
    text_x <- vsegtop_x - unit(0.003,"npc")
    text_y <- rect_y
    
    # Show color bar (legend) for average daily high and low temps
    grob_NormalRange <- grobTree(
      rectGrob(x = rect_x, y = rect_y, width = unit(0.006, "npc"), height = unit(0.12, "npc"),
               just = "centre", hjust = NULL, vjust = NULL, default.units = "npc", name = NULL,
               gp=gpar(col="#A57E69", fill="#A57E69"), vp = NULL),
      segmentsGrob(x0 = vsegtop_x, y0 = vsegtop_y, x1 = vsegbottom_x, y1 = vsegbottom_y,
                   default.units = "npc", arrow = NULL, name = NULL,
                   gp = gpar(col="#A57E69", lwd=2), vp = NULL),
      segmentsGrob(x0 = segup_x0, y0 = segup_y0, x1 = segup_x1, y1 = segup_y1,
                   default.units = "npc", arrow = NULL, name = NULL,
                   gp = gpar(col="#A57E69", lwd=2), vp = NULL),
      segmentsGrob(x0 = segdown_x0, y0 = segdown_y0, x1 = segdown_x1, y1 = segdown_y1,
                   default.units = "npc", arrow = NULL, name = NULL,
                   gp = gpar(col="#A57E69", lwd=2), vp = NULL),
      resizingTextGrob("NORMAL RANGE",  x=text_x, y=text_y, just="right", hjust=NULL,
                       gp=gpar(col="gray30"))
    )
    p <- p + annotation_custom(grob_NormalRange)
  }
  
  # Draw legend for 'Actual' high and low temps
  
  # Position of rectangular (grob) color bar
  rect_x <- unit(0.5, "npc")
  rect_y <- unit(0.19, "npc")
  
  # Position of upper line segment for 'Actual High'
  segup_x0 <- rect_x + unit(0.008,"npc")
  segup_y0 <- unit(0.225, "npc")
  segup_x1 <- rect_x + unit(0.013, "npc")
  segup_y1 <- unit(0.225, "npc")
  
  # Position of lower line segment for 'Actual Low'
  segdown_x0 <- unit(0.508, "npc")
  segdown_y0 <- unit(0.151, "npc")
  segdown_x1 <- unit(0.513, "npc")
  segdown_y1 <- unit(0.151, "npc")
  
  # Position of text 'Actual High' and 'Actual Low' relative to horizontal line segments 
  textup_x <- segup_x1 + unit(0.005, "npc")
  textup_y <- segup_y1
  textdown_x <- segdown_x1 + unit(0.005, "npc")
  textdown_y <- segdown_y1
  
  # Show color bar for 2014 actual highs and lows
  grob_ActualTemps <- grobTree(
    rectGrob(x = rect_x, y = rect_y, width = unit(0.003, "npc"), height = unit(0.08, "npc"),
             just = "centre", hjust = NULL, vjust = NULL, default.units = "npc", name = NULL,
             gp=gpar(col="#4A2123", fill="#4A2123"), vp = NULL),
    segmentsGrob(segup_x0, segup_y0, segup_x1, segup_y1,
                 default.units = "npc", arrow = NULL, name = NULL,
                 gp = gpar(col="#4A2123", lwd=1), vp = NULL),
    segmentsGrob(segdown_x0, segdown_y0, segdown_x1, segdown_y1,
                 default.units = "npc", arrow = NULL, name = NULL,
                 gp = gpar(col="#4A2123", lwd=1), vp = NULL),
    resizingTextGrob("ACTUAL HIGH", textup_x, textup_y,
                     just="left", hjust=NULL, gp=gpar(col="gray30")),
    resizingTextGrob("ACTUAL LOW",  textdown_x, textdown_y,
                     just="left", hjust=NULL, gp=gpar(col="gray30"))
  )
  
  p <- p + annotation_custom(grob_ActualTemps)
  
  #ggsave(file="dublin2014temps.svg", plot=last_plot())
  
  return(p)
}
