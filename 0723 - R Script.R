#######################################################
#######################################################
############    COPYRIGHT - DATA SOCIETY   ############
#######################################################
#######################################################

## 2 INTERACTIVE VISUALIZATION R ##

## NOTE: To run individual pieces of code, select the line of code and
##       press ctrl + enter for PCs or command + enter for Macs


#=================================================-
#### Slide 7: Load tidyverse and set up ggplot2 theme  ####

# Load tidyverse library 
# (it includes ggplot2).
library(tidyverse)
# Save our custom `ggplot` theme to a variable.
my_ggtheme = theme_bw() +                     
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),              
        legend.title = element_text(size = 18),             
        plot.title = element_text(size = 25),               
        plot.subtitle = element_text(size = 18))          


#=================================================-
#### Slide 8: Setup: load dataset for EDA  ####
main_dir = "~/Desktop/af-werx"
data_dir = paste0(main_dir, "/data")
data_dir
setwd(data_dir)
getwd()




# Set working directory to where we store data.
setwd(data_dir)

# Read CSV file called 
#"ChemicalManufacturingProcess.csv".
CMP = 
  read.csv("ChemicalManufacturingProcess.csv",
           header = TRUE)
# View CMP dataset in the tabular data explorer.
View(CMP)


#=================================================-
#### Slide 10: Setup: subset data with select  ####

# Select variables from CMP data.
CMP_subset = select(CMP,                                           #<- set data
                    Yield:BiologicalMaterial03,                    #<- range of columns to select
                    ManufacturingProcess01:ManufacturingProcess03) #<- range of columns to select

# Inspect the first few observations in the subset.
head(CMP_subset)


#=================================================-
#### Slide 11: Setup: wide to long data conversion with gather  ####

CMP_subset_long = CMP_subset %>%
  gather(key = "variable",
         value = "value") 
# Inspect the first few observations.
head(CMP_subset_long)

# Inspect the last few observations.
tail(CMP_subset_long)


#=================================================-
#### Slide 12: Setup: data cleaning with mutate  ####

# Make names of processes and materials
# more user friendly and readable.
CMP_subset_long = CMP_subset_long %>%
  
     # Replace `Biological` with `Bio`.
     mutate(variable = 
              str_replace(variable,     #<- in column `variable`
                          "Biological", #<- replace "Biological"
                          "Bio ")) %>%  #<- with "Bio "
  
     # Replace `Manufacturing` with `Man.`.
     mutate(variable = 
              str_replace(variable, 
                          "Manufacturing", 
                          "Man. ")) %>%
  
     # Remove `0` from numbering.
     mutate(variable = 
              str_replace(variable, 
                          "0", 
                          " "))
# Inspect few first 
# entries in the data.
head(CMP_subset_long)

# Inspect few last 
# entries in the data.
tail(CMP_subset_long)


#=================================================-
#### Slide 16: Set up & link data: make boxplots  ####

# A basic boxplot with pre-saved theme.
boxplots = ggplot(CMP_subset_long,   #<- set the base plot + data
                  aes(x = variable,  #<- map `variable` to x-axis
                      y = value)) +  #<- map `value` to y-axis
           geom_boxplot() +          #<- add boxplot geom
           my_ggtheme                #<- add pre-saved theme

# View plot.
boxplots


#=================================================-
#### Slide 17: Adjust: boxplot aesthetics  ####

# Make color of fill based on variable.
boxplots = ggplot(CMP_subset_long,        
                  aes(x = variable, 
                      y = value, 
                      fill = variable)) + #<- add fill to aesthetics
  geom_boxplot() + 
  my_ggtheme

# View plot.
boxplots


#=================================================-
#### Slide 18: Adjust & polish: boxplot legends  ####

# Remove redundant legend.
boxplots = boxplots + 
  labs(title = "CMP data variables", #<- add title and subtitle
       subtitle = "Boxplot of unscaled data") +
  guides(fill = FALSE)               #<- remove fill legend

# View plot.
boxplots


#=================================================-
#### Slide 20: Setup: normalize data with group_by + mutate  ####

# Normalize the CMP data.
CMP_subset_long = CMP_subset_long %>%
  group_by(variable) %>%              #<- group values by variable
  mutate(norm_value =                 #<- make `norm_value` column
           value/max(value,           #<- divide value by group max
                     na.rm = TRUE))   #<- don't forget the NAs!

CMP_subset_long # norm values included!


#=================================================-
#### Slide 21: Setup: make base plot with normalized data  ####

# Construct the base plot for normalized data.
base_norm_plot = ggplot(CMP_subset_long,        #<- set data
                        aes(x = variable,       #<- map `variable`
                            y = norm_value,     #<- map `norm_value`
                            fill = variable)) + #<- set fill
                 my_ggtheme                     #<- add theme

# View base plot + theme.
base_norm_plot


#=================================================-
#### Slide 22: Adjust: normalized boxplot's effects & legends  ####

# Make color of fill based on variable.
boxplots_norm = base_norm_plot +     #<- set base plot   
                geom_boxplot() +     #<- add geom
                guides(fill = FALSE) #<- remove redundant legend 

# View updated plot.
boxplots_norm


#=================================================-
#### Slide 23: Polish: normalized boxplot details  ####

# Make outliers stand out with red color and bigger size.
boxplots_norm = boxplots_norm +       #<- previously saved plot
  geom_boxplot(outlier.color = "red", #<- adjust outlier color
               outlier.size = 5) +    #<- adjust outlier size
  labs(title = "CMP data variables",  #<- add title and subtitle
       subtitle = "Boxplot of scaled data")

# View updated plot.
boxplots_norm


#=================================================-
#### Slide 25: Setup the density plot of normalized data   ####

# Let's save base plot for density as well.
density_norm = ggplot(CMP_subset_long,   #<- set data
                        aes(x = norm_value, #<- map
                            fill = variable)) + #<- map fill
                 my_ggtheme  + #<- add theme
                geom_density(alpha = 0.3) #<- adjust fill
density_norm


#=================================================-
#### Slide 26: Adjust the axes and visual effects of density plot  ####

# Instead of overlaying densities, split them into individual plots called facets.
density_norm = density_norm +                        #<- previously saved plot
               facet_wrap (~ variable,               #<- make facets by variable
                           ncol = 4) +               #<- set a 4-column grid
              geom_vline(data = CMP_subset_long,     #<- set data
                          aes(xintercept =           #<- set x-intercept 
                                mean(norm_value,     #<- to mean
                                     na.rm = TRUE),  #<- handle NAs!  
                              color = variable),     #<- map color
                          linetype = "dashed",       #<- set line type 
                          size = 1.5)                #<- set line size

# View updated plot.
density_norm


#=================================================-
#### Slide 27: Polish: legends of density plot  ####

# Remove redundant legend.
density_norm = density_norm +           #<- previously saved plot
               guides(fill = FALSE,     #<- no legend for `fill` of density
                      color = FALSE) +  #<- no legend for `color` of line
              theme(strip.text.x = 
         element_text(size = 14)) +     #<- set strip text size
  labs(title = "CMP data variables",    #<- add title and subtitle
       subtitle = "Density of scaled data")
density_norm


#=================================================-
#### Slide 29: Exercise 1  ####




#=================================================-
#### Slide 33: Setup: transform data for scatterplot  ####

CMP_subset_long2 = CMP_subset %>%
  gather(BiologicalMaterial01:ManufacturingProcess03, #<- gather all variables but `Yield`
         key = "variable",                            #<- set key to `variable`
         value = "value") %>%                         #<- set value to `value`
  # All other transformations we've done before.
  mutate(variable = str_replace(variable, "Biological", "Bio ")) %>%
  mutate(variable = str_replace(variable, "Manufacturing", "Man. ")) %>%
  mutate(variable = str_replace(variable, "0", " ")) %>%
  group_by(variable) %>% ## WITHIN EACH GROUP (VARIABLE)
  mutate(norm_value = value/max(value, na.rm = TRUE)) ## Find the norm value

# Inspect the data.
head(CMP_subset_long2)


#=================================================-
#### Slide 34: Setup & link: normalized data base plot  ####

# Create a base plot.
base_norm_plot = ggplot(data = CMP_subset_long2,  #<- set data 
                        aes(x = norm_value,       #<- set x-axis to represent normalized value
                            y = Yield,            #<- y-axis to represent `Yield` 
                            color = variable)) +  #<- set color to depend on `variable`
                        my_ggtheme                #<- set theme


#=================================================-
#### Slide 35: Setup & adjust: normalized data scatterplot  ####

# Create a scatterplot.
scatter_norm = base_norm_plot +        #<- base plot
               geom_point(size = 3,    #<- add point geom with size of point = 3
                          alpha = 0.7) #<- make it 70% opaque

# View updated plot.
scatter_norm


#=================================================-
#### Slide 36: Adjust: add density geom to scatterplot  ####

# Adjust scatterplot to include 2D density.
scatter_norm = scatter_norm +              #<- previously saved plot
               geom_density2d(alpha = 0.7) #<- add 2D density geom with 70% opaque color

# View updated plot.
scatter_norm


#=================================================-
#### Slide 37: Adjust: wrap scatterplots in facets  ####

# Wrap each scatterplot into a facet.
scatter_norm = scatter_norm +                   #<- previously saved plot
               facet_wrap(~ variable, ncol = 3) #<- wrap plots by variable into 3 columns

# View updated plot.
scatter_norm


#=================================================-
#### Slide 38: Adjust & polish: legends and text in scatterplot  ####

# Add finishing touches to the plot.
scatter_norm = scatter_norm +                        #<- previously saved plot
  guides(color = FALSE) +                            #<- remove legend for color mappings
  theme(strip.text.x = element_text(size = 14)) +    #<- increase text size in strips of facets
  labs(title = "CMP data: Yield vs. other variables",#<- add title and subtitle
       subtitle = "2D distribution of scaled data")

# View updated plot.
scatter_norm


#=================================================-
#### Slide 40: Saving plots in R: PNG exported  ####

# Set working directory 
# to where we want to save our plots.
setwd(plot_dir)

png("CMP_boxplots_norm.png",
    width = 1200, 
    height = 600, 
    units = "px")
boxplots_norm
dev.off()

png("CMP_density_norm.png",
    width = 1200, 
    height = 600, 
    units = "px")
density_norm
dev.off()

png("CMP_scatterplot_norm.png",
    width = 1200, 
    height = 600, 
    units = "px")
scatter_norm
dev.off()


#=================================================-
#### Slide 43: Saving plots in R: PDF exported  ####

# Set working directory to where you want to save plots.
setwd(plot_dir)

pdf("CMP_plots.pdf", #<- name of file
    width = 16,      #<- width in inches
    height = 8)      #<- height ...

boxplots_norm        #<- plot 1
density_norm         #<- plot 2
scatter_norm         #<- plot 3

dev.off()            #<- clear graphics device


#=================================================-
#### Slide 45: Exercise 2  ####




#=================================================-
#### Slide 47: Interactive visualizations: highcharter  ####

# Install `highcharter` package.
install.packages("highcharter")

# Load the library.
library(highcharter)

# View documentation.
library(help = "highcharter")


#=================================================-
#### Slide 55: Highcharts generic function hchart: scatter  ####

# Construct an interactive scatterplot.
scatter_interactive =              #<- name the plot   
  hchart(CMP_subset_long2,         #<- set data
         "scatter",                #<- make type "scatter"
          hcaes(x = norm_value,    #<- map x-axis
                y = Yield,         #<- map y-axis
                group = variable)) #<- group by



#=================================================-
#### Slide 56: Highcharts generic function hchart: scatter  ####

scatter_interactive


#=================================================-
#### Slide 58: Highcharts generic function hchart: scatter  ####

# Pipe chart options to original chart.
scatter_interactive = scatter_interactive %>%
  # Use chart options to specify zoom.
  hc_chart(zoomType = "xy") 

scatter_interactive


#=================================================-
#### Slide 59: Highcharts generic function hchart: scatter  ####

# Pipe chart options to original chart.
scatter_interactive = scatter_interactive %>%
 # Add title to your plot.
 hc_title(text = "CMP data: Yield vs. other variables")

scatter_interactive


#=================================================-
#### Slide 60: Correlation matrix with hchart  ####

# Compute a correlation matrix for the first 
# 4 variables in our data.
cor_matrix = cor(CMP_subset[, 1:4])

# Construct a correlation plot by 
# simply giving the plotting function
# a correlation matrix.
correlation_interactive = hchart(cor_matrix) %>%
 # Add title to your plot.
 hc_title(text = "CMP data: correlation")

correlation_interactive


#=================================================-
#### Slide 62: Exercise 3  ####




#=================================================-
#### Slide 65: Summary column plot with hchart  ####

# Create data summary.
CMP_summary = summary(CMP_subset)

# Save it as a dataframe.
CMP_summary = as.data.frame(CMP_summary)

# Inspect the data.
head(CMP_summary)
# Remove an empty variable.
CMP_summary$Var1 = NULL

# Rename remaining columns.
colnames(CMP_summary) = c("Variable", 
                          "Summary")
# Inspect updated data.
head(CMP_summary)


#=================================================-
#### Slide 66: Summary column plot with hchart  ####

# Separate `Summary` column into 2 columns.
CMP_summary = CMP_summary %>%              #<- set original data 
  separate(Summary,                        #<- separate `Summary` variable 
           into = c("Statistic", "Value"), #<- into 2 columns: `Statistic`, `Value`
           sep = ":",                      #<- set separating character
           convert = TRUE)                 #<- where applicable convert data (to numeric)

# Inspect the first few entries in the data.
head(CMP_summary)

# Inspect total number of rows in data including NAs.
nrow(CMP_summary)


#=================================================-
#### Slide 67: Summary column plot with hchart  ####

# Inspect `Value` column for `NAs`.
which(is.na(CMP_summary$Value) == TRUE)

# Subset only rows where `Value` is not NAs.
CMP_summary = subset(CMP_summary, !is.na(Value))

# Now the number of rows should be 4 less.
nrow(CMP_summary)

# Construct the summary chart.
CMP_summary_interactive = 
  hchart(CMP_summary,             #<- set data
         "column",                #<- set type (`column` in highcharts)
         hcaes(x = Statistic,     #<- arrange `Statistics` across x-axis
               y = Value,         #<- map `Value` of each `Statistic` to y-axis
               group = Variable)) #<- group columns by `Variable`



#=================================================-
#### Slide 68: Summary column plot with hchart  ####

CMP_summary_interactive


#=================================================-
#### Slide 69: Summary column plot with hchart  ####

# Adjust tooltip options by piping `hc_tooltip` to base plot.
CMP_summary_interactive = CMP_summary_interactive %>% 
  hc_tooltip(shared = TRUE)  %>%               #<- `shared` needs to be set to `TRUE`
  hc_title(text = "CMP data variable summary") #<- add title to your plot



#=================================================-
#### Slide 70: Summary column plot with hchart  ####

CMP_summary_interactive


#=================================================-
#### Slide 73: Highcharts boxplot: hcboxplot  ####

# Construct an interactive boxplot.
boxplot_interactive =  
  hcboxplot(x = CMP_subset_long$norm_value,
            var = CMP_subset_long$variable,
            name = "Normalized value") %>%
  hc_title(text = "CMP data variables")
boxplot_interactive


#=================================================-
#### Slide 76: Highcharts boxplot: hcboxplot  ####

# Enhance original boxplot with some options.
boxplot_interactive = boxplot_interactive %>% 
  hc_plotOptions(       #<- plot options
    boxplot = list(     #<- for boxplot 
      colorByPoint = TRUE)) #<- color each box
boxplot_interactive


#=================================================-
#### Slide 79: Compound plots: density + lines example  ####

layered_density_interactive = highchart() %>%
  hc_chart(type = "area") %>%
  hc_add_series(data = density(CMP_subset$Yield),
                name = "Yield") %>%
  hc_add_series(data = density(CMP_subset$BiologicalMaterial01),
                name = "Bio Material 1") %>%
  hc_add_series(data = density(CMP_subset$ManufacturingProcess01, na.rm = TRUE),
                name = "Man. Process 1") %>%
  hc_xAxis(plotLines = list(
            list(label = list(text = "Yield avg."),
                  width = 2,
                  color = "red",
                  value = mean(CMP_subset$Yield)),
            list(label = list(text = "Bio Material 1 avg."),
                  width = 2,
                  color = "red",
                  value = mean(CMP_subset$BiologicalMaterial01)),
            list(label = list(text = "Man. Process 1 avg."),
                  width = 2,
                  color = "red",
                  value = mean(CMP_subset$ManufacturingProcess01, na.rm = TRUE)))) %>%
  hc_tooltip(crosshairs = TRUE) %>%
  hc_title(text = "CMP data: density and average of select variables")


#=================================================-
#### Slide 80: Compound plots: highchart with layers  ####

layered_density_interactive


#=================================================-
#### Slide 83: Exercise 4  ####




#######################################################
####  CONGRATULATIONS ON COMPLETING THIS MODULE!   ####
#######################################################
