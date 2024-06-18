#ALE Project 2024. 
#4/6/2024
#First attempt at data manipulation with R
# Set up working directory first
# Created new project in C:/R Projects folder.
# Creating a project is useful bc every script and folder generated will be set
# project's wd. Also creates a relative wd so it is easy to share
# with colleague or across different computers.You can check wd with:
getwd()

#Create folders in wd for data and output
dir.create('data')
dir.create('output/figures')  #can also do this by pressing new folder in Files tab
install.packages('nlstools')
library(ggplot2) 
library(dplyr)
library(nlstools)


#import bw data.  This bw data standardized to initial mass
bw <- read.csv('data/ALE_data.csv', stringsAsFactors = TRUE)
le <- read.csv('data/ALE_le.csv.csv', stringsAsFactors = TRUE)
#check structure
str(bw)
str(le)

boxplot(bw$gr_mid ~ bw$treatment,
        ylab = "mg/g/day", xlab = "treatment") #bw box and whisker
                                               #standardized to initial mass

# # Define custom labels for the treatment and geno variables
# custom_labels_x <- c("AMB" = "Ambient", 
#                    "ELEV" = "Elevated", 
#                    "HI" = "High", 
#                    "XHI" = "Extra High")  # Customize labels for each level of 'treatment'

labels_genotype <- c("C" = "Coopers",
                     "M" = "Marker 9",
                     "S" = "Sunny Isles")

# 
# # Fit asymptotic nonlinear model for each genotype
# geno_models <- bw %>%
#   group_by(geno) %>%
#   do(model = nls(gr_end ~ SSasympOff(ta, Asym, lrc, c0),
#                  data = .,
#                  start = list(Asym = max(.$gr_end), lrc = -1, c0 = 0))) %>%
#   mutate(fitted_values = predict(model))


# # Define a function to fit a non-linear model to each genotype group
# fit_nonlinear <- function(data) {
#   nls_fit <- nls(gr_end ~ a * ta^b, data = data, start = list(a = 1, b = 1))
#   return(predict(nls_fit))
# }
# 
# # Create the ggplot object with non-linear regression lines for each genotype
# bwgenoplot <- ggplot(data = bw, aes(x = ta, y = gr_end, colour = geno)) +
#   geom_point() +
#   geom_smooth(method = "custom", se = FALSE, 
#               method.args = list(fit_function = fit_nonlinear)) +
#   scale_colour_discrete(labels = labels_genotype) +
#   xlab('TA (umol)') +
#   ylab("Growth rate (mg/g/day)") +
#   labs(colour = "Genotype") +
#   theme_minimal()

# Print or display the plot
#print(bwgenoplot)

bwgenoplot <-  ggplot(aes(x = ta, y = gr_end, colour = geno), data = bw) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    #scale_x_discrete(labels = custom_labels_x) + # Set custom labels for x-axis (treatment)
    scale_colour_discrete(labels = labels_genotype) +
    xlab('TA (umol)')+
    ylab("Growth rate (mg/g/day)")+
    labs(colour = "Genotype")
    #scale_y_continuous(breaks = seq(0, 12, by = 1)) # Add more tick marks on the y-axis

bwgenoplot

legenplot <- ggplot(aes(x = ta, y = le_end, colour = geno), data = le)+
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
#scale_x_discrete(labels = custom_labels_x) + # Set custom labels for x-axis (treatment)
  scale_colour_discrete(labels = labels_genotype) +
  xlab('TA (umol)')+
  ylab("Growth rate (mm/cm/day)")+
  labs(colour = "Genotype")

legenplot

lebox_leios <- boxplot(le$le_end ~ le$treatment,
        ylab = "mm/cm/day", xlab = "treatment") #bw box and whisker
#standardized to initial mass

lebox_imj < - boxplot

