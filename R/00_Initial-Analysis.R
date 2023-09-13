###
# Analysis for Forest Biodiversity Data
###

rm(list = ls())
library(ggplot2)
library(dplyr)
library(dunn.test)

# Load in the data
# setwd('C:/Users/abart/OneDrive/Documents/UofSC/Classes/BIOL570L')
# read.csv('forest_biodiversity_data.csv') # whatever you named it

raw_data <- read.csv('./data/2023_WS2_Data-Share.csv') # read in the raw data

# Three-goals we need to calcuate species richness, shannon-wiener index, and pileau's evennes.

# Richness ###############

richness_df <- raw_data |> 
  group_by(Region, Quadrat) |> #group these
  summarise(richness = length(unique(Taxa))) # make a column for the # of unique taxa

head(richness_df)


# |- Plotting ------------------
# need to make a summary df

richness_plot_df <- richness_df |> 
  group_by(Region) |> 
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness))


ggplot(richness_plot_df) +
  geom_bar(aes(x = Region, y = mean_richness),
           stat = 'identity')+
  geom_errorbar(aes(x = Region, ymin = mean_richness,
                    ymax = mean_richness + sd_richness))+
  theme_bw()

# What if I want my figures in a different order?
richness_plot_df$Region <- factor(richness_plot_df$Region, levels = c("High-Disturbance",
                                                                      "Mid-Disturbance",
                                                                      "Low-Disturbance"))

ggplot(richness_plot_df) +
  geom_bar(aes(x = Region, y = mean_richness),
           stat = 'identity', fill = 'black')+
  geom_errorbar(aes(x = Region, ymin = mean_richness,
                    ymax = mean_richness + sd_richness),
                width = 0.5, color = 'black')+
  labs(x = "Region",y = "Mean Richness")+
  theme_bw()



# |- Statistical Analysis ----------------

# is it continuous?
density(richness_df$richness) |> 
  plot()

# clearly no, let's use a non-parametric test
# Here, kruskall-wallace makes sense

kruskal.test(richness_df$richness ~ richness_df$Region) # Tell's us it is significantly different

dunn.test(x = richness_df$richness, g = richness_df$Region) # Gives a group-specific differences



# Shannon's Diversity #################

# more complicated

# just for one case:
quadrat_1 <-  raw_data |> 
  filter(Region == "Low-Disturbance", Quadrat == '1')

quadrat_1

p <- quadrat_1$Count / sum(quadrat_1$Count) # create counts
lnp <- log(p)
-sum(p * lnp)


# We can write a function to do this repeatedly!
diversity_calculator <- function(count) {
  p <- count / sum(count) # create counts
  lnp <- log(p)
  H <-  -sum(p * lnp)
  return(H)
}

diversity_df <- raw_data |> 
  group_by(Region, Quadrat) |> 
  summarise(H = diversity_calculator(Count))

# |- Plotting ------------------------

div_plot <- diversity_df |> 
  group_by(Region) |> 
  summarize(mean_H = mean(H),
            sd_H = sd(H))

# What if I want my figures in a different order?
div_plot$Region <- factor(div_plot$Region, levels = c("High-Disturbance",
                                                                      "Mid-Disturbance",
                                                                      "Low-Disturbance"))

ggplot(div_plot) +
  geom_bar(aes(x = Region, y = mean_H),
           stat = 'identity', fill = 'black')+
  geom_errorbar(aes(x = Region, ymin = mean_H,
                    ymax = mean_H + sd_H),
                width = 0.5, color = 'black')+
  labs(x = "Region",y = "Shannon's H")+
  theme_bw()

# |- Statistics ----------------------------------

density(diversity_df$H) |> 
  plot()

# try a shapiro test
shapiro.test(diversity_df$H)

aov(diversity_df$H ~ diversity_df$Region) |> 
  summary()

aov(diversity_df$H ~ diversity_df$Region) |> 
  TukeyHSD()


# Evenness ####################

# Calculation for one of them

n = quadrat_1$Count
N = sum(quadrat_1$Count)

D = 1 - sum((n*(n-1))/(N*(N-1)))

pileou_calculator <- function(count) {
  n = count
  N = sum(count)
  D = 1 - sum((n*(n-1))/(N*(N-1)))
  return(D)
}

# calculate for all!
evenness_df <- raw_data |> 
  group_by(Region, Quadrat) |> 
  summarize(D = pileou_calculator(Count))


# |- Plotting ------------------

evenness_plot <- evenness_df |> 
  group_by(Region) |> 
  summarize(mean_D = mean(D),
            sd_D = sd(D))


# What if I want my figures in a different order?
evenness_plot$Region <- factor(evenness_plot$Region, levels = c("High-Disturbance",
                                                      "Mid-Disturbance",
                                                      "Low-Disturbance"))

ggplot(evenness_plot) +
  geom_bar(aes(x = Region, y = mean_D),
           stat = 'identity', fill = 'black')+
  geom_errorbar(aes(x = Region, ymin = mean_D,
                    ymax = mean_D + sd_D),
                width = 0.5, color = 'black')+
  labs(x = "Region",y = "Pileou's D")+
  theme_bw()


# |- Stats -----------------------



density(evenness_df$D) |> 
  plot()

# try a shapiro test
shapiro.test(evenness_df$D)


dunn.test(x = evenness_df$D, g =  evenness_df$Region)


