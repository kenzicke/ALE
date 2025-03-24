
library(ggplot2)
library(dplyr)
data <- read.csv('data2/porosity.csv', stringsAsFactors = TRUE)

porplot <- ggplot(data = data, aes(x=treat, y=p))+
  geom_boxplot()+
  theme_classic() +
  labs(x = "Treatment", y = "porosity %")
porplot

bioplot <- ggplot(data = data, aes(x=treat, y=pm))+
  geom_boxplot()+
  theme_classic() +
  labs(x = "Treatment", y = "biomineral density")
bioplot

pbvp <- ggplot(data = data, aes(x=p, y=pb))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  labs (title = "bulk density vs porosity", x = "porosity", y = "bulk density")

pbvp

bdvpm <- ggplot(data = data, aes(x=pm, y=pb))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  labs (title = "bulk density vs biomineral density", x = "biomineral density", y = "bulk density")
bdvpm

pmvp <- ggplot(data = data, aes(x=pm, y=p))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  labs (title = "porosity vs biomineral density", x = "biomineral density", y = "porosity")
pmvp
