# DataRes Winter Visualization

library(tidyverse)
library(dplyr)
pm_conc <- read.csv("Air_Quality__LA_.csv")
tail(pm_conc)

head(pm_conc["Count"])
pm_conc %>%  group_by(Neighborhood) %>% summarize(size=n())

# 2014 
year2014 <- pm_conc %>%
  group_by(Neighborhood) %>%
  filter(Year == "2014") %>%
  summarise(mean_pm = mean(Count)) %>%
  arrange(desc(mean_pm))

# 2015 
year2015 <- pm_conc %>%
  group_by(Neighborhood) %>%
  filter(Year == 2015) %>%
  summarise(mean_pm = mean(Count))

# 2016 
year2016 <- pm_conc %>%
  group_by(Neighborhood) %>%
  filter(Year == 2016) %>%
  summarise(mean_pm = mean(Count))

# 2017 
year2017 <- pm_conc %>%
  group_by(Neighborhood) %>%
  filter(Year == "2017") %>%
  summarise(mean_pm = mean(Count)) %>%
  arrange(desc(mean_pm))


pm_2014 <- year2014$mean_pm[1:10]
pm_2017 <- year2017$mean_pm[1:10]

year_category <- c("2014", "2017")
pm <- c(pm_2014, pm_2017)

jpeg(file="visualization.jpeg")
gfg <- data.frame(pm,
                  grp = rep(year_category, each = 10),
                  subgroup = LETTERS[1:10])
gfg <- reshape(gfg,idvar = "subgroup",
               timevar = "grp",
               direction = "wide")
row.names(gfg) <- gfg$subgroup
gfg <- gfg[ , 2:ncol(gfg)]
colnames(gfg) <- year_category
gfg <- as.matrix(gfg)
color <- colorRampPalette(colors = c("yellow", "red"))(10)

# Create grouped barplot
barplot(height = gfg,beside = TRUE, ylim = c(0,16), xlab = "Year", ylab = "Proportion",
        main = "PM2.5 Concentration in LA from 2014 and 2017", col = color)
dev.off()




