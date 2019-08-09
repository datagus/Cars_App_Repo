library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
setwd("~/Dropbox/Data Science Study/Coursera Hopkins Data Series/9 Developing Data Products/Week 4 Project")
cars <- read.csv("cars.csv")
oilP <- read.csv("oil prices.csv", header = FALSE, col.names = c("year", "pr", "adj"))
cardat <- cars %>% 
        filter(fuel == "Regular" | fuel == "Premium") %>%
        select(year, make, disp, mpg, class) %>%
        group_by(make, year) %>% 
        summarize(m22 = round(100 * mean(mpg >= 22), 1)) %>%
        mutate(oil = oilP[match(year, oilP$year), "adj"], 
               yr = str_pad(year %% 100, 2, pad = "0"))
head(cardat)
str(cardat)

## For testing, read  data for some specific subset
## 
carsamp <- cardat %>% 
        filter(make == "Toyota" | make == "Honda", 
               year >= 1984 & year <= 2016)



head(carsamp)
write.csv(carsamp, "carsample.csv")


## Try freqpoly instead
# 
ggplot(carsamp, aes(x = year, y = m25)) +
        geom_freqpoly(stat = "identity", aes(color = make)) + 
        geom_point(pch = 21, size = 1.7, color = "gray30", 
                   aes(fill = factor(oil < 50, labels = c("Oil over $50", "Oil under $50")))) +
        scale_color_manual(values = c("steelblue", "orangered3")) + 
        scale_fill_manual(values = c("gray60", "white")) +
        scale_x_continuous(breaks = seq(min(carsamp$year) + 5 -  min(carsamp$year) %% 5, 
                                        max(carsamp$year) - max(carsamp$year) %% 5, 
                                        by = 5)) +
        theme(legend.position = c(0.5, 0.9),
              legend.box = "horizontal",
              legend.margin = margin(0, 5, 4, 5),
              legend.key = element_rect(color = NULL, fill = "white"),
              legend.box.background = element_rect(color = "gray40"),
              legend.box.margin = margin(1, 1, 1, 1),
              legend.justification = "center") + 
        guides(fill = guide_legend(title = NULL, nrow = 1),
               color = guide_legend(title = NULL, nrow = 1)) + 
        labs(x = "Year", 
             y = "Percent of Models with MPG > 25", 
             title = "Efficiency Timeline for Car Makes")



## Simple bar
# 
ggplot(mpgDat, aes(x = as.factor(year), y = m25)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill = make)) + 
        scale_fill_manual(values = c("steelblue", "red3")) + 
        scale_x_discrete(breaks = seq(min(mpgDat$year) + 5 -  min(mpgDat$year) %% 5, 
                                      max(mpgDat$year) - max(mpgDat$year) %% 5, 
                                      by = 5)) +
        # labels = c(mpgDat$yr)) + 
        theme(legend.position = c(0.4, 0.9)) + 
        guides(fill = guide_legend(nrow = 1)) + 
        labs(x = "Year", 
             y = "Models with MPG > 25", 
             title = "Efficiency Timeline for Make",
             fill = "Oil Price Greater than $50 (2016 Dollars)")






## Boxplot to compare makes
## 
ggplot(cardat, aes(x = as.factor(year), y = mpg)) +
        geom_boxplot(aes(color = make)) + 
        scale_color_manual(values = c("red3", "dodgerblue"))

ctrs <- barplot(mpgDat$m25, names.arg = mpgDat$year, yaxt = "n", border = "steelblue",
        col = "steelblue", ylim = c(0, 60))
axis(side = 2)
par(new = TRUE)
points(mpgDat$oilPr ~ ctrs, type = "b", cex = 0.6, pch = 21, col = "darkred", 
     yaxt = "n", ylim = c(0, 120))
axis(side = 4)


summary(mpgDat$oilPr)

## Example of multiplot
## 
set.seed(1235)
dat1 <- sample(10, 5)
dat2 <- sample(50, 5)

dev.off()
par(mar = c(2, 4, 2, 4))
cntrs <- barplot(dat1)
xlim0 <- par()$usr[1:2]
par(new = TRUE)
plot.window(xlim = c(0, xlim0), ylim = c(0, 50), xaxs = "i")
points(dat2 ~ cntrs, col = "darkred")
axis(side = 4, col = "darkred")

par()

plot(x,y1,pch=0,type="b",col="red",yaxt="n",ylim=c(-8,2))
par(new=TRUE)
plot(x,y2,pch=1,type="b",col="blue",yaxt="n",ylim=c(98,105))
##
##End example


class(cardat)
plot(cardat$make, cardat$mpg, cex = 0.6, pch = 19)

model <- TRUE

ggplot(cardat, aes(disp, mpg)) + 
        geom_jitter(aes(col = make), width = 0.5, cex = 0.6, pch = 19, alpha = 0.4) + 
        labs(title = "Basic Plot")
        # if(model) { 
        geom_smooth(aes(col = make), method = lm, se = FALSE, lwd = 0.4)
        #                     method = "lm", se = FALSE)
        # }
        

head(cardat$disp, 40)

## Multiplot example
## 
x <- 1:10
y1 <- rnorm(10)
y2 <- rnorm(10)+100

plot(x,y1,pch=0,type="b",col="red",yaxt="n",ylim=c(-8,2))
par(new=TRUE)
plot(x,y2,pch=1,type="b",col="blue",yaxt="n",ylim=c(98,105))

axis(side=2)
axis(side=4)





# sliderInput(inputId = "year", 
#             label = strong("Years to Consider"),
#             min = 1984,
#             max = 2018,
#             value = c(2005, 2015),
#             step = 1,
#             round = TRUE

