# Chapter 3
library(yarrr)
library(tidyverse)

summary(pirates)
View(pirates)


pirates %>% group_by(sex) %>% 
     summarise(mean(age))

ggplot(data=pirates, aes(height, weight))+
     geom_point(alpha=0.1)+
     geom_smooth(method='lm')

pirateplot(formula = age ~ sword.type, 
           data = pirates,
           main = "Pirateplot of ages by favorite sword")


pirateplot(formula = height ~ sex,               # Plot weight as a function of sex
           data = pirates,                       
           main = "Pirateplot of height by sex",
           pal = "pony",                         # Use the info color palette
           theme = 3)                            # Use theme 3

piratepal(palette = "pony",
          plot.result = TRUE,   # Plot the result
          trans = 0.1) 

# Age by headband t-test
t.test(formula = age ~ headband,
       data = pirates,
       alternative = 'two.sided')

cor.test(formula = ~ height + weight,
         data = pirates)

# Create tattoos model
tat.sword.lm <- lm(formula = tattoos ~ sword.type,
                   data = pirates)

# Get ANOVA table
anova(tat.sword.lm)
table(pirates$sword.type)



tchests.model <- lm(formula = tchests ~ age + weight + tattoos,
                    data = pirates)
summary(tchests.model)

library(BayesFactor)

ttestBF(formula = age ~ headband,
        data = pirates)

#Chapter 5
# 5 samples from a Normal dist with mean = 0, sd = 1
rnorm(n = 5, mean = 0, sd = 1)


# 5 samples from Uniform dist with bounds at 0 and 1
runif(n = 5, min = 0, max = 1)

set.seed(100)

rnorm(3, mean = 0, sd = 1)


seq(1,10)
seq(2.1,8.1,by=2)
seq(0,15, by=5)
seq(0,15, length.out = 4)


rnorm(n = 50, mean = 20, sd = 2)
runif(n = 25, min = -100, max = -50)

# Chapter 6

midterm <- c(62, 68, 75, 79, 55, 62, 89, 76, 45, 67)
final <- c(78, 72, 97, 82, 60, 83, 92, 73, 50, 88)
midterm <- midterm + 5
final - midterm
midterm.z <- (midterm - mean(midterm)) / sd(midterm)
final.z <- (final - mean(final)) / sd(final)

a <- 2^(0:63)
sum(a)/64000


movie <- c("Whatever Works", "It Follows", "Love and Mercy", 
           "The Goonies", "Jiro Dreams of Sushi",
           "There Will be Blood", "Moon", 
           "Spice World", "Serenity", "Finding Vivian Maier")

year <- c(2009, 2015, 2015, 1985, 2012, 2007, 2009, 1988, 2005, 2014)

boxoffice <- c(35, 15, 15, 62, 3, 10, 321, 79, 39, 1.5)

genre <- c("Comedy", "Horror", "Drama", "Adventure", "Documentary", 
           "Drama", "Science Fiction", "Comedy", "Science Fiction", 
           "Documentary")

time <- c(92, 97, 120, 90, 81, 158, 97, -84, 119, 84)


rating <- c("PG-13", "R", "R", "PG", "G", "R", "R", 
            "PG-13", "PG-13", "Unrated")

movie[10]
genre[1:4]
movie[movie == "Spice World"] <- "The Naked Gun"
movie[year<1990]


median(boxoffice[rating %in% c("G", "PG")])

mean(rating == "R" | genre == "Comedy")



piratesurvey <- data.frame(
     name = c("Astrid", "Lea", "Sarina", "Remon", "Letizia", "Babice", "Jonas", "Wendy", "Niveditha", "Gioia"),
     sex = c("F", "F", "F", "M", "F", "F", "M", "F", "F", "F"),
     age = c(30, 25, 25, 29, 22, 22, 35, 19, 32, 21),
     superhero = c("Batman", "Superman", "Batman", "Spiderman", "Batman",
                   "Antman", "Batman", "Superman", "Maggott", "Superman"),
     tattoos = c(11, 15, 12, 5, 65, 3, 9, 13, 900, 0),
     stringsAsFactors = FALSE
)

survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" = c(99, 46, 23, 54, 23),
                     stringsAsFactors = FALSE)
health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))
health %>% 
     mutate(bmi= 703*height/(weight^2))

getwd()
ls()


exam <- data.frame(
     id = 1:5,
     q1 = c(1, 5, 2, 3, 2),
     q2 = c(8, 10, 9, 8, 7),
     q3 = c(3, 7, 4, 6, 4))

demographics <- data.frame(
     id = 1:5,
     sex = c("f", "m", "f", "f", "m"),
     age = c(25, 22, 24, 19, 23))

combined <- left_join(exam, demographics, by= "id")
combined %>% 
     group_by(sex) %>% 
     summarise(mean(q1))

combined %>% 
     filter(age>20) %>% 
     group_by(sex) %>% 
     summarise(median(q3))

dim(movies)
movies %>% # From the movies dataframe...
     filter(genre != "Horror" & time > 50) %>% # Select only these rows
     group_by(rating, sequel) %>%
     summarise(n())
     # Group by rating and sequel
     summarise( #
          frequency = n(), # How many movies in each group?
          budget.mean = mean(budget, na.rm = T),  # Mean budget?
          revenue.mean = mean(revenue.all), # Mean revenue?
          billion.p = mean(revenue.all > 1000))

caffeine <- read.table("https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt", sep= "\t" )

names(caffeine)
caffeine %>%
     group_by(gender) %>% 
     summarise(mean(age))

caffeine %>%
     group_by(drink, gender) %>% 
     summarise(mean(age))

caffeine %>%
     group_by(age) %>% 
     summarise(median(score))

caffeine %>%
     filter(gender=="male") %>% 
     group_by(age) %>% 
     summarise(max(score))

caffeine %>%
     group_by(drink) %>% 
     summarise( mean(score), median(score), max(score), sd(score), n()) 
demo("colors")

gray(0.5,0.5)
                

#transparent colours
plot(x = pirates$height, 
     y = pirates$weight, 
     col = yarrr::transparent("blue", trans.val = .9), 
     pch = 16, 
     main = "col = yarrr::transparent('blue', .9)")

plot(x = 1:10,                         # x-coordinates
     y = 1:10,                         # y-coordinates
     type = "p",                       # Just draw points (no lines)
     main = "My First Plot",
     xlab = "This is the x-axis label",
     ylab = "This is the y-axis label",
     xlim = c(0, 11),                  # Min and max values for x-axis
     ylim = c(0, 11),                  # Min and max values for y-axis
     col = "blue",                     # Color of the points
     pch = 11,                         # Type of symbol (16 means Filled circle)
     cex = 2)     

hist(x = ChickWeight$weight,
     main = "Chicken Weights",
     xlab = "Weight",
     xlim = c(0, 500))

hist(x = ChickWeight$weight,
     main = "Fancy Chicken Weight Histogram",
     xlab = "Weight",
     ylab = "Frequency",
     breaks = 20, # 20 Bins
     xlim = c(0, 500),
     col = "papayawhip", # Filling Color
     border = "hotpink") # Border Color

barplot(height = 1:5,  # A vector of heights
        names.arg = c("G1", "G2", "G3", "G4", "G5"), # A vector of names
        main = "Example Barplot", 
        xlab = "Group", 
        ylab = "Height")


# Calculate mean weights for each time period
diet.weights <- aggregate(weight ~ Time, 
                          data = ChickWeight,
                          FUN = mean)
diet.weights
# Create barplot
barplot(height = diet.weights$weight,
        names.arg = diet.weights$Time,
        xlab = "Week",
        ylab = "Average Weight",
        main = "Average Chicken Weights by Time",
        col = "mistyrose")

diet.weights.tidy <- ChickWeight %>% 
     group_by(Time) %>% 
     summarise(mean.weight=mean(weight))
diet.weights.tidy

ggplot(data=diet.weights.tidy)+
     geom_bar(aes(x=Time, y= mean.weight), stat = "identity", colour="black",fill="mistyrose")+
     labs(title="Average Chicken Weights by Time",
          subtitle= "Tried to reproduce in tidyverse",
          x= "Week",
          y= "Awarage Weight",
          caption= "Not sure what to type here")

swim.data <- cbind(c(2.1, 3), # Naked Times
                   c(1.5, 3)) # Clothed Times
swim.data <- as.data.frame(swim.data)
colnames(swim.data) <- c("Naked", "Clothed")
swim.data$shark.situ <- cbind(c("No Shark", "Shark"))
swim.data
swim.data.long <- reshape2::melt(swim.data)

swim.data.long

ggplot(swim.data.long)+
     geom_bar(aes(x=shark.situ, y=value, fill= shark.situ), stat = "identity")+
     facet_wrap(~variable)

yarrr::pirateplot(formula = weight ~ Time, # dv is weight, iv is Diet
                  data = ChickWeight,
                  main = "Pirateplot of chicken weights",
                  xlab = "Diet",
                  ylab = "Weight")

library(yarrr)
yarrr::pirateplot(formula = height ~ sex + headband,    # DV = height, IV1 = sex, IV2 = headband
                  data = pirates,           
                  theme = 3,
                  main = "Pirate Heights",
                  pal = "gray")

download.file(url = "https://github.com/ndphillips/ThePiratesGuideToR/raw/master/data/BeardLengths.txt",
                   destfile = "beardlengths.txt")
a <- read.table("beardlengths.txt")
str(a)

barplot(height = a$Beard,
        names.arg = a$Ship)
 
beards.sum <- aggregate(Beard~Ship,
                    data=a,
                    FUN = sum)

barplot(height = beards.sum$Beard,
        names.arg = beards.sum$Ship)

str(BeardLengths)
yarrr::pirateplot(formula = Beard~Ship, data = BeardLengths)

?plot

plot( pirates$age, pirates$parrots,
     pch = 16, col = gray(level = .5, alpha = .1),
     xlab = "Age", ylab = "Parrots",
     main = "Pirate age and number of parrots owned"
)

yarrr::piratepal("basel",            
                 plot.result = TRUE,
                 trans = .1)
installed.packages('yarrr')
yarrr::piratepal("all")

yarrr::piratepal("evildead",            
                 plot.result = TRUE,
                 trans = .1) 

google.cols <- yarrr::piratepal(palette = "google", 
                         trans = .2)
barplot(height = 1:5,
        col = google.cols,
        border = "white",
        main = "Barplot with the google palette")

library("RColorBrewer")
display.brewer.all()


#Chapter 13 hypothesis testing
library(yarrr)
t.test(x = pirates$age, 
       mu = 30)

sex.ttest <- t.test(formula = tattoos ~ sex,
                    data = pirates, 
                    subset = sex %in% c("male", "female"))
sex.ttest # Print result

## Access specific values from test
sex.ttest$statistic
sex.ttest$p.value
sex.ttest$conf.int
cor.test(formula = ~ age + height,
         data = pirates)

# Body piercing data
american.bp <- c(3, 5, 2, 1, 4, 4, 6, 3, 5, 4)
european.bp <- c(6, 5, 7, 7, 6, 3, 4, 6, 5, 4)

# Store data in a dataframe
bp.survey <- data.frame("bp" = c(american.bp, european.bp),
                        "group" = rep(c("American", "European"), each = 10),
                        stringsAsFactors = FALSE)
yarrr::pirateplot(bp ~ group,
                  data = bp.survey,
                  main = "Body Piercing Survey",
                  ylab = "Number of Body Piercings",
                  xlab = "Group", 
                  theme = 2, point.o = .8, cap.beans = TRUE)

summary(american.bp)
summary(european.bp)


# Conduct a two-sided t-test comparing the vectors american.bp and european.bp
#  and save the results in an object called bp.test
bp.test <- t.test(x = american.bp,
                  y = european.bp,
                  alternative = "two.sided")

# Print the main results
bp.test
## 
##  Welch Two Sample t-test
## 
## data:  american.bp and european.bp
## t = -3, df = 20, p-value = 0.02
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.93 -0.27
## sample estimates:
## mean of x mean of y 
##       3.7       5.3
# library(yarrr)
height.htest <- t.test(formula = height ~ sex,
                       data = pirates,
                       subset = sex %in% c("male", "female"))
height.htest


# Compare the tattoos of pirates aged 29 and 30:
tatage.htest <- t.test(formula = tattoos ~ age,
                       data = pirates,
                       subset = age %in% c(29, 30))  # Compare age of 29 to 30

tatage.htest


# Is there an effect of college on # of tattoos
#  only for female pirates?

t.test(formula = tattoos ~ college,
       data = pirates,
       subset = sex == "female")

age.parrot.cor <- cor.test(formula= ~age+parrots,
                           data = pirates )

age.parrot.cor

# alternatively

# Method 2: Vector notation
age.parrots.ctest <- cor.test(x = pirates$age,
                              y = pirates$parrots)
age.parrots.ctest
names(age.parrots.ctest)



# Is there a correlation between age and 
#  number parrots ONLY for female pirates?

cor.test(formula = ~ age + parrots,
         data = pirates,
         subset = sex == "female")

table(pirates$college)

college.chitest <- chisq.test(x = table(pirates$college))
college.chitest


table(pirates$eyepatch, 
      pirates$college)

# Is there a relationship between a pirate's
# college and whether or not they wear an eyepatch?

colpatch.cstest <- chisq.test(x = table(pirates$college,
                                        pirates$eyepatch))

colpatch.cstest
## 

test.result <- t.test(age ~ headband,
                      data = pirates)
test.result

yarrr::apa(test.result)

age.parrots.ctest <- cor.test(formula = ~ age + parrots,
                              data = pirates)
age.parrots.ctest
yarrr::apa(age.parrots.ctest)



beard.sex.htest <- t.test(formula = beard.length ~ sex,
                          subset = sex %in% c("male", "female"),
                          data = pirates)
beard.sex.htest
str(pirates)



table(pirates$fav.pixar)

pirates2 <- filter(pirates, fav.pixar  %in%  c("Up","Inside Out") )
                   
pirates2

contingency.table <- table(pirates2$fav.pixar, pirates2$eyepatch)

age.patch.chitest <- chisq.test(contingency.table)
age.patch.chitest
apa(age.patch.chitest)

cor.test(formula= ~ time+budget ,
        data=movies)


revenue.rating.htest <- t.test(formula = revenue.all ~ rating,
                               subset = rating %in% c("R", "PG-13"),
                               data = movies)
revenue.rating.htest
apa(revenue.rating.htest)

genre.table <- table(movies$genre)
genre.table
genre.chisqtest <- chisq.test(genre.table)
genre.chisqtest
apa(genre.chisqtest)

genre.sequel.table <- table(movies$genre, movies$sequel)
genre.sequel.table
genre.sequel.htest <- chisq.test(genre.sequel.table)
genre.sequel.htest
apa(genre.sequel.htest)

# ch14 ANOVA

# 4 Steps to conduct an ANOVA

# Step 1: Create an aov object
mod.aov <- aov(formula = y ~ x1 + x2 + ...,
               data = data)
# y=dep variable
# x1= x2...xn independent variables


# Step 2: Look at a summary of the aov object
summary(mod.aov)

# Step 3: Calculate post-hoc tests (if necessary)
TukeyHSD(mod.aov)

# Step 4: Look at coefficients (if necessary)
mod.lm <- lm(formula = y ~ x1 + x2 + ...,
             data = data)

summary(mod.lm)


# One-way ANOVA example

library(yarrr)

str(poopdeck)

yarrr::pirateplot(time ~ cleaner, 
                  data = poopdeck, 
                  theme = 2, 
                  cap.beans = TRUE,
                  main = "formula = time ~ cleaner")
?pirateplot

# Step 1: aov object with time as DV and cleaner as IV
cleaner.aov <- aov(formula = time ~ cleaner,
                   data = poopdeck) 

# Step 2: Look at the summary of the anova object
summary(cleaner.aov)

# Now we know that statistically there is a difference between groups but
# we do not know which group is the different one. 
# To answer this, we need to conduct a post-hoc test.

# Step 3: Conduct post-hoc tests
TukeyHSD(cleaner.aov) 

# c is learly different

# Step 4: Create a regression object
cleaner.lm <- lm(formula = time ~ cleaner,
                 data = poopdeck)

# Show summary
summary(cleaner.lm)

# Ex: Two-way ANOVA

# Step 1: Create ANOVA object with aov()
cleaner.type.aov <- aov(formula = time ~ cleaner + type,
                        data = poopdeck)
# Step 2: Get ANOVA table with summary()
summary(cleaner.type.aov)
# Step 3: Conduct post-hoc tests
TukeyHSD(cleaner.type.aov)

# The only non-significant group difference we found is between cleaner b and cleaner a. All other comparisons were significant.

# Step 4: Look at regression coefficients
cleaner.type.lm <- lm(formula = time ~ cleaner + type,
                      data = poopdeck)

summary(cleaner.type.lm)
# ANOVA with interactions
# Does the effect of cleaners depend on the type of poop they are used to clean?

# Step 1: Create ANOVA object with interactions
cleaner.type.int.aov <- aov(formula = time ~ cleaner * type,
                            data = poopdeck)

# Step 2: Look at summary table
summary(cleaner.type.int.aov)


# To understand the nature of the difference, we’ll look at the regression coefficients from a regression object:
     
     # Step 4: Calculate regression coefficients
     cleaner.type.int.lm <- lm(formula = time ~ cleaner * type,
                               data = poopdeck)

summary(cleaner.type.int.lm)

# Again, to interpret this table, we first need to know what the default values are. 
# We can tell this from the coefficients that are ‘missing’ from the table. 
# Because I don’t see terms for cleanera or typeparrot, this means that cleaner = "a"
# and type = "parrot" are the defaults. Again, we can interpret the coefficients as 
# differences between a level and the default. It looks like for parrot poop, 
# cleaners b and c both take more time than cleaner a (the default). Additionally, 
# shark poop tends to take much longer than parrot poop to clean (the estimate for 
#                                                                 typeshark is positive).
# 
# The interaction terms tell us how the effect of cleaners changes when one is cleaning
# shark poop. The negative estimate (-16.96) for cleanerb:typeshark means that 
# cleaner b is, on average 16.96 minutes faster when cleaning shark poop compared to 
# parrot poop. Because the previous estimate for cleaner b (for parrot poop) was just 8.06,
# this suggests that cleaner b is slower than cleaner a for parrot poop, but faster than 
# cleaner a for shark poop. Same thing for cleaner c which simply has stronger effects in 
# both directions.

# Type I, Type II, and Type III ANOVAs

