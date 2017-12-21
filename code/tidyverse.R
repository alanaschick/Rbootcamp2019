### Code for tasks doen in session 2: tidyverse
### compiled by Hena Ramay 
### 20.12.2017


## Load libraries
library(gapminder)
library(dplyr)
library(ggplot2)
library(readr)


## Filter
gapminder %>% filter(year=='2007' & continent=="Asia")


## Arrange

gapminder %>% 
  filter(year=='2007' & continent=="Asia") %>%
  arrange(desc(lifeExp))


## Group and Summarize
gapminder %>% 
  filter(year=='2007' & continent=="Asia") %>%
  arrange(desc(lifeExp)) %>%
  group_by(country) %>%
  summarize(median=mean(lifeExp))

## Mutate
gapminder_asia<-gapminder %>% 
  filter(year=='2007' & continent=="Asia") %>%
  arrange(desc(lifeExp)) %>%
  group_by(country,pop,gdpPercap) %>%
  summarize(median_life=mean(lifeExp)) %>%
  mutate(med_life_months=median_life*12)

## Plotting examples

ggplot(gapminder_asia,aes(x=median_life,y=pop)) + geom_point() 


## Size
ggplot(gapminder_asia,aes(x=median_life,y=pop,size=gdpPercap)) + geom_point()


## label

ggplot(gapminder_asia,aes(x=median_life,y=pop, label=country)) + geom_point() +geom_label()


## Everything together: dplyr + gpplot

gapminder %>% 
  filter(year=='2007' & continent=="Asia") %>%
  arrange(desc(lifeExp)) %>%
  group_by(country,pop,gdpPercap) %>%
  summarize(median_life=mean(lifeExp)) %>%
  mutate(med_life_months=median_life*12) %>%
  ggplot(aes(x=median_life,y=pop,size=gdpPercap)) + geom_point()



## Read in files



iris<-read_csv("data/iris.csv")

# View a snapshot of the data
head(iris)

#check dimensions
dim(iris)


## make a plot to get an idea about the distribution of Petal length for examples
ggplot(iris, aes(x=Sepal.Length,Petal.Length))+geom_point() + ggtitle("Answer 1")

## Make a facet plot for species

ggplot(iris, aes(x=Sepal.Length,Petal.Length))+geom_point()+facet_wrap(~Species)+ ggtitle("Answer 2")

## Make a histogram for petal length
ggplot(iris, aes(x=Petal.Length,fill=Species)) + geom_histogram() + ggtitle("Answer 3",)

## Filter Sepal length and keep only lengths > 5

# filter if length of Sepal length is > 5
 iris_small <- iris %>%
 filter(Sepal.Length > 5)

#Make a scatter plot of Petal length and width for the filtered data
 ggplot(iris_small, aes(x=Petal.Length,y=Petal.Width)) + geom_point() + ggtitle('Answer 2')



 # filter the sepal length > 5, group by species and calculate the median petal length
 # draw a column plot
 by_species <- iris %>%
 filter(Sepal.Length > 5) %>%
 group_by(Species) %>%
 summarize(medianPL=median(Petal.Length))

 ggplot(by_species, aes(x=Species,
 y=medianPL)) +geom_col()+ ggtitle('Answer 4')
 

