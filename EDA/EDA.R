library(tidyverse)
df <- Orange
dim(df)
summary(df)

mode <- function(x,na.rm = TRUE){
  which.max(tabulate(x))
}

Orange %>%
group_by(Tree) %>%
summarise(mean = mean(age, na.rm = TRUE),
          median = median(age, na.rm = TRUE),
          mode = mode(age, na.rm = TRUE),
          var = var(age, na.rm = TRUE),
          sd = sd(age, na.rm = TRUE))
          

mode <- function(x){
  which.max(tabulate(x))
}

Orange %>%
  group_by(Tree) %>%
  summarise(mean = mean(circumference),
            median = median(circumference),
            mode = mode(circumference),
            var = var(circumference),
            sd = sd(circumference))
          max(Orange$age) - min(Orange$age)            

mode <- function(x,na.rm = TRUE){
  which.max(tabulate(x))
}
          
Orange %>%
  summarise(mean = mean(age, na.rm = TRUE), 
             median = median(age, na.rm = TRUE),
             mode = mode(age,na.rm = TRUE),
             var = var(age, na.rm = TRUE),
             sd = sd(age, na.rm = TRUE)) 

mode <- function(x, na.rm = TRUE){
  which.max(tabulate(x))
}

Orange %>%
  summarise(mean = mean(circumference, na.rm = TRUE), 
            median = median(circumference,na.rm = TRUE),
            mode = mode(circumference, na.rm = TRUE),
            var = var(circumference, na.rm = TRUE),
            sd = sd(circumference, na.rm = TRUE)) 






ggplot(data = Orange) + 
  geom_boxplot(mapping = aes(y = age, x = factor(Tree)) +
               labs(title = "BOXPLOT", y = "Age", x = "Tree") +
               theme(plot.title = element_text(color = "red", size = 25, face = "bold")) +
               theme(text = element_text(size = 20)) ) 
              


ggplot(data = Orange) +
  geom_point(mapping = aes(x = age, y = circumference, color = factor(Tree), size = 20)) +
             labs(title = "SCATTERPLOT", x = "Age", y = "Circumference") + 
             theme(plot.title = element_text(color = "red", size = 25, face = "bold")) +
             theme(text = element_text(size = 20))

ggplot(data = Orange) +
   geom_histogram(mapping = aes(x = age), bins = 10) +
   labs(title = "HISTOGRAM OF AGE", x = "Age", y  = "Frequency") +
   theme(plot.title = element_text(color = "red", size = 25, face = "bold")) +
   theme(text = element_text(size = 20))

