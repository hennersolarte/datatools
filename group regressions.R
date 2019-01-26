#Regressions by groups using a single database
library(purrr)
#We use the default mtcars database available in R
head(mtcars)

cyl<-split(mtcars,mtcars$cyl) #Split() divide mtcars by groups of cyl (A list of 4 dataframes)
str(cyl)                      
four_cyl<-cyl[[1]]            #Get the first dataframe (cyl = 4) 
lm(mpg~wt,four_cyl)           #does the regression (mpg=wt) for the first dataframe of our list (cyl=4)

#It is possible to do a regression for each group using the map function, this function receives a database as the first argument 
#and a function as a second argument and iterates on the database.

map(cyl,function(x) lm(mpg~wt,data=x))
