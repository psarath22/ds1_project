library(rvest)
library(tidyverse)
library(dplyr)

file_path <- "immortal-gaming.txt"
product_urls <- readLines(file_path)

len <- length(product_urls)

Titles <- character(length = len)
Reviews <- character(length = len)
Ratings <- character(length = len)
Sale_Price <- character(length = len)
Regular_Price <- character(length = len)

#Discount percentage
Offer <- character(length = len)
No_Of_Colors <- integer(length = len)

Warranty <- character(length = len)

#Country of origin
Country_Of_Origin <- character(length = len)

#Charging Time
Charging_Time <- character(length = len)

#Charging Duration
Charging_Duration <- character(length = len)

for(i in 1:len){
  
  print(i)
  new_html <- read_html(product_urls[i])
  
  Titles[i] <- (new_html %>% html_elements(".product-meta__title.heading.h3")%>%html_text())[1]
  
  Reviews[i] <- new_html %>% html_element(".rating__caption")%>%html_text()
  Ratings[i] <- (new_html %>% html_elements(".rating__stars")%>%html_text())[1]
  
  Sale_Price[i] <- new_html %>% html_elements(".price.price--highlight.price--large")%>%html_text()
  Regular_Price[i] <- new_html %>% html_elements(".price.price--compare.line-through")%>%html_text()
  
  #Discount percentage
  Offer[i] <- new_html %>% html_elements(".custom-saved-price") %>% html_text()
  
  No_Of_Colors[i] <- length(new_html %>% html_elements(".color-swatch__item") %>% html_text())
  Warranty[i] <- new_html %>% html_element(".mb-0.promis_icon_text")%>% html_text()
  
  #Country Of Origin
  a <- (new_html %>% html_table())[[1]]
  Country_Of_Origin[i] <- a[a[1:dim(a)[1],1] == "Country Of Origin" , 2]
  
  Charging_Time[i] <- a[a[1:dim(a)[1],1] == "Charging Time" , 2]
  
  Charging_Duration[i] <- a[a[1:dim(a)[1],1] == "Music Playtime" , 2]
  
  
}

Country_Of_Origin <- sapply(1:len,function(k) Country_Of_Origin[[k]][1])
Charging_Time <- sapply(1:len,function(k) Charging_Time[[k]][1])
Charging_Duration <- sapply(1:len,function(k) Charging_Duration[[k]][1])



immortal_gaming <- data.frame(ProductName = Titles,
                                ProductRating = Ratings,
                                ReviewCount =  Reviews,
                                Sale_Price,
                                Regular_Price,
                                Discount_percentage = Offer,
                                ColourOptions = No_Of_Colors,
                                Warranty = Warranty,
                                Country_Of_Origin,
                                Charging_Time,
                                Charging_Duration
                              )

#Data cleaning 
dims = dim(immortal_gaming)
n = dims[1] 

#Making Warranty in year numeric variable
immortal_gaming$Warranty = as.numeric(substring(immortal_gaming$Warranty , 1,1))

#Making Discount_percentage  numeric variable
foo = immortal_gaming$Discount_percentage 

foo = strsplit(foo , "%")

temp = as.numeric(sapply(1:n , function(k) substring(foo[[k]][1] , nchar(foo[[k]][1])-1 ,nchar(foo[[k]][1]))))

immortal_gaming$Discount_percentage <- temp

#Making Regular_Price numeric variable  

foo = immortal_gaming$Regular_Price

foo = strsplit(foo , "₹")

temp =sapply(1:n , function(k) substring(foo[[k]][2] , 1 ,nchar(foo[[k]][2])-3))

temp = as.numeric(gsub("," , "" , temp))

immortal_gaming$Regular_Price = temp

#Making Sale_Price numeric variable

foo = immortal_gaming$Sale_Price

foo = strsplit(foo , "₹")

temp =sapply(1:n , function(k) substring(foo[[k]][2] , 1 ,nchar(foo[[k]][2])))

temp = as.numeric(gsub("," , "" , temp))

immortal_gaming$Sale_Price = temp 


#Making ReviewCount numeric variable
foo = immortal_gaming$ReviewCount

foo[is.na(foo)] = 0

foo = strsplit( foo , " ")

temp =sapply(1:n , function(k) substring(foo[[k]][1] , 1 ,nchar(foo[[k]][1])))

temp = as.numeric(temp)

immortal_gaming$ReviewCount = temp

#MAking ProductRating numeric variable 
foo = immortal_gaming$ProductRating

foo = strsplit(foo , "\n")

temp =sapply(1:n , function(k) substring(foo[[k]][3] ,  nchar(foo[[k]][3])-2 ,nchar(foo[[k]][3])))

temp = as.numeric(temp)

temp[is.na(temp)]=0 

immortal_gaming$ProductRating = temp

#Making Product Names clean
foo = immortal_gaming$ProductName

foo = strsplit(foo , "\n") 

temp =sapply(1:n , function(k) substring(foo[[k]][2] ,   1, nchar(foo[[k]][2])))

excpt = c(94 , 76,60 , 56,40,14,11,9,5)
for( i in 1:n)
{
  if( i != 2)
  {
    temp[i] = substring(temp[i] , 13)
  }
  else
  {
    temp[i] = substring(temp[i] , 12)
  }
  
}

immortal_gaming$ProductName = temp


#Country Of Origin
immortal_gaming$Country_Of_Origin <- immortal_gaming$Country_Of_Origin
#Scraped Cleaned Data

#Charging Duration
immortal_gaming$Charging_Duration <- as.numeric(substring(immortal_gaming$Charging_Duration , 7,8))

#Charging Time
temp <- immortal_gaming$Charging_Time
immortal_gaming$Charging_Time <- as.numeric(sapply(1:len,function(k) substring(temp[k], 1,nchar(temp[k])-6)))


#New column for category
immortal_gaming$Category = "Wireless"

#New column to specify that they are immortal_gaming 
immortal_gaming$Type = "immortal_gaming"
