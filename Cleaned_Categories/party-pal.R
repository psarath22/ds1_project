library(rvest)
library(tidyverse)
library(dplyr)

file_path <- "party-pal.txt"
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
  
  a <- (new_html %>% html_table())[[1]]
  
  Country_Of_Origin[i] <- a[a[1:dim(a)[1],1] == "Country Of Origin" , 2]
  
  Charging_Time[i] <- a[a[1:dim(a)[1],1] == "Charging Time" , 2]
  
  Charging_Duration[i] <- a[a[1:dim(a)[1],1] == "Music Playtime" , 2]
  
}


Country_Of_Origin <- sapply(1:len,function(k) Country_Of_Origin[[k]][1])
Charging_Time <- sapply(1:len,function(k) Charging_Time[[k]][1])
Charging_Duration <- sapply(1:len,function(k) Charging_Duration[[k]][1])


##DO CLEANING HERE

party_pal <- data.frame(ProductName = Titles,
                      ProductRating = Ratings,
                      ReviewCount =  Reviews,
                      Sale_Price,
                      Regular_Price,
                      Discount_percentage = Offer,
                      ColourOptions = No_Of_Colors,
                      Warranty = Warranty,
                      Country_Of_Origin,
                      Charging_Time,
                      Charging_Duration)



###########################################################################################
###########################################################################################
#Data cleaning 
dims = dim(party_pal)
n = dims[1] 

#Making Warranty in year numeric variable
party_pal$Warranty = as.numeric(substring(party_pal$Warranty , 1,1))

#Making Discount_percentage  numeric variable
foo = party_pal$Discount_percentage 

foo = strsplit(foo , "%")

temp = as.numeric(sapply(1:n , function(k) substring(foo[[k]][1] , nchar(foo[[k]][1])-1 ,nchar(foo[[k]][1]))))

party_pal$Discount_percentage = temp

party_pal$Discount_percentage

#Making Regular_Price numeric variable  

foo = party_pal$Regular_Price

foo = strsplit(foo , "₹")

temp =sapply(1:n , function(k) substring(foo[[k]][2] , 1 ,nchar(foo[[k]][2])-3))

temp = as.numeric(gsub("," , "" , temp))

party_pal$Regular_Price = temp

#Making Sale_Price numeric variable

foo = party_pal$Sale_Price

foo = strsplit(foo , "₹")

temp =sapply(1:n , function(k) substring(foo[[k]][2] , 1 ,nchar(foo[[k]][2])))

temp = as.numeric(gsub("," , "" , temp))

party_pal$Sale_Price = temp 


#Making ReviewCount numeric variable
foo = party_pal$ReviewCount

foo[is.na(foo)] = 0

foo = strsplit( foo , " ")

temp =sapply(1:n , function(k) substring(foo[[k]][1] , 1 ,nchar(foo[[k]][1])))

temp = as.numeric(temp)

party_pal$ReviewCount = temp

#MAking ProductRating numeric variable 
foo = party_pal$ProductRating

foo = strsplit(foo , "\n")

temp =sapply(1:n , function(k) substring(foo[[k]][3] ,  nchar(foo[[k]][3])-2 ,nchar(foo[[k]][3])))

temp = as.numeric(temp)

temp[is.na(temp)]=0 

party_pal$ProductRating = temp

#Making Product Names clean
foo = party_pal$ProductName

foo = strsplit(foo , "\n") 

temp =sapply(1:n , function(k) substring(foo[[k]][2] ,   1, nchar(foo[[k]][2])))

for( i in 1:n)
{
  if( i!=3 )
  {
    temp[i] = substring(temp[i] , 14)
  }
  else
  {
    temp[i] = substring(temp[i] , 13)
  }
  
}

party_pal$ProductName = temp

#New column for category
party_pal$Category = "Wireless"

#New column to specify that they are party_pal 
party_pal$Type = "party_pal"


#Country Of Origin
temp <- party_pal$Country_Of_Origin
temp[is.na(temp)] <- ""
temp -> party_pal$Country_Of_Origin


#Charging Time
temp <- party_pal$Charging_Time
temp[is.na(temp)] <- ""

temp <- sapply(1:len,function(k) strsplit(temp[k]," ")[[1]][1])
temp <- as.numeric(temp)
as.numeric(temp) -> party_pal$Charging_Time

#Charging Duration
temp <- party_pal$Charging_Duration 
temp[is.na(temp)] <- ""

temp <- substring(temp,7,7)
temp <- as.numeric(temp)
temp -> party_pal$Charging_Duration 



#Cleaned data frame 
party_pal 