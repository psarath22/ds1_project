#Making list of all files of cleaned categories data
file.names = c("wireless_speakers.Rdata" , "wireless_headphones.Rdata" , "wired_headphones.Rdata" ,"wireless_earbuds.Rdata",
              "trbel.Rdata",
              "supersale.Rdata" , 
              "watches.Rdata",
               "party_pal.Rdata",
              "misfit.Rdata",
              "limited_edition.Rdata",
              "immortal_gaming.Rdata",
              "home_audio.Rdata",
              "chargers.Rdata",
              "car_acc.Rdata",
              "cables.Rdata",
              "wireless_earphones.RData",
              "power_banks.Rdata" )


for(file in file.names)
{
  load(file)
}

#Removing one extra column from home_audio df 

home_audio = home_audio[ ,!colnames(home_audio) %in% c("NO_Of_Drivers")]

Big_Frame = rbind(wireless_speakers,wireless_headphones,wired_headphones,wireless_earbuds,
                  trbel,
                  supersale , 
                  Watches,
                  party_pal,
                  misfit,
                  limited_edition,
                  immortal_gaming,
                  home_audio,
                  chargers,
                  car_acc,
                  cables,
                  wireless_earphones,
                  power_banks)


# Find positions of 0 values
zero_positions <- which(Big_Frame$ProductRating == 0)

# Replace 0 with NA
Big_Frame$ProductRating[zero_positions] <- NA

#Replacing 0 color options with 1 
Big_Frame$ColourOptions[Big_Frame$ColourOptions == 0] <- 1

write.csv(Big_Frame, "./../../Big_Frame.csv")
