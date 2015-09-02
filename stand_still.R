#### script to determine movements that are breaking standstills ####

#### exemptions ####
# record those movements breaking the standstill legally from those that aren't
# Animals between premises occupied by the same person or business
# Animals from any of the Scottish Island areas going to a market
# Animals moving direct from premises to a slaughterhouse
# Animals moving direct from premises to a market for animals intended for immediate slaughter
# Calves under 30 days old from holding of birth moving for fostering
# Imported animals; import locations all converted to 0 in the data
# Exported animals; not important
# Animals to a show/exhibition

#setwd('C:/Users/Michael/SAM') 
#rm(list=ls()) # clears objects
#rm(list=ls(all=TRUE)) # clears environment if needed
#load('stand_still.RData') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### constrain to 2011-2013 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

x2011 <- as.Date('01/01/2011', format='%d/%m/%Y')
x2013 <- as.Date('31/12/2013', format='%d/%m/%Y')
library(lubridate) # used to manipulate dates
library(plyr) # used for the count function; much faster than as.data.frame(table())
library(ggplot2)
library(gridExtra)
library(scales)
library(automap)
library(MapGAM)
library(PBSmapping) 
library(raster) # used to crop the map
library(rgeos) 

#### animals info ####
# used to determine animal type and calculate age of animal at time of movement
# imported animals assumed to be over 30 days old

# animals <- read.delim(file = '../cts201404/animals2.txt', 
#                       na.strings='\\N',
#                       fill = TRUE, # allows rows to have different lengths
#                       header=FALSE,
#                       stringsAsFactors=FALSE)
# names(animals) <- c('animal.id','sex','breed','eartag','cts.indicator','birth.date','death.date','import.country.code','import.date')
# animals$birth.date <- as.Date(animals$birth.date) # convert birth date to class date
# animals$death.date <- as.Date(animals$death.date) # convert death date to class date
# animals$import.date <- as.Date(animals$import.date) # convert import date to class date
# save(animals, file='animals.RData') # save in order to allow for quicker loading time just in case
load('animals.RData') # nrow=45360053

#### process animal info ####  

animals.temp1 <- animals[animals$death.date >= x2011 - days(280) & animals$birth.date > x2013,] # remove animals that have died before start date or born after end date; extra 9 months for bulls
animals.temp1 <- animals.temp1[!is.na(animals.temp1$animal.id),] # remove any rogue NAs introduced through subsetting
animals.temp2 <- animals[is.na(animals$death.date),] # animals that should still be alive

animals.new <- rbind(animals.temp1, animals.temp2) # stack two data.frames
animals.new <- animals.new[!is.na(animals.new$animal.id),] # nrow = 8284601
rm(animals, animals.temp1, animals.temp2)

#### bulls ####
# use preprocessed animals file to generate a list of bull ids
# UNIX steps in bulls_readme.txt
# 280 days before the bull is first recorded as a sire

bulls.ids <- read.delim('../cts201404/bulls_ids.txt', 
                        header=FALSE, 
                        fill = TRUE, # rows have different lengths
                        na.strings='\\N',
                        stringsAsFactors=FALSE)
names(bulls.ids) <- c('animal.id','relationship.date')
bulls.ids$relationship.date <- as.Date(bulls.ids$relationship.date, format='%Y-%m-%d')
bulls.ids <- bulls.ids[complete.cases(bulls.ids),] 
bulls.ids <- bulls.ids[order(bulls.ids$relationship.date),] # sort by dates
sire.date <- bulls.ids[!duplicated(bulls.ids$animal.id),] # take the earliest date for each bull by removing all duplicated entries; nrow = 139666 

bulls.info <- animals.new[animals.new$animal.id %in% sire.date$animal.id,] # subset male cattle ids for the reported bull ids
bulls.info <- merge(bulls.info, sire.date, all.x=TRUE) # nrow = 29230
bulls.info <- bulls.info[c('animal.id','birth.date','death.date','import.date','relationship.date')] # subset for relevant columns
names(bulls.info) <- c('animal.id','birth.date','death.date','import.date','first.bull')
bulls.info$bull.date <- bulls.info$first.bull - days(280) # 280 days before first birth of a calf; gestation is normally 277
rm(bulls.ids, sire.date)

#### read in direct movements file ####
# births and internal movements already removed

# movements.direct <- read.table('C:/Users/Michael/cts201404/movements_direct2.txt', # read in processed cattle movements file
#                                header=FALSE, 
#                                stringsAsFactors=FALSE) 
# names(movements.direct) <- c('animal.id', 'date', 'off', 'on') # rename movements file
# movements.direct$date <- as.Date(movements.direct$date, format='%Y-%m-%d') # convert character to date
# movements.direct$off <- as.integer(movements.direct$off)
# movements.direct$on <- as.integer(movements.direct$on)
#save(movements.direct, file='movements_direct.RData') # nrow = 88657066
load('movements_direct.RData') # saved as movements_direct.RData

# constrain movements to 2011-2013 

movements.direct <- movements.direct[movements.direct$date >= x2011 & movements.direct$date <= x2013,] # nrow=28122448
movements.direct$off[is.na(movements.direct$off)] <- 0 # for imports; assign NA off location as 0; NAs don't appear with the table function

#### remove direct movements to slaughter and movements to slaughter via markets
# remove those concatenated values from the direct movements

sam.coords <- read.csv('C:/Users/Michael/cts201404/locations.csv', header=TRUE, stringsAsFactors=FALSE) # read in location information
sam.coords <- sam.coords[c('location_id', 'premises_type_code', 'cph', 'county_id', 'location_name', 'x', 'y')] # subset for only these columns
sam.coords$cph <- gsub('/', '', sam.coords$cph) # removes '/'
sam.coords$cph <- as.numeric(substr(sam.coords$cph, 1, nchar(sam.coords$cph)-3)) # remove last 3 digits; as numeric
sam.coords$parish <- substr(sam.coords$cph, 1, nchar(sam.coords$cph)-4) # remove last 4 digits of CPH to get County+Parish

#### remove other movements types ####
# remove AI centres, calving centres, BSE collection centres, exports and showgrounds

other.types.id <- sam.coords[sam.coords$premises_type_code %in% c('AI','CA','CC','EX','SG'),]$location_id
movements.direct <- movements.direct[!movements.direct$off %in% other.types.id,]
movements.direct <- movements.direct[!movements.direct$on %in% other.types.id,]
rm(other.types.id)

market.ids <- sam.coords[sam.coords$premises_type_code=='MA',]$location_id
market.movements.off <- movements.direct[movements.direct$off %in% market.ids,] # subset off movements for all markets 
###

slaughter.ids <- sam.coords[sam.coords$premises_type_code %in% c('CR','CS','HK','KY','IP','MP','PP','SM','SR','SW'),]$location_id
market.to.slaughter <- market.movements.off[market.movements.off$on %in% slaughter.ids,]
rm(market.ids, market.movements.off)

movements.no.slaughter <- movements.direct[!movements.direct$on %in% slaughter.ids,] # subset out 6878937 movements to slaughter; nrow=21243511 
rm(movements.direct, slaughter.ids)

### do something clever eventually
# subset out the market to slaughter animal ids and dates
# concatenate

market.to.slaughter.concat <- paste0(market.to.slaughter$animal.id,'_',market.to.slaughter$date)
movements.no.slaughter.concat <- paste0(movements.no.slaughter$animal.id,'_',movements.no.slaughter$date)
vector <- movements.no.slaughter.concat %in% market.to.slaughter.concat # vector of trues and falses then combine to movements.no.slaughter
new.movements <- movements.no.slaughter[!vector,] # only keeps movements of animals not involved in a market.to.slaughter movement
rm(market.to.slaughter, market.to.slaughter.concat, movements.no.slaughter, movements.no.slaughter.concat, vector)

names(sam.coords) <- c('off', 'premises_type_code.off', 'cph.off', 'county_id.off', 'location_name.off', 'off.x', 'off.y',  'parish.off') 
new.movements <- merge(new.movements, sam.coords[c('off', 'cph.off', 'county_id.off')], all.x=TRUE)
names(sam.coords) <- c('on', 'premises_type_code.on', 'cph.on', 'county_id.on', 'location_name.on', 'on.x', 'on.y', 'parish.on') 
new.movements <- merge(new.movements, sam.coords[c('on', 'cph.on', 'county_id.on')], all.x=TRUE)

#### list of cattle holdings and markets ####
# to determine mixed holdings later on

cattle.cph <- unique(c(new.movements$cph.off, new.movements$cph.on))

#### only scottish destinations and departures ####

movements.scot.1 <- new.movements[new.movements$county_id.off>=66 & # scottish counties are 66 and above 
                                    new.movements$county_id.off<=98,]
movements.scot.2 <- new.movements[new.movements$county_id.on>=66 &
                                    new.movements$county_id.on<=98,] 
movements.scot <- rbind(movements.scot.1, movements.scot.2)
rm(new.movements, movements.scot.1, movements.scot.2)
movements.scot <- unique(movements.scot) # remove duplicates; if both on and off locations are scottish, then the row would be duplicated
movements.scot <- movements.scot[movements.scot$county_id.off!=99,] # remove odd types
movements.scot <- movements.scot[movements.scot$county_id.on!=99,] # remove odd type 

#### bull movements ####

bull.movements <- movements.scot[movements.scot$animal.id %in% bulls.info$animal.id,] # bull movements
bull.movements <- merge(bull.movements, bulls.info) # merge bull.movements with bull information
bull.movements$bull.move <- ifelse(bull.movements$date >= bull.movements$bull.date, 'TRUE','FALSE')
bull.move <- bull.movements[bull.movements$bull.move==TRUE,]
bull.move <- bull.move[c('date','cph.off','cph.on')]
bull.move <- bull.move[!is.na(bull.move$date),]
rm(bulls.info, bull.movements)

#### bull batches ####

bull.batches <- count(bull.move, c('date','cph.off','cph.on')) # 5276 obs
names(bull.batches) <- c('date', 'cph.off', 'cph.on', 'no.bulls') # rename columns
rm(bull.move)
#### calves ####

movements.scot.animal.info <- merge(movements.scot, animals.new[c('animal.id','birth.date')]) # 778329 obs
movements.scot.animal.info$move.age <- as.integer(movements.scot.animal.info$date - movements.scot.animal.info$birth.date) # subtract birth date from movement date to get age at move

calves.movements <- movements.scot.animal.info[movements.scot.animal.info$move.age <= 30,] # 1% are calve movements
calves.movements <- calves.movements[!is.na(calves.movements$animal.id),] # 26652 obs

#### calf batches ####

calf.batches <- count(calves.movements, c('date', 'cph.off', 'cph.on')) # 12824 obs
names(calf.batches) <- c('date', 'cph.off', 'cph.on', 'no.calves') # rename columns
rm(animals.new, movements.scot.animal.info, calves.movements)

#### massive batches data.frame ####

batches <- count(movements.scot, c('date','cph.off','cph.on'))
names(batches) <- c('date', 'cph.off', 'cph.on', 'no.animals') # rename columns
#save(batches, file='batches_scot_stand_still_direct.RData') 
#load('batches_scot_stand_still_direct.RData') # load for quicker processing time
rm(movements.scot)

#### join location information with batched movements ####
# imports don't have a cph

names(sam.coords) <- c('off','premises.type.code.off','cph.off','county.id.off', 'location.name.off', 'off.x', 'off.y','parish.off') # off
batches.scot <- merge(batches, sam.coords, all.x=TRUE) # merge location information
###
names(sam.coords) <- c('on','premises.type.code.on','cph.on','county.id.on', 'location.name.on', 'on.x', 'on.y','parish.on') # on
batches.scot <- merge(batches.scot, sam.coords, all.x=TRUE) # merge location information

###
rm(batches) # free up memory

#### movements from scottish islands to markets are exempt ####
# create an islands subset
# use island parishes

island.list <- c('Arran', 'Barra', 'Benbecula', 'Bute', 'Bute Pa20', 'Harris', 'Isla Of Lewis', 
                 'Isle-of-skye', 'Isle Of Arran', 'Isle Of Barra', 'Isle Of Benbecula', 'Isle Of Bute', 'Isle Of Canna',
                 'Isle Of Coll', 'Isle Of Cumbrae', 'Isle Of Eigg', 'Isle Of Harris',
                 'Isle Of Islay', 'Isle Of Jura', 'Isle Of Lewis', 'Isle Of Lewis Hs2 Odp',
                 'Isle Of Muck', 'Isle Of Mull', 'Isle Of N Uist', 'Isle Of North Uist',                
                 'Isle Of S Uist', 'Isle Of Sky', 'Isle Of Skye', 'Isle Of South Uist', 'Isle Of Syke', 'Isle Of Tiree',
                 'Isle Of Uist', 'Lewis', 'North Bute', 'North Uist', 'Orkney', 'Orkney Island', 'Orkney Islands',
                 'Orkney Islands Scotland', 'Orkney Isles', 'Rothesay', 'Shetland',  'Shetland Islands',
                 'Shetland Ze2', 'Shetlands', 'Skye', 'Small Isles',
                 'South Uist', 'Struan Isle Of Skye', 'Unst', 'Western Isles', 
                 'Western Isles Scotland', 'Wetsern Isles', 'Whalsay')
names(sam.coords) <- c('location_id', 'premises_type_code', 'cph', 'county_id', 'location_name', 'x', 'y','parish')
island.coords <- sam.coords[sam.coords$location_name %in% island.list,] # 2645 farms
island.parishes <- unique(as.integer(c(island.coords$parish))) # 81 island parishes in CTS data
rm(island.list, island.coords)

#### sibylle's sheep data ####
load('sheepmovements2011-2013.RData') # named 'sheep'
# only keep neccessary columns
batches.scot <- batches.scot[,c('cph.off',
                                'date',
                                'cph.on',
                                'no.animals',
                                'premises.type.code.off',
                                'off.x',
                                'off.y',
                                'premises.type.code.on',
                                'on.x',
                                'on.y')]
# line up sheep names with batches.scot
sheep <- sheep[,c('CPHoff','date','CPHon','sheep','premisesOFF','off_easting','off_northing','premisesON','on_easting','on_northing')]
names(sheep) <- c('cph.off',
                  'date',
                  'cph.on',
                  'no.animals',
                  'premises.type.code.off',
                  'off.x',
                  'off.y',
                  'premises.type.code.on',
                  'on.x',
                  'on.y')
# remove AI centres and gatherings
sheep <- sheep[!sheep$premises.type.code.off %in% c('AI Centre','Gathering'),]
sheep <- sheep[!sheep$premises.type.code.on %in% c('AI Centre','Gathering'),]

#### unique list of sheep cphs ####
# for determining mixed holdings after standstill have been calculated

sheep.cph <- unique(c(sheep$cph.off, sheep$cph.on))
mixed.cph <- cattle.cph[cattle.cph %in% sheep.cph]

# line up variable coding
sheep$premises.type.code.off <- ifelse(sheep$premises.type.code.off=='Agricultural Holding','AH',
                                       ifelse(sheep$premises.type.code.off=='Landless Keeper','LK',
                                              ifelse(sheep$premises.type.code.off=='Slaughterhouse','SR',
                                                     ifelse(sheep$premises.type.code.off=='MA','MA','UNKNOWN'))))
sheep$premises.type.code.on <- ifelse(sheep$premises.type.code.on=='Agricultural Holding','AH',
                                      ifelse(sheep$premises.type.code.on=='Landless Keeper','LK',
                                             ifelse(sheep$premises.type.code.on=='Slaughterhouse','SR',
                                                    ifelse(sheep$premises.type.code.on=='MA','MA','UNKNOWN'))))
# try to assign some of the unknown types
names(sam.coords) <- c('off','premises.type.code.off.2','county.id.off', 'location.name.off', 'off.x.2', 'off.y.2', 'cph.off','parish.off') # on
sheep.merge <- merge(sheep, sam.coords[c('cph.off', 'premises.type.code.off.2', 'off.x.2', 'off.y.2')], all.x=TRUE) # merge location information
sheep.merge$premises.type.code.off <- ifelse(sheep.merge$premises.type.code.off=='UNKNOWN',
                                             sheep.merge$premises.type.code.off.2, 
                                             sheep.merge$premises.type.code.off)
sheep.merge$off.x <- ifelse(is.na(sheep.merge$off.x),
                            sheep.merge$off.x.2,
                            sheep.merge$off.x)
sheep.merge$off.y <- ifelse(is.na(sheep.merge$off.y),
                            sheep.merge$off.y.2,
                            sheep.merge$off.y)
sheep.merge <- sheep.merge[!sheep.merge$premises.type.code.off %in% c('CA','CC','CS','KY','SG'),] # remove other types
###
names(sam.coords) <- c('on','premises.type.code.on.2','county.id.on', 'location.name.on', 'on.x.2', 'on.y.2', 'cph.on','parish.on') # on
sheep.merge <- merge(sheep.merge, sam.coords[,c('cph.on', 'premises.type.code.on.2','on.x.2', 'on.y.2')], all.x=TRUE) # merge location information
sheep.merge$premises.type.code.on <- ifelse(sheep.merge$premises.type.code.on=='UNKNOWN',
                                            sheep.merge$premises.type.code.on.2, 
                                            sheep.merge$premises.type.code.on)
sheep.merge$on.x <- ifelse(is.na(sheep.merge$on.x),
                            sheep.merge$on.x.2,
                            sheep.merge$on.x)
sheep.merge$on.y <- ifelse(is.na(sheep.merge$on.y),
                            sheep.merge$on.y.2,
                            sheep.merge$on.y)
sheep.merge <- sheep.merge[!sheep.merge$premises.type.code.on %in% c('CA','CC','CS','KY','SG'),] # remove other types

###
sheep.merge <- sheep.merge[,c('cph.off',
                              'date',
                              'cph.on',
                              'no.animals',
                              'premises.type.code.off',
                              'off.x',
                              'off.y',
                              'premises.type.code.on',
                              'on.x',
                              'on.y')] # remove extra columns
rm(sheep)

#### remove movements to slaughter ####

market.movements.off <- sheep.merge[sheep.merge$premises.type.code.off=='MA',] # subset off movements for all markets 
###

market.to.slaughter <- market.movements.off[market.movements.off$premises.type.code.on %in% c('CR','CS','HK','KY','IP','MP','PP','SM','SR','SW'),]
rm(market.movements.off)
movements.no.slaughter <- sheep.merge[!sheep.merge$premises.type.code.on %in% c('CR','CS','HK','KY','IP','MP','PP','SM','SR','SW'),]

# concatenate

market.to.slaughter.concat <- paste0(market.to.slaughter$cph.off,
                                     '_',
                                     market.to.slaughter$date,
                                     '_',
                                     market.to.slaughter$cph.on)
movements.no.slaughter.concat <- paste0(movements.no.slaughter$cph.off,
                                        '_',
                                        movements.no.slaughter$date,
                                        '_',
                                        movements.no.slaughter$cph.on)
vector <- movements.no.slaughter.concat %in% market.to.slaughter.concat # vector of trues and falses then combine to movements.no.slaughter
new.batches.sheep <- movements.no.slaughter[!vector,] # only keeps movements of animals not involved in a market.to.slaughter movement
rm(market.to.slaughter, market.to.slaughter.concat, movements.no.slaughter, movements.no.slaughter.concat, vector, sheep.merge)

#### only sheep movements with a scottish destination or departure ####
new.batches.sheep$county.off <- as.integer(substr(new.batches.sheep$cph.off, 1, nchar(new.batches.sheep$cph.off)-7)) 
new.batches.sheep$county.on <- as.integer(substr(new.batches.sheep$cph.on, 1, nchar(new.batches.sheep$cph.on)-7)) 

scot.1 <- new.batches.sheep[new.batches.sheep$county.off>=66 & # scottish counties are 66 and above 
                              new.batches.sheep$county.off<=98,]
scot.2 <- new.batches.sheep[new.batches.sheep$county.on>=66 &
                              new.batches.sheep$county.on<=98,] 
scot.sheep <- rbind(scot.1, scot.2)
rm(scot.1, scot.2, new.batches.sheep)
scot.sheep <- unique(scot.sheep) # remove duplicates; if both on and off locations are scottish, then the row would be duplicated
scot.sheep <- scot.sheep[scot.sheep$county.on!=99,] # remove odd type; 
scot.sheep <- scot.sheep[scot.sheep$county.off!=99,] # remove odd type

#### combine the sheep and cattle movements ####

batches.scot$species <- 'cattle'
scot.sheep$species <- 'sheep'
scot.sheep$county.off <- scot.sheep$county.on <- NULL # remove county variables 
batches.combined <- rbind(batches.scot, scot.sheep)
rm(batches.scot, scot.sheep)

#### create a unique list of scottish CPHs found in the movement data ####
# use TRUE/FALSE vectors
# only AH and LK
# occassionally a premises is listed as a market and an animal holding; treat these as markets; listed as markets in sheep data; 660650083, 681748001, 692140062

market.cph <- as.integer(unique(c(batches.combined[batches.combined$premises.type.code.off=='MA',]$cph.off,
                               batches.combined[batches.combined$premises.type.code.on=='MA',]$cph.on)))
cphs <- as.integer(c(batches.combined[batches.combined$premises.type.code.off %in% c('AH','LK'),]$cph.off, 
                     batches.combined[batches.combined$premises.type.code.on %in% c('AH','LK'),]$cph.on)) # 1592837
counties <- as.integer(substr(cphs, 1, nchar(cphs)-7)) # 1592837
scot.counties <- counties >= 66 # TRUE/FALSE vector
scot.farms <- cphs[scot.counties] # 1487516
scot.farms <- unique(scot.farms) # 20247
scot.farms <- scot.farms[!scot.farms %in% market.cph] # cphs that are listed as markets in the sheep data
scot.farms <- scot.farms[!is.na(scot.farms)] # 20244; NAs will stop stand still loop
rm(market.cph, cphs, counties, scot.counties)

#### calculate the amount of time between each on and subsequent off movement ####
# will need to run through entire list of scottish on locations

results.combined <- data.frame() # initialise data frame

for (farm in scot.farms) { # for each on location; a list of scottish farms
  
  if (which(farm == scot.farms) %%  500 == 0) { # progress bar; prevents over printing
    cat(which(farm == scot.farms),'of',length(scot.farms),'\n') # only print ever 500 lines
  }
  
  movements.subset <- batches.combined[batches.combined$cph.on == farm | batches.combined$cph.off ==  farm,] # subset rows where the farm is present in either on or off
  movements.subset <- movements.subset[!is.na(movements.subset$date),] # remove any rogue NAs
  ordered.movements <- movements.subset[order(movements.subset$date),] # order the dates
  
  if (nrow(ordered.movements)<=1) { # only proceed if there are more than 1 movement; a single movement seems to cause the loop to fall over
    next
  }
  
  differences <- data.frame() # initialise data.frame
  difference.in.days <- 0
  
  pass.key <- 0 # initialise/reset 'token'
  first.move.index <- 0 # initialise/reset index of first movment
  
  for (i in 1:(nrow(ordered.movements)-1)) { # -1 to prevent looping through an on movement in the last row
    
    if (ordered.movements$cph.on[i] == farm) { # Is this date associated with an on movement?
      
      pass.key <- 'token' # store a token 
      first.move.index <- i # store the index value
      
    } else if (ordered.movements$cph.off[i] == farm & # off location is a farm
                 pass.key == 'token') { # this is the next off movement
      
      difference.in.days <- ordered.movements$date[i] - ordered.movements$date[first.move.index] # subtract the on movement date from the off movement date
      
      if (difference.in.days > 0) { # make sure it's greater than 0
        
        differences <- rbind(differences, 
                             cbind(rbind(ordered.movements[first.move.index,], 
                                         ordered.movements[i,]),
                                   rbind(difference.in.days, 
                                         difference.in.days)))
      }
      
      pass.key <- 0 # reset 'token'
      
    }
    
  } 
  
  results.combined <- rbind(results.combined, differences) # store in result
  
}

# 205960 obs
results.combined$diff <- results.combined$'rbind(difference.in.days, difference.in.days)' # rename the variable
results.combined$'rbind(difference.in.days, difference.in.days)' <- NULL
results.combined$mixed.holding <- ifelse(results.combined$cph.off %in% mixed.cph, 'mixed.holding', results.combined$species)
rm(farm, movements.subset, ordered.movements, pass.key, first.move.index, difference.in.days, differences) # clear variable

# find the mixed standstills

mixed <- NULL

for (i in 1:nrow(results.combined)) {
  
  if (i %% 2 == 0) { # if it's an even number
    
    x <- ifelse(results.combined$species[i]==results.combined$species[i-1],
                results.combined$species[i],
                'mixed')# store the species type
    mixed <- c(mixed,x)
    
  } else {
    
    mixed <- c(mixed, NA)
    
  }
  
}

results.combined$mixed.standstill <- mixed
rm(x, mixed)

result.single <- results.combined[c(FALSE,TRUE),] # only every other difference; ### N.B needs to be the second movement to test for exemptions

#~~~~~#
#### MAKE FLOW DIAGRAM SHOWING THE AMOUNT OF MOVES THAT ARE EXEMPT AT EACH STAGE
# Some movements may have multiple exemptions. Maybe a Venn diagram as well?
#~~~~~#

#### Seperation aggreements ####
# read in seperation aggreements
# incomplete data
# make an informed decision i.e. if they are present at all in any of the lists, include treat them as having and agreement
# also, if any holding has an agreement in place, assume that any movement within 13 day standstill is exempt; no way to tell otherwise
# there are duplicates

seperation.agreements <- c('separation_agreements_13-01-2010.csv',
                           'separation_agreements_18-12-2013.csv',
                           'separation_agreements_18-12-2013.csv')
sep.agreements <- c()
for (i in seperation.agreements) {
  
  csv <- read.csv(i, stringsAsFactors=FALSE, header=TRUE) # read.csv
  names(csv) <- c('area.office', 'cph', 'start', 'end')
  sep.agreements <- append(sep.agreements, as.numeric(gsub('/', '', csv$cph))) # convert cph to numeric
  
}

sep.agreements <- unique(sep.agreements) # takes unique CPHs
rm(i, csv, seperation.agreements) # clear 

result.single$sep.agreements <- ifelse(result.single$cph.off %in% sep.agreements, TRUE, FALSE)
#rm(sep.agreements)

#### bull movements exempt ####
# needs to be batches

result.bull <- merge(result.single, bull.batches, all.x=TRUE)
result.bull$no.bulls[is.na(result.bull$no.bulls)] <- 0 # Set NAs of no.bulls to 0
result.bull$bull.move <- ifelse(result.bull$no.animals==result.bull$no.bulls, TRUE, FALSE) # only those movements with the same number as bulls will be considered exempt
result.bull$exempt.bull <- ifelse(result.bull$diff <= 13 & result.bull$bull.move==TRUE, TRUE,
                                  ifelse(result.bull$no.bulls > 0 & result.bull$sep.agreements==TRUE, TRUE, FALSE)) # 2436 exempt bulls

#### calve movements exempt ####
# assuming only calves are being moved together; not mixed batches

result.calf <- merge(result.bull, calf.batches, all.x=TRUE)
result.calf$no.calves[is.na(result.calf$no.calves)] <- 0 # Set NAs of no.bulls to 0
result.calf$calf.move <- ifelse(result.calf$no.animals==result.calf$no.calves, TRUE, FALSE) # only those movements with the same number as bulls will be considered exempt
result.calf$exempt.calf <- ifelse(result.calf$diff <= 13 & result.calf$calf.move==TRUE, TRUE,
                                  ifelse(result.calf$no.calves > 0 & result.calf$sep.agreements==TRUE, TRUE, FALSE))  

#### CPH owner info ####
# exemption states that movements between premises occupied by the same person or business are exempt
# multiple owners and occupiers for some farms with multiple CPHHs

cph.keeper <- read.csv('REQUESTqryMDeason_CPHOwners_v2_MDcleaned.txt', header=TRUE, stringsAsFactors=FALSE) # cleaned to remove obvious typos; e.g. 2209 instead of 2009
names(cph.keeper) <- c('cph', 'cphh', 'lookup.description', 'id', 'status', 'from.date', 'to.date')
cph.keeper <- cph.keeper[cph.keeper$lookup.description=='Is primary keeper of', ] # only interested in keepers
cph.keeper$lookup.description <- NULL
cph.keeper$cph <- as.integer(gsub('/', '', cph.keeper$cph)) # removes '/'; #  103166 unique CPHs
cph.keeper$from.date <- as.Date(cph.keeper$from.date, format='%d/%m/%Y') # 1970-01-01 very likely an error
cph.keeper$to.date <- as.Date(cph.keeper$to.date, format='%d/%m/%Y')
cph.keeper <- cph.keeper[!cph.keeper$from.date > x2013, ] # remove those records that begin after the end date
cph.keeper <- cph.keeper[cph.keeper$cph %in% scot.farms, ] # only those farms in scot.farms; shouldn't be keepers in multiple countries

# loop through the result file to find the keeper id that fits the movement date
# occationally more than one keeper per CPH

keeper.results <- data.frame()

for (i in 1:nrow(result.calf)) {
  
  if (i %%  1000 == 0) { # progress bar; prevents over printing
    cat(i,'of',nrow(result.calf),'\n')
  }
  
  cph.off <- as.integer(result.calf$cph.off[i])
  cph.on <- as.integer(result.calf$cph.on[i])
  date <- as.character(result.calf$date[i])
  
  keeper.off <- cph.keeper[cph.keeper$cph == cph.off &
                             cph.keeper$from.date < date &
                             cph.keeper$to.date > date,]$id
  
  if (length(keeper.off)==0) {
    
    keeper.off <- NA
    
  }
  
  keeper.on <- cph.keeper[cph.keeper$cph == cph.on &
                            cph.keeper$from.date < date &
                            cph.keeper$to.date > date,]$id
  
  if (length(keeper.on)==0) {
    
    keeper.on <- NA
    
  }
  
  keeper.results <- rbind(keeper.results, # happened 13 times with cbind: 'number of rows of result is not a multiple of vector length'  
                          cbind(cph.off, cph.on, date, keeper.off, keeper.on))  
    
}

keeper.results$date <- as.Date(keeper.results$date, origin='1970-01-01')
keeper.results <- unique(keeper.results) # take unique; occationally the same keeper will be associated with multiple CPHHs at the same CPH
keeper.results$cph.off <- as.integer(as.character(keeper.results$cph.off)) # remove factoring introduced by loop above
keeper.results$cph.on <- as.integer(as.character(keeper.results$cph.on)) # remove factoring introduced by loop above
keeper.results$keeper.off <- as.integer(as.character(keeper.results$keeper.off))
keeper.results$keeper.on <- as.integer(as.character(keeper.results$keeper.on))
rm(cph.off, cph.on, date, keeper.off, keeper.on)

### merge with results data ###

result.cph.keeper <- merge(result.calf, keeper.results, all.x=TRUE)
result.cph.keeper$keeper.move <- ifelse(result.cph.keeper$keeper.off==result.cph.keeper$keeper.on, TRUE, FALSE)
result.cph.keeper$keeper.move[is.na(result.cph.keeper$keeper.move)] <- FALSE

result.cph.keeper <- result.cph.keeper[order(result.cph.keeper$keeper.move, decreasing=TRUE),] # puts keeper.move==TRUE on top
result.cph.keeper <- result.cph.keeper[!duplicated(result.cph.keeper[c('cph.on', 'cph.off', 'date')]), ] # removes any duplicates; assumes that if there is a shared owner, they trade between himself

result.cph.keeper$parish.off <- substr(result.cph.keeper$cph.off, 1, nchar(result.cph.keeper$cph.off)-4) # remove last 4 digits of CPH to get County+Parish
result.cph.keeper$parish.on <- substr(result.cph.keeper$cph.on, 1, nchar(result.cph.keeper$cph.on)-4) # remove last 4 digits of CPH to get County+Parish
result.cph.keeper$island.exempt <- ifelse(result.cph.keeper$parish.off %in% island.parishes & 
                                            !result.cph.keeper$parish.on %in% island.parishes, TRUE, FALSE)

result.cph.keeper$standstill.exempt <- ifelse(result.cph.keeper$sep.agreements==TRUE | # farm has a seperation agreement or
                                                result.cph.keeper$exempt.bull==TRUE | # exempt bull move or
                                                result.cph.keeper$exempt.calf==TRUE | # exempt calf move or
                                                result.cph.keeper$keeper.move==TRUE | # movements from islands are exempt
                                                result.cph.keeper$island.exempt==TRUE, 'exempt', 'not_exempt') # movement between same keeper
result.cph.keeper$case.final <- ifelse(result.cph.keeper$diff <= 13 & result.cph.keeper$standstill.exempt=='exempt', 0, 
                                       ifelse(result.cph.keeper$diff <= 13 & result.cph.keeper$standstill.exempt=='not_exempt', 1, 0))

#### add herd sizes for the date of the movement breaking the standstill period ####

sam.herd.sizes <- read.csv('REQUESTqryMDeasonHoldingSizes2002To2013.csv', header=TRUE, stringsAsFactors=FALSE) # read in herd size information
names(sam.herd.sizes) <- c('cph', 'year', 'month', 'herd.size')
sam.herd.sizes <- unique(sam.herd.sizes) # removes duplicate 2011 data
sam.herd.sizes$cph <- as.integer(gsub('/', '', sam.herd.sizes$cph)) # removes / ; as integer
sam.herd.sizes <- sam.herd.sizes[complete.cases(sam.herd.sizes[, c('cph','year','herd.size')]), ] # removes any NAs in CPH, year or herd.size
sam.herd.sizes$date <- as.Date(paste0(sam.herd.sizes$year,'-',sam.herd.sizes$month,'-',1), format='%Y-%m-%d')# make a date

herd.sizes <- sam.herd.sizes[sam.herd.sizes$cph %in% result.cph.keeper$cph.off,]
herd.sizes$year <- herd.sizes$month <- NULL
herd.sizes <- herd.sizes[herd.sizes$date >= x2011 & herd.sizes$date <= x2013,] # reduce size of data.frame

sizes <- data.frame() 

for (i in 1:nrow(result.cph.keeper)) { 
  
  if (i %%  1000 == 0) { # prevents over printing
    cat(i, 'of', nrow(result.cph.keeper),'\n')
  }  
  
  farm <- result.cph.keeper$cph.off[i] # subset the sizes data for farm of interest
  date <- result.cph.keeper$date[i]
  x <- herd.sizes[herd.sizes$cph==farm,]
  x <- x[order(x$date),]
  
  if (nrow(x)>0) { # if x has more than 0 rows
    
    if (date>=tail(x,1)$date) { # if the date in the final row is before the date of interest, use the final row date
      
      sizes <- rbind(sizes, cbind(i, farm, date, tail(x,1)$herd.size))
      
    } else if (date < head(x,1)$date) { # if date is before the date in the first row
      
      sizes <- rbind(sizes, cbind(i, farm, date, -99))
      
    } else for (j in 1:nrow(x)) {
      
      if (date >= x$date[j] & date<x$date[j+1]) { # loop through ordered herd size dates
        
        sizes <- rbind(sizes, cbind(i, farm, date, x$herd.size[j]))
        
      }  
      
    }
    
  } else { # if (nrow(x)>0) loop 
    
    sizes <- rbind(sizes, cbind(i, farm, date, -99)) # if there is no info, double check with sam herd info for usual size
    
  } # end of if (nrow(x)>0) loop 
  
} # end of big loop; 26 movements do not have any herd size information

rm(i, j, sam.herd.sizes)
sizes$i <- NULL # remove index, used for debugging
names(sizes) <- c('cph.off', 'date', 'herd.size')
sizes$cph.off <- as.integer(as.character(sizes$cph.off))
sizes$herd.size <- as.integer(as.character(sizes$herd.size))
sizes[sizes$herd.size==-99, ] <- NA
sizes$date <- as.Date(as.integer(as.character(sizes$date)), origin='1970-01-01') # date needs to be converted from days again with origin='1970-01-01'

#### merge with the results ####
# rows match perfectly because of the 'for' loop

result.sizes <- cbind(result.cph.keeper, sizes$herd.size)
result.sizes$herd.size <- result.sizes$'sizes$herd.size'
result.sizes$'sizes$herd.size' <- NULL

#### sheep herd sizes ####
# sibylle's sheep and goat inventory 
# only yearly

load('my_inv.RData')
inv$cph <- as.integer(inv$CPH <- gsub('/', '', inv$CPH)) # removes '/'; stores as integer
inv <- inv[c('cph','year','number_of_sheep')]
inv <- inv[inv$cph > 9999,] # remove common grazing CPHs
names(inv) <- c('cph.off', 'year', 'flock.size') # new column; flock.size
result.sizes$year <- as.integer(format(result.sizes$date, '%Y'))
result.inv <- merge(result.sizes, inv, all.x=TRUE)

#### herd type info ####
sam.herd <- read.csv('tblccdHerd_FOI.txt', header=FALSE, stringsAsFactors=FALSE, na.strings = '')
sam.herd <- sam.herd[c('V13', 'V11', 'V14', 'V16', 'V15', 'V21')]
names(sam.herd) <- c('cph.off', 'chType', 'chArchDate', 'chLivedate', 'keeper.herd.size')

###
sam.herd$chLivedate <- as.Date(sam.herd$chLivedate, format='%d/%m/%Y') # convert character to date
sam.herd$chLivedate[is.na(sam.herd$chLivedate)] <- as.Date('20/02/1920', '%d/%m/%Y')# replace NA's archive date with arbitrary date in the past
sam.herd <- sam.herd[sam.herd$chLivedate <= x2011,]

###
sam.herd$chArchDate <- as.Date(sam.herd$chArchDate, format='%d/%m/%Y') # convert character to date
sam.herd$chArchDate[is.na(sam.herd$chArchDate)] <- as.Date('20/02/2020', '%d/%m/%Y')# replace NA's archive date with arbitrary date in the future
sam.herd <- sam.herd[sam.herd$chArchDate > x2013,]

###
beef.types <- c('Beef Dealer','Dealer','DEALER','Beef','BEEF','Beef Bull Hirer','Beef Heifer Rearer','BISON','Bull Beef','Meat','Stores','Suckler','BEEF SUCKLER','STORES','Finishing')
dairy.types <- c('Dairy Dealer','DAIRY/DEAL','Dairy','DAIRY','Dairy Bull Hirer','Dairy Heifer Rearer','DAIRY P/R','Domestic','Producer','Producer/Caterer','Producer/Processor','Producer/Retailer','Producer/Wholesaler')
mixed.types <- c('BEEF/DAIRY','MIXED', 'MX' )
# assign herd types based on lists above
# other includes c('Approved Dealer','Artificial Insemination', 'BISON', 'BUFFALO', 'BULL HIRER', 'CALF REARER', 'CATTLE MIX', 'CHS', 'CITY FARM', 'DEALER', 'HEIFER', 'HEIFER REARER', 'NO STOCK', 'Not Applicable', 'Not Known', 'OTHER', 'Temporary Gathering') 
sam.herd$herd.type <- ifelse(sam.herd$chType %in% beef.types, 'beef',
                             ifelse(sam.herd$chType %in% dairy.types, 'dairy', 
                                    ifelse(sam.herd$chType %in% mixed.types, 'mixed', 'other')))
###
rm(list=ls(pattern='types')) # removes objects with 'types' in name
sam.herd$chType <- NULL # remove herd type variable
sam.herd <- unique(sam.herd) # 82885 unique CPHs

#### trim down to only herds of interest ####
sam.herd <- sam.herd[sam.herd$cph.off %in% result.sizes$cph.off, ]

####
# multiple herd types for certain CPHs needs to be addressed before merge
# if more than one type, say it's 'mixed'

herd.merge <- data.frame()

for (i in unique(sam.herd$cph.off)) {
  
  x <- sam.herd[sam.herd$cph.off==i,]
  
  if (nrow(x)>1) {
    
    x$herd.type <- 'mixed'
    herd.merge <- rbind(herd.merge, x[1,]) # only take the first row
    
  } else {
    
    herd.merge <- rbind(herd.merge, x)
    
  }
  
}

result.type.sizes <- merge(result.inv, herd.merge[c('cph.off', 'herd.type')], all.x=TRUE)
rm(x)

# obvious databasing errors #
result.type.sizes[result.type.sizes$cph.off==783930036 & result.type.sizes$year==2011,]$flock.size <- 3855

# sum total number of animals (sheep and cattle) on farm
result.type.sizes$total.animals <- rowSums(cbind(result.type.sizes$herd.size, result.type.sizes$flock.size), na.rm=TRUE)


#### find mixed holdings ####
# column 'mixed' already used to define type of standstill

result.type.sizes$mixed.holding <- ifelse(result.type.sizes$cph.off %in% mixed.cph, 'mixed.holding', result.type.sizes$species)

#### scotland map ####
# map in long_lat / data in eastings and northings
# Variables for holding the coordinate system types 
latlong <- '+init=epsg:4326'
ukgrid <- '+init=epsg:27700'

load('GBR_adm1.RData')
scotland <- gadm[gadm@data$NAME_1=='Scotland',] # map of scotland including rockall :(
rm(gadm)
scotland <- crop(scotland,  extent(-8, -0.761805, 54.63292, 60.84582)) # removes rockall
scotland.xy <- spTransform(scotland, CRS(ukgrid)) # convert scotland into eastings and northings

#### generate parish level easting and northing values for farms with missing x&y ####
# 334 in total

names(sam.coords) <- c('location_id', 'premises_type_code', 'cph', 'county_id', 'location_name', 'x', 'y','parish')
new.coords <- data.frame() # initialise empty data.frame

for (i in unique(result.type.sizes$cph.off)) {
  
  cph.subset <- result.type.sizes[result.type.sizes$cph.off==i,] # subset for the cph of interest
  cph.subset <- cph.subset[!is.na(cph.subset$cph.off),] # remove any rogue NAs
  
  if(is.na(cph.subset$off.x[1])) { # if the coords are missing from one, they should be missing from all
    
    parish.data <- sam.coords[sam.coords$parish==cph.subset$parish.off[1],] # subset locations data for parish of interest
    parish.data <- parish.data[!is.na(parish.data$x),] # remove any rogue NAs
    
    repeat { # repeat this loop until the appropriate coords are found i.e. coords within Scotland
      
      if(nrow(parish.data)>2) { # use a randomly distributed jitter for the sampled coords
        
        new.off.x <- round(sample(parish.data$x, 1) + rnorm(1, 0, density(parish.data$x)$bw))
        new.off.y <- round(sample(parish.data$y, 1) + rnorm(1, 0, density(parish.data$y)$bw))
        
      } else { # if there aren't enough to calculate density, just take the mean; 
        
        new.off.x <- signif(mean(parish.data$x), digits = 5) # only 5 significant digits just in case there is only one farm 
        new.off.y <- signif(mean(parish.data$y), digits = 5) # hopefully the values aren't both already 5 significant digits long
        
      } 
      
      if (!is.na(over(SpatialPoints(cbind(Easting = new.off.x, # ensure point is in scotland
                                          Northing = new.off.y), 
                                    proj4string = CRS('+init=epsg:27700')), 
                      as(scotland.xy,'SpatialPolygons')))) 
        break 
    }
    
    new.coords <- rbind(new.coords, cbind(i, new.off.x, new.off.y)) # save new coords
    
  } 
  
}

names(new.coords) <- c('cph.off','new.off.x','new.off.y')
new.coords$new.off.x <- as.integer(as.character(new.coords$new.off.x)) # remove factoring
new.coords$new.off.y <- as.integer(as.character(new.coords$new.off.y)) # remove factoring


result.type.sizes <- merge(result.type.sizes, new.coords, all.x=TRUE)
result.type.sizes$off.x <- ifelse(is.na(result.type.sizes$off.x),
                          result.type.sizes$new.off.x,
                          result.type.sizes$off.x)
result.type.sizes$off.y <- ifelse(is.na(result.type.sizes$off.y),
                          result.type.sizes$new.off.y,
                          result.type.sizes$off.y)

#### MapGAM package ####

result.single.gam <- result.type.sizes[c('case.final','off.x','off.y','total.animals', 'diff', 'year')]
result.single.gam <- result.single.gam[complete.cases(result.single.gam[c('off.x','off.y')]),] # complete data for location

#### mean number of animals per farm ####

holding.mean <- aggregate(total.animals ~ off.x + off.y,
                          FUN = mean,
                          data = result.single.gam)

#### mean number of animals per 1 km grid square ####

# create 10km grid squares
library(stringr)
holding.mean$X2 <- substr(holding.mean$off.x, 1, nchar(holding.mean$off.x)-4) # column removing 4 char from eastings
holding.mean$Y2 <- substr(holding.mean$off.y, 1, nchar(holding.mean$off.y)-4) # column removing 4 char from northings
holding.mean <- within(holding.mean, { # pad X's and Y's with 0's to ensure similar lengths for later substringing
  x3 <- str_pad(X2, width=2, side='left', pad='0') 
  y3 <- str_pad(Y2, width=2, side='left', pad='0')
})

holding.mean$quadrant <- paste0('x', holding.mean$x3, 'y', holding.mean$y3) # concatenate the strings without spaces 

# mean number of animals per square
scot.mean.density <- aggregate(total.animals ~ quadrant,
                               FUN = mean,
                               data = holding.mean)
names(scot.mean.density) <- c('quadrant', 'mean.animals')
scot.mean.density <- merge(scot.mean.density, unique(holding.mean[c('quadrant','X2','Y2')]), all.x=TRUE)
scot.mean.density$x <- as.integer(scot.mean.density$X2)*10000
scot.mean.density$y <- as.integer(scot.mean.density$Y2)*10000
scot.mean.density.sp <- SpatialPointsDataFrame(cbind(Easting = scot.mean.density$x, 
                                                       Northing = scot.mean.density$y), 
                                                 data = data.frame(scot.mean.density[c('mean.animals')]), 
                                                 proj4string = CRS('+init=epsg:27700'))

#### make a new grid ####
# use the scotland map instead of scot.mean.density to find range to make grid

scot.mean.grid <- as.data.frame(expand.grid(X = seq(extent(scotland.xy)[1], # x-min
                                                    extent(scotland.xy)[2], 
                                                    by = 1000), 
                                            Y = seq(extent(scotland.xy)[3],
                                                    extent(scotland.xy)[4],
                                                    by = 1000)))
scot.mean.grid.sp <- SpatialPoints(cbind(Easting = scot.mean.grid$X, 
                                                     Northing = scot.mean.grid$Y), 
                                            proj4string = CRS('+init=epsg:27700'))
scot.mean.grid.sp.trim <- scot.mean.grid.sp[!is.na(over(scot.mean.grid.sp, as(scotland.xy,'SpatialPolygons'))),]

#### convert eastings and northings to lat long ####
library(rgdal)

coords <- cbind(Easting = as.numeric(as.character(result.single.gam$off.x)),
                 Northing = as.numeric(as.character(result.single.gam$off.y)))
scottish.result <- SpatialPointsDataFrame(coords, # Create the SpatialPointsDataFrame
                                          data = data.frame(result.single.gam), 
                                          proj4string = CRS(ukgrid))
scottish.ll <- spTransform(scottish.result, CRS(latlong))
colnames(scottish.ll@coords)[colnames(scottish.ll@coords) == 'Easting'] <- 'Longitude' # rename the column
colnames(scottish.ll@coords)[colnames(scottish.ll@coords) == 'Northing'] <- 'Latitude' # rename the column
#### find the points that are outside scotland
# could use the trimdata function in MapGAM as well...
scotland <- spTransform(scotland, CRS(latlong)) # same projection needed

scottish.gam <- as.data.frame(scottish.ll[!is.na(over(scottish.ll, as(scotland,'SpatialPolygons'))),])
scottish.gam <- scottish.gam[c('case.final', 'Longitude', 'Latitude','total.animals', 'diff', 'year')] # reorder columns for MapGAM
scottish.gam$log.animals <- log10(scottish.gam$total.animals) # INFs will be introduced for farms with 0 reported animals

###

# density.grid <- cbind(Easting = as.numeric(as.character(scot.mean.grid$X)),
#                       Northing = as.numeric(as.character(scot.mean.grid$Y)))
# scottish.result.grid <- SpatialPoints(density.grid, proj4string = CRS('+init=epsg:27700'))
# scottish.ll.grid <- spTransform(scottish.result.grid, CRS(latlong))
# colnames(scottish.ll.grid@coords)[colnames(scottish.ll.grid@coords) == 'Easting'] <- 'Longitude' # rename the column
# colnames(scottish.ll.grid@coords)[colnames(scottish.ll.grid@coords) == 'Northing'] <- 'Latitude' # rename the column
# # converted grid of 10km squares
# scottish.density.grid <- as.data.frame(scottish.ll.grid[!is.na(over(scottish.ll.grid,
#                                                                     as(scotland,'SpatialPolygons'))),])
###

# coords.density <- cbind(Easting = as.numeric(as.character(scot.mean.density.merge$X)),
#                         Northing = as.numeric(as.character(scot.mean.density.merge$Y)))
# scottish.result.density <- SpatialPointsDataFrame(coords.density, 
#                                                   data = data.frame(scot.mean.density.merge), 
#                                                   proj4string = CRS('+init=epsg:27700'))
# scottish.ll.density <- spTransform(scottish.result.density, CRS(latlong))
# colnames(scottish.ll.density@coords)[colnames(scottish.ll.density@coords) == 'Easting'] <- 'Longitude' # rename the column
# colnames(scottish.ll.density@coords)[colnames(scottish.ll.density@coords) == 'Northing'] <- 'Latitude' # rename the column
# scottish.gam.density <- as.data.frame(scottish.ll.density[!is.na(over(scottish.ll.density, as(scotland,'SpatialPolygons'))),])

#### try kriging ####
library(automap)
# x.grid <- predgrid(dataXY = single.gam.xy, map = scotland.xy, nrow=500, ncol=500)
# x.grid.sp <- SpatialPoints(cbind(Easting = x.grid$Easting, 
#                                  Northing = x.grid$Northing), 
#                            proj4string = CRS('+init=epsg:27700'))
# autoKrige needs projections that aren't LongLat?; use eastings and northings instead
scot.krig <- autoKrige(log10(mean.animals)~1,
                       input_data = scot.mean.density.sp[scot.mean.density.sp$mean.animals>0,],
                       new_data = scot.mean.grid.sp.trim)
# scot.krig <- autoKrige(log10(mean.animals)~1,
#                        input_data = scot.mean.density.sp[scot.mean.density.sp$mean.animals>0,],
#                        new_data = x.grid.sp)

interp_data = as.data.frame(scot.krig$krige_output)
colnames(interp_data) = c('Easting','Northing','log.animals','mean.animals_var','mean.animals_stdev')

#### convert interp_data to long/lat ####

coords <- cbind(Easting = as.numeric(as.character(interp_data$Easting)),
                Northing = as.numeric(as.character(interp_data$Northing)))
scottish.result <- SpatialPointsDataFrame(coords, # Create the SpatialPointsDataFrame
                                          data = data.frame(interp_data), 
                                          proj4string = CRS(ukgrid))
scottish.ll <- spTransform(scottish.result, CRS(latlong))
colnames(scottish.ll@coords)[colnames(scottish.ll@coords) == 'Easting'] <- 'Longitude' # rename the column
colnames(scottish.ll@coords)[colnames(scottish.ll@coords) == 'Northing'] <- 'Latitude' # rename the column
interp_long_lat <- as.data.frame(scottish.ll[!is.na(over(scottish.ll, as(scotland,'SpatialPolygons'))),])

#png('scotland_animal_density_working.png', width=9, height=10, units='in',res=300)
# ggplot(data=interp_data, aes(x=Easting, y=Northing)) + 
#   geom_polygon(data=shape.fort.xy, aes(long, lat, group=group),
#                colour='black',
#                fill='white') + 
#   geom_tile(aes(fill=log.mean.animals),color=NA) +
#   stat_contour(aes(z=log.mean.animals), bins=20, color='#999999') +
#   scale_fill_gradient2(expression(Log[10]*' Animals'), low='blue',mid='white',high='red', midpoint=mean(interp_data$log.mean.animals)) +
#   coord_equal() +
#   geom_point(data=result.single.gam, aes(x=off.x,y=off.y), color='black', size=0.5) +
#   theme(legend.position='bottom',
#         legend.title=element_text(size = rel(1.25)),
#         legend.key.height=unit(0.5,'in'),
#         legend.key.width=unit(0.5,'in'))
#dev.off()
##### fit a density model ####
# library(gam)
# scot.grid.density <- gam(formula=mean.animals ~ lo(Longitude, Latitude, span = 0.05), 
#                          data=scottish.gam.density, 
#                          family=gaussian)
# pred.scot.grid.density <- predict.gam(scot.grid.density, scottish.density.grid)
# new.scot.density.grid <- cbind(scottish.density.grid, pred.scot.grid.density) # grid combined with the predicted log density values 
# names(new.scot.density.grid) <- c('Longitude', 'Latitude', 'pred.mean.animals')# rename for modgam prediction
# 

#### density plot ####
# ggplot() + 
#     geom_polygon(data=shape.fort, aes(long, lat, group=group),
#                  colour='black',
#                  fill='white') + 
#     theme(panel.background = element_rect(fill = 'azure2'),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           panel.border = element_blank()) + 
#   geom_point(data=new.scot.density.grid, aes(x=Longitude,
#                                             y=Latitude,
#                                             color=pred.mean.animals)) + 
#   scale_color_gradientn('', 
#                         colours=topo.colors(2252)) +
#   theme(legend.position='bottom',
#         legend.title=element_text(size = rel(1.25)),
#         legend.key.height=unit(0.5,'in'),
#         legend.key.width=unit(0.5,'in')))

#### make annual data sets ####
# a farm only appears once per year
# if a farm broke the standstill during that year=1, else=0

# scottish.gam.annual <- unique(scottish.gam[c('case.final', 'Longitude', 'Latitude', 'year')])
# scottish.gam.annual <- scottish.gam.annual[order(scottish.gam.annual$case.final, decreasing=TRUE),] # sort for case.final so that case.final==1 is on top
# scottish.gam.annual <- scottish.gam.annual[!duplicated(scottish.gam.annual[c('Longitude', 'Latitude', 'year')]),] # remove duplicates; if case.final==1, that would have been kept for a year
# 
# scottish.gam.annual.l13 <- unique(scottish.gam[scottish.gam$diff<=13,][c('case.final', 'Longitude', 'Latitude', 'year')])
# scottish.gam.annual.l13 <- scottish.gam.annual.l13[order(scottish.gam.annual.l13$case.final, decreasing=TRUE),] # sort for case.final so that case.final==1 is on top
# scottish.gam.annual.l13 <- scottish.gam.annual.l13[!duplicated(scottish.gam.annual.l13[c('Longitude', 'Latitude', 'year')]),] # remove duplicates; if case.final==1, that would have been kept for a year
# 

#### which herd types have sheep? ####
# movements
# sheep.table <- table(scottish.gam.annual$sheep, scottish.gam.annual$herd.type)
# rownames(sheep.table) <- c('Cattle Only','Cattle & Sheep')
# colnames(sheep.table) <- c('Beef','Dairy','Mixed','Other')

#png('standstill_doc/mixed_herd_type_table.png', width=6.25,height=2.5,units='in',res=300) # save as hi-res png file
# ggtable +
#   annotation_custom(grob = tableGrob(sheep.table,
#                                      gpar.coltext = gpar(cex = 1.2), # font sizes
#                                      gpar.rowtext = gpar(cex = 1.2)))
# #dev.off()
# 
# #### do these mixed herds have separation agreements? ####
# sheep.sep.table <- unique(result.type.sizes[c('cph.off','sep.agreements','sheep')])
# sheep.sep.table <- table(sheep.sep.table$sep.agreements, sheep.sep.table$sheep) # first element is the row value
# rownames(sheep.sep.table) <- c('Agreement','No Agreement')
# colnames(sheep.sep.table) <- c('Cattle Only','Cattle & Sheep')
# 
# png('standstill_doc/mixed_herd_sep_table.png', width=6.25,height=2.5,units='in',res=300) # save as hi-res png file
# ggtable +
#   annotation_custom(grob = tableGrob(sheep.sep.table,
#                                      gpar.coltext = gpar(cex = 1.2), # font sizes
#                                      gpar.rowtext = gpar(cex = 1.2)))
# dev.off()
# 
# #### case/control holdings ####
# # 9723 unique CPHsobservations
# 
# result.case.control <- result.type.sizes[c('cph.off','off.x','off.y','date','case.final','herd.type','sheep')] # copy just in case; remove extra columns later
# result.case.control$year <- format(result.case.control$date, '%Y') # add year column
# result.case.control$date <- NULL
# result.case.control <- result.case.control[order(result.case.control$case.final, decreasing=TRUE),]# sort for case.final so that case.final==1 is on top
# result.case.control <- result.case.control[!duplicated(result.case.control[c('cph.off','year')]),] # remove duplicates; if case.final==1, that would have been kept for a year
# result.case.control$sheep2 <- ifelse(result.case.control$sheep==TRUE,1,2)
# 
# #### map in ggplot2 ####
# # http://stackoverflow.com/questions/11052544/convert-map-data-to-data-frame-using-fortify-ggplot2-for-spatial-objects-in-r
# # reduce the density of points when plotting sheep holdings; only farms, not movements
# # plots look very similar; try to differentiate some how or don't include 
# 
test <- scotland
test@data$id <- rownames(test@data) # grab row names
shape.fort <- fortify(test, region='id')
shape.fort <- shape.fort[order(shape.fort$order), ] 
rm(test)

test <- scotland.xy
test@data$id <- rownames(test@data) # grab row names
shape.fort.xy <- fortify(test, region='id')
shape.fort.xy <- shape.fort.xy[order(shape.fort.xy$order), ] 
rm(test)
# ###
# scot.plot <- ggplot() + 
#   geom_polygon(data=shape.fort, aes(long, lat, group=group),
#                colour='black',
#                fill='khaki') +
#   theme(panel.background = element_rect(fill = 'azure2'),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         panel.border = element_blank())
# 
#
# png('standstill_doc/map_exempt_not_exempt_13days.png', width=5,height=8,units='in',res=300) # save as hi-res png file
#map_case_control_l13
# dev.off()
# 
# ### mixed holding plot
# png('standstill_doc/map_mixed_holdings.png', width=5,height=8,units='in',res=300) # save as hi-res png file
# scot.plot + 
#   geom_point(data=scottish.gam.annual, aes(x=Longitude, 
#                                                y=Latitude,
#                                                color=factor(sheep)), 
#              alpha=0.5,
#              size=0.75) +
#   scale_color_hue('',labels = c('Cattle Only','Cattle & Sheep')) +
#   theme(legend.position='bottom') +
#   guides(colour = guide_legend(override.aes = list(size=4, alpha=1)))
# dev.off()
###
# 
# result.single.gam.grid <- predgrid(dataXY = scottish.gam, 
#                                    map = scotland,
#                                    nrow = 500,
#                                    ncol = 500) # creates a grid; 0.8801 km between points
# 
#### find the optimum span size when running modgam
# done automatically when sp=NULL, but should speed up running the permutations; span=0.05; using custom function, span=0.015 has lower AIC 150340.30, but rubbish results
#result.single.opt.span <- optspan(scottish.gam, 
#                                  m='crude',
#                                  family='binomial',
#                                  verbose=TRUE)
#
#### modgam re-runs ####
# result.single.fit <- modgam(scottish.gam, # outcome in 1st column, X&Y in 2 & 3, other variables after
#                             result.single.gam.grid,  
#                             permute=1000, 
#                             m='crude', # only locations used in prediction
#                             sp=0.05,
#                             verbose=TRUE)

# single.gam.sp <- SpatialPointsDataFrame(cbind(Easting = result.single.gam$off.x, 
#                                               Northing = result.single.gam$off.y), 
#                                         data = data.frame(result.single.gam[c('case.final','total.animals', 'diff', 'mixed.holding', 'year')]), 
#                        proj4string = CRS('+init=epsg:27700'))

single.gam.xy <- result.single.gam
names(single.gam.xy) <- c('case.final', 'Easting', 'Northing', 'total.animals', 'diff', 'mixed.holding', 'year')
single.gam.xy$Easting <- as.numeric(single.gam.xy$Easting)
single.gam.xy$Northing <- as.numeric(single.gam.xy$Northing)
result.single.fit.xy <- modgam(single.gam.xy,
                               interp_data[c('Easting','Northing')],
                               permute=1000,
                               m='crude',
                               sp=0.05,
                               verbose=TRUE)

#### custom modgam re-runs ####
single.gam.xy$log.mean.animals <- log10(single.gam.xy$total.animals) # only named log.mean.animals to match interp_data
result.custom.fit.xy <- modgam_custom(single.gam.xy[single.gam.xy$total.animals>0,][c('case.final','Easting','Northing','log.mean.animals')],
                                      interp_data[c('Easting','Northing','log.mean.animals')],
                                      permute=1000,
                                      m='adjusted',
                                      sp=0.05,
                                      keep=TRUE,
                                      verbose=TRUE)

library(grid) # for grid.draw()
# total case control
gA <- ggplotGrob(ggplot(single.gam.xy) +
    geom_polygon(data=shape.fort.xy, aes(long, lat, group=group),
                 colour='black',
                 fill='white') + 
    coord_equal() +
    geom_point(data=result.single.fit.xy$grid, aes(x=Easting,
                                       y=Northing,
                                       color=result.single.fit.xy$OR),
               size=0.8) +
    scale_color_gradientn('Unadjusted Odds Ratios ', 
                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
    theme(legend.position='bottom',
          legend.title=element_text(size = rel(1)),
          legend.key.height=unit(0.25,'in'),
          legend.key.width=unit(0.4,'in'),
          panel.background = element_rect(fill = 'azure2'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank()))
    
# density controlled
gB  <- ggplotGrob(ggplot(single.gam.xy) +
    geom_polygon(data=shape.fort.xy, aes(long, lat, group=group),
                 colour='black',
                 fill='white') + 
    coord_equal() +
    geom_point(data=result.custom.fit.xy$grid, aes(x=Easting,
                                                   y=Northing,
                                                   color=result.custom.fit.xy$OR),
               size=0.8) +
    scale_color_gradientn('Adjusted Odds Ratios ', 
                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
      theme(legend.position='bottom',
            legend.title=element_text(size = rel(1)),
            legend.key.height=unit(0.25,'in'),
            legend.key.width=unit(0.4,'in'),
            panel.background = element_rect(fill = 'azure2'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.border = element_blank()))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

#png('standstill_doc/new_map_total_crude_v_controlled_animal_density.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
#dev.off()
rm(gA, gB, g)

# modgam doesn't work with new grid; spits out warnings and doesn't perform permution test #
# points in the sea.
# works with the old grid
# too many points? works on a 10k grid (791 rows), 5k (3162), but now a 1k grid (79375)

# x.grid.trim <- trimdata(rdata = data.frame(scot.mean.grid.sp), map = scotland.xy)
# names(x.grid.trim) <- c('Easting','Northing')
# x <- gam(formula = case.final ~ lo(Easting, Northing, span=0.05),
#          data=single.gam.xy,
#          family=binomial)
# x.pred <- predict.gam(x, 
#                       #x.grid)
#                       x.grid.trim)
# 
# x.adj <- gam(formula = case.final ~ lo(Easting, Northing, span=0.05) + log.mean.animals,
#          data=single.gam.xy[single.gam.xy$log.mean.animals>0,],
#          family=binomial)
# x.adj.pred <- predict.gam(x.adj, interp_data[c('Easting','Northing','log.mean.animals')])
# 
#
# 
# result.l13.fit.xy <- modgam(single.gam.xy[single.gam.xy$diff<=13,],
#                             interp_data[c('Easting','Northing')],
#                             #permute=1000,
#                             m='crude',
#                             sp=0.05,
#                             verbose=TRUE)

result.fit.long.lat <- modgam(scottish.gam,
                              interp_long_lat[c('Longitude', 'Latitude')],
                              permute=1000,
                              m='crude',
                              sp=0.05,
                              verbose=TRUE)
result.custom.fit.long.lat <- modgam(scottish.gam[scottish.gam$total.animals>0,][c('case.final','Longitude', 'Latitude','log.animals')],
                                     interp_long_lat[c('Longitude', 'Latitude', 'log.animals')],
                                     m='adjusted',
                                     permute=1000,
                                     sp=0.05,
                                     verbose=TRUE)

result.l13.fit.long.lat <- modgam(scottish.gam[scottish.gam$diff<=13,],
                                  interp_long_lat[c('Longitude', 'Latitude')],
                                  permute=1000,
                                  m='crude',
                                  sp=0.05,
                                  verbose=TRUE)
result.l13.custom.fit.long.lat <- modgam(scottish.gam[scottish.gam$total.animals>0 & 
                                                        scottish.gam$diff<=13,][c('case.final','Longitude', 'Latitude','log.animals')],
                                         interp_long_lat[c('Longitude', 'Latitude', 'log.animals')],
                                         m='adjusted',
                                         permute=1000,
                                         sp=0.05,
                                         verbose=TRUE)
# north rona = 181100,1032300         

# north.rona <- single.gam.xy[single.gam.xy$Easting<300000 &
#                               single.gam.xy$Northing>976500,]
# north.rona.grid <- interp_data[interp_data$Easting<300000 &
#                                  interp_data$Northing>976500,]
# new.grid <- interp_data[-c(which(interp_data$Easting<300000 & interp_data$Northing>976500)),]
# 
# #### custom modgam re-runs ####
# 
# result.l13.custom.fit.xy <- modgam_custom(single.gam.xy[single.gam.xy$total.animals>0 & 
#                                                          single.gam.xy$diff<=13,][c('case.final','Easting','Northing','log.mean.animals')],
#                                       interp_data[c('Easting','Northing','log.mean.animals')],
#                                       permute=1000,
#                                       m='adjusted',
#                                       sp=0.05,
#                                       keep=TRUE,
#                                       verbose=TRUE)
# case control
gA <- ggplotGrob(ggplot(scottish.gam) +
                   geom_polygon(data=shape.fort, aes(long, lat, group=group),
                                #colour='black',
                                fill='white') + 
                   #coord_equal() +
                   geom_point(data=result.fit.long.lat$grid, aes(x=Longitude,
                                                                     y=Latitude,
                                                                     color=result.fit.long.lat$OR),
                              size=0.8) +
                   scale_color_gradientn('Unadjusted Odds Ratios ', 
                                         colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                   theme(legend.position='bottom',
                         legend.title=element_text(size = rel(1)),
                         legend.key.height=unit(0.25,'in'),
                         legend.key.width=unit(0.4,'in'),
                         panel.background = element_rect(fill = 'azure2'),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.ticks = element_blank(),
                         axis.text = element_blank(),
                         axis.title = element_blank(),
                         panel.border = element_blank()))
# density controlled
gB  <- ggplotGrob(ggplot(scottish.gam) +
                    geom_polygon(data=shape.fort, aes(long, lat, group=group),
                                 #colour='black',
                                 fill='white') + 
                    #coord_equal() +
                    geom_point(data=result.custom.fit.long.lat$grid, aes(x=Longitude,
                                                                             y=Latitude,
                                                                             color=result.custom.fit.long.lat$OR),
                               size=0.8) +
                    scale_color_gradientn('Adjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in'),
                          panel.background = element_rect(fill = 'azure2'),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text = element_blank(),
                          axis.title = element_blank(),
                          panel.border = element_blank()))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

#png('C:/Users/Michael/Dropbox/new_map_total_crude_v_controlled_animal_density.png', width=11, height=8.5, units = 'in', res=300)
#png('standstill_doc/new_map_total_crude_v_controlled_animal_density.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
#dev.off()
rm(gA, gB, g)

# l13 case control
gA <- ggplotGrob(ggplot(scottish.gam) +
                   geom_polygon(data=shape.fort, aes(long, lat, group=group),
                                #colour='black',
                                fill='white') + 
                   #coord_equal() +
                   geom_point(data=result.l13.fit.long.lat$grid, aes(x=Longitude,
                                                                     y=Latitude,
                                                                     color=result.l13.fit.long.lat$OR),
                              size=0.8) +
                   scale_color_gradientn('Unadjusted Odds Ratios ', 
                                         colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                   theme(legend.position='bottom',
                         legend.title=element_text(size = rel(1)),
                         legend.key.height=unit(0.25,'in'),
                         legend.key.width=unit(0.4,'in'),
                         panel.background = element_rect(fill = 'azure2'),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.ticks = element_blank(),
                         axis.text = element_blank(),
                         axis.title = element_blank(),
                         panel.border = element_blank()))
# l13 density controlled
gB  <- ggplotGrob(ggplot(scottish.gam) +
                    geom_polygon(data=shape.fort, aes(long, lat, group=group),
                                 #colour='black',
                                 fill='white') + 
                    #coord_equal() +
                    geom_point(data=result.l13.custom.fit.long.lat$grid, aes(x=Longitude,
                                                                   y=Latitude,
                                                                   color=result.l13.custom.fit.long.lat$OR),
                               size=0.8) +
                    scale_color_gradientn('Adjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in'),
                          panel.background = element_rect(fill = 'azure2'),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text = element_blank(),
                          axis.title = element_blank(),
                          panel.border = element_blank()))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

png('standstill_doc/new_map_13_fewer_crude_v_controlled_animal_density.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
dev.off()
rm(gA, gB, g)

###

library(grid) # for grid.draw()
# total case control
gA <- ggplotGrob(scot.plot + 
                   geom_point(data=scottish.gam, aes(x=Longitude, 
                                                     y=Latitude,
                                                     color=factor(case.final)), 
                              alpha=0.5,
                              size=0.75) +
                   scale_color_manual('',
                                      values=c('red','black'),
                                      labels=c('Exempt','Not Exempt')) +
                   theme(legend.position='bottom',                     
                         legend.text=element_text(face='bold',
                                                  size = rel(1))) +
                   guides(colour = guide_legend(override.aes = list(size=4, alpha=1))))
# unadjusted odds ratios
gB  <- ggplotGrob(scot.plot + 
                    geom_point(data=result.single.fit$grid, aes(x=Longitude,
                                                                y=Latitude,
                                                                color=result.single.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Unadjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

#png('standstill_doc/map_standstill_crude.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
#dev.off()
rm(gA, gB, g)

####

result.13.fewer.fit <- modgam(scottish.gam[scottish.gam$diff<=13,], 
                              result.single.gam.grid,  
                              permute=1000, 
                              m='crude',
                              sp=0.05,
                              verbose=TRUE)


# 'Total Records: Standstill \u2264 13 days',
# diff<=13  case control
gA  <- ggplotGrob(scot.plot + 
                    geom_point(data=scottish.gam[scottish.gam$diff<=13,], aes(x=Longitude, 
                                                                              y=Latitude,
                                                                              color=factor(case.final)), 
                               alpha=0.5,
                               size=0.75) +
                    scale_color_manual('',
                                       values=c('red','black'),
                                       labels=c('Exempt','Not Exempt')) +
                    theme(legend.position='bottom',                     
                          legend.text=element_text(face='bold',  size = rel(1))) +
                    guides(colour = guide_legend(override.aes = list(size=4, alpha=1))))
# unadjusted odds ratios
gB  <- ggplotGrob(scot.plot + 
                    geom_point(data=result.13.fewer.fit$grid, aes(x=Longitude,
                                                                  y=Latitude,
                                                                  color=result.13.fewer.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Unadjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

png('standstill_doc/map_standstill_l13_crude.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
dev.off()
rm(gA, gB, g)

####

result.annual.single.fit <- modgam(scottish.gam.annual, 
                                   result.single.gam.grid,  
                                   permute=1000, 
                                   m='crude',   
                                   sp=0.05,
                                   verbose=TRUE)

####

# main='Annual Records',
# annual case control
gA  <- ggplotGrob(scot.plot + 
                    geom_point(data=scottish.gam.annual, aes(x=Longitude, 
                                                             y=Latitude,
                                                             color=factor(case.final)), 
                               alpha=0.5,
                               size=0.75) +
                    scale_color_manual('',
                                       values=c('red','black'),
                                       labels=c('Exempt','Not Exempt')) +
                    theme(legend.position='bottom',                     
                          legend.text=element_text(face='bold', size = rel(1))) +
                    guides(colour = guide_legend(override.aes = list(size=4, alpha=1))))
# unadjusted odds ratios
gB  <- ggplotGrob(scot.plot + 
                    geom_point(data=result.annual.single.fit$grid, aes(x=Longitude,
                                                                       y=Latitude,
                                                                       color=result.annual.single.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Unadjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

png('standstill_doc/map_standstill_annual.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
dev.off()
rm(gA, gB, g)

####

result.annual.l13.fit <- modgam(scottish.gam.annual.l13, 
                                result.single.gam.grid,  
                                permute=1000, 
                                m='crude',   
                                sp=0.05,
                                verbose=TRUE)

####

# main='Annual Records: Standstill \u2264 13 days',
# annual diff<=13  case control
gA  <- ggplotGrob(scot.plot +
                    geom_point(data=scottish.gam.annual.l13, aes(x=Longitude, 
                                                                 y=Latitude,
                                                                 color=factor(case.final)), 
                               alpha=0.5,
                               size=0.75) +
                    scale_color_manual('',
                                       values=c('red','black'),
                                       labels=c('Exempt','Not Exempt')) +
                    theme(legend.position='bottom',                     
                          legend.text=element_text(face='bold', size = rel(1))) +
                    guides(colour = guide_legend(override.aes = list(size=4, alpha=1))))
# unadjusted odds ratios
gB  <- ggplotGrob(scot.plot +
                    geom_point(data=result.annual.l13.fit$grid, aes(x=Longitude,
                                                                    y=Latitude,
                                                                    color=result.annual.l13.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Unadjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

#png('standstill_doc/map_standstill_annual_l13.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
#dev.off()
rm(gA, gB, g)

#### fit a gam to find the density of animals ####

#hist(log10(scottish.gam$total.animals)) # looks roughly symmetrical; try family=gaussian, maybe Gamma
### log10(0) is -Inf; remove the 556 of 63298 movements with 0 animals.
# scot.density.data <- scottish.gam[scottish.gam$total.animals > 0,]
# scot.density <- gam(formula=log10(total.animals) ~ lo(Longitude, Latitude, span = 0.05), data=scot.density.data, family=gaussian)
# pred.scot.density <- predict.gam(scot.density, result.single.gam.grid)
# scot.density.grid <- cbind(result.single.gam.grid, pred.scot.density) # grid combined with the predicted log density values 
# names(scot.density.grid) <- c('Longitude', 'Latitude', 'log.total.animals')# rename for modgam prediction

#### find the density of animals for each grid square of pred.grid


#### use customised modgam function to allow for a fitted log population value in the grid ####
# to make modgam_custom, lines 28:37 from modgam function commented out
result.total.animals.fit <- modgam_custom(scot.density.data[c('case.final','Longitude','Latitude','log.total.animals')], 
                                          scot.density.grid, 
                                          permute=1000,
                                          m='adjusted',   
                                          sp=0.05,
                                          verbose=TRUE)

result.total.animals.13.fewer.fit <- modgam_custom(scot.density.data[scot.density.data$diff<=13,][c('case.final','Longitude','Latitude','log.total.animals')], 
                                                   scot.density.grid, 
                                                   permute=1000,
                                                   m='adjusted',   
                                                   sp=0.05,
                                                   verbose=TRUE)

# result.mixed.holdings.fit <- modgam(scottish.gam[c('case.final','Longitude','Latitude','mixed.holding')], # outcome in 1st column, X&Y in 2 & 3, other variables after
#                                     result.single.gam.grid,  
#                                     #permute=1000, # 1000 recommended
#                                     m='adjusted', # sheep and herd.type taken in as predictors
#                                     sp=0.05,
#                                     #sp=NULL, # checks all possible smoothing coefficients, makes things much slower...
#                                     verbose=TRUE)#,
# 
# result.mixed.holdings.13.fewer.fit <- modgam(scottish.gam[scottish.gam$diff<=13,][c('case.final','Longitude','Latitude','mixed.holding')], # outcome in 1st column, X&Y in 2 & 3, other variables after
#                                              result.single.gam.grid,  
#                                              permute=1000, # 1000 recommended
#                                              m='adjusted', # sheep and herd.type taken in as predictors
#                                              sp=0.05,
#                                              #sp=NULL, # checks all possible smoothing coefficients, makes things much slower...
#                                              verbose=TRUE)#,



# result.total.animals_and_mixed.holdings.fit <- modgam(scottish.gam[c('case.final','Longitude','Latitude','total.animals','mixed.holdings')], # outcome in 1st column, X&Y in 2 & 3, other variables after
#                                                       result.single.gam.grid,  
#                                                       permute=1000, # 1000 recommended
#                                                       m='adjusted', # sheep and herd.type taken in as predictors
#                                                       sp=0.05,
#                                                       #sp=NULL, # checks all possible smoothing coefficients, makes things much slower...
#                                                       verbose=TRUE)#,
# 
# result.total.animals_and_mixed.holdings.13.fewer.fit <- modgam(scottish.gam[scottish.gam$diff<=13,][c('case.final','Longitude','Latitude','total.animals','mixed.holdings')], # outcome in 1st column, X&Y in 2 & 3, other variables after
#                                                                result.single.gam.grid,  
#                                                                permute=1000, # 1000 recommended
#                                                                m='adjusted', # sheep and herd.type taken in as predictors
#                                                                sp=0.05,
#                                                                #sp=NULL, # checks all possible smoothing coefficients, makes things much slower...
#                                                                verbose=TRUE)#,

###
# scottish.gam.annual$sheep <- ifelse(scottish.gam.annual$sheep=='sheep_cattle',1,0)
# result.mixed.fit <- modgam(scottish.gam.annual[c('sheep','Longitude','Latitude','herd.type')], # outcome in 1st column, X&Y in 2 & 3, other variables after
#                            result.single.gam.grid,  
#                            permute=1000, # 1000 recommended
#                            m='crude', # only locations used in prediction
#                            #m='adjusted', # herd.type taken in as predictors
#                            sp=0.05,
#                            #sp=NULL, # checks all possible smoothing coefficients, makes things much slower...
#                            verbose=TRUE)#,
###The global statistic for the unadjusted model is <0.001###


# plot a map of the grid with the points; do in ggplot eventually
library(scales)
library(gridExtra)
#png('standstill_doc/scotland_standstill_OR.png', width=11, height=8.5, units = 'in', res=300)
# par(xpd=TRUE, # allows plotting in the margins
#     mfrow=c(1,2), # one row, two columns
#     mai=c(1,0,0,0)) # c(bottom, left, top, right) margins in inches

# plot(scotland, col='khaki', bg='azure2') # nice colours :)
# legend(x=-4.75, y=54.35, pch=16, bty='n', col = c('red','black'), legend = c('Case','Control'))
# points(scottish.gam$Longitude, 
#        scottish.gam$Latitude, 
#        pch=16,
#        cex=0.3, 
#        col=scottish.gam$case.final+1) # black is control; red is case
# # color map projected on the scotland map

# test.gam <- gam(formula = case.final ~ lo(Longitude, Latitude, span = 0.05) + log.total.animals, 
#                 family = binomial, 
#                 data = scot.density.data[c('case.final','Longitude','Latitude','log.total.animals')])
# test.fit <- predict.gam(test.gam, scot.density.grid)

#png('standstill_doc/map_standstill_density_corrected.png', width=11, height=8.5, units = 'in', res=300)
# animal density
grid.arrange(ncol=2, 
             #main='Animal Density Controlled',
             scot.plot + 
               geom_point(data=scot.density.grid, aes(x=Longitude,
                                                      y=Latitude,
                                                      color=10^log.total.animals), 
                          size=0.8) + 
               scale_color_gradientn('Fitted Animal Population', 
                                     colours=topo.colors(2252)) +
               theme(legend.position='bottom',
                     legend.title=element_text(size = rel(1.25)),
                     legend.key.height=unit(0.5,'in'),
                     legend.key.width=unit(0.5,'in')),
             
             # adjusted odds ratios
             scot.plot + 
               geom_point(data=result.total.animals.fit$grid, aes(x=Longitude,
                                                                  y=Latitude,
                                                                  color=result.total.animals.fit$OR), 
                          size=0.8) + 
               scale_color_gradientn('Adjusted Odds Ratios', 
                                     colours=rev(rainbow(2252, start = 0, end = 0.66))) +
               theme(legend.position='bottom',
                     legend.title=element_text(size = rel(1.25)),
                     legend.key.height=unit(0.5,'in'),
                     legend.key.width=unit(0.5,'in'))
)
#dev.off()

# less than or equal to 13 days
#png('standstill_doc/map_l13_standstill_density_corrected.png', width=11, height=8.5, units = 'in', res=300)
# animal density

grid.arrange(ncol=2,
             #main='Animal Density Controlled: Standstill \u2264 13 days', 
             scot.plot + 
               geom_point(data=scot.density.grid, aes(x=Longitude,
                                                      y=Latitude,
                                                      color=10^log.total.animals), 
                          size=0.8) + 
               scale_color_gradientn('Fitted Animal Population', 
                                     colours=topo.colors(2252)) +
               theme(legend.position='bottom',
                     legend.title=element_text(size = rel(1.25)),
                     legend.key.height=unit(0.5,'in'),
                     legend.key.width=unit(0.5,'in')),
             
             # adjusted odds ratios
             scot.plot + 
               geom_point(data=result.total.animals.13.fewer.fit$grid, aes(x=Longitude,
                                                                           y=Latitude,
                                                                           color=result.total.animals.13.fewer.fit$OR), 
                          size=0.8) + 
               scale_color_gradientn('Adjusted Odds Ratios', 
                                     colours=rev(rainbow(2252, start = 0, end = 0.66))) +
               theme(legend.position='bottom',
                     legend.title=element_text(size = rel(1.25)),
                     legend.key.height=unit(0.5,'in'),
                     legend.key.width=unit(0.5,'in'))
)
#dev.off()

#### map: Farm location and density ####

holdings <- unique(scottish.gam[c('Longitude','Latitude','mixed.holding')]) # subset data
holdings.xy <- unique(result.single.gam[c('off.x','off.y','mixed.holding')])
# take the mean of each holdings
# scottish.density.mean.size <- aggregate(total.animals ~ Longitude + Latitude + mixed.holding,
#                                         FUN = mean,
#                                         data=scot.density.data)
# names(scottish.density.mean.size) <- c('Longitude','Latitude','mixed.holding','mean.holding.size')

gA  <- ggplotGrob(ggplot(data=interp_data, aes(x=Easting, y=Northing)) +
                    geom_polygon(data=shape.fort.xy, aes(long, lat, group=group),
                                 #fill='black',              
                                 fill='khaki',
                                 colour='black') +
                    coord_equal() + 
                    geom_point(data=holdings.xy, aes(x=off.x, 
                                                  y=off.y,
                                                  color=factor(mixed.holding)), 
                               alpha=0.5,
                               size=0.75) +
                    scale_color_manual('',
                                       values=c('darkgreen','cyan3','magenta3'),
                                       #values=c('green1','cyan1','magenta'), # for 'black' version
                                       labels=c('Cattle','Mixed','Sheep')) +
                    theme(legend.position='bottom',                     
                          legend.text=element_text(size = rel(1.25)), 
                          panel.background = element_rect(fill = 'azure2'),
                          # map themes
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text = element_blank(),
                          axis.title = element_blank(),
                          panel.border = element_blank()) +
                    guides(colour = guide_legend(override.aes = list(size=4, alpha=1))))
# Density 
gB  <- ggplotGrob(ggplot(data=interp_data, aes(x=Easting, y=Northing)) + 
                    geom_polygon(data=shape.fort.xy, aes(long, lat, group=group),
                                 colour='black',
                                 fill='white') + 
                    geom_tile(aes(fill=log.mean.animals),color=NA) +
                    stat_contour(aes(z=log.mean.animals), bins=nbin, color='#999999') +
                    scale_fill_gradient2(expression(Log[10]*' Animals'), low='blue',mid='white',high='red', midpoint=mean(interp_data$log.mean.animals)) +
                    coord_equal() +
                    geom_point(data=result.single.gam, aes(x=off.x,y=off.y), color='black', size=0.5) +
                    theme(legend.position='bottom',
                          legend.title=element_text(face='bold', size = rel(1.25)),
                          # map themes
                          legend.key.height=unit(0.5,'in'),
                          legend.key.width=unit(0.5,'in'),
                          panel.background = element_rect(fill = 'azure2'),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text = element_blank(),
                          axis.title = element_blank(),
                          panel.border = element_blank()))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)
png('standstill_doc/map_farm_location_animal_density_xy.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
dev.off()
rm(gA, gB, g)

#### map: crude v. density controlled ####
# crude
gA  <- ggplotGrob(scot.plot + 
                    geom_point(data=result.single.fit$grid, aes(x=Longitude,
                                                                y=Latitude,
                                                                color=result.single.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Unadjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))
# Density corrected
gB  <- ggplotGrob(scot.plot +    
                    geom_point(data=result.total.animals.fit$grid, aes(x=Longitude,
                                                                       y=Latitude,
                                                                       color=result.total.animals.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Adjusted Odds Ratios', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

png('standstill_doc/map_total_crude_v_controlled_animal_density.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
dev.off()
rm(gA, gB, g)

#### map: l13 crude v. density controlled ####

gA  <- ggplotGrob(scot.plot + 
                    geom_point(data=result.13.fewer.fit$grid, aes(x=Longitude,
                                                                  y=Latitude,
                                                                  color=result.13.fewer.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Unadjusted Odds Ratios ', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))
# Density corrected
gB  <- ggplotGrob(scot.plot +    
                    geom_point(data=result.total.animals.13.fewer.fit$grid, aes(x=Longitude,
                                                                                y=Latitude,
                                                                                color=result.total.animals.13.fewer.fit$OR), 
                               size=0.8) + 
                    scale_color_gradientn('Adjusted Odds Ratios', 
                                          colours=rev(rainbow(2252, start = 0, end = 0.66))) +
                    theme(legend.position='bottom',
                          legend.title=element_text(size = rel(1)),
                          legend.key.height=unit(0.25,'in'),
                          legend.key.width=unit(0.4,'in')))

g <- cbind(gA, gB, size='first')
g$heights = unit.pmax(gA$heights, gB$heights)

png('standstill_doc/map_l13_crude_v_controlled_animal_density.png', width=11, height=8.5, units = 'in', res=300)
grid.draw(g)
dev.off()
rm(gA, gB, g)

# #### create seasons for dates ####
# 
# new.results$month <- factor(format(new.results$date, '%m'))
# new.results$month.3 <- ceiling(as.numeric(format(new.results$date, '%m'))/12*4) # 3 month periods
# new.results$year <- format(new.results$date, '%Y')
# 
# #### logisitic regression ####
# library(arm) # to rescale herd size
# library(lme4) # for the regression
# library(LMERConvenienceFunctions) # for auto-fit
# 
# new.results$herd.size.rescale <- rescale(new.results$herd.size)
# new.results$herd.type <- factor(new.results$herd.type)
# new.results$month.3 <- factor(new.results$month.3)
# new.results$year <- factor(new.results$year)
# # calculate degree distribution for each farm during each 3 month period
# # calculate trade volume for each farm during each 3 month period
# new.results$case.final <- factor(new.results$case.final)
# 
# new.results.mod <- glmer(case.final ~ herd.size.rescale + herd.type + month + (1|year),
#                          data=new.results, family=binomial)
# new.results.mod.best.fit <- fitLMER.fnc(new.results.mod, 
#                                         method='AIC', 
#                                         backfit.on=c('t'), # backfit.on = t needed for glmers
#                                         log.file.name='stand_still_mod_log_month.txt') 
# 
# ### model with individual months has lower AIC; 36336.99 vs. 36385.52 for seasons ####
# # compare predictions
# new.result.pred <- predict(object = new.results.mod.best.fit, newdata = new.results[c('herd.size.rescale','herd.type','month','year')]) 
# 
# ### herd.type 'other' risky, maybe seperate out all dealers? ###

#### plots and tables ####

#### summary stats for mixed holdings ####
# map showing the location of holdings breaking standstill vs those that aren't
# map mixed vs. just cattle (just sheep?)
# tables table(result.type.sizes$sheep, result.type.sizes$herd.type); OR?
# differences in herd size mean(result.type.sizes[result.type.sizes$sheep==TRUE,]$herd.size)
# box plots for herd size
# library(ggplot2)
# 
# #### herd size and breaking standstills ####
# # only exempt/non-exempt within 13 days
# ggplot(result.type.sizes[result.type.sizes$diff<=13,], aes(x=factor(case.final),y=herd.size)) + 
#   geom_boxplot() + scale_y_log10()
# 
# 
# #### where are the farmers sending their animals after breaking a standstill? ####
# library(gridExtra) # for tableGrob()
# 
# broken.standstill.table <- table(result.type.sizes[result.type.sizes$diff<=13,]$case.final,
#                                  result.type.sizes[result.type.sizes$diff<=13,]$premises.type.code.on)
# rownames(broken.standstill.table) <- c('Exempt','Not Exempt')
# colnames(broken.standstill.table) <- c('Animal Holdings','Landless Keepers','Markets')
# 
# ggtable <- qplot(1, 1, geom = 'blank') + # dummy variable; number of rows and columns doesn't matter
#   theme_bw() + # black and white theme; removes gray background
#   theme(line = element_blank(),
#         text = element_blank()) 
# 
# png('standstill_doc/broken_standstill_table.png', width=6.25,height=2.5,units='in',res=300) # save as hi-res png file
# ggtable +
#   annotation_custom(grob = tableGrob(broken.standstill.table,
#                                      gpar.coltext = gpar(cex = 1.2), # font sizes
#                                      gpar.rowtext = gpar(cex = 1.2)))
# dev.off()
# 
# 

#### create a histogram with a precentage line ####

case.final.percentages <- count(df = result.type.sizes[result.type.sizes$diff<=13,], 
                                vars = c('diff','case.final'))
test <- aggregate(freq ~ diff, case.final.percentages, sum)
names(test) <- c('diff','sum')
test2 <- merge(case.final.percentages, test)
test2$percent <- test2$freq / test2$sum
test3 <- test2[test2$case.final==1,]

# http://rpubs.com/kohske/dual_axis_in_ggplot2
library(ggplot2)
library(gtable)
library(grid)
# two plots
p1 <- ggplot(result.type.sizes[result.type.sizes$diff<=13,], aes(x=factor(diff), width=0.25)) +
  geom_histogram() +
  scale_x_discrete('Days', limits=c(1:13)) +
  scale_y_continuous('Number of Movements') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

p2 <- ggplot(test3, aes(x=diff, y=percent)) +
  geom_line(color='red2', size=2) +
  scale_x_discrete('Days', limits=c(1:13)) +
  scale_y_continuous(labels=percent, limits=c(0,1)) +
  #theme_bw() %+replace% 
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == 'panel', se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == 'panel')]], pp$t, 
                     pp$l, pp$b, pp$l)

ia <- which(g2$layout$name == 'axis-l')
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, 'npc') + unit(0.15, 'cm')
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
#png('standstill_doc/number_of_movements_two_axes.png', width = 6, height = 5, res = 300, units = 'in')
grid.draw(g)
#dev.off()  
rm(p1, p2, g1, g2, pp, g, ia, ga, ax)

# #### standstill times seperated between cattle only and sheep and cattle; to 100 days ####
# #png('standstill_doc/number_of_movements_mixed_holdings.png', width = 6, height = 5, res = 300, units = 'in')
# ggplot(result.type.sizes[result.type.sizes$diff<=100,], aes(x=diff, width=0.05)) + 
#   geom_bar(position = 'dodge', breaks=1:100, aes(fill=factor(sheep))) +
#   scale_y_continuous('Number of Movements') + 
#   scale_x_continuous('Number of Days') +
#   scale_fill_hue(name = '', # uses the default colour palette
#                  labels = c('Cattle Only','Cattle & Sheep')) +
#   theme(legend.position='bottom')
# #dev.off()  