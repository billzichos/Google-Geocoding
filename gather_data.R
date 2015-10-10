setwd("~/GitHub/Google-Geocoding")

# Read in the data and format - START

        # Read the csv file
        df <- read.csv("Buildings.csv", as.is = TRUE, col.names = c(
                "Building.Code", "Building.Description", "Location.Code",
                "address.street", "Location.Address.Line.2","address.city", 
                "address.stateAbbr", "address.zip"), colClasses = c("character",
                "character", "character", "character", "character", "character",
                "character", "character"))
        
        # Remove unused columns - location code, address line 2
        df <- df[df$Building.Code!="",-c(3, 5)]

# Read in the data and format - END

        
# Data cleansing - START

        # The building at 9020 Mendenhall Court (code: 33800) has an invalid zip
        # code, 20145.  This needs to be changed to 21045.
        df$address.zip[df$Building.Code=="33800"] <- "21045"
        
        # The building at 200 International Circle (code: 40650) should be coded
        # to the 21030 zip code, not 21031.
        df$address.zip[df$Building.Code=="40650"] <- "21030"
        
        # The 2 buildings at 201 International Circle (code: 40700, 40730) should 
        #   be coded to the 21030 zip code, not 21031.
        df$address.zip[df$Building.Code=="40700"] <- "21030"
        df$address.zip[df$Building.Code=="40730"] <- "21030"
        
        # The building at 1501 S Clinton Street (code: 41900) has the wrong zip 
        # code assigned.  21202 is a valid zip code, but the first mariner 
        # building is really at 21224.
        df$address.zip[df$Building.Code=="41900"] <- "21224"

        # The buildings at 15 & 45 West Gude Drive should be coded to zip code 20850
        #   instead of 20852.
        df$address.zip[df$Building.Code=="45700"] <- "20850"
        df$address.zip[df$Building.Code=="45750"] <- "20850"
        
        # The six building on Commerce Drive in Dahlgren, VA are in zip 22485,
        #   not, 22448
        df$address.zip[df$Building.Code=="48050"] <- "22485"
        df$address.zip[df$Building.Code=="48150"] <- "22485"
        df$address.zip[df$Building.Code=="48200"] <- "22485"
        df$address.zip[df$Building.Code=="48250"] <- "22485"
        df$address.zip[df$Building.Code=="48270"] <- "22485"
        df$address.zip[df$Building.Code=="48300"] <- "22485"

        # The building at 1915 Aerotech Drive (code: 51050) is coded to the wrong zip.
        #   Should be 80916 instead of 80921
        df$address.zip[df$Building.Code=="51050"] <- "80916"
        
        # The building at 1055 N Newport Road (code: 51150) is coded to the wrong zip.
        #   Should be 80916 instead of 80915
        df$address.zip[df$Building.Code=="51150"] <- "80916"
        
        # The building at 1670 N Newport (code: 51250) has the wrong zip code assigned.
        # 80915 is a valid zip code, but this address is really at 80916.
        df$address.zip[df$Building.Code=="51250"] <- "80916"
        
        # The buildings at 5755 Mark Dabling Blvd (codes: 51800 & 51802) have the
        # wrong zip code assigned.  80921 is a valid zip code, but this address
        # is really at 80919.
        df$address.zip[df$Building.Code=="51800"] <- "80919"
        df$address.zip[df$Building.Code=="51802"] <- "80919"

        # The building at 5775 Mark Dabling Blvd (code: 51850) has the wrong zip
        # code assigned.  80921 is a valid zip code, but this address is really
        # at 80919.
        df$address.zip[df$Building.Code=="51850"] <- "80919"
        
        # The building at 2605 International Drive (code: 53600) has an invalid zip
        # code, 17710.  This needs to be changed to 17110.
        df$address.zip[df$Building.Code=="53600"] <- "17110"

        # The building at 2601 MarketPlace (code: 53650) has an invalid zip code,
        # 17710.  This needs to be changed to 17110.
        df$address.zip[df$Building.Code=="53650"] <- "17110"
        
        # The building at 1201 M Street (code: 58300) needs an "SE" indicator as there are
        #   three 1201 M Streets in Washington, DC.
        df$address.street[df$address.street=="1201 M Street"] <- "1201 M Street SE"
        
# Data cleansing - END

# convert to factors
df$address.stateAbbr <- as.factor(df$address.stateAbbr)
df$address.zip <- as.factor(df$address.zip)
df$address.city <- as.factor(df$address.city)

# Data processing - START

        # This is the base url for the Google Geocoding API
        url.base <- "https://maps.googleapis.com/maps/api/geocode/"

        # Specifies that we want the results to be returned in JSON format
        url.format<- "json?"
        # format <- "xml?"
        
        #address.street.number <- "314"
        #address.street.name <- "Wood Hollow"
        #address.street.type <- "Court"
        #address.street <- paste(address.street.number, address.street.name, address.street.type, sep = " ")
        #address.city <- "Annapolis"
        #address.stateAbbr <- "MD"
        url.address <- gsub(" ", "+",with(df, paste(address.street, address.city, address.stateAbbr, sep = ", ")))

        # The API key
        url.apiKey <- "AIzaSyCSFWbr6V-tpFXh46UhCav3YK7qCmvaMr0"

        # Combine the base, format, address and api key to create a URL that 
        #   calls the Google Geocoding API
        df$url <- paste(url.base, url.format, "address=", url.address, "&components=postal_code:", df$address.zip, "&key=", url.apiKey, sep = "")
        
# Data processing - END


# Webscrape - START
        
        library("jsonlite")
        
        getLat <- function(url) {
                fromJSON(url)$results$geometry$location$lat
        }
        
        getLng <- function(url) {
                fromJSON(url)$results$geometry$location$lng
        }
        
# Webscrape - END







#rawJSON <- fromJSON(url)

#df$latitude  <- rawJSON$results$geometry$location$lat
#df$longitude <- rawJSON$results$geometry$location$lng



#df$lat <- sapply(df$url, getLat)
#df$lng <- sapply(df$url, getLng)


a <- sapply(df$url, getLat)
bz.a <- a
bz.b <- bz.a
rm(a)
names(bz.b) <- NULL
bz.a[unlist(lapply(bz.b, is.null))]

df$lat <- cbind(df, unlist(sapply(df$url, getLat)))
df$lng <- cbind(df, unlist(sapply(df$url, getLng)))

#df <- c("https://maps.googleapis.com/maps/api/geocode/json?address=314+Wood+Hollow+Court,+Annapolis,+MD&key=AIzaSyCSFWbr6V-tpFXh46UhCav3YK7qCmvaMr0", "https://maps.googleapis.com/maps/api/geocode/json?address=419+Maple+Road,+Severna+Park,+MD&key=AIzaSyCSFWbr6V-tpFXh46UhCav3YK7qCmvaMr0")
#df <- as.data.frame.AsIs(df)