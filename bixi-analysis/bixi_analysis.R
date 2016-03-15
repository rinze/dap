library(XML)
library(dplyr)
library(reshape2)
library(ggplot2)
theme_set(theme_bw(14))
library(ggmap)
library(animation)

Sys.setenv(LANG = "en")
Sys.setlocale(locale = "en_US.utf8")

XMLPATH <- "/home/chema/tmp/bixi/"

#### Functions ####
processXMLFile <- function(file) {
    cat("Processing", file, "...")
    
    # Some XML is malformed, so let's try to catch that right at the beginning
    tryCatch({
        # If there is an error, it will be triggered by this line
        xml <- xmlParse(file)
        latestUpdateTime <- xpathSApply(xml, path = "//station/latestUpdateTime", xmlValue)
        if (length(latestUpdateTime) == 0)
            latestUpdateTime <- NA
        
        lastCommWithServer <- xpathSApply(xml, path = "//station/lastCommWithServer", xmlValue)
        if (length(lastCommWithServer) == 0)
            lastCommWithServer <- NA
        
        # Obtain date from the file name
        datepart <- strsplit(file, "/|\\.", perl = TRUE)[[1]][7]
        datepart <- as.POSIXct(datepart, format = "%Y-%m-%d_%H:%M:%S")
        
        res <- data.frame(id = as.numeric(xpathSApply(xml, path = "//station/id", xmlValue)),
                          name = xpathSApply(xml, path = "//station/name", xmlValue),
                          lat = as.numeric(xpathSApply(xml, path = "//station/lat", xmlValue)),
                          long = as.numeric(xpathSApply(xml, path = "//station/long", xmlValue)),
                          nbBikes = as.numeric(xpathSApply(xml, path = "//station/nbBikes", xmlValue)),
                          nbEmptyDocks = as.numeric(xpathSApply(xml, path = "//station/nbEmptyDocks", xmlValue)),
                          latestUpdateTime = as.numeric(latestUpdateTime),
                          lastCommWithServer = as.numeric(lastCommWithServer),
                          filedate = datepart,
                          stringsAsFactors = FALSE)
        cat("\n")
        return(res)
    },
        error = function(cond) {
            message("ERROR! Skipping file")
            return(NULL)
        }, warning = function(cond) {
            message("Warning! Skipping file")
            return(NULL)
        }
    )
}


#### Main ####

# Read all data into a single data.frame
cachefile <- "data/bixi.Rda" # and don't do it every time I run it
if (!file.exists(cachefile)) {
    datafiles <- dir("/home/chema/tmp/bixi/", pattern = "xml", full.names = TRUE)
    # We can skip some obviously wrong files by looking at the size
    sizes <- sapply(datafiles, function(x) file.info(x)$size)
    datafiles <- datafiles[sizes > 100000]
    # Now read the files using the XML function
    bixi <- lapply(datafiles, processXMLFile)
    bixi <- do.call(rbind, bixi)
    save(bixi, file = cachefile)
} else {
    load(cachefile)
}

# Check what latestUpdateTime and lastCommWithServer mean.
plot(bixi$latestUpdateTime, bixi$lastCommWithServer, 
     main = "latestUpdateTime and lastCommWithServer",
     xlab = "latestUpdateTime", ylab = "lastCommWithServer",
     pch = 19, col = rgb(0, 0, 0, 0.2))
abline(a = 0, b = 1, col = "blue")

# Conver to proper dates
bixi$latestUpdateTime <- as.POSIXlt(bixi$latestUpdateTime / 1000, 
                                    origin = "1970-01-01")
bixi$lastCommWithServer <- as.POSIXlt(bixi$lastCommWithServer / 1000, 
                                      origin = "1970-01-01")
bixi$filedate <- bixi$filedate + 3 * 60 * 60

# Remove incorrect latitude and longitude
bixi <- bixi[bixi$lat != 0 & bixi$long != 0, ]

mean(bixi$filedate - bixi$latestUpdateTime)
sum(is.na(bixi$latestUpdateTime))

# Remove cases with no bikes and unused columns
bixi <- bixi[bixi$nbBikes + bixi$nbEmptyDocks > 0, ]
bixi$latestUpdateTime <- NULL
bixi$lastCommWithServer <- NULL

# Subset by date
bixi <- bixi[bixi$filedate >= as.POSIXct("2015-09-21 00:00:00 EDT") &
             bixi$filedate < as.POSIXct("2015-11-23 00:00:00 EDT"), ] 

n_stations <- bixi %>% group_by(name) %>% summarise(n = n())
n_stations

bixi <- bixi[!bixi$name %in% n_stations[n_stations$n < 1471, ]$name, ]

# Check latitude and longitude
latcheck <- bixi %>%
    group_by(name) %>%
    summarise(uniquelat = length(unique(lat)),
              uniquelong = length(unique(long)),
              uniquecoords = uniquelat + uniquelong) %>% # if this is not 2, we cry)
    arrange(desc(uniquecoords))
latcheck

# Why?
check1 <- unique(bixi[bixi$name == "Atwater / Sherbrooke", "lat"])
check1
check1[1] - check1[2]

# Round, then re-check (just re-run the `latcheck` code above)
bixi$lat <- round(bixi$lat, digits = 10)
bixi$long <- round(bixi$long, digits = 10)

#### First question ####
# Let's reshape the dataset
bixi$day <- strftime(bixi$filedate, "%Y-%m-%d")
bixi$hour <- as.numeric(strftime(bixi$filedate, "%H"))

bixipca <- dcast(bixi, day ~ hour + name, value.var = "nbBikes", 
                 fun.aggregate = mean)

# Can't run it yet, crashes
#pca1 <- prcomp(bixipca[, -1])

nans_per_day <- melt(bixipca, id.vars = "day") %>% 
                    group_by(day) %>% 
                    summarise(nas = sum(is.na(value))) %>% 
                    arrange(desc(nas))
nans_per_day

# What is going on on Sep 30th?
dcast(bixi[bixi$filedate >= as.POSIXct("2015-09-30 00:00:00") & 
           bixi$filedate < as.POSIXct("2015-10-01 00:00:00"), ], 
      day ~ hour, value.var = "nbBikes", fun.aggregate = mean)

# Truncate
bixipca <- bixipca[!bixipca$day %in% nans_per_day[nans_per_day$nas > 0, ]$day, ]

pca1 <- prcomp(bixipca[, -1])

bixipca$day_of_week <- strftime(as.Date(bixipca$day), "%a")
pcares <- data.frame(pc1 = pca1$x[, 1], pc2 = pca1$x[, 2], 
                     day_of_week = bixipca$day_of_week)
pcares$day_of_week <- factor(pcares$day_of_week, 
                             levels(pcares$day_of_week)[c(2, 6, 7, 5, 1, 3, 4)])

plt1 <- ggplot(pcares) + 
        geom_point(aes(x = pc1, y = pc2, colour = day_of_week), size = 4) +
        scale_color_brewer(palette = "Set1", name = "Day of week") +
        xlab("First principal component") + ylab("Second principal component")
print(plt1)

# Check the "offending days"
# Add the day to have a proper reference
pcares$day <- bixipca$day
pcares[pcares$pc1 > 750, ]

# What happened those days?
plot(as.numeric(bixipca[bixipca$day == "2015-11-16", -1]))

# And the weird Monday?
pcares[pcares$day_of_week == "Mon" & pcares$pc2 < -100, ]

#### Second question
bixi <- bixi[bixi$filedate >= as.POSIXct("2015-10-19 00:00:00") &
             bixi$filedate < as.POSIXct("2015-10-26 00:00:00"), ]

sset1 <- bixi[bixi$name == "Rachel/Des Érables", ]
sset2 <- bixi[bixi$name == "Union/René Lévesque", ]
total <- rbind(sset1, sset2)
plt2 <- ggplot(total, aes(x = filedate, y = nbBikes)) + 
        geom_line(aes(color = name)) +
        scale_color_manual(values = c("red", "blue"), name = "Base") +
        scale_x_datetime(date_breaks = "1 day") +
        xlab("Time") + ylab("Available bikes at station")
print(plt2)

scores <- bixi %>%
    group_by(name) %>%
    summarise(residential_score = cor(nbBikes, sset1$nbBikes),
              business_score = cor(nbBikes, sset2$nbBikes))

scores$status <- ifelse(scores$residential_score > 0.2, "Residential",
                        ifelse(scores$business_score > 0.2, "Business", "Unknown"))

bixi <- left_join(bixi, scores)

# Plot all on a map
# Plot data for a single day, just to test
testdata <- bixi[bixi$filedate == as.POSIXct("2015-10-19 12:00:00"), ]
mymap <- get_map(location = make_bbox(long, lat, bixi),
                 source = "osm", maptype = "roadmap")
ggmap(mymap) + geom_point(data = testdata, aes(x = long, y = lat, color = status, 
                                               size = nbBikes), alpha = 0.5) +
    scale_color_manual(values = c("red", "darkgreen", "#0000ee"), name = "Base type") +
    scale_size_area(name = "Number of available bikes") +
    geom_text(data = testdata, x = -73.535, y = 45.432, aes(label = filedate),
              size = 5) +
    xlab("Longitude") + ylab("Latitude")

# Animated gif.
# Let's assume we already have mymap, so we don't have to request it every time.
draw_bases <- function(timepoint) {
    testdata <- bixi[bixi$filedate == as.POSIXct(timepoint), ]
    timepointstr <- strftime(timepoint, "%Y-%m-%d %H:%M")
    testdata$timepointstr <- timepointstr
    plt_dyn <- ggmap(mymap) + 
               geom_point(data = testdata, aes(x = long, y = lat, color = status, 
                                               size = nbBikes), alpha = 0.5) +
               scale_color_manual(values = c("red", "darkgreen", "#0000ee"), name = "Base type") +
               scale_size_area(name = "Number of available bikes",
                               breaks = c(0, 10, 20, 30, 40),
                               labels = c(0, 10, 20, 30, 40),
                               limits = c(0, 40)) +
               geom_text(data = testdata, x = -73.535, y = 45.432, aes(label = timepointstr),
                         size = 5) +
              xlab("Longitude") + ylab("Latitude")
    ggsave(plt_dyn, filename = paste0("/tmp/", timepointstr, ".png"))
    #print(plt_dyn)
}

draw_animation <- function() {
    times <- unique(bixi$filedate)
    lapply(times, draw_bases)
}

