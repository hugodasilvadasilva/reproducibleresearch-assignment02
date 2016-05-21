rm(list = ls())

#list all required packages
packages.names <-
    c("dplyr", "plyr", "stringr", "R.utils", "tidyr", "ggplot2", "plotly")

# Install required packages if not installed yet
if (length(setdiff(packages.names, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages.names, rownames(installed.packages())))
}

# Load required packages
for (package.name in packages.names) {
    library(package.name, character.only = TRUE)
}

projectdirectory.name <- "reproducibleresearch-assignment02"

# Check if the current directory (working space) is the project directory or if this one is a
# subdirectory of that one.
# In the first case (current directory is the project directory), nothing happens.
# If the project directory is a subdirectory from current direct (working space), set the working
# space with the projec directory.
# If the project directory is neither the current directory or a subdiretory, it's created then and
# the working space is setted with it.
if (!(str_sub(
    string = getwd(),
    start = -1 * nchar(projectdirectory.name)
) == projectdirectory.name)) {
    if (!(dir.exists(projectdirectory.name))) {
        dir.create(projectdirectory.name)
        setwd(paste(getwd(), projectdirectory.name, sep = "/"))
    } else{
        setwd(paste(getwd(), projectdirectory.name, sep = "/"))
    }
}

zipfile.url <-
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile.name <- "stormdata.bz2"
zipfile.path <- paste(getwd(), zipfile.name, sep = "/")

#Download bz2 data file if it's not at available at data subfolder
if (!file.exists(zipfile.path)) {
    download.file(url = zipfile.url,
                  destfile = zipfile.name,
                  method = "curl")
    downloaded.at <- Sys.time()
}

# Loads the file to memory, if it's not yet
if (!exists("storm.rawdata") || nrow(storm.rawdata) == 0) {
    storm.rawdata <-
        read.table(
            bzfile(zipfile.path),
            header = TRUE,
            sep = ",",
            stringsAsFactors = TRUE,
            quote = "\""
        )
}

# Calculates the mean injuries and fatalities and generates the harm index (which is the result of
# mean injuries plus mean fatalities) for each type of event. Then, it subset only those which
# harm index value are greater then 0. The result is ordered by harm index column in desc order
storm.avgHarm <-
    ddply(
        storm.rawdata,
        .(EVTYPE),
        .fun = summarise,
        avg.harm = mean(INJURIES + FATALITIES)
    ) %>% subset(avg.harm > 0) %>% arrange(-avg.harm)

storm.maxHarm <-
    ddply(
        storm.rawdata,
        .(EVTYPE),
        .fun = summarise,
        max.harm = max(INJURIES + FATALITIES)
    ) %>% subset(max.harm > 0) %>% arrange(-max.harm)

par(mfrow = c(1,2), mar = c(4, 4, 2, 1))
plot(
    storm.avgHarm$avg.harm, 
    xlab = "Event Index", 
    ylab = "Number of Victims", 
    main = "Most harmfull events by the avg. number of victims")
text(storm.avgHarm$avg.harm, row.names(storm.avgHarm$EVTYPE), cex=0.6, pos=4, col="red")

plot(
    storm.maxHarm$max.harm, 
    xlab = "Event Index", 
    ylab = "Number of Victims", 
    main = "Most harmfull events by the max. number of victims")
text(storm.maxHarm$max.harm, row.names(storm.maxHarm$EVTYPE), cex=0.6, pos=4, col="red")


head(storm.avgHarm, n = 12)
head(storm.maxHarm, n = 7)

# Now analyses the economic impact of each type of event
storm.damages <- storm.rawdata %>% select(EVTYPE, PROPDMG, CROPDMG)
storm.avgDamages <-
    ddply(
        storm.damages,
        .(EVTYPE),
        .fun = summarise,
        avg.damages = mean(PROPDMG + CROPDMG)
    ) %>% subset(avg.damages > 0) %>% arrange(-avg.damages)

storm.maxDamages <-
    ddply(
        storm.damages,
        .(EVTYPE),
        .fun = summarise,
        max.damage = max(PROPDMG + CROPDMG)
    ) %>% subset(max.damage > 0) %>% arrange(-max.damage)

par(mfrow = c(1,2), mar = c(4, 4, 2, 1))
plot(
    storm.avgDamages$avg.damages, 
    xlab = "Event Index", 
    ylab = "Number of Victims", 
    main = "Greatest economic impact event by the average damages value")
text(storm.avgDamages$avg.damages, row.names(storm.avgDamages$EVTYPE), cex=0.6, pos=4, col="red")

plot(
    storm.maxDamages$max.damage, 
    xlab = "Event Index", 
    ylab = "Number of Victims", 
    main = "Greatest economic impact event by the maximum damages value")
text(storm.maxDamages$max.damage, row.names(storm.maxDamages$EVTYPE), cex=0.6, pos=4, col="red")

head(storm.avgHarm, n = 23)
head(storm.maxHarm, n = 7)