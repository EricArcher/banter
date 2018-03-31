library(gdata)
ac.df <- read.xls("1647_detections.xlsx", stringsAsFactors = FALSE)
colnames(ac.df) <- tolower(colnames(ac.df))

# species ID formatting
vid.good <- sapply(ac.df$vis.id, function(x) length(grep("-999", x)) == 0)
detect.good <- tolower(ac.df$x.1.detection) == "no"
spp.good <- sapply(ac.df$species..vis, function(x) {
  x <- as.numeric(unlist(strsplit(x, ",")))
  one.spp <- length(x) == 1
  bad.spp <- any(x %in% c(-999, 5, 77, 1317, 2227, 63, 18, 32))
  one.spp & !bad.spp
})
ac.df$ac.id <- gsub(", ", "_", ac.df$ac.id)
rownames(ac.df) <- ac.df$ac.id
bad.events <- c("33", "43", "44", "105", "106.107", "380",
                "384", "401.402", "707", "708", "710",
                "293", "309")
event.good <- !ac.df$ac.id %in% bad.events

ac.df$training <- vid.good & detect.good & spp.good & event.good

# calculate event duration
start <- strptime(ac.df$starttime..gmt., "%m/%d/%Y %T", tz = "GMT")
end <- strptime(ac.df$endtime..gmt., "%m/%d/%Y %T", tz = "GMT")
ac.df$ev.duration <- as.numeric(end - start)

# get position
library(swfscMisc)
das <- das.read("CalC1647.das")
pos <- sapply(ac.df$vis.id, function(x) {
  i <- which(das$Sight == x)
  if(length(i) == 0) return(c(NA, NA)) else unlist(das[i[1], c("Lat", "Long")])
})
ac.df$lat <- pos[1, ]
ac.df$lon <- pos[2, ]

# create species identifiers
species.codes <- c(
  '17' = "D. delphis", '16' = "D. capensis", '13' = "S. coeruleoalba",
  '21' = "G. griseus", '22' = "L. obliquidens", '27' = "L. borealis",
  '32' = "F. attenuata", '36' = "G. macrorhynchus", '37' = "O. orcinus")
ac.df$species <- factor(species.codes[ac.df$species..vis], levels = species.codes)


summary(ac.df)
save(ac.df, file = "ac.df.rdata")
