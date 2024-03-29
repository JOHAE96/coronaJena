library(zoo)
url <- "https://opendata.jena.de/dataset/2cc7773d-beba-43ad-9808-a420a67ffcb3/resource/d3ba07b6-fb19-451b-b902-5b18d8e8cbad/download/corona_erkrankungen_jena.csv"
corona <- read.csv2(url, sep=",")

# einwohner = 111343 # https://twitter.com/jenalichtstadt/status/1392060745769172998
einwohner =  108127 # Einwohner Zahl 19.10.2020 laut jena.de/corona leider keine open data quelle

#corona <- corona[!(corona$zeit == 1640991599),] # remove row with the wrong date

days = 50

no.rows <- nrow(corona)

if (days > no.rows) {
  days = no.rows
}

seit = no.rows-days
time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01"))

par(mfcol = c(3, 3)) #for multiple plots

df.neu <- data.frame(
  time <- time,
  neu <- corona$neu.erkrankte
)

lastweek = df.neu[no.rows-7,1];
last2week = df.neu[no.rows-14,1];

plot(df.neu[seit:no.rows,], xlab = "date", ylab = "neu", main = "neu infiziert",
     col=ifelse(df.neu[seit:no.rows,1] > lastweek, "red", "black"),
     pch = ifelse(df.neu[seit:no.rows,1] > lastweek ,19,1))

abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()



df.7days <- data.frame(
  time <- time,
  day.index <-rollsumr(corona$neu.erkrankte, k = 7, fill = NA) *100000/einwohner
)


plot(df.7days[seit:no.rows,], xlab = "date", ylab = "7 Tage Inzidenz",
     main = "7-Tage Inzidenz",
      col=ifelse(df.7days[seit:no.rows,2] > 200, "purple",
          ifelse(df.7days[seit:no.rows,2] > 100, "blue",
                  ifelse(df.7days[seit:no.rows,2] > 50, "red",
                       ifelse(df.7days[seit:no.rows,2] > 35, "yellow",
                              "black")))
                            ), pch = ifelse(df.7days[seit:no.rows,2] > 35,19,1))
abline(h = 35, col = 'yellow3', lwd = 1)
abline(h = 50, col = 'coral2', lwd = 1)
abline(h = 100, col = 'blue', lwd = 1)
abline(h = 150, col = 'blue', lwd = 1)
abline(h = 165, col = 'blue', lwd = 1)
abline(h = 200, col = 'purple', lwd = 1)
abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()





df.neu.aktiv <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  neu <- corona$neu.erkrankte - rollapply(corona$genesene, 2, diff, fill = NA, align = "right", by.column=FALSE)
)
plot(df.neu.aktiv[seit:no.rows,], xlab = "date", ylab = "neu aktiv", main = "neu aktive fälle")
abline(h = 0, col = 'coral2', lwd = 1)
abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()

df.neu.aktiv.7day <- data.frame(
  time <- time,
  neu <- rollsumr(corona$neu.erkrankte- rollapply(corona$genesene, 2, diff, fill = NA, align = "right", by.column=FALSE),
                  k = 7, fill = NA) *100000/einwohner
)
plot(df.neu.aktiv.7day[seit:no.rows,], xlab = "date", ylab = "neu aktiv 7 tage", main = "neu aktiv in den letzen 7 Tagen")
abline(h = 0, col = 'coral2', lwd = 1)
abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()

df.aktiv <- data.frame(
  time <- time,
  neu <- corona$aktive.faelle
)
plot(df.aktiv[seit:no.rows,], xlab = "date", ylab = "aktiv", main = "aktuell infiziert")
abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()


df.infiziert <- data.frame(
  time <- time,
  neu <- corona$erkrankte
)
plot(df.infiziert[seit:no.rows,], xlab = "date", ylab = "infiziert", main = "insgesamt infiziert")
abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()

df.schwere <- data.frame(
  time <- time,
  stationaer <- corona$stationaer,
  schwerer.verlauf <- corona$schwerer.verlauf
)
plot(df.schwere[seit:no.rows,]$time, df.schwere[seit:no.rows,]$stationaer, xlab = "date", ylab = "anzahl", main = "schwere und stationäre Verläufe")
par(new = TRUE)                             # Add new plot
plot(df.schwere[seit:no.rows,]$time, df.schwere[seit:no.rows,]$schwerer.verlauf, pch = 17, col="red", axes = FALSE, xlab = "", ylab = "")
abline(v = lastweek, col = 'black', lwd = 1)
abline(v = last2week, col = 'black', lwd = 1)
grid()
legend(x = "topleft",          # Position
       legend = c("stationär", "schwerer"),  # Legend texts
       fill = c("white","red"))       # Color of the squares




print("Datum:")
print(df.7days[no.rows,1])
print("7Tage index heute:")
print(df.7days[no.rows,2])
print("aktuell infiziert heute:")
print(df.aktiv[no.rows,2])

