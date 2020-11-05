library(zoo)
url <- "https://opendata.jena.de/dataset/2cc7773d-beba-43ad-9808-a420a67ffcb3/resource/d3ba07b6-fb19-451b-b902-5b18d8e8cbad/download/corona_erkrankungen_jena.csv"
corona <- read.csv2(url, sep=",")

days = 40
no.rows <- nrow(corona)
if (days > no.rows) {
  days = no.rows
}

seit = no.rows-days  

par(mfcol = c(3, 2)) #for multiple plots

df.neu <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  neu <- corona$neu_erkrankte
  )
plot(df.neu[seit:no.rows,], xlab = "date", ylab = "neu", main = "neu infiziert")
grid()


df.7days <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  day.index <-rollsumr(corona$neu_erkrankte, k = 7, fill = NA) *100000/108127 # Einwohner Zahl 19.10.2020 laut jena.de/corona leider keine open data quelle
)


plot(df.7days[seit:no.rows,], xlab = "date", ylab = "7 Tage index", main = "7-Tage Index", col=ifelse(df.7days[seit:no.rows,2] > 50, "red", ifelse(df.7days[seit:no.rows,2] > 35 , "yellow", "black")), pch = ifelse(df.7days[seit:no.rows,2] > 35,19,1))
grid()
print("Datum:")
print(df.7days[no.rows,1])
print("7Tage index heute:")
print(df.7days[no.rows,2])


df.neu.aktiv <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  neu <- corona$neu_erkrankte - rollapply(corona$genesene, 2, diff, fill = NA, align = "right", by.column=FALSE)
)
plot(df.neu.aktiv[seit:no.rows,], xlab = "date", ylab = "neu aktiv", main = "neu aktive fälle")
grid()

df.neu.aktiv.7day <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  neu <- rollsumr(corona$neu_erkrankte - rollapply(corona$genesene, 2, diff, fill = NA, align = "right", by.column=FALSE), k = 7, fill = NA) *100000/108127 # Einwohner Zahl 19.10.2020 laut jena.de/corona leider keine open data quelle
)
plot(df.neu.aktiv.7day[seit:no.rows,], xlab = "date", ylab = "neu aktiv 7 tage", main = "neu aktiv in den letzen 7 Tagen")
grid()

df.aktiv <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  neu <- corona$aktive_faelle
)
plot(df.aktiv[seit:no.rows,], xlab = "date", ylab = "aktiv", main = "aktuell infiziert")
grid()


df.infiziert <- data.frame(
  time <- as.Date(as.POSIXct(corona$zeit, origin="1970-01-01")),
  neu <- corona$erkrankte
)
plot(df.infiziert[seit:no.rows,], xlab = "date", ylab = "infiziert", main = "insgesamt infiziert")
grid()



#ifelse(day.index > 50, "red", ifelse(day.index > 35 , "yellow", "black"))