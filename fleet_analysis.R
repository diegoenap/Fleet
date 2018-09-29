library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
source("fleet_func.R")

# Read Visitors data ====

fleet_visitors <- readVisitorData()
#fleet_visitors <- fleet_visitors[Date >= "2017-07-01"]
head(fleet_visitors)
summary(fleet_visitors)
str(fleet_visitors)

# Read GA data ====
ga_allws_nochn_all <- readQueryExplorerData("data/ga/fleet_allws_nochn_all.tsv", c("Date"))
ga_allws_nochn_os <- readQueryExplorerData("data/ga/fleet_allws_nochn_os.tsv", c("Date"))
ga_allws_nochn_us <- readQueryExplorerData("data/ga/fleet_allws_nochn_us.tsv", c("Date"))
ga_allws_nochn_do <- readQueryExplorerData("data/ga/fleet_allws_nochn_do.tsv", c("Date"))
ga_allws_nochn_lo <- readQueryExplorerData("data/ga/fleet_allws_nochn_lo.tsv", c("Date"))

ga_allws_chn_all <- readQueryExplorerData("data/ga/fleet_allws_chn_all.tsv", c("Date", "Channel"))
ga_allws_chn_os <- readQueryExplorerData("data/ga/fleet_allws_chn_os.tsv", c("Date", "Channel"))
ga_allws_chn_us <- readQueryExplorerData("data/ga/fleet_allws_chn_us.tsv", c("Date", "Channel"))
ga_allws_chn_do <- readQueryExplorerData("data/ga/fleet_allws_chn_do.tsv", c("Date", "Channel"))
ga_allws_chn_lo <- readQueryExplorerData("data/ga/fleet_allws_chn_lo.tsv", c("Date", "Channel"))

ga_admin_nochn_all <- readQueryExplorerData("data/ga/fleet_admin_nochn_all.tsv", c("Date"))
ga_admin_nochn_os <- readQueryExplorerData("data/ga/fleet_admin_nochn_os.tsv", c("Date"))
ga_admin_nochn_us <- readQueryExplorerData("data/ga/fleet_admin_nochn_us.tsv", c("Date"))
ga_admin_nochn_do <- readQueryExplorerData("data/ga/fleet_admin_nochn_do.tsv", c("Date"))
ga_admin_nochn_lo <- readQueryExplorerData("data/ga/fleet_admin_nochn_lo.tsv", c("Date"))

ga_admin_chn_all <- readQueryExplorerData("data/ga/fleet_admin_chn_all.tsv", c("Date", "Channel"))
ga_admin_chn_os <- readQueryExplorerData("data/ga/fleet_admin_chn_os.tsv", c("Date", "Channel"))
ga_admin_chn_us <- readQueryExplorerData("data/ga/fleet_admin_chn_us.tsv", c("Date", "Channel"))
ga_admin_chn_do <- readQueryExplorerData("data/ga/fleet_admin_chn_do.tsv", c("Date", "Channel"))
ga_admin_chn_lo <- readQueryExplorerData("data/ga/fleet_admin_chn_lo.tsv", c("Date", "Channel"))

ga_shows_nochn_all <- readQueryExplorerData("data/ga/fleet_shows_nochn_all.tsv", c("Date"))
ga_shows_nochn_os <- readQueryExplorerData("data/ga/fleet_shows_nochn_os.tsv", c("Date"))
ga_shows_nochn_us <- readQueryExplorerData("data/ga/fleet_shows_nochn_us.tsv", c("Date"))
ga_shows_nochn_do <- readQueryExplorerData("data/ga/fleet_shows_nochn_do.tsv", c("Date"))
ga_shows_nochn_lo <- readQueryExplorerData("data/ga/fleet_shows_nochn_lo.tsv", c("Date"))

ga_shows_chn_all <- readQueryExplorerData("data/ga/fleet_shows_chn_all.tsv", c("Date", "Channel"))
ga_shows_chn_os <- readQueryExplorerData("data/ga/fleet_shows_chn_os.tsv", c("Date", "Channel"))
ga_shows_chn_us <- readQueryExplorerData("data/ga/fleet_shows_chn_us.tsv", c("Date", "Channel"))
ga_shows_chn_do <- readQueryExplorerData("data/ga/fleet_shows_chn_do.tsv", c("Date", "Channel"))
ga_shows_chn_lo <- readQueryExplorerData("data/ga/fleet_shows_chn_lo.tsv", c("Date", "Channel"))

ga_exhib_nochn_all <- readQueryExplorerData("data/ga/fleet_exhib_nochn_all.tsv", c("Date"))
ga_exhib_nochn_os <- readQueryExplorerData("data/ga/fleet_exhib_nochn_os.tsv", c("Date"))
ga_exhib_nochn_us <- readQueryExplorerData("data/ga/fleet_exhib_nochn_us.tsv", c("Date"))
ga_exhib_nochn_do <- readQueryExplorerData("data/ga/fleet_exhib_nochn_do.tsv", c("Date"))
ga_exhib_nochn_lo <- readQueryExplorerData("data/ga/fleet_exhib_nochn_lo.tsv", c("Date"))

ga_exhib_chn_all <- readQueryExplorerData("data/ga/fleet_exhib_chn_all.tsv", c("Date", "Channel"))
ga_exhib_chn_os <- readQueryExplorerData("data/ga/fleet_exhib_chn_os.tsv", c("Date", "Channel"))
ga_exhib_chn_us <- readQueryExplorerData("data/ga/fleet_exhib_chn_us.tsv", c("Date", "Channel"))
ga_exhib_chn_do <- readQueryExplorerData("data/ga/fleet_exhib_chn_do.tsv", c("Date", "Channel"))
ga_exhib_chn_lo <- readQueryExplorerData("data/ga/fleet_exhib_chn_lo.tsv", c("Date", "Channel"))

ga_exhib2_nochn_all <- readQueryExplorerData("data/ga/fleet_exhib2_nochn_all.tsv", c("Date"))
ga_exhib2_nochn_os <- readQueryExplorerData("data/ga/fleet_exhib2_nochn_os.tsv", c("Date"))
ga_exhib2_nochn_us <- readQueryExplorerData("data/ga/fleet_exhib2_nochn_us.tsv", c("Date"))
ga_exhib2_nochn_do <- readQueryExplorerData("data/ga/fleet_exhib2_nochn_do.tsv", c("Date"))
ga_exhib2_nochn_lo <- readQueryExplorerData("data/ga/fleet_exhib2_nochn_lo.tsv", c("Date"))

ga_exhib2_chn_all <- readQueryExplorerData("data/ga/fleet_exhib2_chn_all.tsv", c("Date", "Channel"))
ga_exhib2_chn_os <- readQueryExplorerData("data/ga/fleet_exhib2_chn_os.tsv", c("Date", "Channel"))
ga_exhib2_chn_us <- readQueryExplorerData("data/ga/fleet_exhib2_chn_us.tsv", c("Date", "Channel"))
ga_exhib2_chn_do <- readQueryExplorerData("data/ga/fleet_exhib2_chn_do.tsv", c("Date", "Channel"))
ga_exhib2_chn_lo <- readQueryExplorerData("data/ga/fleet_exhib2_chn_lo.tsv", c("Date", "Channel"))


# Create weekend dataframe ====

daysinfo <- data.table(Date = seq(as.Date("2017-07-01"), as.Date("2018-07-12"), by = "day"))
daysinfo$weekNum <- wday(daysinfo$Date, week_start = 1)
daysinfo$isFirstSunday <- wday(daysinfo$Date, week_start = 1) == 7 & day(daysinfo$Date) <= 7


# Exploring Visitors ====

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(stat = "identity", size = 0.25) +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  theme_bw()

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(stat = "identity", size = 0.25) +
  theme_bw() +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  coord_cartesian(xlim = as.Date(c("2017-01-01", "2017-12-31")), ylim = c(0, 5000))

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey") +
  geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Weekends") +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  coord_cartesian(xlim = as.Date(c("2017-06-01", "2017-07-31")), ylim = c(0, 5000))

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey") +
  geom_point(size = 0.75, col = "grey15") +
  geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Weekends and Thursdays") +
  geom_vline(xintercept = weekends[isThursday == TRUE, Date], col = "indianred2", lty = 2) +
  geom_vline(xintercept = weekends[isWeekend == TRUE, Date], col = "lightblue", lty = 2) +
  coord_cartesian(xlim = as.Date(c("2017-09-01", "2017-10-31")), ylim = c(0, 5000))

ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey") +
  geom_point(size = 0.5, col = "grey15") +
  #geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  geom_line(data = trendValues(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Months") +
  coord_cartesian(xlim = as.Date(c("2017-01-01", "2017-12-31")), ylim = c(0, 5000))


ggplot(asianart_visitors, aes(Date, visitors)) +
  geom_line(size = 0.5, col = "grey40") +
  #geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  geom_vline(xintercept = daysinfo[weekNum == 7, Date], col = "lightblue", lty = 2) +
  geom_vline(xintercept = daysinfo[isFirstSunday == TRUE, Date], col = "red", lty = 2) +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Months") +
  coord_cartesian(xlim = as.Date(c("2017-06-15", "2017-12-31")))

ggplot(ga_allws_chn_us[Channel == "Organic Search"], aes(Date, UniquePageviews)) +
  geom_line(size = 0.5, col = "grey40") +
  #geom_line(data = movingAverage(asianart_visitors), aes(Date, visitors), size = 0.5, col = "blue") +
  geom_vline(xintercept = daysinfo[weekNum == 7, Date], col = "lightblue", lty = 2) +
  geom_vline(xintercept = daysinfo[isFirstSunday == TRUE, Date], col = "red", lty = 2) +
  theme_bw() +
  labs(title = "Asian Art Museum visits - Months") +
  coord_cartesian(xlim = as.Date(c("2017-06-15", "2017-12-31")))



# Correlation tests ====

teststart <- "2017-07-01"  # Values for Full Data
testend <- "2018-06-11"  # 31 days smaller than limit 2018-06-30
maxlag <- 31
visitordata <- fleet_visitors[Date >= teststart & Date <= as.Date(testend) + maxlag]

# --- All Website
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_allws_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "AllWS-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_allwebsite.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_allwebsite.csv")

# --- Admissions
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_admin_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_admin_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_admin_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_admin_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_admin_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_admin_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "ADMIN-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_admin.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_admin.csv")


# --- Shows
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_shows_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_shows_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_shows_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_shows_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_shows_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_shows_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "SHOWS-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_shows.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_shows.csv")


# --- Exhibitions
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_exhib.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_exhib.csv")


# --- Exhibitions V2
rm(res, res01, res02, res03, res04, res05)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_nochn_all[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_nochn_os[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_nochn_us[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_nochn_do[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_nochn_lo[Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO")
res <- rbind(res01, res02, res03, res04, res05)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_all[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-All-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_os[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-OS-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_us[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-US-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_do[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-DO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

rm(res01, res02, res03, res04, res05, res06, res07, res08)
res01 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "(Other)" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-(Others)")
res02 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Direct" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Direct")
res03 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Display" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Display")
res04 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Email" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Email")
res05 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Organic Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Organic Search")
res06 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Paid Search" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Paid Search")
res07 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Referral" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Referral")
res08 <- getMaxCorrelationsTable(completeGADates(ga_exhib2_chn_lo[Channel == "Social" & Date >= teststart & Date <= testend], limitDates = c(teststart, testend)), visitordata, tptype = "EXHIB2-LO-Social")
res <- rbind(res, res01, res02, res03, res04, res05, res06, res07, res08)

fwrite(res, file = "output/correlations_exhib2.csv")
fwrite(getBestCorrelations(res), file = "output/best_correlations_exhib2.csv")


plotLagComparisonv2(movingAverage(ga_allws_chn_us[Channel == "Organic Search"]),
                    movingAverage(visitordata),
                    plotstart = "2017-06-15",
                    plotend = "2017-12-31",
                    metric = "UniquePageviews",
                    dlag = 1)

plotLagComparisonv2(ga_allws_chn_us[Channel == "Organic Search"],
                    visitordata,
                    plotstart = "2017-06-15",
                    plotend = "2017-12-31",
                    metric = "UniquePageviews",
                    dlag = 1)
