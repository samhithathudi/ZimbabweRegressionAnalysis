ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EXP
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
cols <- rainbow(length(vars))   # you can also use 1:length(vars)
plot(
ZimbabweData$Year,
ZimbabweData[[ vars[1] ]],
type = "l",
col  = cols[1],
lwd  = 2,
xlab = "Year",
ylab = "Value",
main = "Time Series of All Variables"
)
for(i in 2:length(vars)) {
lines(
ZimbabweData$Year,
ZimbabweData[[ vars[i] ]],
col = cols[i],
lwd = 2
)
}
legend(
"topright",
legend = vars,
col    = cols,
lty    = 1,
cex    = 0.8
)
library(readxl)
file_path <- file.choose()
ZimbabweData <- read_excel(file_path)
ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EXP
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
summary_table <- data.frame(
Variable = vars,
Mean     = round(sapply(ZimbabweData[vars], mean, na.rm = TRUE), 3),
StdDev   = round(sapply(ZimbabweData[vars], sd,   na.rm = TRUE), 3),
Min      = round(sapply(ZimbabweData[vars], min,  na.rm = TRUE), 3),
Max      = round(sapply(ZimbabweData[vars], max,  na.rm = TRUE), 3)
)
print(summary_table)
library(readxl)
file_path <- file.choose()
ZimbabweData <- read_excel(file_path)
ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EXP
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
summary_table <- data.frame(
Variable = vars,
Mean     = round(sapply(ZimbabweData[vars], mean, na.rm = TRUE), 3),
StdDev   = round(sapply(ZimbabweData[vars], sd,   na.rm = TRUE), 3),
Min      = round(sapply(ZimbabweData[vars], min,  na.rm = TRUE), 3),
Max      = round(sapply(ZimbabweData[vars], max,  na.rm = TRUE), 3)
library(readxl)
zim <- read_excel("ZimbabweData.xlsx", sheet = 1)
library(readxl)
file_path     <- file.choose()
ZimbabweData  <- read_excel(file_path)
ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EXP
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
# 4. Build the summary‐stats table
summary_table <- data.frame(
Variable = vars,
Mean     = round(sapply(ZimbabweData[ vars ], mean, na.rm = TRUE),  3),
StdDev   = round(sapply(ZimbabweData[ vars ], sd,   na.rm = TRUE),  3),
Min      = round(sapply(ZimbabweData[ vars ], min,  na.rm = TRUE),  3),
Max      = round(sapply(ZimbabweData[ vars ], max,  na.rm = TRUE),  3)
)
print(summary_table)
library(readxl)
file_path     <- file.choose()
ZimbabweData  <- read_excel(file_path)
ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EXP
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
cor_matrix <- round(cor(ZimbabweData[vars], use = "complete.obs"), 3)
print(cor_matrix)
View(ZimbabweDataCleaned_v2)
library(readxl)
ZimbabweData <- read_excel("Desktop/ZimbabweData.xlsx")
View(ZimbabweData)
ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EX
ZimbabweData$OPEN <- ZimbabweData$IMP + ZimbabweData$EXP
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
plot(
ZimbabweData$Year,
ZimbabweData[[ vars[1] ]],
type="l",
lwd=2,
xlab="Year",
ylab="Value",
main="Time Series of All Variables"
)
for(i in 2:length(vars)) {
lines(
ZimbabweData$Year,
ZimbabweData[[ vars[i] ]],
lwd=2
)
}
legend(
"topright",
legend=vars,
lty=1,
cex=0.8
)
vars <- c("GDPgr","GFC","INFL","POPgr","GOV","IMP","EXP","OPEN")
cols <- rainbow(length(vars))
plot(
ZimbabweData$Year,
ZimbabweData[[ vars[1] ]],
type="l", lwd=2,
col=cols[1],
xlab="Year", ylab="Value",
main="Time Series with Colored Lines"
)
for(i in 2:length(vars)) {
lines(
ZimbabweData$Year,
ZimbabweData[[ vars[i] ]],
lwd=2,
col=cols[i]
)
}
legend(
"topright",
legend=vars,
col=cols,
lty=1,
cex=0.8
)
summary_table <- data.frame(
Variable = vars,
Mean     = round(sapply(ZimbabweData[ vars ], mean, na.rm = TRUE),  3),
StdDev   = round(sapply(ZimbabweData[ vars ], sd,   na.rm = TRUE),  3),
Min      = round(sapply(ZimbabweData[ vars ], min,  na.rm = TRUE),  3),
Max      = round(sapply(ZimbabweData[ vars ], max,  na.rm = TRUE),  3)
)
print(summary_table)
cor_matrix <- round(cor(ZimbabweData[vars], use = "complete.obs"), 3)
print(cor_matrix)
#Regression Model 1
Regressions
#Regressions
RegModel1=lm(GDPgr~GFC,data=ZimbabweData)
Summary(RegModel1)
install.packages("readxl")
install.packages("readxl")
install.packages("readxl")
install.packages("readxl")
# install.packages(c("readxl","stargazer","dplyr"))  # run once
library(readxl); library(dplyr); library(stargazer)
install.packages("readxl")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("car")
install.packages("corrplot")
install.packages("stargzer)
install.packages("stargazer")
install.packages("stargazer")
install.packages("lmtest")
install.packages("sandwich")
install.packages("nlme")
library(readxl); library(dplyr); library(stargazer)
file_path    <- file.choose(ZimbabweData)
file_path    <- file.choose(ZimbabweData)
ZimbabweData <- ZimbabweData %>%
arrange(Year) %>%
mutate(
OPEN      = IMP + EXP,
GDPgr_lag = lag(GDPgr, 1)
)
ZimbabweData <- ZimbabweData %>%
arrange(Year) %>%
mutate(
OPEN      = IMP + EXP,
GDPgr_lag = lag(GDPgr, 1)
)
install.packages("stargazer")
library(stargazer)
stargazer(
m1, m2, m3, m4, m5, m6,
type            = "text",
title           = "Basic Growth Regressions",
dep.var.caption = "Dependent: GDPgr",
column.labels   = c(
"1: GFC",
"2: GFC + POPgr",
"3: GFC + POPgr + INFL",
"4: … + GOV",
"5: … + OPEN",
"6: Lag GDPgr + All"
),
keep.stat      = c("n","rsq","adj.rsq","f"),
star.cutoffs   = c(0.05, 0.01, 0.001),
digits         = 3,
no.space       = TRUE,
align          = TRUE
)
install.packages("stargazer")
library(stargazer)
stargazer(
m1, m2, m3, m4, m5, m6,
type            = "text",
title           = "Basic Growth Regressions",
dep.var.caption = "Dependent: GDPgr",
column.labels   = c(
"1: GFC",
"2: GFC + POPgr",
"3: GFC + POPgr + INFL",
"4: … + GOV",
"5: … + OPEN",
"6: Lag GDPgr + All"
),
keep.stat      = c("n","rsq","adj.rsq","f"),
star.cutoffs   = c(0.05, 0.01, 0.001),
digits         = 3,
no.space       = TRUE,
align          = TRUE
)
library(readxl); library(dplyr); library(stargazer)
ZimbabweData <- read_excel(file.choose())
ZimbabweData <- ZimbabweData %>%
arrange(Year) %>%
mutate(OPEN = IMP + EXP,
GDPgr_lag = lag(GDPgr, 1))
m1 <- lm(GDPgr ~ GFC,                                   data = ZimbabweData)
m2 <- lm(GDPgr ~ GFC + POPgr,                          data = ZimbabweData)
m3 <- lm(GDPgr ~ GFC + POPgr + INFL,                   data = ZimbabweData)
m4 <- lm(GDPgr ~ GFC + POPgr + INFL + GOV,             data = ZimbabweData)
m5 <- lm(GDPgr ~ GFC + POPgr + INFL + GOV + OPEN,      data = ZimbabweData)
m6 <- lm(GDPgr ~ GDPgr_lag + GFC + POPgr + INFL + GOV + OPEN, data = ZimbabweData)
stargazer(
m1, m2, m3, m4, m5, m6,
type            = "text",
title           = "Basic Growth Regressions",
dep.var.caption = "Dependent: GDPgr",
column.labels   = c(
"1: GFC",
"2: GFC + POPgr",
"3: GFC + POPgr + INFL",
"4: … + GOV",
"5: … + OPEN",
"6: Lag GDPgr + All"
),
keep.stat      = c("n","rsq","adj.rsq","f"),
star.cutoffs   = c(0.05, 0.01, 0.001),
digits         = 3,
no.space       = TRUE,
align          = TRUE
)
View(ZimbabweData)
ZimbabweData <- ZimbabweData %>%
mutate(
OilCrisis = ifelse(Year >= 1973 & Year <= 1979, 1, 0),
GFC2007   = ifelse(Year >= 2007 & Year <= 2009, 1, 0)
)
m_oil <- lm(GDPgr ~ GFC + POPgr + INFL + GOV + OPEN + OilCrisis, data = ZimbabweData)
m_gfc <- lm(GDPgr ~ GFC + POPgr + INFL + GOV + OPEN + GFC2007,   data = ZimbabweData)
stargazer(
m_oil, m_gfc,
type            = "text",
title           = "Effects of Major External Shocks on GDP per Capita Growth",
dep.var.caption = "Dependent: GDPgr",
column.labels   = c("Oil Crisis (’73–’79)", "Global Financial Crisis (’07–’09)"),
keep.stat       = c("n", "rsq", "adj.rsq", "f"),
star.cutoffs    = c(0.05, 0.01, 0.001),
digits          = 3,
no.space        = TRUE,
align           = TRUE
)
library(lmtest)
library(sandwich)
library(nlme)
dw5 <- dwtest(m5)
dw6 <- dwtest(m6)
m6_gls <- gls(
GDPgr ~ GDPgr_lag + GFC + POPgr + INFL + GOV + OPEN,
data        = ZimbabweData,
correlation = corAR1(form = ~ Year)
)
library(lmtest)
library(lmtest)
dw5 <- dwtest(m5)    # Regression 5
dw6 <- dwtest(m6)    # Regression 6
print(dw5)
print(dw6)
library(sandwich)
coeftest(m5, vcov = NeweyWest(m5, prewhite = FALSE, adjust = TRUE))
coeftest(m6, vcov = NeweyWest(m6, prewhite = FALSE, adjust = TRUE))
savehistory("~/Desktop/Untitled.Rhistory")
