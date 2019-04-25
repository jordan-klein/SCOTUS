#### Import bonica data ####

x <- read.csv("~/Documents/bonica_sen_fed_judges_20180709.csv")

names(x) <- gsub("X.", "", names(x))

paste(x$judge.last.name., x$judge.first.name., sep = ", ") -> x$name

judge_name <- c("Ginsburg, Ruth", "Breyer, Stephen", "Sotomayor, Sonia", "Roberts, John", 
                "Thomas, Clarence", "Alito, Samuel", "Gorsuch, Neil", "Kennedy, Anthony", "Garland, Merrick", 
                "Scalia, Antonin", "Kavanaugh, Brett", "Kethledge, Raymond", "Souter, David")

x <- x[grepl(paste(judge_name, collapse = "|"), x$name, ignore.case = TRUE), ]

bonica <- subset(x, select = c("name", "party.affiliation.of.president.", "enter.year1.", "court.name.", "state.", 
                             "nomination.date.senate.executive.journal.", "dime.cfscore.", "imputed.dime.cfscore.", 
                             "jcs.score.dw.", "jcs.cfscore.cf."))

bonica <- bonica[-(grepl("Sotomayor, Sonia", bonica$name) & grepl("Republican", bonica$party.affiliation.of.president.)), ]

#### Merge MQ score & dw_nom score database I generated ####

db <- read.csv("~/Documents/SCOTUS.csv")

db1 <- db[db$name %in% bonica$name, ]
db2 <- rbind(db1, subset(db, name == "Stevens, John Paul" | name == "Kagan, Elena" | name == "Barrett, Amy"))

ct <- merge(bonica, db2, by = "name", all = TRUE)

ct$name[ct$name == "Ginsburg, Ruth"] <- "Ginsburg, Ruth Bader"

#### Import & merge CBI scores ####

library(readxl)
JudgeCBIScores <- read_excel("~/Downloads/JudgeCBIScores.xls", 
                             sheet = "SupremeCourtJustices")
JudgeCBIScores$`Justice Name` <- trimws(JudgeCBIScores$`Justice Name`, which = "both")

judge <- JudgeCBIScores[JudgeCBIScores$`Justice Name` %in% ct$name, ]

scot <- merge(ct, judge, by.x = "name", by.y = "Justice Name", all = TRUE)

#### Create final dataset ####

SCOTUS_data <- subset(scot, select = c("name", "party", "imputed.dime.cfscore.", "jcs.score.dw.", 
                                       "jcs.cfscore.cf.", "post_mn", "CBI Score", "dw_nominate"))

names(SCOTUS_data) <- c("name", "party", "imported_cf_score", "imported_jcs_dw_nominate", "imported_jcs_cf_score", 
                        "imported_mq_score", "imported_cbi_score", "derived_dw_nominate")

SCOTUS_data$imported_mq_score[SCOTUS_data$name == "Scalia, Antonin"] <-1.556

summary(lm(imported_cf_score ~ derived_dw_nominate, data = SCOTUS_data))
summary(lm(imported_jcs_dw_nominate ~ derived_dw_nominate, data = SCOTUS_data))
summary(lm(imported_jcs_cf_score ~ derived_dw_nominate, data = SCOTUS_data))
summary(lm(imported_mq_score ~ derived_dw_nominate, data = SCOTUS_data))
summary(lm(imported_cbi_score ~ derived_dw_nominate, data = SCOTUS_data))

SCOTUS_data$calculated_mq_score1[is.na(SCOTUS_data$imported_mq_score)] <- 
  -1.0714 + 3.0916*SCOTUS_data$derived_dw_nominate[is.na(SCOTUS_data$imported_mq_score)]

SCOTUS_data$mq_score1[!is.na(SCOTUS_data$imported_mq_score)] <- 
  SCOTUS_data$imported_mq_score[!is.na(SCOTUS_data$imported_mq_score)]
SCOTUS_data$mq_score1[!is.na(SCOTUS_data$calculated_mq_score1)] <- 
  SCOTUS_data$calculated_mq_score1[!is.na(SCOTUS_data$calculated_mq_score1)]

SCOTUS_data$mix_dw_nominate[!is.na(SCOTUS_data$imported_jcs_dw_nominate)] <- 
  SCOTUS_data$imported_jcs_dw_nominate[!is.na(SCOTUS_data$imported_jcs_dw_nominate)]
SCOTUS_data$mix_dw_nominate[is.na(SCOTUS_data$imported_jcs_dw_nominate)] <- 
  SCOTUS_data$derived_dw_nominate[is.na(SCOTUS_data$imported_jcs_dw_nominate)]

summary(lm(imported_mq_score ~ mix_dw_nominate, data = SCOTUS_data))

mq_dw <- lm(imported_mq_score ~ derived_dw_nominate, data = SCOTUS_data)

plot(mq_dw$fitted.values, mq_dw$residuals)

SCOTUS_data$sq_dw_nominate <- SCOTUS_data$derived_dw_nominate^2

model(SCOTUS_data$imported_mq_score, SCOTUS_data$sq_dw_nominate)
plot(SCOTUS_data$sq_dw_nominate, SCOTUS_data$imported_mq_score)

abline(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$sq_dw_nominate))


SCOTUS_data$dw_4th <- SCOTUS_data$derived_dw_nominate^4
SCOTUS_data$dw_3rd <- SCOTUS_data$derived_dw_nominate^3
SCOTUS_data$dw_sqrt <- sqrt(SCOTUS_data$derived_dw_nominate)
SCOTUS_data$dw_log <- log(SCOTUS_data$derived_dw_nominate)
SCOTUS_data$dw_log1 <- log(SCOTUS_data$derived_dw_nominate + 1)

summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_4th))
summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_3rd))

summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_sqrt))
summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_log))
summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_log1))

SCOTUS_data$dw_1 <- 1/SCOTUS_data$derived_dw_nominate
SCOTUS_data$dw_12 <- 1/(SCOTUS_data$derived_dw_nominate^2)
SCOTUS_data$dw_13 <- 1/(SCOTUS_data$derived_dw_nominate^3)
SCOTUS_data$dw_14 <- 1/(SCOTUS_data$derived_dw_nominate^4)

summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_1))
summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_12))
summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_13))
summary(lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_14))

model <- lm(SCOTUS_data$imported_mq_score ~ SCOTUS_data$dw_3rd)

SCOTUS_data$dw_nominate_3 <- SCOTUS_data$dw_3rd

### Plot model ###

plot(SCOTUS_data$dw_nominate_3, SCOTUS_data$imported_mq_score)
abline(model)

plot(model$fitted.values, model$residuals)


### Estimate missing vars ###

SCOTUS_data$calculated_mq_score2[is.na(SCOTUS_data$imported_mq_score)] <- 
  -1.3481 + 10.0206*SCOTUS_data$derived_dw_nominate[is.na(SCOTUS_data$imported_mq_score)]^3 

SCOTUS_data$mq_score2[is.na(SCOTUS_data$imported_mq_score)] <- 
  SCOTUS_data$calculated_mq_score2[is.na(SCOTUS_data$imported_mq_score)]
SCOTUS_data$mq_score2[!is.na(SCOTUS_data$imported_mq_score)] <- 
  SCOTUS_data$imported_mq_score[!is.na(SCOTUS_data$imported_mq_score)]


#### Clean file for export ####

SCOTUS_data_clean <- subset(SCOTUS_data, select = c("name", "party", "imported_cf_score", 
                                                    "derived_dw_nominate", "imported_cbi_score", 
                                                    "imported_mq_score", "calculated_mq_score1", 
                                                    "calculated_mq_score2", "mq_score1", "mq_score2"))
write.csv(SCOTUS_data_clean, "~/Documents/SCOTUS_clean_file.csv")


#### Work w Lee epstein data ####

library(foreign)

epstein_sc <- read.dta("~/Downloads/JCS115.01/JCS justices 2018.dta")

epstein_fed <- read.dta("~/Downloads/JCS115.01/JCS appeals 2018.dta")

install.packages("readstata13")

library(readstata13)

library(haven)

epstein_sc <- haven::read_dta("~/Downloads/JCS115.01/JCS justices 2018.dta")
epstein_fed <- haven::read_dta("~/Downloads/JCS115.01/JCS appeals 2018.dta")


names <- c("Ginsburg", "Breyer", "Sotomayor", "Roberts", "Thomas", "Alito", 
           "Gorsuch", "Kennedy", "Garland", "Scalia, Antonin", "Kavanaugh, Brett", "Kethledge, Raymond", "Souter, David")



ep_sc <- subset(epstein_sc, !is.na(jcs2016) | justiceName == "AScalia" | 
                  justiceName == "JPStevens" | justiceName == "DHSouter")

ep_sc$jcs <- ep_sc$jcs2016 
ep_sc$jcs[ep_sc$justiceName == "AScalia"] <- ep_sc$jcs2015[ep_sc$justiceName == "AScalia"]
ep_sc$jcs[ep_sc$justiceName == "JPStevens"] <- ep_sc$jcs2009[ep_sc$justiceName == "JPStevens"]
ep_sc$jcs[ep_sc$justiceName == "DHSouter"] <- ep_sc$jcs2008[ep_sc$justiceName == "DHSouter"]

ep_fed <- subset(epstein_fed, name == "Kethledge, Raymond" | name == "Kavanaugh, Brett" | 
                   name == "Hardiman, Thomas", select = c("name", "JCS2018"))

ep_sc <- subset(ep_sc, select = c("justiceName", "jcs"))
names(ep_sc) <- c("name", "jcs")
names(ep_fed) <- c("name", "jcs")

ep_data <- rbind(ep_sc, ep_fed)

write.csv(ep_data, "~/Documents/scotus_data.csv")



#### Well known politicians ####

congress <- read.csv("~/Downloads/HSall_members.csv")

congress$bioname <- lapply(congress$bioname, tolower)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), 
        sep = "", collapse = " ")
}

congress$bioname <- lapply(congress$bioname, simpleCap)

  simpleCap(congress$bioname)

politicians <- subset(congress, (congress == 115 & bioname == "Sanders, Bernard") | 
                        (congress == 111 & bioname == "Clinton, Hillary Rodham") | (congress == 113 & bioname == "Obama, Barack") | 
                        (congress == 115 & bioname == "Mcconnell, Addison Mitchell (mitch)") | (congress == 115 & bioname == "Ryan, Paul D.") | 
                        (congress == 115 & bioname == "Cruz, Rafael Edward (ted)") | (congress == 115 & bioname == "Collins, Susan Margaret") | 
                        (congress == 115 & bioname == "Manchin, Joe, Iii") | (congress == 115 & bioname == "Schumer, Charles Ellis (chuck)") | 
                        (congress == 115 & bioname == "Pelosi, Nancy") | (congress == 115 & bioname == "Flake, Jeff") | (congress == 115 & bioname == "Hirono, Mazie"))

politicians$party[politicians$party_code == 100] <- "Democrat"
politicians$party[politicians$party_code == 328] <- "Democrat"
politicians$party[politicians$party_code == 200] <- "Republican"

politicians$dw_nominate <- politicians$nominate_dim1
politicians$name <- politicians$bioname

politicians <- subset(politicians, select = c("name", "dw_nominate", "party"))
politicians$name <- as.character(politicians$name)

write.csv(politicians, "~/Documents/politicians.csv")



congress <- subset(congress, congress == 115)

senate <- subset(congress, chamber == "Senate")





#### Modeling function ####

model <- function(y, x) {
  mod <- lm(y ~ x)
  return(summary(mod))
  return(plot(x, y))
  return(abline(mod))
}


summary(mq_dw)
plot(SCOTUS_data$derived_dw_nominate, SCOTUS_data$imported_mq_score)
abline(mq_dw)

SCOTUS_data$derived_dw_nominate


SCOTUS <- SCOTUS[grepl(paste(judge_name, collapse = "|"), SCOTUS$name, ignore.case = TRUE), ]

SCOTUS <- unique(SCOTUS)

write.csv(SCOTUS, "~/Documents/SCOTUS.csv")




court <- subset(justices, term == 2016)

court$justiceName <- c("Kennedy, Anthony", "Thomas, Clarence", "Kagan, Elena", "Roberts, John", 
                       "Gorsuch, Neil", "Ginsburg, Ruth", "Alito, Samuel", "Breyer, Stephen", "Sotomayor, Sonia")

scotus <- merge(SCOTUS, court, by.x = "name", by.y = "justiceName", all = TRUE)

scotus$der_cfscore <- .08232 + .26125*scotus$post_mn

scotus$cfscore[is.na(scotus$cfscore)] <- scotus$der_cfscore[is.na(scotus$cfscore)]


scotus$party[scotus$name == "Kagan, Elena" | scotus$name == "Ginsburg, Ruth" | scotus$name == "Breyer, Stephen" | 
               scotus$name == "Sotomayor, Sonia" | scotus$name == "Garland, Merrick"] <- "Democrat"
scotus$party[is.na(scotus$party)] <- "Republican"

scotus$name[scotus$name == "EID, ALLISON"] <- "Eid, Allison"

scotus <- scotus[-grep("Souter, David Hackett", scotus$name, ignore.case = TRUE), ]

scotus <- subset(scotus, select = c("name", "cfscore", "post_mn", "party"))

write.csv(scotus, "~/Documents/scotus.csv")



mem <- HSall_members


write.csv(SCOTUS, "~/Documents/SCOTUS.csv")




x$cfscore <- x$imputed.dime.cfscore.

SCOTUS <- subset(x, select = c("name", "cfscore"))



state <- subset(bw_ssc_db, select = c("name", "cfscore"))

SCOTUS <- rbind(SCOTUS, state)

SCOTUS_data$sq_mix_dw <- SCOTUS_data$mix_dw_nominate^2



judge_name <- c("Ginsburg, Ruth", "Breyer, Stephen", "Kagan, Elena", "Sotomayor, Sonia", "Roberts, John", 
                "Thomas, Clarence", "Alito, Samuel", "Gorsuch, Neil", "Kennedy, Anthony", "Garland, Merrick", 
                "Scalia, Antonin", "Barrett, Amy", "Colloton, Steven", "Eid, Allison", "Gruender, Raymond", 
                "Hardiman, Thomas", "Kavanaugh, Brett", "Kethledge, Raymond", "Larsen, Joan", "Moreno, Federico", 
                "Newsom, Kevin", "Pryor, William", "Ryan, Margaret", "Stras, David", "Sykes, Diane", 
                "Thapar, Amul", "Tymkovich, Timothy", "Blackwell, Keith", "Canady, Charles", "Grant, Britt", 
                "Lee, Thomas", "Mansfield, Edward", "Young, Robert", "Willett, Don", "Wyrick, Patrick", "Souter, David")
