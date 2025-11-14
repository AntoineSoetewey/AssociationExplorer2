library(dplyr)
library(naniar)
library(forcats)

# Filter for Belgium and select relevant columns
dat_BE <- read.csv("data/ESS11_full_dataset.csv", stringsAsFactors = TRUE) |>
  filter(cntry == "BE")

dat <- dat_BE |>
  select(nwspol:pplhlp, polintr:vote, happy:rlgblg, rlgdgr:dscrgrp, ctzcntr:brncntr,
         livecnta, feethngr, facntr, mocntr, ccrdprs, wrclmch, ctrlife:alcwknd, alcbnge:weighta, medtrun,
         stflife, gndr, agea, rshpsts, ipstrgva, vacc19)

# Replace specific values with NA
dat <- dat %>%
  naniar::replace_with_na(replace = list(
    # media and social trust
    nwspol = c(7777, 8888, 9999),
    netusoft = c(7, 8, 9),
    netustm = c(6666, 7777, 8888, 9999),
    ppltrst = c(77, 88, 99),
    pplfair = c(77, 88, 99),
    pplhlp = c(77, 88, 99),
    # politics
    polintr = c(7, 8, 9),
    psppsgva = c(7, 8, 9),
    actrolga = c(7, 8, 9),
    psppipla = c(7, 8, 9),
    cptppola = c(7, 8, 9),
    trstprl = c(77, 88, 99),
    trstlgl = c(77, 88, 99),
    trstplc = c(77, 88, 99),
    trstplt = c(77, 88, 99),
    trstprt = c(77, 88, 99),
    trstep = c(77, 88, 99),
    trstun = c(77, 88, 99),
    vote = c(7, 8, 9),
    # subjective well-being, social exclusion, etc.
    happy = c(77, 88, 99),
    sclmeet = c(77, 88, 99),
    inprdsc = c(77, 88, 99),
    sclact = c(7, 8, 9),
    crmvct = c(7, 8, 9),
    aesfdrk = c(7, 8, 9),
    health = c(7, 8, 9),
    hlthhmp = c(7, 8, 9),
    atchctr = c(77, 88, 99),
    atcherp = c(77, 88, 99),
    rlgblg = c(7, 8, 9),
    rlgdgr = c(77, 88, 99),
    rlgatnd = c(77, 88, 99),
    pray = c(77, 88, 99),
    dscrgrp = c(7, 8, 9),
    ctzcntr = c(7, 8, 9),
    brncntr = c(7, 8, 9),
    livecnta = c(6666, 7777, 8888, 9999),
    feethngr = c(7, 8, 9),
    facntr = c(7, 8, 9),
    mocntr = c(7, 8, 9),
    ccrdprs = c(66, 77, 88, 99),
    wrclmch = c(6, 7, 8, 9),
    # health and inequality
    ctrlife = c(77, 88, 99),
    etfruit = c(77, 88, 99),
    eatveg = c(77, 88, 99),
    dosprt = c(77, 88, 99),
    cgtsmok = c(7, 8, 9),
    alcfreq = c(77, 88, 99),
    alcwkdy = c(6666, 7777, 8888, 9999),
    alcwknd = c(6666, 7777, 8888, 9999),
    alcbnge = c(7, 8, 9),
    height = c(777, 888, 999),
    weighta = c(777, 888, 999),
    medtrun = c(7, 8, 9),
    stflife = c(77, 88, 99),
    gndr = c(9),
    agea = c(999),
    rshpsts = c(66, 77, 88, 99),
    ipstrgva = c(66, 77, 88, 99),
    vacc19 = c(7, 8, 9)
  ))

## Recoding dat$netusoft
dat$netusoft <- dat$netusoft %>%
  as.character() %>%
  fct_recode(
    "1 - Never" = "1",
    "2 - Only occasionally" = "2",
    "3 - A few times a week" = "3",
    "4 - Most days" = "4",
    "5 - Every day" = "5"
  )

## Recoding dat$polintr
dat$polintr <- dat$polintr %>%
  as.character() %>%
  fct_recode(
    "1 - Very interested" = "1",
    "2 - Quite interested" = "2",
    "3 - Hardly interested" = "3",
    "4 - Not at all interested" = "4"
  )

## Recoding dat$psppsgva
dat$psppsgva <- dat$psppsgva %>%
  as.character() %>%
  fct_recode(
    "1 - Not at all" = "1",
    "2 - Very little" = "2",
    "3 - Some" = "3",
    "4 - A lot" = "4",
    "5 - A great deal" = "5"
  )

## Recoding dat$actrolga
dat$actrolga <- dat$actrolga %>%
  as.character() %>%
  fct_recode(
    "1 - Not at all able" = "1",
    "2 - A little able" = "2",
    "3 - Quite able" = "3",
    "4 - Very able" = "4",
    "5 - Completely able" = "5"
  )

## Recoding dat$psppipla
dat$psppipla <- dat$psppipla %>%
  as.character() %>%
  fct_recode(
    "1 - Not at all" = "1",
    "2 - Very little" = "2",
    "3 - Some" = "3",
    "4 - A lot" = "4",
    "5 - A great deal" = "5"
  )

## Recoding dat$cptppola
dat$cptppola <- dat$cptppola %>%
  as.character() %>%
  fct_recode(
    "1 - Not at all confident" = "1",
    "2 - A little confident" = "2",
    "3 - Quite confident" = "3",
    "4 - Very confident" = "4",
    "5 - Completely confident" = "5"
  )

## Recoding dat$vote
dat$vote <- dat$vote %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2",
    "Not eligible to vote" = "3"
  )

## Recoding dat$sclmeet
dat$sclmeet <- dat$sclmeet %>%
  as.character() %>%
  fct_recode(
    "1 - Never" = "1",
    "2 - Less than once a month" = "2",
    "3 - Once a month" = "3",
    "4 - Several times a month" = "4",
    "5 - Once a week" = "5",
    "6 - Several times a week" = "6",
    "7 - Every day" = "7"
  )

## Recoding dat$inprdsc
dat$inprdsc <- dat$inprdsc %>%
  as.character() %>%
  fct_recode(
    "0 - None" = "0",
    "4-6" = "4",
    "7-9" = "5",
    "10 or more" = "6"
  )

## Recoding dat$sclact
dat$sclact <- dat$sclact %>%
  as.character() %>%
  fct_recode(
    "1 - Much less than most" = "1",
    "2 - Less than most" = "2",
    "3 - About the same" = "3",
    "4 - More than most" = "4",
    "5 - Much more than most" = "5"
  )

## Recoding dat$crmvct
dat$crmvct <- dat$crmvct %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$aesfdrk
dat$aesfdrk <- dat$aesfdrk %>%
  as.character() %>%
  fct_recode(
    "1 - Very safe" = "1",
    "2 - Safe" = "2",
    "3 - Unsafe" = "3",
    "4 - Very unsafe" = "4"
  )

## Recoding dat$health
dat$health <- dat$health %>%
  as.character() %>%
  fct_recode(
    "1 - Very good" = "1",
    "2 - Good" = "2",
    "3 - Fair" = "3",
    "4 - Bad" = "4",
    "5 - Very bad" = "5"
  )

## Recoding dat$hlthhmp
dat$hlthhmp <- dat$hlthhmp %>%
  as.character() %>%
  fct_recode(
    "1 - Yes a lot" = "1",
    "2 - Yes to some extent" = "2",
    "3 - No" = "3"
  )

## Recoding dat$rlgblg
dat$rlgblg <- dat$rlgblg %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$rlgatnd
dat$rlgatnd <- dat$rlgatnd %>%
  as.character() %>%
  fct_recode(
    "1 - Every day" = "1",
    "2 - More than once a week" = "2",
    "3 - Once a week" = "3",
    "4 - At least once a month" = "4",
    "5 - Only on special holy days" = "5",
    "6 - Less often" = "6",
    "7 - Never" = "7"
  )

## Recoding dat$pray
dat$pray <- dat$pray %>%
  as.character() %>%
  fct_recode(
    "1 - Every day" = "1",
    "2 - More than once a week" = "2",
    "3 - Once a week" = "3",
    "4 - At least once a month" = "4",
    "5 - Only on special holy days" = "5",
    "6 - Less often" = "6",
    "7 - Never" = "7"
  )

## Recoding dat$dscrgrp
dat$dscrgrp <- dat$dscrgrp %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$ctzcntr
dat$ctzcntr <- dat$ctzcntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$brncntr
dat$brncntr <- dat$brncntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$feethngr
dat$feethngr <- dat$feethngr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$facntr
dat$facntr <- dat$facntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$mocntr
dat$mocntr <- dat$mocntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$wrclmch
dat$wrclmch <- dat$wrclmch %>%
  as.character() %>%
  fct_recode(
    "1 - Not at all worried" = "1",
    "2 - Not very worried" = "2",
    "3 - Somewhat worried" = "3",
    "4 - Very worried" = "4",
    "5 - Extremely worried" = "5"
  )

## Recoding dat$etfruit
dat$etfruit <- dat$etfruit %>%
  as.character() %>%
  fct_recode(
    "1 - Three times or more a day" = "1",
    "2 - Twice a day" = "2",
    "3 - Once a day" = "3",
    "4 - Less than once a day but at least 4 times a week" = "4",
    "5 - Less than 4 times a week but at least once a week" = "5",
    "6 - Less than once a week" = "6",
    "7 - Never" = "7"
  )

## Recoding dat$eatveg
dat$eatveg <- dat$eatveg %>%
  as.character() %>%
  fct_recode(
    "1 - Three times or more a day" = "1",
    "2 - Twice a day" = "2",
    "3 - Once a day" = "3",
    "4 - Less than once a day but at least 4 times a week" = "4",
    "5 - Less than 4 times a week but at least once a week" = "5",
    "6 - Less than once a week" = "6",
    "7 - Never" = "7"
  )

## Recoding dat$cgtsmok
dat$cgtsmok <- dat$cgtsmok %>%
  as.character() %>%
  fct_recode(
    "1 - I smoke daily, 10 or more cigarettes" = "1",
    "2 - I smoke daily, 9 or fewer cigarettes" = "2",
    "3 - I smoke but not every day" = "3",
    "4 - I don’t smoke now but I used to" = "4",
    "5 - I have only smoked a few times" = "5",
    "6 - I have never smoked" = "6"
  )

## Recoding dat$alcfreq
dat$alcfreq <- dat$alcfreq %>%
  as.character() %>%
  fct_recode(
    "1 - Every day" = "1",
    "2 - Several times a week" = "2",
    "3 - Once a week" = "3",
    "4 - 2-3 times a month" = "4",
    "5 - Once a month" = "5",
    "6 - Less than once a month" = "6",
    "7 - Never" = "7"
  )

## Recoding dat$alcbnge
dat$alcbnge <- dat$alcbnge %>%
  as.character() %>%
  fct_recode(
    NULL = "0",
    "1 - Daily or almost daily" = "1",
    "2 - Weekly" = "2",
    "3 - Monthly" = "3",
    "4 - Less than monthly" = "4",
    "5 - Never" = "5",
    "6 - Not applicable" = "6"
  )

## Recoding dat$medtrun
dat$medtrun <- dat$medtrun %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

# Recoding gndr
dat$gndr <- dat$gndr %>%
  as.character() %>%
  fct_recode(
    "Male" = "1",
    "Female" = "2"
  )

# Recoding rshpsts
dat$rshpsts <- dat$rshpsts %>%
  as.character() %>%
  fct_recode(
    "Legally married" = "1",
    "Living with my partner - not legally recognised" = "3",
    "Living with my partner - legally recognised" = "4",
    "Legally divorced/Civil union dissolved" = "6"
  )

## Recoding dat$ipstrgva
dat$ipstrgva <- dat$ipstrgva %>%
  as.character() %>%
  fct_recode(
    "1 - Very much like me" = "1",
    "2 - Like me" = "2",
    "3 - Somewhat like me" = "3",
    "4 - A little like me" = "4",
    "5 - Not like me" = "5",
    "6 - Not like me at all" = "6"
  )

# Recoding vacc19
dat$vacc19 <- dat$vacc19 %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

# save cleaned dataset
write.csv(dat, "data/ESS11_BE_data.csv", row.names = FALSE)
