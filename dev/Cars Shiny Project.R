library(dplyr)
library(stringr)
carsOrig <- read.csv("vehicles.csv", header = TRUE, stringsAsFactors = FALSE)
carsDat <- carsOrig %>% transmute(year = year,
                                  make = make,
                                  model = model,
                                  mod = model,
                                  mpg = comb08,
                                  mpgE = combE,
                                  class = as.factor(VClass),
                                  cyl = cylinders,
                                  tran = str_extract(carsOrig$trany, "(Automatic|Manual)"),
                                  gear = str_extract(carsOrig$trany, "\\d+|(variable)"),                                disp = displ,
                                  drive = drive,
                                  alt = as.factor(atvType),
                                  fuel = as.factor(fuelType),
                                  co2 = co2TailpipeGpm,
                                  turbo = ifelse(is.na(tCharger), FALSE, tCharger),
                                  super = ifelse(sCharger == "S", TRUE, FALSE),
                                  guzz = ifelse(guzzler == "", FALSE, TRUE)
        )
## Locate and filter for only larger companies (makes > 40)
bigCo <- carsDat %>% 
        group_by(make) %>% 
        summarise(tot = n()) %>% 
        filter(tot > 40) %>%
        select(make) %>%
        arrange(make) %>%
        unlist(use.names = FALSE)

cars <- carsDat %>% filter(make %in% bigCo)


peek <- function(s) {
        cars <- mutate(cars, model = str_replace_all(str_trim(model), " +", " "))
        mdl <- ifelse(cars$mod != cars$model, 
                      paste(cars$mod, cars$model, sep = " --> "),
                      cars$model)
        set <- str_detect(cars$mod, s)
        sort(unique(sample(mdl[set], 20)))
        }


# Drive - AWD, 4WD, etc
# Find drive-like text and move to a new column
# 
driveWds <- c("4x4", "All-Trac", "Syncro", ".WD", "quattro", "xDrive", "All4", "4Matic", "4Motion")
w <- paste0("\\b", driveWds, "\\b", collapse = "|")
wd <- str_extract_all(cars$model, regex(w, ignore_case = TRUE), simplify = TRUE)
dim(wd)
wd[wd[ , 2] != "" & wd[ , 2] != wd[ , 1], 1:2]
cars <- mutate(cars, drv = wd[ , 1])
table(cars$drv)
cars <- mutate(cars, model = str_replace_all(model, regex(w, ignore_case = TRUE), ""))
peek(w)


# Deal with hybrid/electric words
# 
elecWds <- c("e-tron", "BEV", "REX", "EV", "Electric", "ActiveHybrid", "Hybrid", "E-Hybrid", 
            "(Energi)* Plug-in Hybrid")
el <- paste0("\\b", elecWds, "\\b", collapse = "|")
elc <- str_extract_all(cars$model, regex(el, ignore_case = TRUE), simplify = TRUE)
dim(elc) 
elc[elc[ , 2] != "", 1:2]
table(elc)
cars <- mutate(cars, elec = elc[ , 1])
table(cars$elec)
cars <- mutate(cars, model = str_replace_all(model, regex(el, ignore_case = TRUE), "")) # Strip all elec text
peek(el) # Take a look


# Take care of engine size tags 
# 
ltr <- "(\\d+\\.\\d+) *(l|liter|litre|.*)\\b"
sz <- str_match(cars$model, regex(ltr, ignore_case = TRUE))
unique(sz[!is.na(sz[ , 1]), ])
cars <- mutate(cars, liters = sz[ , 2])
cars <- mutate(cars, model = str_replace_all(model, "\\d+\\.\\d+ *", " "))
peek(ltr)



# Engine words
engWds <- c("V6", "V8", "V12", "16-Valve", "Turbo", "Twin-Turbo")
e <- paste0("\\b", engWds, "\\b", collapse = "|")
eng <- str_extract_all(cars$model, regex(e, ignore_case = TRUE), simplify = TRUE)
dim(eng)
eng[eng[ , 2] != "", 1:2]
table(eng[ , 1])
cars <- mutate(cars, engine = eng[ , 1])
cars <- mutate(cars, model = str_replace_all(model, e, ""))
peek(e)



# Deal with fuel words
# 
fuelWds <- c("Bluetec", "CNG", "FFV", "Natural Gas", "LPG", "Bi-*fuel", "Dual-fuel")
af <- paste0("\\b", fuelWds, "\\b", collapse = "|")
afl <- str_extract_all(cars$model, regex(af, ignore_case = TRUE), simplify = TRUE)
dim(afl) # Some have 2
afl[afl[ , 2] != "" & afl[ , 2] != afl[ , 1], 1:2]
afl <- paste(afl[ , 1], afl[ , 2])
table(afl)
cars <- mutate(cars, altfuel = afl)
table(cars$altfuel)
cars <- mutate(cars, model = str_replace_all(model, regex(af, ignore_case = TRUE), ""))
peek(af) # Take a look

# Pull out battery info
# 
b <- "\\b(\\d+).*[Bb]attery"
bat <- str_match(cars$model, b)
unique(bat[ , 2])
cars <- mutate(cars, battery = ifelse(is.na(bat[ , 2]), "", bat[ , 2]))
table(cars$battery)
cars <- mutate(cars, model = str_replace_all(model, b, "")) # Strip all battery text
peek(b)


# Deal with door words in model variable
# 
# d <- "([2-5])[ -]*([Dd]r|[Dd]oor)"
d <- regex("([2-5])[ -]*(dr|door)s*", ignore_case = TRUE)
do <- str_match(cars$model, d)
dim(do)
unique(do[!is.na(do[ , 1]), 1:3])
cars <- mutate(cars, doors = ifelse(is.na(do[ , 2]), "", paste0(do[ , 2], "-Door")))
table(cars$doors)
cars <- mutate(cars, model = str_replace_all(model, d, ""))
peek(d)

# Deal with RS as a model not a modifier
# This allows other RS tags to be handled in their group
rs <- "(^RS) *(\\d)\\b|(\\bRS\\b)"
rsAudi <- str_match(cars$model, rs)
dim(rsAudi)
unique(rsAudi[!is.na(rsAudi[ , 2]), ])
unique(cars[!is.na(rsAudi[ , 2]), "model"])
cars <- mutate(cars, model = str_replace_all(model, "(^RS) *(\\d)\\b", paste0(rsAudi[ , 2], rsAudi[ , 3])))
peek(rs)

modWds <- c("Nismo", "Type-R", "Kompressor,", "RS", "AMG", "Active Drive II", "Eco", 
            "LWB", "GLI", "Ext", "HF")
md <- paste0("\\b", modWds, "\\b", collapse = "|")
modi <- str_extract_all(cars$model, regex(md, ignore_case = TRUE), simplify = TRUE)
dim(modi)
modi[modi[ , 2] != "", 1:2]
cars <- mutate(cars, modify = str_trim(paste(modi[ , 1], modi[ , 2])))
table(cars$modify)
cars <- mutate(cars, model = str_replace_all(model, regex(md, ignore_case = TRUE), ""))
peek(md) # Take a look

formatWds <- c("Pickup", "Avant", "Hatchback", "Coupe", "Crosstour", 
               "Van")
f <- paste0("\\b", formatWds, "\\b", collapse = "|")
frm <- str_extract_all(cars$model, regex(f, ignore_case = TRUE), simplify = TRUE)
dim(frm)
frm[frm[ , 2] != "", 1:2]
cars <- mutate(cars, format = frm[ , 1])
table(cars$format)
cars <- mutate(cars, model = str_replace_all(model, regex(f, ignore_case = TRUE), ""))
peek(f) # Take a look

dropWds <- c("A/C", "liter", "litre")
dp <- paste0("\\b", dropWds, "\\b", collapse = "|")
drop <- str_extract_all(cars$model, regex(dp, ignore_case = TRUE), simplify = TRUE)
dim(drop)
table(drop[ , 1])
cars <- mutate(cars, model = str_replace_all(model, regex(dp, ignore_case = TRUE), ""))
peek(dp)


# Deal with "S" as the model, not a modifier
# 
# Tesla Model S


styleWds <- c("SQ4", "Executive", "HC", "PI", "L", "i", "si", "is", "XE", "SLC", "Series", 
              "ZR2", "LT", "GT", "DL", "Panel", "Classic", "(super)*sporty*s*(Combi)*", 
              "Gran( Turismo)", "XL7", "FE", "NX", "SX", "XL", "Plus", "CS", "Cab[ /]Chassis", 
              "Roadster", "Chassis", "Limited", "Double Cab", "Plus", "Prime", "Camper", "16v", 
              "Dune", "New", "Cabriolet", "Convertible", "Red", "Soft-top", "Hardtop",
              "Cargo", "Passenger", "LE", "FX", "XLE", "XFE", "XSE", "C-HR", "iA", "iM", 
              "EZ", "GL", "III", "SE", "Kit",  "Touring", "Inc")
st <- paste0("\\b", styleWds, "\\b", collapse = "|")
sty <- str_extract_all(cars$model, regex(st, ignore_case = TRUE), simplify = TRUE)
dim(sty)
sty[sty[ , 3] != "", 1:3]
qunique(sty[sty[ , 2] != "" & sty[ , 2] != sty[ , 1], 1:2])
sty <- str_trim(paste(sty[ , 1], sty[ , 2], sty[ , 3]))
table(sty)
cars <- mutate(cars, style = sty)
table(cars$style)
cars <- mutate(cars, model = str_replace_all(model, regex(st, ignore_case = TRUE), "")) 
peek(st) # Take a look


# Looking at dashes
# Keep those not adjoined to spaces
dash <- str_detect(cars$model, " -|- ")
table(cars[dash, "model"])
cars <- mutate(cars, model = str_replace(model, " -|- ", " "))
peek(" -|- ")

# Find parentheses text and extract it to explore
p <- "\\(.*\\)" # Regex for () and all between
parens <- str_extract_all(cars$model, p, simplify = TRUE)
unique(parens[parens != ""]) # Take a look, none critical
cars <- mutate(cars, model = str_replace(model, p, ""))
peek(p)

# Looking at slashes
slash <- str_extract_all(cars$model, "\\b.*/.*", simplify = TRUE)
sl <- str_detect(cars$model, "/")
table(cars[sl, "model"])
sample(slash[slash != ""], 20)
unique(slash)
cars <- mutate(cars, model = str_replace(model, "/.*", ""))
sum(str_detect(cars$model, "/")) # Check



# Deal with fuel variable
table(cars$fuel)
cars %>% group_by(fuel) %>% summarize(tot = n()) %>% arrange(desc(tot))
pro <- cars %>% filter(str_detect(fuel, "propane"))
unique(pro$model)
el <- cars %>% filter(fuel == "Premium and Electricity")
hy <- cars %>% filter(str_detect(model, "Prius"))
sort(unique(hy$fuel))


# Later
cars <- mutate(cars, fullName = paste(year, make, model))
badges <- function(x) {
        tbl <- cars %>% filter(grepl(x, fullName)) %>%
                group_by(fullName) %>%
                summarize(tot = n()) %>%
                arrange(desc(fullName))
        print(tbl, n = 1000)
}
info <- function(x) {
        cars %>% filter(grepl(x, fullName)) %>%
                select(year, fullName, tran, fuel, turbo) %>%
                group_by(year) %>%
                arrange(desc(year))
}

# Matrix method to try later
# Separate words out and deal with them in columns
cars <- mutate(cars, model = str_replace_all(str_trim(model), "  *", " "))
sample(cars$model, 40)
modM <- str_split(cars$model, " ", simplify = TRUE)
dim(modM)
wdC <- apply(modM, 2, function(x) sum(x != ""))
wdU <- apply(modM, 2, function(x) length(unique(x)))
wdList <- apply(modM, 2, unique)
sort(unique(modM[ , 4]))
