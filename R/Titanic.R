all_data <- read.csv("titanic_reorganized.csv", head=TRUE, sep=",")


###################################################################
#QUESTION: Did the price of the ticket correlate with survival rate?
###################################################################

surv <- all_data[,"Survived"]
fare <- all_data[,"Fare"]

#use built-in functions to find max and min -> will need
#for data normalization
max <- max(fare, na.rm = TRUE)
min <- min (fare, na.rm = TRUE)

#function for data normalization
normalize <- function(x, min, max) {
    z <- (x - min) / (max - min)
    return(z)
}


n_fare <- c() #create new empty vector
for(i in fare) { #for loop to normalize data
    z <- normalize(i, min, max)
    n_fare <- c(n_fare, z) #fill empty vector with normalized values
}

cor1 <- cor(surv,n_fare) #use built-in function to check correlation coefficient

price_data <- data.frame(surv, n_fare)
colnames(price_data) <- c("Survived", "Price")

#write.csv(price_data, "Normalized-fare-data.csv", sep = " ", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names=TRUE)

write.csv(price_data, "Normalized-fare-data.csv", eol = "\n", na = "NA")

###################################################################
#QUESTION: Did the cabin number correlate with survival rate?
###################################################################

#function to compute total number of survived based on cabin group
survived <- function(x, y) {
    total <- 0
    for(i in 1:nrow(x)) {
        s <- x[i, 1]
        c <- x[i, 2]
        if(s == 1 && c == y) {
            total <- total + 1
        }
    }
    return(total)
}


cabin <- all_data[,"Cabin"] # create vector containing cabin info
#what is the classification issue?

c <- c() #declare empty vector
for(i in cabin) {
    if(grepl("A", i)) {
        i = 1
    }
    else if(grepl("B", i)) {
        i = 2
    }
    else if(grepl("C", i)) {
        i = 3
    }
    else if(grepl("D", i)) {
        i = 4
    }
    else if(grepl("E", i)) {
        i = 5
    }
    else if(grepl("F", i)) {
        i = 6
    }
    else if(grepl("G", i)) {
        i = 7
    }
    else { #handle NA
        i = 0
    }
    c <- c(c, i) #fill empty vector
}
cabin_data <- data.frame(surv, c)
colnames(cabin_data) <- c("Survived", "Cabin")
#look at totals survived
totalA <- survived(cabin_data, 1)
totalB <- survived(cabin_data, 2)
totalC <- survived(cabin_data, 3)
totalD <- survived(cabin_data, 4)
totalE <- survived(cabin_data, 5)
totalF <- survived(cabin_data, 6)
totalG <- survived(cabin_data, 7)

cabin_survived <- data.frame(c('A', 'B', 'C', 'D', 'E', 'F', 'G'),c(totalA, totalB, totalC, totalD, totalE, totalF, totalG))
colnames(cabin_survived) <- c("Cabin", "Total Survived")

###################################################################
#QUESTION: Did gender correlate with survival rate?
###################################################################
g <- c()
gender <- all_data[,"Sex"]
for(i in gender) {
    if(grepl("female", i)) {
        i = 1
    } else {
        i = 0
    }
    g <- c(g, i)
}
gender_data <- data.frame(all_data[,"Survived"], g)
colnames(gender_data) <- c("Survived", "Gender")

percent <- function(x, total) {
    p <- (x/total) * 100
    return(p)
}
surv_female <- 0
surv_male <- 0

for(i in 1:length(g)) {
    s <- as.integer(gender_data[i, "Survived"])
    g <- as.integer(gender_data[i, "Gender"])
    if(s == 1) {
        if(g == 1) {
            surv_female <- surv_female + 1;
        } else {
            surv_male <- surv_male + 1;
        }
    }
}

f <- percent(surv_female, length(all_data[,"Survived"]))
m <- percent(surv_male, length(all_data[,"Survived"]))

