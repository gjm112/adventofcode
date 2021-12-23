#Advent of Code 
##########################
#Day 1
##########################
##########################
#Part 1
##########################
input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day1_input.txt")
input <- input$V1

sum(diff(input) > 0)

##########################
#Part 2
##########################
ma3 <- c()
for (i in 1:(length(input)-2)){print(i)
ma3 <- c(ma3,sum(input[i:(i+2)]))
}

sum(diff(ma3) > 0)


##########################
#Day 2
##########################
  
##########################
#Part 1
##########################
input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day2_input.txt")

#Forward
#Horizontal
h <- sum(input$V2[input$V1 == "forward"])

#vertical
v <- sum(input$V2[input$V1 == "down"]) - sum(input$V2[input$V1 == "up"])


h*v

##########################
#Part 2
##########################

h <- d <- a <- 0
for (i in 1:nrow(input)){print(i)
  x <- input$V2[i]
  if (input$V1[i] == "forward"){
    h <- h + x
    d <- d + a*x
  }
  if (input$V1[i] == "up"){
    a <- a - x
  }
  if (input$V1[i] == "down"){
    a <- a + x
  }
}
  

h*d

##########################
#Day 3
##########################

##########################
#Part 1
##########################
mode <- function(x){
  if (sum(x) > 500) {
    return(1)
  } else {
    return(0)
  }
}
input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day3_input.txt", colClasses = "character")
mat <- matrix(NA, nrow = nrow(input), ncol = 12)
for (j in 1:12){
  mat[,j] <- as.numeric(substring(input$V1,j,j))
}

gamma <- apply(mat, 2, mode)
epsilon <- (!gamma) + 0
  
#Convert to decimal
g <- sum(gamma*(2^(11:0)))
e <- sum(epsilon*(2^(11:0)))


g*e

##########################
#Part 2
##########################
mode <- function(x){
  if (table(x)[1] > table(x)[2]) {
    return(0)
  } else {
    return(1)
  }
}

input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day3_input.txt", colClasses = "character")
mat <- matrix(NA, nrow = nrow(input), ncol = 12)
for (j in 1:12){
  mat[,j] <- as.numeric(substring(input$V1,j,j))
}

#Finding Oxygen generator
j <- 1
while (nrow(mat) > 1) {
  bit <- mode(mat[, j])
  ind <- which(mat[, j] == bit)
  mat <- mat[ind, ]
  j <- j + 1
}

oxy <- mat


input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day3_input.txt", colClasses = "character")
mat <- matrix(NA, nrow = nrow(input), ncol = 12)
for (j in 1:12){
  mat[,j] <- as.numeric(substring(input$V1,j,j))
}
#Finding CO2 scrubber generator
j <- 1
while (nrow(mat) > 1) {
  bit <- (!mode(mat[, j])) + 0
  ind <- which(mat[, j] == bit)
  mat <- mat[ind, ]
  j <- j + 1
}

co2 <- mat

co2 <- sum(co2*2^(11:0))
oxy <- sum(oxy*2^(11:0))

co2*oxy


##########################
#Day 4 - Bingo
##########################

##########################
#Part 1
##########################
draws <- c(84,28,29,75,58,71,26,6,73,74,41,39,87,37,16,79,55,60,62,80,64,95,46,15,5,47,2,35,32,78,89,90,96,33,4,69,42,30,54,85,65,83,44,63,20,17,66,81,67,77,36,68,82,93,10,25,9,34,24,72,91,88,11,38,3,45,14,56,22,61,97,27,12,48,18,1,31,98,86,19,99,92,8,43,52,23,21,0,7,50,57,70,49,13,51,40,76,94,53,59)

input <-
  read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day4_input.txt")

#Make a list of the bingo cards.
cards <- spots <- list()
ind <- seq(1, 500, 5)
for (i in 1:100) {
  cards[[i]] <- input[ind[i]:(ind[i] + 4), ]
  spots[[i]] <- matrix(0, ncol = 5, nrow = 5)
}

i <- 1
while (!any(unlist(lapply(spots, function(x){any(c(apply(x, 1, sum), apply(x, 2, sum)) == 5)})))) {
  #Now start calling numbers
  x <- draws[i]
  
  #does the card have the number
  #Update the spots
  for (j in 1:length(cards)) {
    spots[[j]][cards[[j]] == x] <- 1
  }
  
  i <- i + 1
}

#Which card is the winner?
winner <- which(unlist(lapply(spots, function(x){any(c(apply(x, 1, sum), apply(x, 2, sum)) == 5)})))

s <- sum(cards[[winner]][spots[[winner]] == 0])
d <- draws[i-1]

s*d

##########################
#Part 2
##########################
draws <- c(84,28,29,75,58,71,26,6,73,74,41,39,87,37,16,79,55,60,62,80,64,95,46,15,5,47,2,35,32,78,89,90,96,33,4,69,42,30,54,85,65,83,44,63,20,17,66,81,67,77,36,68,82,93,10,25,9,34,24,72,91,88,11,38,3,45,14,56,22,61,97,27,12,48,18,1,31,98,86,19,99,92,8,43,52,23,21,0,7,50,57,70,49,13,51,40,76,94,53,59)

input <-
  read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day4_input.txt")

#Make a list of the bingo cards.
cards <- spots <- list()
ind <- seq(1, 500, 5)
for (i in 1:100) {
  cards[[i]] <- input[ind[i]:(ind[i] + 4), ]
  spots[[i]] <- matrix(0, ncol = 5, nrow = 5)
}

i <- 1
live <- c(1:100)
dead <- c()
while (!all(unlist(lapply(spots, function(x){any(c(apply(x, 1, sum), apply(x, 2, sum)) == 5)})))) {
  #Now start calling numbers
  x <- draws[i]
  #does the card have the number
  #Update the spots
  for (j in live) {
    spots[[j]][cards[[j]] == x] <- 1
  }
  
  #Remove cards that have won already
  for (j in live) {
    if(any(c(apply(spots[[j]],1,sum),apply(spots[[j]],2,sum)) == 5)){
      dead <- c(dead,j)
      live <- setdiff(live,j)
    }
  }
  
  i <- i + 1
}




#Which card is the winner?
winner <- dead[100]

s <- sum(cards[[winner]][spots[[winner]] == 0])
d <- draws[i-1]

s*d


##########################
#Day 5 - Hydrothermal Venture
##########################

##########################
#Part 1
##########################
input <-
  read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day5_input.txt")

dat <-
  data.frame(x1 = as.numeric(do.call(rbind, strsplit(input$V1, ","))[, 1]),
             y1 = as.numeric(do.call(rbind, strsplit(input$V1, ","))[, 2]))

dat$x2 <- as.numeric(do.call(rbind, strsplit(input$V3, ","))[, 1])
dat$y2 <- as.numeric(do.call(rbind, strsplit(input$V3, ","))[, 2])

#Note: This is a better way to do it: 
#dat %>% mutate(across(where(is.character), as.numeric))

#find the vertical and horizontal lines
dat_sub <- subset(dat, x1 == x2 | y1 == y2)

mat <- matrix(0,ncol = 1000, nrow = 1000)

#First do vertical lines. 
for (i in 1:nrow(dat_sub)){
  if (dat_sub$x1[i] == dat_sub$x2[i]){
    rng <- sort(c(dat_sub$y1[i], dat_sub$y2[i]))
    mat[dat_sub$x1[i],rng[1]:rng[2]] <- mat[dat_sub$x1[i],rng[1]:rng[2]]  + 1
  }
  
  if (dat_sub$y1[i] == dat_sub$y2[i]){
    rng <- sort(c(dat_sub$x1[i], dat_sub$x2[i]))
    mat[rng[1]:rng[2],dat_sub$y1[i]] <- mat[rng[1]:rng[2],dat_sub$y1[i]] + 1
  }
  
}

sum(mat > 1)

##########################
#Part 2
##########################
input <-
  read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day5_input.txt")

dat <-
  data.frame(x1 = as.numeric(do.call(rbind, strsplit(input$V1, ","))[, 1]),
             y1 = as.numeric(do.call(rbind, strsplit(input$V1, ","))[, 2]))

dat$x2 <- as.numeric(do.call(rbind, strsplit(input$V3, ","))[, 1])
dat$y2 <- as.numeric(do.call(rbind, strsplit(input$V3, ","))[, 2])

#Note: This is a better way to do it: 
#dat %>% mutate(across(where(is.character), as.numeric))

mat <- matrix(0,ncol = 1000, nrow = 1000)


#First do vertical lines.
for (i in 1:nrow(dat)) {
  if (dat$x1[i] == dat$x2[i]) {
    rng <- sort(c(dat$y1[i], dat$y2[i]))
    mat[dat$x1[i], rng[1]:rng[2]] <-
      mat[dat$x1[i], rng[1]:rng[2]]  + 1
  } else if (dat$y1[i] == dat$y2[i]) {
    rng <- sort(c(dat$x1[i], dat$x2[i]))
    mat[rng[1]:rng[2], dat$y1[i]] <-
      mat[rng[1]:rng[2], dat$y1[i]] + 1
  } else {
    xrng <- c(dat$x1[i]: dat$x2[i])
    yrng <- c(dat$y1[i]: dat$y2[i])
    for (q in 1:length(xrng)){
      mat[xrng[q],yrng[q]] <- mat[xrng[q],yrng[q]] + 1
    }
      
  }
  
}

sum(mat > 1)



##########################
#Day 6 - Lantern Fish
##########################

##########################
#Part 1
##########################
lantern <- c(3,5,4,1,2,1,5,5,1,1,1,1,4,1,4,5,4,5,1,3,1,1,1,4,1,1,3,1,1,5,3,1,1,3,1,3,1,1,1,4,1,2,5,3,1,4,2,3,1,1,2,1,1,1,4,1,1,1,1,2,1,1,1,3,1,1,4,1,4,1,5,1,4,2,1,1,5,4,4,4,1,4,1,1,1,1,3,1,5,1,4,5,3,1,4,1,5,2,2,5,1,3,2,2,5,4,2,3,4,1,2,1,1,2,1,1,5,4,1,1,1,1,3,1,5,4,1,5,1,1,4,3,4,3,1,5,1,1,2,1,1,5,3,1,1,1,1,1,5,1,1,1,1,1,1,1,2,2,5,5,1,2,1,2,1,1,5,1,3,1,5,2,1,4,1,5,3,1,1,1,2,1,3,1,4,4,1,1,5,1,1,4,1,4,2,3,5,2,5,1,3,1,2,1,4,1,1,1,1,2,1,4,1,3,4,1,1,1,1,1,1,1,2,1,5,1,1,1,1,2,3,1,1,2,3,1,1,3,1,1,3,1,3,1,3,3,1,1,2,1,3,2,3,1,1,3,5,1,1,5,5,1,2,1,2,2,1,1,1,5,3,1,1,3,5,1,3,1,5,3,4,2,3,2,1,3,1,1,3,4,2,1,1,3,1,1,1,1,1,1)
lantern <- 5
days <- list()
days[[1]] <-  lantern - 1

for (i in 2:30){print(i)
  temp <- days[[i - 1]] - 1
  temp[temp < 0] <- 6
  days[[i]] <- c(temp,rep(8,sum(days[[i - 1]] == 0)))
}

length(days[[80]])

diff(unlist(lapply(days, length)))

plot()
unlist(lapply(days, length))[-1]/unlist(lapply(days, length))[-120]
##########################
#Part 2
##########################
#how many lantern fish does this one produce
lantern <- c(3,5,4,1,2,1,5,5,1,1,1,1,4,1,4,5,4,5,1,3,1,1,1,4,1,1,3,1,1,5,3,1,1,3,1,3,1,1,1,4,1,2,5,3,1,4,2,3,1,1,2,1,1,1,4,1,1,1,1,2,1,1,1,3,1,1,4,1,4,1,5,1,4,2,1,1,5,4,4,4,1,4,1,1,1,1,3,1,5,1,4,5,3,1,4,1,5,2,2,5,1,3,2,2,5,4,2,3,4,1,2,1,1,2,1,1,5,4,1,1,1,1,3,1,5,4,1,5,1,1,4,3,4,3,1,5,1,1,2,1,1,5,3,1,1,1,1,1,5,1,1,1,1,1,1,1,2,2,5,5,1,2,1,2,1,1,5,1,3,1,5,2,1,4,1,5,3,1,1,1,2,1,3,1,4,4,1,1,5,1,1,4,1,4,2,3,5,2,5,1,3,1,2,1,4,1,1,1,1,2,1,4,1,3,4,1,1,1,1,1,1,1,2,1,5,1,1,1,1,2,3,1,1,2,3,1,1,3,1,1,3,1,3,1,3,3,1,1,2,1,3,2,3,1,1,3,5,1,1,5,5,1,2,1,2,2,1,1,1,5,3,1,1,3,5,1,3,1,5,3,4,2,3,2,1,3,1,1,3,4,2,1,1,3,1,1,1,1,1,1)
lantern <- c(8)

yesterday <-  lantern - 1

for (i in 2:256){print(i)
  temp <- yesterday - 1
  temp[temp < 0] <- 6
  today <- c(temp,rep(8,sum(yesterday == 0)))
  yesterday <- today
}
length(today)

days <- 256
sum(c(1:0,rep(6:0,100))[2:(256 + 1)] == 0) 
vec <- 256 - (which(c(3:0,rep(6:0,100))[2:(days + 1)] == 0) + 1)

offspring <- function(days){
  sum(c(8:0,rep(6:0,100))[2:(days + 1)] == 0)
}
ind <- which(c(8:0,rep(6:0,100))[2:(256 + 1)]==0)
sum(c(8:0,rep(6:0,100))[2:(256 + 1)]==0)
offspring <- Vectorize(offspring)
offspring(ind)

sum(choose(36,1:36))


lantern <- c(3,5,4,1,2,1,5,5,1,1,1,1,4,1,4,5,4,5,1,3,1,1,1,4,1,1,3,1,1,5,3,1,1,3,1,3,1,1,1,4,1,2,5,3,1,4,2,3,1,1,2,1,1,1,4,1,1,1,1,2,1,1,1,3,1,1,4,1,4,1,5,1,4,2,1,1,5,4,4,4,1,4,1,1,1,1,3,1,5,1,4,5,3,1,4,1,5,2,2,5,1,3,2,2,5,4,2,3,4,1,2,1,1,2,1,1,5,4,1,1,1,1,3,1,5,4,1,5,1,1,4,3,4,3,1,5,1,1,2,1,1,5,3,1,1,1,1,1,5,1,1,1,1,1,1,1,2,2,5,5,1,2,1,2,1,1,5,1,3,1,5,2,1,4,1,5,3,1,1,1,2,1,3,1,4,4,1,1,5,1,1,4,1,4,2,3,5,2,5,1,3,1,2,1,4,1,1,1,1,2,1,4,1,3,4,1,1,1,1,1,1,1,2,1,5,1,1,1,1,2,3,1,1,2,3,1,1,3,1,1,3,1,3,1,3,3,1,1,2,1,3,2,3,1,1,3,5,1,1,5,5,1,2,1,2,2,1,1,1,5,3,1,1,3,5,1,3,1,5,3,4,2,3,2,1,3,1,1,3,4,2,1,1,3,1,1,1,1,1,1)
table(lantern)

35*sum(choose(36,1:36)) + 265*sum(choose(37,1:37))


lantern <- 5
days <- list()
days[[1]] <-  lantern - 1

for (i in 2:150){print(i)
  temp <- days[[i - 1]] - 1
  temp[temp < 0] <- 6
  days[[i]] <- c(temp,rep(8,sum(days[[i - 1]] == 0)))
}
sum(c(5:0,rep(6:0,100))[2:(150 + 1)]==0)
sum(choose(19,1:19))
length(days[[150]])


##########################
#Day 7 - The Treachery of Whales
##########################

##########################
#Part 1
##########################
crabs <- c(1101,1,29,67,1102,0,1,65,1008,65,35,66,1005,66,28,1,67,65,20,4,0,1001,65,1,65,1106,0,8,99,35,67,101,99,105,32,110,39,101,115,116,32,112,97,115,32,117,110,101,32,105,110,116,99,111,100,101,32,112,114,111,103,114,97,109,10,478,1187,253,1892,900,155,20,787,17,248,1397,407,167,686,638,1020,960,124,840,220,1824,700,373,4,551,229,294,567,254,350,1144,679,124,361,145,483,335,202,1334,367,60,870,11,557,482,645,672,1296,1538,427,78,542,1135,13,65,0,140,705,13,642,187,1085,36,1118,349,601,382,584,941,26,949,200,763,198,430,204,1352,1135,210,342,11,1089,830,1523,9,523,167,762,254,805,8,132,29,102,1299,936,756,59,134,183,235,316,139,48,182,44,88,213,113,93,169,565,601,1899,1191,189,796,770,32,1183,365,374,867,918,1084,86,75,20,47,99,1140,2,99,1024,366,455,752,556,1220,66,326,450,213,1,342,756,49,675,160,280,68,221,193,379,88,179,94,16,109,570,1145,1207,824,355,1389,1601,168,86,236,923,120,759,14,478,460,84,167,1723,1005,269,6,171,861,311,832,952,701,3,1598,1466,96,780,57,161,631,572,276,105,594,276,17,405,688,1444,173,23,199,177,689,19,565,472,151,986,76,379,1430,212,928,106,25,143,84,833,942,860,1555,271,239,720,596,1209,235,535,361,1794,79,283,275,17,342,1687,1434,173,967,740,217,1370,18,1579,1259,546,94,623,475,834,1000,456,101,520,120,1023,360,167,213,617,42,1149,629,760,17,33,27,1347,414,646,1116,1340,134,259,143,407,249,328,968,677,241,438,98,313,27,791,1,634,3,918,1482,213,123,444,45,24,26,26,1203,64,67,1562,1,4,298,12,384,32,443,37,268,674,356,202,286,694,272,163,950,1022,54,59,21,73,519,462,106,76,1112,10,72,388,194,6,120,9,645,209,1121,75,599,362,661,439,69,62,339,390,23,1247,365,1266,4,246,511,47,467,134,276,497,130,458,427,669,1191,701,917,168,1191,294,641,236,801,375,106,872,800,87,356,583,1096,253,459,951,1331,719,66,1091,525,15,370,290,141,1201,30,43,37,76,1131,616,297,172,402,1016,654,301,63,872,303,69,1195,502,351,52,1659,86,104,294,807,166,120,190,333,60,283,819,198,184,144,278,343,1395,496,103,705,485,172,642,225,181,583,188,38,436,801,91,5,634,180,28,20,146,488,676,121,420,965,220,1564,1011,241,423,3,1631,709,106,725,164,1032,65,205,503,188,397,1072,49,121,761,721,249,418,87,126,258,712,500,435,157,127,681,108,270,647,504,505,83,407,212,165,1177,160,715,1292,491,195,141,25,829,1316,242,754,364,1707,33,594,434,488,368,298,183,1156,29,1674,537,378,8,9,860,240,571,749,471,331,501,156,62,427,1103,52,12,832,1198,284,388,827,556,194,288,218,397,84,1485,95,401,739,986,994,305,668,1324,1437,312,993,15,822,923,707,135,42,423,37,1183,1344,997,19,699,395,119,7,168,1711,50,151,38,20,163,686,1364,21,24,411,32,335,188,55,628,274,1766,439,180,286,1024,87,15,1498,290,561,971,32,294,67,113,219,42,18,715,3,664,242,583,221,1045,236,74,46,1612,639,325,164,100,69,518,38,502,26,329,112,1174,127,124,90,144,527,468,152,1098,800,125,349,191,290,191,27,651,446,267,9,1304,269,586,64,983,152,236,512,8,248,177,109,311,957,47,126,69,13,709,204,381,1151,580,340,994,865,258,190,9,1149,930,1128,321,100,471,0,507,1308,326,585,813,1088,76,174,333,387,631,186,430,988,24,820,11,45,173,167,1494,98,1467,456,167,21,1363,1173,394,318,1601,1111,1249,757,282,672,1227,1214,277,336,815,136,1192,681,689,431,130,1488,154,465,14,709,339,1123,68,151,1280,143,1797,23,250,1231,1007,302,1103,2,585,552,1732,994,225,771,1495,82,229,700,910,15,38,159,1122,316,1044,711,1436,920,1722,523,1398,188,443,1032,93,33,397,272,187,24,489,53,79,1277,671,1094,68,1705,984,1096,512,145,389,167,161,1174,94,4,534,1295,648,75,24,366,995,175,220,714,843,412,267,634,1209,66,1094,125,822,1114,1513,694,1520,30,676,817,245,26,77,1146,552,143,165,39,343,971,87,0,90,1434,588,616,99,297,1034,114,5,702,917,582,733,31,54,820,0,212,192,282,33,639,1661,460,75,680,115,178,194,271,274,582,1008,89,139,611,707,0,376,65,9,161,135,40,134,566,66,601,95,817,745,202,352,447,322,842,6,1247,175,468,330,608,368,139,21,29,486,121,9,1293,298,73,328,302,145,889,1794,677,56,952,520,80)
sum(abs(crabs - median(crabs)))
           
##########################
#Part 2
##########################
tri <- function(x){
  return(sum(1:x))
}
tri <- Vectorize(tri)
results <- rep(NA, 1000)
for (i in 1:1900){print(i)
  results[i] <- sum(tri(abs(crabs - i)))
}

which.min(results)
results[464]           

                   
##########################
#Day 8 - Seven Segment Search
##########################

##########################
#Part 1
##########################
input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day8_input.txt")

patterns <- input[,1:10]
output <- input[,12:15]


a <- sum(nchar(output$V12) %in% c(2,3,4,7))
b <- sum(nchar(output$V13) %in% c(2,3,4,7))
c <- sum(nchar(output$V14) %in% c(2,3,4,7))
d <- sum(nchar(output$V15) %in% c(2,3,4,7))

a + b + c + d

##########################
#Part 2
##########################
library(tidyverse)
input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day8_input.txt")

lll <- list()
code <- patterns[1,]
for (i in 1:10){
  lll[[i]] <- rep(0,7)
  for (j in 1:7){
  if (grepl(letters[j],code[i])){lll[[i]][j] <- 1}
  }
}

greg <- do.call(rbind,lll)
row <- apply(greg,1,sum)
col <- apply(greg,2,sum)

greg_correct <- matrix(NA, ncol = 7, nrow = 10)
greg_correct[,5] <- greg[,col == 4]
greg_correct[,6] <- greg[,col == 9]
greg_correct[,2] <- greg[,col == 6]

#one
greg_correct[row == 2,] <- c(0,0,1,0,0,1,0)
#seven
greg_correct[row == 3,] <- c(1,0,1,0,0,1,0)
#four
greg_correct[row == 4,] <- c(0,1,1,1,0,1,0)
#eight
greg_correct[row == 7,] <- c(1,1,1,1,1,1,1)
#Three
greg_correct[greg_correct[,2]==0 & greg_correct[,5]==0 & greg_correct[,6]==1 & is.na(greg_correct[,c(1)]),] <- c(1,0,1,1,0,1,1)





col <- apply(greg,2,sum)


##########################
#Part 2 - Attempt 2
##########################

library(tidyverse)
input <- read.table("/Users/gregorymatthews/Dropbox/statsinthewild/twitch/Day8_input.txt")
output_number <- rep(NA,nrow(input))

for (k in 1:nrow(input)){print(k)
lll <- list()
code <- patterns[k,]
for (i in 1:10){
  lll[[i]] <- rep(0,7)
  for (j in 1:7){
    if (grepl(letters[j],code[i])){lll[[i]][j] <- 1}
  }
}

greg <- do.call(rbind,lll)
row <- apply(greg,1,sum)
col <- apply(greg,2,sum)


tar <- list()
target <- output[k,]
for (i in 1:4){
  tar[[i]] <- rep(0,7)
  for (j in 1:7){
    if (grepl(letters[j],target[i])){tar[[i]][j] <- 1}
  }
}


greg <- do.call(rbind,lll)
decode_this <- do.call(rbind,tar)
#Take each row of decode_this and find out what row it matches to in greg
#Then take that matched index and pull out the number in key and you will have the number.
#Now just fill in the rest of key.  
row <- apply(greg,1,sum)
col <- apply(greg,2,sum)

key <- rep(NA,10)
key[row == 2] <- 1
key[row == 3] <- 7
key[row == 4] <- 4
key[row == 7] <- 8

key_col <- rep(NA,7)
key_col[col == 9] <- "f"
key_col[col == 4] <- "e"
key_col[col == 6] <- "b"
#find the 3
temp <- apply(cbind(greg[,which(key_col == "b")] == 0,greg[,which(key_col == "e")] == 0,greg[,which(key_col == "f")] == 1), 1, sum)
ind <- which(temp == 3 & !key %in% c(1,4,7,8))
key[ind] <- 3

#find the 5
temp <- apply(cbind(greg[,which(key_col == "b")] == 0,greg[,which(key_col == "e")] == 1,greg[,which(key_col == "f")] == 0), 1, sum)
ind <- which(temp == 3 & !key %in% c(1,4,7,8))
key[ind] <- 5

#find the 2
key[which(row == 5 & is.na(key))] <- 2

#find the 9
temp <- apply(cbind(greg[,which(key_col == "b")] == 1,greg[,which(key_col == "e")] == 0,greg[,which(key_col == "f")] == 1), 1, sum)
ind <- which(temp == 3 & !key %in% c(1:5,7,8))
key[ind] <- 9

#Now find 6
ids <- which(apply(greg[is.na(key),],2,sum) == 1)
sixorzero <- apply(greg[!is.na(key),],2,sum)[ids]
id <- ids[sixorzero == 6]
loc <- which(is.na(key) & greg[,id] == 1)
key[loc] <- 4
#Finally find the 0
key[is.na(key)] <- 0

#Now convert output to numbers
a <- key[which(apply(greg,1,function(x){all(x == decode_this[1,])}))]
b <- key[which(apply(greg,1,function(x){all(x == decode_this[2,])}))]
c <- key[which(apply(greg,1,function(x){all(x == decode_this[3,])}))]
d <- key[which(apply(greg,1,function(x){all(x == decode_this[4,])}))]

output_number[k] <- sum(c(a,b,c,d) * c(1000,100,10,1))
}

sum(output_number)




