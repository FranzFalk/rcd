################################
#### R-Script by Franz Falk ####
########### 26-30-01 ########### 
################################

set.seed(1698)

wheel1 <- c(0, 16, 1000, 3000, 5000, 9800, 16000, 45000, "second wheel")

wheel2 <- c(
  "half a car",
  "back to first wheel",
  "flip a coiun stay or go back",
  "go to third wheel",
  "go back and double"
)

wheel3 <- rep(NA, 10)

coin <- c(1, 2)

regularsecondwheelspin <- function() {
  repeat {
    a <- sample(wheel2, 1)
    
    if (a == "half a car") {
      z <- NA
      
    } else if (a == "back to first wheel") {
      z <- sample(wheel1, 1)
      
    } else if (a == "flip a coiun stay or go back") {
      b <- sample(coin, 1)
      z <- if (b == 1) sample(wheel2, 1) else sample(wheel1, 1)
      
    } else if (a == "go to third wheel") {
      z <- sample(wheel3, 1)
      
    } else if (a == "go back and double") {
      b <- sample(wheel1, 1)
      z <- if (b == "second wheel") sample(wheel2, 1) else as.numeric(b) * 2
    }
    
    if (is.na(z) || is.numeric(z)) return(z)
  }
}

resolve_wheel1 <- function(x) {
  if (x == "second wheel") {
    regularsecondwheelspin()
  } else {
    as.numeric(x)
  }
}

coindesission <- function() {
  a_raw <- sample(wheel1, 1)
  b_raw <- sample(wheel1, 1)
  
  a <- resolve_wheel1(a_raw)
  b <- resolve_wheel1(b_raw)
  
  if (is.na(a) || is.na(b)) {
    NA
  } else {
    if (sample(coin, 1) == 1) min(a, b) else max(a, b)
  }
}

order <- coindesission()


order_stats <- replicate(160098, coindesission())

max(order_stats, na.rm = T)

mean(order_stats, na.rm = T)

summary(order_stats)

table(order_stats)

counts <- table(order_stats)

probabilities <- round((counts / 160098)*100,digits = 2) 

na.prob<-round(((160098-sum(counts))/160098)*100,digits = 2)

data.frame(
  NA.prob=na.prob
)

probabilities<-c(probabilities,na.prob)

names(probabilities)<-c(names(probabilities)[1:15],"NA")

probabilities
