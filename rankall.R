rankall <- function(outcome, num = "best"){
  
  if (num == "best"){
    num = 1
  }
  
  read_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  attack_data <- read_data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  names(attack_data) <- c("state", "hospital", "attack")
  
  failure_data <- read_data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  names(failure_data) <- c("state", "hospital", "failure")
  
  pneumonia_data <- read_data[c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  names(pneumonia_data) <- c("state", "hospital", "pneumonia")
  
  attack_clean <- na.omit(attack_data)
  failure_clean <- na.omit(failure_data)
  pneumonia_clean <- na.omit(pneumonia_data)
  
  attack_sort <- attack_clean[order(attack_clean$state, attack_clean$attack, attack_clean$hospital),]
  failure_sort <- failure_clean[order(failure_clean$state, failure_clean$failure, failure_clean$hospital),]
  pneumonia_sort <- pneumonia_clean[order(pneumonia_clean$state, pneumonia_clean$pneumonia, pneumonia_clean$hospital),]
  
  attack_split <- split(attack_sort, attack_sort$state)
  failure_split <- split(failure_sort, failure_sort$state)
  pneumonia_split <- split(pneumonia_sort, pneumonia_sort$state)
  
  if (outcome == "heart attack"){
    for (i in 1:50){
      z <- attack_split[[i]]
      if (num == "worst"){
        num = nrow(z)
      }
      print(z$hospital[num], z$i)
    }
  }
    
  else if (outcome == "heart failure"){
    for (i in 1:50){
      z <- failure_split[[i]]
      if (num == "worst"){
        num = nrow(z)
      }
      print(z$hospital[num], z$i)
    }  
  }
  else if (outcome == "pneumonia"){
    for (i in 1:50){
      z <- pneumonia_split[[i]]
      if (num == "worst"){
        num = nrow(z)
      }
      print(z$hospital[num], z$i)
    }
  }
  else {
    stop('invalid outcome')
  }
}