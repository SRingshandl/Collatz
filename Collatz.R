#Collatz conjecture calculating function
collatz <- function(number){
  if(number %% 2 == 0){
    number <- number / 2
  } else{
    number <- number * 3 + 1
  }
  return(number)
}

#function necessary to read last written line. For starting after stopping
ReadLastLines <- function(x,n,...){    
  con <- file(x)
  open(con)
  out <- scan(con,n,what="char(0)",sep="\n",quiet=TRUE,...)
  
  while(TRUE){
    tmp <- scan(con,1,what="char(0)",sep="\n",quiet=TRUE)
    if(length(tmp)==0) {close(con) ; break }
    out <- c(out[-1],tmp)
  }
  out
}


#checks whether Output file exists, otherwise creates it
if(!file.exists("Collatz_Output.txt")){
  write(paste("Number", "Turns", sep = ", "),file="Collatz_Output.txt",append=TRUE)
}


#how many numbers should be added to the Output file;
#NA for start and end lets the scripts start at 1
#number_end is included in the output
turns <- 1000
number_start <- NA
number_end <- NA

#define range
last_value <- ReadLastLines("Collatz_Output.txt",1)[1]
last_value <- strsplit(last_value, ",")[[1]][1]
last_value <- as.numeric(strsplit(last_value, ",")[[1]][1])

if(!is.na(number_start)){
  minnumber <- number_start
} else if (is.na(last_value)){
  minnumber <- 1
} else {
  minnumber <- last_value + 1
}

if(!is.na(number_end)){
  maxnumber <- number_end + 1 #1 is necessary for correct turns
} else{
  maxnumber <- minnumber + turns
}


#run Collatz function the defined amount of turns
flag <- 0
counter <- 0

for(number in minnumber:(maxnumber-1)){
  if(number %% 1000 == 0){print(number)}
  current <- number
  while(flag == 0){
    number <- collatz(number)
    counter <- counter + 1
    if(number == 1){
      flag <- 1
      write(paste(current, counter, sep = ", "),file="Collatz_Output.txt",append=TRUE)
    }
  }
  counter <- 0
  flag <- 0
}
