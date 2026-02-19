sleep1 <- function(x, y) {
  x + y
  #Sys.sleep(x)
  sleep2(y)
}

sleep2 <- function(y) {
  force(y)
  a <- 1 + 2
  #sleep3(y)
  y
}

sleep3 <- function(y) {
  #Sys.sleep(y)
  y
}

# rig_in_namespace(sleep1, sleep2)

#sleep1 <- rig(sleep1)
#sleep2 <- rig(sleep2)
#sleep3 <- rig(sleep3)
#
#options(boomer.clock = TRUE)
#
#sleep1(1,2)
#globals$times

