# devices
printf <- function(...) invisible(print(sprintf(...)))

while (dev.cur() < 3){
    dev.new()
}
a <- array(0,dim=c(5,5))
a[1,2] <- 1
a[2,4] <- 1
a[3,3] <- 1
a[4,5] <- 1
a[5,5] <- 1

initConsts(numlanes=3, numcars=5)
STARTFUEL = 5000
# MINCARSPEED = 5
printState <<- F
count <<- 0
dontskip <<- F
start_time <- Sys.time()
for (i in 44){
    # c(4)
    # 1 - optimal lane is to the left, to the left
    # 2 - you're on it, keep going
    # 3 - optimal lane is to the right, to the right
    # 4 - you're not on the optimal lane, but the way is blocked, go slow
    # 5 - you're on the optiml lane, but the asshole in front of you doesn't know what
    # the gas pedal is, so you gotta go slow and wait it out
    qmat <- qlearning(c(5), maxtrials = i)
    printState <<- T
    simulation(qmat)
}
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)