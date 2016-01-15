# devices
printf <- function(...) invisible(print(sprintf(...)))

while (dev.cur() < 3){
    dev.new()
}
a <- array(c(0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1),dim=c(4,5))
initConsts(numlanes=3, numcars=5)
STARTFUEL = 5000
# MINCARSPEED = 5
printState <<- F
count <<- 0
dontskip <<- F
start_time <- Sys.time()
for (i in 22){
    # qmat <- qlearning(c(rep(NUMLANES+2, NUMLANES), 2, 2, 2, NUMLANES, 5, 2), maxtrials = i)
    # c(2,2,2,3,2)
    # is something crashable to the left, 1=no, 2=yes
    # is something crashable to the right, 1=no, 2=yes
    # is something crashable in front, 1=no, 2=yes
    # where is the optimal lane - 1=to the left, 2=you're on it, 3=to the right
    # is my speed max possible for situation - 1=no, 2=yes
    qmat <- qlearning(c(4), maxtrials = i)
    printState <<- T
    simulation(qmat)
}
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)