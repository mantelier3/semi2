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
for (i in 11){
    # c(5)
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

count500 = 0
trials <- 500
loops <- 5
for (i in 1:loops){
    qmat500 <- qlearning(c(5), maxtrials = trials)
    if (sum(apply(qmat500,1,which.max) - apply(a,1,which.max)) == 0)
        count500 <- count500 + 1
}
count500 <- count500 / loops
count500


count = 0
trials <- 10
loops <- 10
hists <- array(0,c(loops,5))
for (i in 1:loops){
    qmat <- qlearning(c(5), maxtrials = trials, gamma=0.2)
    hists[i,] <- whichmax(qmat)
    if (sum(apply(qmat,1,which.max) - apply(a,1,which.max)) == 0)
        count <- count + 1
}
count <- count / loops
count

