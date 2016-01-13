rm(list=ls())
source("simulation.R")

# return distance of object in collision course
yDistances <- function(so) {
  list( cars = so$ybottomright[which(so$type == "car")]
      , fuel = so$ybottomright[which(so$type == "fuel")])
}

# get lane positions of objects
lanePos <- function(so, i)
{
  origin <- so[so$type == "leftside", "xtopleft"]
  ls     <- so[i, "xtopleft"] - origin
  rs     <- so[i, "xbottomright"] - origin

  lcoord <- ls %/% (LANEWIDTH + 1)
  rcoord <- rs %/% (LANEWIDTH + 1)

  unique(c(lcoord, rcoord)) + 1
}

# is object i in collision course with car?
collisionCourse <- function(so, i)
{
  if (isOverlapped(so[1, "xtopleft"],
                   so[1, "ytopleft"] + Inf,
                   so[1, "xbottomright"],
                   so[1, "ytopleft"],
                   so[i, "xtopleft"],
                   so[i, "ytopleft"],
                   so[i, "xbottomright"],
                   so[i, "ybottomright"]))
    1
  else
    0
}

getStateDesc <- function(so)
{
    #so <<- so
    so[so$type=="leftside", "ytopleft"]      <- so[1, "ytopleft"]
    so[so$type=="rightside", "ytopleft"]     <- so[1, "ytopleft"]
    so[so$type=="leftside", "ybottomright"]  <- so[1, "ybottomright"]
    so[so$type=="rightside", "ybottomright"] <- so[1, "ybottomright"]

    state <- rep(1, NUMLANES + 6)

    names(state) <- c( paste("lane", 1:NUMLANES)
                     , "left"
                     , "right"
                     , "front"
                     , "mycarpos"
                     , "mycarspeed"
                     , "mycarofflane")

    so$lane <- rep(0, nrow(so))

    for (i in 1:nrow(so)) {
      so$lane[i] <- lanePos(so, i)[1]
    }

    lanes  <- rep(Inf, NUMLANES)
    so.car <- so[order(so$ybottomright), ]
    sel    <- which(so.car$type == "car")

    for (i in sel) {
        index <- so.car[i, "lane"]
        if (is.infinite(lanes[index])) {
          lanes[index] <- so.car$ybottomright[i]
        }
    }

    lanes <- order(lanes, decreasing=F)

    sel <- which( so$type == "car"
                | so$type == "leftside" 
                | so$type == "rightside")


    for (i in sel)
    {
        if (isOverlapped( so[1, "xtopleft"], 
                          so[1, "ytopleft"] + CARLENGTH,
                          so[1, "xbottomright"],
                          so[1, "ytopleft"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["front"] <- 2

        if (isOverlapped( so[1, "xtopleft"] - CARWIDTH, 
                          so[1, "ytopleft"],
                          so[1, "xtopleft"],
                          so[1, "ybottomright"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["left"] <- 2

        if (isOverlapped( so[1, "xbottomright"], 
                          so[1, "ytopleft"],
                          so[1, "xbottomright"] + CARWIDTH,
                          so[1, "ybottomright"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["right"] <- 2
    }

    state[1:NUMLANES] <- lanes

    carpos <- lanePos(so, 1)
    state["mycarpos"]     <- carpos[1]
    state["mycarofflane"] <- length(carpos)
    state["mycarspeed"]   <- so[1, "speed"]

    #print(state)
    #print(length(state))
    print(so)
    state

}

WEIGHTS = c(30,30,10,1,10,1)

getReward <- function(state, action, hitObjects)
{
    # action 1 - nothing
    # action 2 - steer left
    # action 3 - steer right
    # action 4 - speed up
    # action 5 - speed down
    rewards <- c(lr=0, front=0, lanes=0, hit=0, speed=0, off=0)

    # lr, front
    if (state["left"] == 2 && action == 2)
      rewards["lr"] <- rewards["lr"] - 1
    if (state["right"] == 2 && action == 3)
      rewards["lr"] <- rewards["lr"] - 1
    if (state["front"] == 2 && action != 5)
      rewards["front"] <- rewards["front"] - 1

    # hit
#   if (length(hitObjects) > 0 && hitObjects != "fuel")
#     rewards["hit"] <- 1
#   if (length(hitObjects) > 0 && hitObjects == "fuel")
#     rewards["hit"] <- 1

    rewards["lanes"] <- state[1:NUMLANES][state["mycarpos"]]
    rewards["speed"] <- exp(state["mycarspeed"] - 2) / exp(3)

    if (state["mycarofflane"] == 2 && action != 2 && action != 3)
      rewards["off"] <- -1

    print(state)
    print(rewards * WEIGHTS)

    reward <- sum(rewards * WEIGHTS)
    reward
}

# devices
while (dev.cur() < 3){
    dev.new()
}

initConsts(numlanes=3, numcars=5)
STARTFUEL = 2000
# MINCARSPEED = 5
for (i in 10){
    qmat <- qlearning(c(rep(NUMLANES+1, NUMLANES), 2, 2, 2, NUMLANES, 5, 2), maxtrials = i)
    simulation(qmat)
}
