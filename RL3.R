rm(list=ls())
source("simulation.R")


# Get lane positions of objects and append them to so
lanePos <- function(so)
{
    origin <- so[so$type == "leftside", "xtopleft"]
    ls     <- so[, "xtopleft"] - origin
    rs     <- so[, "xbottomright"] - origin

    lcoord <- ls %/% (LANEWIDTH + 1)
    rcoord <- rs %/% (LANEWIDTH + 1)

    cbind(so, data.frame(lcoord=lcoord + 1, rcoord=rcoord + 1))
}

# Check left right and front sides of myCar for other cars and sides.
overlapping <- function(so, speed)
{
    state <- c(left=1, right=1, front=1)
    sel   <- which( so$type == "car"
                  | so$type == "leftside"
                  | so$type == "rightside")

    for (i in sel)
    {
        if (isOverlapped( so[1, "xtopleft"],
                          so[1, "ytopleft"] + CARLENGTH * speed,
                          so[1, "xbottomright"],
                          so[1, "ytopleft"],
                          so[i, "xtopleft"],
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["front"] <- 2

        if (isOverlapped( so[1, "xtopleft"] - CARWIDTH %/% 3, # mogoče mn
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
                          so[1, "xbottomright"] + CARWIDTH %/% 3, # mogoče mn
                          so[1, "ybottomright"],
                          so[i, "xtopleft"],
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["right"] <- 2
    }
    state
}

getStateDesc <- function(so)
{
    state <- c(best  = 1, # 1 → left, 2 → center, 3 → right
               front = 1, # 1 → free, 2 → taken
               left  = 1, # 1 → free, 2 → taken
               right = 1, # 1 → free, 2 → taken
               speed = 1) # 1:MAXCARSPEED

    .so <<- so

    # Add y coordinates to sides, for collision detection.
    so[so$type=="leftside", "ytopleft"]      <- so[1, "ytopleft"]
    so[so$type=="rightside", "ytopleft"]     <- so[1, "ytopleft"]
    so[so$type=="leftside", "ybottomright"]  <- so[1, "ybottomright"]
    so[so$type=="rightside", "ybottomright"] <- so[1, "ybottomright"]

    # Add lane positions to all objects
    so <- lanePos(so)

    # Only take the nearest car for each lane.
    cars <- so[ so$type=="car"
              # & so$speed < MAXCARSPEED
              & so$ytopleft > -CARLENGTH
              , c("ybottomright", "lcoord")]
    cars <- rbind(cars, data.frame(ybottomright = rep(Inf, NUMLANES),
                                   lcoord       = 1:NUMLANES))

    fuels <- so[so$type=="fuel", c("ybottomright", "lcoord")]
    fuels <- rbind(fuels, data.frame(ybottomright = rep(Inf, NUMLANES),
                                     lcoord       = 1:NUMLANES))

    c.lanes <- aggregate(ybottomright ~ lcoord, cars, min)$ybottomright
    f.lanes <- aggregate(ybottomright ~ lcoord, fuels, min)$ybottomright

    lanes <- order(c.lanes, decreasing = T)
    lanes[c.lanes > f.lanes] = NUMLANES + 1

    lane.dists <- aggregate(ybottomright ~ lcoord, cars, min)$ybottomright
    # Make offroad lanes the worst.
    lane.dists <- c(-Inf, lanes, -Inf)

    mycar   <- so[1, ]
    mycar.l <- mycar$lcoord + 1
    mycar.r <- mycar$rcoord + 1

    # Find optimal direction.
    best.direction <- if (mycar.l == mycar.r) {
        which.max(lane.dists[(mycar.l-1):(mycar.l+1)])
    } else {
        (which.max(lane.dists[c(mycar.l, mycar.r)]) - 1) * 2 + 1
    }

    state["best"]  <- best.direction
    state["speed"] <- mycar$speed

    # Check overlapping sides.
    overlaps       <- overlapping(so, mycar$speed)
    state["left"]  <- overlaps["left"]
    state["right"] <- overlaps["right"]
    state["front"] <- overlaps["front"]

    state
}

WEIGHTS <- c(sides = 3,
             front = 5,
             steer = 1,
             speed = 1)

getReward <- function(state, action, hitObjects)
{
    # Action 1 - nothing
    # Action 2 - steer left
    # Action 3 - steer right
    # Action 4 - speed up
    # Action 5 - speed down

    rewards <- c(sides = 1,
                 front = 0,
                 steer = 0,
                 speed = 0)

    # Don't steer into stuff.
    if (state["left"] == 2  && action == 2 ||
        state["right"] == 2 && action == 3){
        rewards["sides"] <- -1
    }

    # Brake if you're about to get hit.
    if (state["front"] == 2 && action == 5) {
        rewards["front"] <- 1
    }
    if (state["front"] == 2 && (action == 2 || action == 3)) {
        rewards["front"] <- .5
    }

    # Steer towards the better lane.
    if (state["best"] == 1 && action == 2) {
      rewards["steer"] <- 1
    }
    if (state["best"] == 2 && action != 2 && action != 3) {
      rewards["steer"] <- 1
    }
    if (state["best"] == 3 && action == 3) {
      rewards["steer"] <- 1
    }

    # Gotta go fast.
    rewards["speed"] <- (state["speed"] ^ 2) / (MAXCARSPEED ^ 2)

    sum(rewards * WEIGHTS)
}


# Fix devices.
while (dev.cur() < 3){
    dev.new()
}

initConsts(numlanes=3, numcars=5)
STARTFUEL = 2000
# MINCARSPEED = 5
for (i in 10){
    qmat <- qlearning(c(3, 2, 2, 2, MAXCARSPEED), maxtrials = i)
    simulation(qmat)
}
