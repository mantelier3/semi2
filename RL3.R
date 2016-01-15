rm(list=ls())
source("simulation.R")

# Treat warnings as errors.
options(warn=2)

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
    front <- 1
    sel   <- which( so$type == "car" )
                  #| so$type == "leftside"
                  #| so$type == "rightside")

    for (i in sel)
    {
        # Steer here
        if (isOverlapped( so[1, "xtopleft"],
                          so[1, "ytopleft"] + LANEWIDTH * speed * 2,
                          so[1, "xbottomright"],
                          so[1, "ytopleft"],
                          so[i, "xtopleft"],
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            front <- 2

        # Brake here
        if (isOverlapped( so[1, "xtopleft"],
                          so[1, "ytopleft"] + sum(1:speed) * 2,
                          so[1, "xbottomright"],
                          so[1, "ytopleft"],
                          so[i, "xtopleft"],
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            front <- 3
    }
    front
}

# Pick the max index of v. Prefer center if tied.
which.max3 <- function(v, mycarpos)
{
  maxs <- which(v == max(v))
  if (length(maxs) > 1) {
    maxs[2]
  } else {
    maxs
  }
}

getStateDesc <- function(so)
{
    state <- c(best  = 1, # 1 → left, 2 → center, 3 → right
               front = 1) # 1 → free, 2 → taken
              #left  = 1, # 1 → free, 2 → taken
              #right = 1, # 1 → free, 2 → taken
              #speed = 1  # 1:MAXCARSPEED + 1 %/% 2

    .so <<- so

    top <- LANELENGTH

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
              & so$ytopleft > -2 * CARLENGTH
              , c("ybottomright", "lcoord")]
    cars <- rbind(cars, data.frame(ybottomright = rep(top, NUMLANES),
                                   lcoord       = 1:NUMLANES))

    fuels <- so[so$type=="fuel", c("ybottomright", "lcoord")]
    fuels <- rbind(fuels, data.frame(ybottomright = rep(top, NUMLANES),
                                     lcoord       = 1:NUMLANES))

    c.lanes <- aggregate(ybottomright ~ lcoord, cars, min)$ybottomright
    f.lanes <- aggregate(ybottomright ~ lcoord, fuels, min)$ybottomright
    f.lanes <- f.lanes + 2 * CARLENGTH

    # Lanes with fuel are worth more.
    lanes <- c.lanes
    lanes[c.lanes > f.lanes] <- 2 * top - order(f.lanes[c.lanes > f.lanes])

    # Make middle slightly more desirable
    weight  <- - ((1:NUMLANES)^2 + (NUMLANES:1)^2) / NUMLANES^4
    lanes   <- lanes + weight
    lane.dists <- c(-Inf, lanes, -Inf)

    mycar   <- so[1, ]
    mycar.l <- mycar$lcoord + 1
    mycar.r <- mycar$rcoord + 1

    #state["speed"] <- (mycar$speed + 1) %/% 2

    # Check overlapping sides.
    state["front"] <- overlapping(so, mycar$speed)

    # Find optimal direction.
    best.direction <- if (mycar.l == mycar.r) {
        which.max(lane.dists[(mycar.l-1):(mycar.l+1)])
    } else {
        # Ignore front!
        state["front"] <- 1
        (which.max(lane.dists[c(mycar.l, mycar.r)]) - 1) * 2 + 1
    }
    state["best"]  <- best.direction

    state
}

WEIGHTS <- c(#sides = 0,
             front = 1,
             steer = 2,
             speed = 3,
             fuel  = 1)

#TODO: v uno front dej collision imminent in safe to steer away

getReward <- function(state, action, hitObjects)
{
    # Action 1 - nothing
    # Action 2 - steer left
    # Action 3 - steer right
    # Action 4 - speed up
    # Action 5 - speed down

    rewards <- c(#sides = 0,
                 front = 0,
                 steer = 0,
                 speed = 0,
                 fuel  = 0)

    # Don't steer into stuff.
#   if (state["left"] == 2  && action == 2 ||
#       state["right"] == 2 && action == 3){
#       rewards["sides"] <- -1
#   }

    # Brake if you're about to get hit.
      # Collision imminent
    if (state["front"] == 3 && action != 5) {
        rewards["front"] <- -1
    }
      # Can steer away
    if (state["front"] == 2 && state["best"] == 1 && action != 2) {
        rewards["front"] <- 1
    }
    if (state["front"] == 2 && state["best"] == 3 && action != 3) {
        rewards["front"] <- 1
    }
    if (state["front"] == 2 && state["best"] == 2 && action != 5) {
        rewards["front"] <- -1
    }

    # Steer towards the better lane.
    # Left:
    if (state["best"] == 1 && action == 2) {
      rewards["steer"] <- 1
    }
    if (state["best"] == 1 && action == 3) {
      rewards["steer"] <- -1
    }
    # Right:
    if (state["best"] == 3 && action == 3) {
      rewards["steer"] <- 1
    }
    if (state["best"] == 3 && action == 2) {
      rewards["steer"] <- -1
    }
    # Gotta go fast.
    if (state["best"] == 2 && action == 4 && state["front"] == 1) {
      rewards["speed"] <- 1
    }
    if (state["best"] == 2 && action == 5 && state["front"] == 1) {
      rewards["speed"] <- -1
    }
    if (state["best"] == 2 && (action == 2 || action == 3)) {
      rewards["steer"] <- -1
    }

    rewards["front"] <- (1 + rewards["front"]) / 2
    rewards["steer"] <- (1 + rewards["steer"]) / 2
    rewards["speed"] <- (1 + rewards["speed"]) / 2

    # Gotta go fast.
    #rewards["speed"] <- (state["speed"] ^ 2) /
    #                   (((MAXCARSPEED + 1) %/% 2) ^ 2)


    reward <- 1

    # Hitting stuff
    if (length(hitObjects) > 0) {
      if (hitObjects[1] == "car")
        reward <- 0
      if (hitObjects[1] == "leftside")
        reward <- 0
      if (hitObjects[1] == "rightside")
        reward <- 0
      if (hitObjects[1] == "fuel")
        rewards["fuel"] <- 1
    }

    reward * sum(rewards * WEIGHTS)
}


# Fix devices.
while (dev.cur() < 3){
    dev.new()
}

#initConsts(numlanes=3, numcars=5)
initConsts(numlanes=3, numcars=5)
# STARTFUEL = 2000
# MINCARSPEED = 5
for (i in 10){
    qmat <- qlearning(c(3, 3), maxtrials = i)
    simulation(qmat)
}
