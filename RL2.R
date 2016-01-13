source("simulation.R")

collisionCourse <- function(so, xoffset=0, type)
{
    selection = which(so$type == type)

    offsets <- rep(Inf, NUMLANES)

    for (i in selection) {
        for (o in -2:2) {
            if (offset >= 0) {
                as.numeric(
                    isOverlapped(
                        so[1, "xtopleft"]
                      , so[1, "ytopleft"] + Inf
                      , so[1, "xbottomright"] + CARWIDTH * o
                      , so[1, "ytopleft"]
                      , so[i, "xtopleft"]
                      , so[i, "ytopleft"]
                      , so[i, "xbottomright"]
                      , so[i, "ybottomright"]))
            } else {
                as.numeric(
                    isOverlapped(
                        so[1, "xtopleft"] + CARWIDTH * o
                      , so[1, "ytopleft"] + Inf
                      , so[1, "xbottomright"]
                      , so[1, "ytopleft"]
                      , so[i, "xtopleft"]
                      , so[i, "ytopleft"]
                      , so[i, "xbottomright"]
                      , so[i, "ybottomright"]))
            }
        }
    }
}

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

overlapping <- function(so)
{
    state <- c(left=1, right=1, front=1)
    sel   <- which( so$type == "car"
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
    state
}

getStateDesc <- function(so)
{
    # State description.
    state <- rep(1, NUMLANES + 6)
    names(state) <- c(paste0("laneScore", 1:NUMLANES),
                      "myCarSpeed", "myCarPosL", "myCarPosR",
                      "left", "right", "front")

    # Enhance so table.
    so <- lanePos(so)
    so[so$type=="leftside", "ytopleft"]      <- so[1, "ytopleft"]
    so[so$type=="rightside", "ytopleft"]     <- so[1, "ytopleft"]
    so[so$type=="leftside", "ybottomright"]  <- so[1, "ybottomright"]
    so[so$type=="rightside", "ybottomright"] <- so[1, "ybottomright"]

    # Find which lanes contain nearest cars.
    cars <- so[so$type=="car" & so$speed < MAXCARSPEED,
               c("ybottomright", "lcoord")]
    cars <- rbind(cars, data.frame(ybottomright = rep(Inf, NUMLANES),
                                   lcoord       = 1:NUMLANES))

    fuels <- so[so$type=="fuel", c("ybottomright", "lcoord")]
    fuels <- rbind(fuels, data.frame(ybottomright = rep(Inf, NUMLANES),
                                     lcoord       = 1:NUMLANES))

    c.lanes <- aggregate(ybottomright ~ lcoord, cars, min)$ybottomright
    f.lanes <- aggregate(ybottomright ~ lcoord, fuels, min)$ybottomright

    lanes <- order(c.lanes, decreasing = T)
    lanes[c.lanes > f.lanes] = NUMLANES + 1

    state[1:NUMLANES] <- lanes

    # Check overlapping sides.
    overlaps       <- overlapping(so)
    state["left"]  <- overlaps["left"]
    state["right"] <- overlaps["right"]
    state["front"] <- overlaps["front"]

    # Speed and coords.
    state["myCarSpeed"] <- so[1, "speed"]
    state["myCarPosL"]  <- so[1, "lcoord"]
    state["myCarPosR"]  <- so[1, "rcoord"]

    state
}

WEIGHTS <- c(sides = 10,
             front = 5,
             lanes = 1,
             speed = 1)

getReward <- function(state, action, hitObjects)
{
    # Action 1 - nothing
    # Action 2 - steer left
    # Action 3 - steer right
    # Action 4 - speed up
    # Action 5 - speed down

    # Allow punishment.
    baseline <- sum(WEIGHTS * c(1, 1, 0, MAXCARSPEED %/% 2))

    rewards <- c(sides = 0,
                 front = 0,
                 lanes = 0,
                 speed = 0)


    # Don't steer into stuff.
    if (state["left"] == 2  && action == 2 ||
        state["right"] == 2 && action == 3){
        rewards["sides"] <- -1
    }

    # Brake if you're about to get hit.
    if (state["front"] == 2 && action != 5) {
        rewards["front"] <- -1
    }

    # Follow the best path.
    rewards["lanes"] <- state[1:NUMLANES][state["myCarPosL"]]
                      + state[1:NUMLANES][state["myCarPosR"]]

    # Be fast.
    rewards["speed"] <- state["myCarSpeed"] - (MAXCARSPEED %/% 2)

    sum(rewards * WEIGHTS) + baseline
}

# Fix devices.
while (dev.cur() < 3){
    dev.new()
}

initConsts(numlanes=3, numcars=5)
STARTFUEL = 2000
# MINCARSPEED = 5
for (i in 10){
    qmat <- qlearning(c(rep(NUMLANES+1, NUMLANES),
                      NUMLANES, NUMLANES, 5, 2, 2, 2), maxtrials = i)
    simulation(qmat)
}
