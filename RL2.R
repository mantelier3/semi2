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
    state <- rep(1, 3)
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
                      "carSpeed", "carPosL", "carPosR",
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
    state[c("left", "right", "front")] <- overlapping(so)

    state["carSpeed"] <- so[1, "speed"]
    state["carPosL"]  <- so[1, "lcoord"]
    state["carPosR"]  <- so[1, "rcoord"]

    state
}

getReward <- function(state, action, hitObjects)
{
    # action 1 - nothing
    # action 2 - steer left
    # action 3 - steer right
    # action 4 - speed up
    # action 5 - speed down

}
