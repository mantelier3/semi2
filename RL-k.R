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
    
    state <- c(1,1,1,1,1)
    

    names(state) <- c(   "left"
                       , "right"
                       , "front"
                       , "optimal_lane_dir"
                       , "maxspeed")
    
    so$lane <- rep(0, nrow(so))
    
    for (i in 1:nrow(so)) {
        so$lane[i] <- lanePos(so, i)[1]
    }

    lanes  <- rep(Inf, NUMLANES)
    so.car <- so[order(so$ybottomright), ]
    sel    <- which(so.car$type == "car")
    
#     if (any(so$type == "car") && any(so$type == "fuel")){
#         sop <<- so 
#         print(so)
#         browser()
#     }
    

    
    so.car.fuel <- so[so$type == "car" | so$type == "fuel",]
    so.car.fuel.sorted <- so.car.fuel[order(so.car.fuel[,"lane"],so.car.fuel[,"ybottomright"]),]
    so.car.fuel.sorted <- so.car.fuel.sorted[!duplicated(so.car.fuel.sorted$lane),]
    so.car.fuel.sorted <- so.car.fuel.sorted[order(so.car.fuel.sorted[,"ybottomright"]),]
    lane_indexes <- so.car.fuel.sorted$lane
    lane_order <- order(so.car.fuel.sorted$lane)
    lane_order[which(so.car.fuel.sorted$type=="fuel")] <- NUMLANES + 2
    lanes[lane_indexes] <- lane_order
    lanes[is.infinite(lanes)] <- NUMLANES+1

    optimal_lane <- which.max(lanes)
    
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
        
        if (isOverlapped( so[1, "xtopleft"] - 4, 
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
                          so[1, "xbottomright"] + 4,
                          so[1, "ybottomright"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["right"] <- 2
    }
    
    
    
    carpos <- lanePos(so, 1)
    if (length(carpos) > 1)
        carpos <- sum(carpos)/2
    if(carpos < optimal_lane)
        state["optimal_lane_dir"] <- 1
    else if (carpos == optimal_lane)
        state["optimal_lane_dir"] <- 2
    else 
        state["optimal_lane_dir"] <- 3
        
    state["maxspeed"] <- 2
    if(state["front"] == 1)
        state["maxspeed"] <- 1
        
    #print(state)
    #print(length(state))
    # print(so)
    state
    
}
#  lr front lanes hit speed off
# WEIGHTS = c(30,30,10,1,10,1)
WEIGHTS = c(50,50,10,1,10,1)

getReward <- function(state, action, hitObjects)
{
    # action 1 - nothing
    # action 2 - steer left
    # action 3 - steer right
    # action 4 - speed up
    # action 5 - speed down
    # rewards <- c(lr=0, front=0, lanes=0, hit=0, speed=0, off=0)
    reward <- 100
    if(state["left"] == 2 && action == 2)
        reward <- 0
    if (state["right"] == 2 && action == 3)
        reward <- 0
    if (state["front"] && (action != 2 || action != 3 || action != 5))
        reward <- 0
    if (state["optimal_lane_dir"] == 1 && action != 2)
        reward <- reward/10
    if (state["optimal_lane_dir"] == 3 && action != 3)
        reward <- reward/10
    if (state["maxspeed"] == 1 && action != 4)
        reward <- reward/5
    
        
    
    
#         rewards["off"] <- -0.5
#     
# #     print(state)
# #     print(rewards)
# #     browser()
#     if (any(is.na(rewards))){
#         print(rewards)
#         print(state)
#     }
    reward <- 1
    reward
}

# devices
while (dev.cur() < 3){
    dev.new()
}

initConsts(numlanes=3, numcars=5)
STARTFUEL = 5000
# MINCARSPEED = 5
printState <<- F
for (i in 100){
    # qmat <- qlearning(c(rep(NUMLANES+2, NUMLANES), 2, 2, 2, NUMLANES, 5, 2), maxtrials = i)
    # c(2,2,2,3,2)
    # is something crashable to the left, 1=no, 2=yes
    # is something crashable to the right, 1=no, 2=yes
    # is something crashable in front, 1=no, 2=yes
    # where is the optimal lane - 1=to the left, 2=you're on it, 3=to the right
    # is my speed max possible for situation - 1=no, 2=yes
    qmat <- qlearning(c(2,2,2,3,2), maxtrials = i)
    printState <<- T
    simulation(qmat)
}
