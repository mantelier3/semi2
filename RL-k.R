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
    
    state <- c(1,1,1,1)
    

    names(state) <- c(   "leftright"
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
    

    

    
    so.car.fuel <- so[so$type == "car" | so$type == "fuel",]
    so.car.fuel <- so.car.fuel[so.car.fuel$ytopleft > -20,]
    so.car.fuel.sorted <- so.car.fuel[order(so.car.fuel[,"lane"],so.car.fuel[,"ybottomright"]),]
    so.car.fuel.sorted <- so.car.fuel.sorted[!duplicated(so.car.fuel.sorted$lane),]
    so.car.fuel.sorted <- so.car.fuel.sorted[order(so.car.fuel.sorted[,"ybottomright"]),]
    lane_order <- order(so.car.fuel.sorted$lane)
    so.car.fuel.sorted <- so.car.fuel.sorted[order(so.car.fuel.sorted$lane),]
    lane_indexes <- so.car.fuel.sorted$lane
    lane_order[which(so.car.fuel.sorted$type=="fuel")] <- NUMLANES + 1 # not priority fuel
    lane_order[which(so.car.fuel.sorted$type=="fuel")[1]] <- NUMLANES + 2 # priority fuel
    lanes[lane_indexes] <- lane_order
    lanes[is.infinite(lanes)] <- NUMLANES+1 # empty lane

    optimal_lane <- which.max(lanes)
    
    sel <- which( so$type == "car"
                  | so$type == "leftside" 
                  | so$type == "rightside")
    
    
    for (i in sel)
    {
        if (isOverlapped( so[1, "xtopleft"]-1, 
                          so[1, "ytopleft"] + 2*CARLENGTH,
                          so[1, "xbottomright"]+1,
                          so[1, "ytopleft"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["front"] <- 2
        
        if (isOverlapped( so[1, "xtopleft"] - 5, 
                          so[1, "ytopleft"],
                          so[1, "xtopleft"],
                          so[1, "ybottomright"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["leftright"] <- 2
        
        if (isOverlapped( so[1, "xbottomright"], 
                          so[1, "ytopleft"],
                          so[1, "xbottomright"] + 5,
                          so[1, "ybottomright"],
                          so[i, "xtopleft"], 
                          so[i, "ytopleft"],
                          so[i, "xbottomright"],
                          so[i, "ybottomright"]))
            state["leftright"] <- 2
    }
    
    
    
    carpos_orig <- lanePos(so, 1)
    if (length(carpos_orig) > 1)
        carpos <- sum(carpos_orig)/2
    else
        carpos <- carpos_orig
    # browser()
    if(carpos > optimal_lane)
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
    
#     count <<- count + 1
#     if ((dontskip || count %% 100 == 0) && any(so$type == "car") && any(so$type == "fuel") && nrow(so[so$type=="fuel",]) > 1){
#         sop <<- so 
#         print(so)
#         browser()
#     }
    
    state <- state[c("optimal_lane_dir")]
    sot <<- so
    print(so)
    printf("indexes %s",paste(lane_indexes,collapse=" "))
    printf("order %s",paste(lane_order,collapse=" "))
    printf("lanes %s",paste(lanes,collapse=" "))
    printf("optimal lane %s",paste(optimal_lane,collapse=" "))
    printf("state %s",paste(state,collapse=" "))
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
#     if(state["left"] == 2 && action == 2)
#         reward <- 0
#     if (state["right"] == 2 && action == 3)
#         reward <- 0
#     if (state["front"] && ( action != 5 || action != 2 || action != 3))
#         reward <- 0
#     if(state["left"] == 2)
#         reward <- reward*0.8
#     if (state["right"] == 2)
#         reward <- reward*0.8
#     if (state["leftright"] == 2 && ( action == 2 || action == 3))
#         reward <- 0
#     if (state["leftright"] == 2)
#         reward <- reward/2
    if (state["optimal_lane_dir"] == 2)
        reward <- reward + 1000
    else if (state["optimal_lane_dir"] == 1 && action != 2)
        reward <- reward/10
    else if (state["optimal_lane_dir"] == 2 && action != 3)
        reward <- reward/10
#     if (state["maxspeed"] == 2)
#         reward <- reward*5
#     if (state["maxspeed"] == 1 && action != 4)
#         reward <- reward/2

    if(length(hitObjects) > 0){
        if(length(hitObjects) > 2)
            print("woooooooooooooooooooooooooooooooooooooooooooooooooo")
        print(hitObjects)
        print("fooooooooooooooooooooooooooo")
        if(hitObjects == "fuel")
            reward <- 10000000
        else
            reward <- 0
    }        
    
    
#     
# #     print(state)
# #     print(rewards)
# #     browser()
#     if (any(is.na(rewards))){
#         print(rewards)
#         print(state)
#     }
#     if (state["maxspeed"] == 1)
#         print(state)
#         browser()
#         reward <- 0
    reward
}

