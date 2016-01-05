source("simulation.R")

getStateDesc <- function(sceneObjects)
{
	state <- c(1, 1, 1)
	names(state) <- c("left", "front", "right")
	
	leftedge <- sceneObjects[which(sceneObjects$type == "leftside"), "xtopleft"]
	rightedge <- sceneObjects[which(sceneObjects$type == "rightside"), "xbottomright"]

	if (abs(leftedge) < 2)
		state["left"] <- 2
	if (rightedge - sceneObjects[1,"xbottomright"] < 2)
		state["right"] <- 2

	sel <- which(sceneObjects$type == "car")
	for (i in sel)
	{
		if (isOverlapped(sceneObjects[1, "xtopleft"], 
                             sceneObjects[1, "ytopleft"] + CARLENGTH,
                             sceneObjects[1, "xbottomright"],
                             sceneObjects[1, "ytopleft"],
                             sceneObjects[i, "xtopleft"], 
                             sceneObjects[i, "ytopleft"],
                             sceneObjects[i, "xbottomright"],
                             sceneObjects[i, "ybottomright"]))
			state["front"] <- 2

		if (isOverlapped(sceneObjects[1, "xtopleft"] - CARWIDTH, 
                             sceneObjects[1, "ytopleft"],
                             sceneObjects[1, "xtopleft"],
                             sceneObjects[1, "ybottomright"],
                             sceneObjects[i, "xtopleft"], 
                             sceneObjects[i, "ytopleft"],
                             sceneObjects[i, "xbottomright"],
                             sceneObjects[i, "ybottomright"]))
			state["left"] <- 2

		if (isOverlapped(sceneObjects[1, "xbottomright"], 
                             sceneObjects[1, "ytopleft"],
                             sceneObjects[1, "xbottomright"] + CARWIDTH,
                             sceneObjects[1, "ybottomright"],
                             sceneObjects[i, "xtopleft"], 
                             sceneObjects[i, "ytopleft"],
                             sceneObjects[i, "xbottomright"],
                             sceneObjects[i, "ybottomright"]))
			state["right"] <- 2
	}

	state
}

getReward <- function(state, action, hitObjects)
{
	# action 1 - nothing
	# action 2 - steer left
	# action 3 - steer right
	# action 4 - speed up
	# action 5 - speed down

	reward <- 1

	reward	
}


initConsts(numlanes=3, numcars=5)
qmat <- qlearning(c(2, 2, 2))
simulation(qmat)