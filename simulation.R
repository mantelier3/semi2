library(grid)

initConsts <- function(numlanes = 3, numcars = 7)
{
	if (numlanes < 2 || numlanes > 6)
		stop("Illegal number of lanes")

	if (numcars < 1 || numcars > 10)
		stop("Illegal number of cars")


	assign("NUMLANES", numlanes, envir = .GlobalEnv)
	assign("NUMSEGMENTS", 20, envir = .GlobalEnv)
	assign("NUMCARS", numcars, envir = .GlobalEnv)
	assign("SIDEWIDTH", 10, envir = .GlobalEnv)
	assign("LANEWIDTH", 20, envir = .GlobalEnv)
	assign("STRIPWIDTH", 1, envir = .GlobalEnv)
	assign("STRIPLENGTH", 10, envir = .GlobalEnv)
	assign("STRIPGAP", 10, envir = .GlobalEnv)
	assign("LANELENGTH", (STRIPLENGTH + STRIPGAP) * NUMSEGMENTS, envir = .GlobalEnv)
	assign("CARWIDTH", 12, envir = .GlobalEnv)
	assign("CARLENGTH", 20, envir = .GlobalEnv)
	assign("CARCOLOR", 0.6, envir = .GlobalEnv)
	assign("MYCARCOLOR", 0.9, envir = .GlobalEnv)
	assign("SIDECOLOR", 0.4, envir = .GlobalEnv)
	assign("ROADCOLOR", 0, envir = .GlobalEnv)
	assign("STRIPCOLOR", 1.0, envir = .GlobalEnv)
	assign("FUELCOLOR", 1.0, envir = .GlobalEnv)
	assign("MINCARSPEED", 1, envir = .GlobalEnv)
	assign("MAXCARSPEED", 5, envir = .GlobalEnv)
	assign("MINGAP", 2, envir = .GlobalEnv)
	assign("MAXGAP", 10, envir = .GlobalEnv)
	assign("SAFEGAP", 5, envir = .GlobalEnv)
	assign("MYCAROFFSET", 15, envir = .GlobalEnv)
	assign("STARTLANE", ceiling(NUMLANES/2), envir = .GlobalEnv)
	assign("NUMFUELITEMS", 3, envir = .GlobalEnv)
	assign("MINFUELGAP", 50, envir = .GlobalEnv)
	assign("MAXFUELGAP", 500, envir = .GlobalEnv)
	assign("FUELWIDTH", 12, envir = .GlobalEnv)
	assign("FUELADD", 100, envir = .GlobalEnv)
	assign("STARTFUEL", 5000, envir = .GlobalEnv)
	assign("STEER", 1, envir = .GlobalEnv)
	assign("SPEEDUP", 1, envir = .GlobalEnv)
	assign("SPEEDDOWN", 1, envir = .GlobalEnv)
	assign("NUMACTIONS", 5, envir = .GlobalEnv)
}

getLaneCenter <- function(lane)
{
	as.integer((lane-1) * (LANEWIDTH + STRIPWIDTH) + SIDEWIDTH + LANEWIDTH / 2)
}

prepareHighway <- function()
{
	img <- matrix(0, ncol = 2 * SIDEWIDTH + NUMLANES * LANEWIDTH + (NUMLANES-1) * STRIPWIDTH, nrow = LANELENGTH)
	img[,c(1:SIDEWIDTH, (ncol(img)-SIDEWIDTH):ncol(img))] = SIDECOLOR

	stripseq <- rep(c(rep(STRIPCOLOR, STRIPLENGTH), rep(ROADCOLOR, STRIPGAP)), NUMSEGMENTS)
 
	n <- SIDEWIDTH
	for (i in 1:(NUMLANES-1))
	{
		n <- n + LANEWIDTH 
		img[,n:(n+STRIPWIDTH)] <- stripseq
		n <- n + STRIPWIDTH
	}

	img
}	

updateHighway <- function(simData)
{
	if (simData$mycar["speed"] > 0)
	{
		n <- nrow(simData$hw) - simData$mycar["speed"]

		simData$hw <- simData$hw[c((n+1):nrow(simData$hw), 1:n),]
	}
	
	simData
}

prepareMyCar <- function()
{
	myCar <- vector()
	myCar[1] <- getLaneCenter(STARTLANE) - CARWIDTH %/% 2
	myCar[2] <- MYCAROFFSET 
	myCar[3] <- MINCARSPEED
	myCar[4] <- STARTFUEL

	names(myCar) <- c("xpos", "ypos", "speed", "fuel")
	myCar	
}

prepareCars <- function()
{
	xpos <- rep(0, NUMCARS)
	ypos <- rep(-Inf, NUMCARS)
	speed <- rep(0, NUMCARS)
	lane <- sample(1:NUMLANES, NUMCARS, T)
	front <- rep(0, NUMCARS)

	data.frame(xpos, ypos, speed, lane, front)
}

updateCars <- function(simData)
{
	for (i in 1:nrow(simData$cars))
	{
		newspeed <- simData$cars$speed[i] + rnorm(1, 0, 0.2)
		simData$cars$speed[i] <- min(MAXCARSPEED, max(MINCARSPEED, newspeed))

		if (simData$cars$front[i] != 0 && simData$cars$speed[i] > simData$cars$speed[simData$cars$front[i]] && simData$cars$ypos[i] + CARLENGTH + SAFEGAP > simData$cars$ypos[simData$cars$front[i]])
		{
			simData$cars$speed[i] <- simData$cars$speed[simData$cars$front[i]]
		}

		simData$cars$ypos[i] <- simData$cars$ypos[i] + simData$cars$speed[i]
	}

	sel <- which(simData$cars$ypos < simData$distance - CARLENGTH)
	
	for (i in sel)
	{
		l <- sample(1:NUMLANES, 1, T)
		basepos <- 0
		basespeed <- simData$cars$speed[i]

		j <- which(simData$cars$lane == l & simData$cars$front == 0)
		if (any(j))
		{
			basepos <- simData$cars$ypos[j] + SAFEGAP + CARLENGTH
			basespeed <- runif(1, min = MINCARSPEED, max = MAXCARSPEED)
			simData$cars$front[j] <- i	
		}

		simData$cars$xpos[i] <- getLaneCenter(l) - CARWIDTH %/% 2
		simData$cars$ypos[i] <- max(basepos, simData$distance + LANELENGTH)
		simData$cars$speed[i] <- basespeed
		simData$cars$lane[i] <- l
		simData$cars$front[i] <- 0
	}

	simData
}


updateMyCar <- function(simData, action)
{
	# action 1 - nothing
	# action 2 - steer left
	# action 3 - steer right
	# action 4 - speed up
	# action 5 - speed down

	if (action == 2)
		simData$mycar["xpos"] <- simData$mycar["xpos"] - STEER
	else if (action == 3)
		simData$mycar["xpos"] <- simData$mycar["xpos"] + STEER
	else if (action == 4)
		simData$mycar["speed"] <- min(MAXCARSPEED, simData$mycar["speed"] + SPEEDUP)
	else if (action == 5)
		simData$mycar["speed"] <- max(MINCARSPEED, simData$mycar["speed"] - SPEEDDOWN)
	
	simData$mycar["ypos"] <- simData$mycar["ypos"] + simData$mycar["speed"]
	simData$distance <- simData$distance + simData$mycar["speed"]

	simData$mycar["fuel"] <- simData$mycar["fuel"] - 1 - simData$mycar["speed"] %/% 2
	if (simData$mycar["fuel"] < 0)
		simData$running <- F

	simData <- checkOverlappingFuel(simData)
	simData <- checkRoadSide(simData)
	simData <- checkOverlappingCars(simData)
	
	simData
}

prepareFuelItems <- function()
{
	xpos <- rep(0, NUMFUELITEMS)
	ypos <- rep(-Inf, NUMFUELITEMS)

	data.frame(xpos, ypos)
}

updateFuelItems <- function(simData)
{
	sel <- which(simData$fuelItems$ypos < simData$distance - FUELWIDTH)
	basepos <- max(c(simData$fuelItems$ypos, simData$distance + LANELENGTH))

	for (i in sel)
	{
		l <- sample(1:NUMLANES, 1, T)
		basepos <- basepos + runif(1, min = MINFUELGAP, max = MAXFUELGAP)	

		simData$fuelItems$xpos[i] <- getLaneCenter(l) - FUELWIDTH %/% 2
		simData$fuelItems$ypos[i] <- basepos
	}

	simData
}

collectSceneObjects <- function(simData)
{
	type <- vector()
	xtopleft <- vector()
	ytopleft <- vector()
	xbottomright <- vector()
	ybottomright <- vector()
	speed <- vector()
	fuel <- vector()

	type[1] <- "mycar"
	xtopleft[1] <- 0
	ytopleft[1] <- 0
	xbottomright[1] <- CARWIDTH
	ybottomright[1] <- -CARLENGTH
	speed[1] <- simData$mycar["speed"]
	fuel[1] <- simData$mycar["fuel"]

	type <- c(type, "leftside")
	xtopleft <- c(xtopleft, SIDEWIDTH - simData$mycar["xpos"])
	ytopleft <- c(ytopleft, NA)
	xbottomright <- c(xbottomright, SIDEWIDTH - simData$mycar["xpos"])
	ybottomright <- c(ybottomright, NA)
	speed <- c(speed, NA)
	fuel <- c(fuel, NA)

	type <- c(type, "rightside")
	xtopleft <- c(xtopleft, NUMLANES * (LANEWIDTH + STRIPWIDTH) + SIDEWIDTH - simData$mycar["xpos"])
	ytopleft <- c(ytopleft, NA)
	xbottomright <- c(xbottomright, NUMLANES * (LANEWIDTH + STRIPWIDTH) + SIDEWIDTH - simData$mycar["xpos"])
	ybottomright <- c(ybottomright, NA)
	speed <- c(speed, NA)
	fuel <- c(fuel, NA)
	
	for (i in 1:nrow(simData$fuelItems))
	{
		yval <- simData$fuelItems$ypos[i]-simData$mycar["ypos"]
		if (yval + FUELWIDTH + MYCAROFFSET > 0 && yval + MYCAROFFSET < LANELENGTH)
		{
			xval <- simData$fuelItems$xpos[i]-simData$mycar["xpos"]

			type <- c(type, "fuel")
			xtopleft <- c(xtopleft, xval)
			ytopleft <- c(ytopleft, yval - CARLENGTH + FUELWIDTH)
			xbottomright <- c(xbottomright, xval + FUELWIDTH)
			ybottomright <- c(ybottomright, yval - CARLENGTH)
			speed <- c(speed, 0)
			fuel <- c(fuel, NA)
		}
	}

	for (i in 1:nrow(simData$cars))
	{
		yval <- simData$cars$ypos[i]-simData$mycar["ypos"]
		
		if (yval + CARLENGTH + MYCAROFFSET > 0 && yval + MYCAROFFSET < LANELENGTH)
		{
			xval <- simData$cars$xpos[i]-simData$mycar["xpos"]

			type <- c(type, "car")
			xtopleft <- c(xtopleft, xval)
			ytopleft <- c(ytopleft, yval)
			xbottomright <- c(xbottomright, xval + CARWIDTH)
			ybottomright <- c(ybottomright, yval - CARLENGTH)
			speed <- c(speed, cars$speed[i])
			fuel <- c(fuel, NA)
		}
	}

	res <- data.frame(type, xtopleft, ytopleft, xbottomright, ybottomright, speed, fuel)
	res
}

isOverlapped <- function(topleftX1, topleftY1, bottomrightX1, bottomrightY1, topleftX2, topleftY2, bottomrightX2, bottomrightY2)
{
	bottomrightX1 >= topleftX2 &&  
	topleftX1 <= bottomrightX2 && 
	bottomrightY1 <= topleftY2 && 
	topleftY1 >= bottomrightY2
}

checkOverlappingCars <- function(simData)
{
	for (i in 1:nrow(simData$cars))
	{
		if (isOverlapped(simData$cars[i,"xpos"], simData$cars[i,"ypos"] + CARLENGTH, simData$cars[i,"xpos"] + CARWIDTH, simData$cars[i,"ypos"],
				     simData$mycar["xpos"], simData$mycar["ypos"] + CARLENGTH, simData$mycar["xpos"] + CARWIDTH, simData$mycar["ypos"]))
		{
			simData$hitItems <- c(simData$hitItems, "car")
			simData$running <- F
			break
		}
	}

	simData
}

checkOverlappingFuel <- function(simData)
{
	res <- list()

	for (i in 1:nrow(simData$fuelItems))
	{
		if (isOverlapped(simData$fuelItems[i,"xpos"], simData$fuelItems[i,"ypos"] + FUELWIDTH, simData$fuelItems[i,"xpos"] + FUELWIDTH, simData$fuelItems[i,"ypos"],
				     simData$mycar["xpos"], simData$mycar["ypos"] + CARLENGTH, simData$mycar["xpos"] + CARWIDTH, simData$mycar["ypos"]))
		{
			simData$fuelItems[i, "ypos"] <- -Inf
			simData$mycar["fuel"] <- simData$mycar["fuel"] + FUELADD
			simData$hitItems <- c(simData$hitItems, "fuel") 
		}	
	}

	simData
}

checkRoadSide <- function(simData)
{
	if (simData$mycar["xpos"] <= SIDEWIDTH)
	{
		simData$hitItems <- c(simData$hitItems, "leftside")
		simData$running <- F
	}
	else if (simData$mycar["xpos"] >= NUMLANES * (LANEWIDTH + STRIPWIDTH) + SIDEWIDTH - CARWIDTH)
	{
		simData$hitItems <- c(simData$hitItems, "rightside")
		simData$running <- F
	}

	simData
}

dim2sub <- function (iarr, dim) 
{
    iarr <- t(iarr)
    pdim <- c(1, cumprod(dim[-length(dim)]))
    iarr <- iarr - 1
    colSums(apply(iarr, 1, "*", pdim)) + 1
}

selectAction <- function(state, Q)
{
	len <- length(state)
	if (len > 1)
	{
		d <- dim(Q)
		n <- dim2sub(state, d[1:len])
	}
	else
	{
		n <- state[1]
	}

	vals <- apply(Q, len+1, '[', n)

	m <- max(vals)
	candidates <- which(vals==m)
	if (length(candidates) == 1)
		action <- candidates
	else
       	action <- sample(candidates,1)
	action
}

prepareSimData <- function()
{
	data <- list()
	data$hw <- prepareHighway()
	data$mycar <- prepareMyCar()
	data$cars <- prepareCars()
	data$fuelItems <- prepareFuelItems()
	data$hitItems <- vector()
	data$running <- T
	data$distance <- 0
	data
}

simulationStep <- function(simData, action)
{
	simData$hitItems <- vector()

	simData <- updateMyCar(simData, action)
	simData <- updateCars(simData)
	simData <- updateFuelItems(simData)
	simData <- updateHighway(simData)

	simData
}

simulationDraw <- function(simData)
{
	img <- simData$hw  
		
	from.row <- simData$mycar["ypos"] - simData$distance
	to.row <- from.row + CARLENGTH
		
	from.col <- simData$mycar["xpos"]
	to.col <- from.col + CARWIDTH
  
	img[nrow(img) - (from.row:to.row), from.col:to.col] = MYCARCOLOR

	for (i in 1:nrow(simData$fuelItems))
	{
		from.row <- max(1, simData$fuelItems$ypos[i] - simData$distance)
		to.row <- min(nrow(img), simData$fuelItems$ypos[i] - simData$distance + FUELWIDTH)
		
		from.col <- max(1, simData$fuelItems$xpos[i])
		to.col <- min(ncol(img), simData$fuelItems$xpos[i] + FUELWIDTH)
 
		if (to.row >= from.row & to.col >= from.col)
		{
			img[nrow(img) - (from.row:to.row), from.col:to.col] = FUELCOLOR

			img[nrow(img) - ((from.row+to.row) %/% 2), from.col:to.col] = ROADCOLOR
			img[nrow(img) - (from.row:to.row), (from.col + to.col) %/% 2] = ROADCOLOR
		}
	}

	for (i in 1:nrow(simData$cars))
	{
		from.row <- max(1, simData$cars$ypos[i] - simData$distance)
		to.row <- min(nrow(img), simData$cars$ypos[i] - simData$distance + CARLENGTH)
		
		from.col <- max(1, simData$cars$xpos[i])
		to.col <- min(ncol(img), simData$cars$xpos[i] + CARWIDTH)
 
		if (to.row >= from.row & to.col >= from.col)
			img[nrow(img) - (from.row:to.row), from.col:to.col] = CARCOLOR
	}	

	grid.newpage()
	grid.raster(img, interpolate=F)
	grid.text(paste("distance = ", simData$distance), x=0.1, y=0.9, just = "left", gp=gpar(fontsize=12, col="red"))
	grid.text(paste("fuel = ", simData$mycar["fuel"]), x=0.1, y=0.85, just = "left", gp=gpar(fontsize=12, col="red"))
}


simulation <- function(qmat)
{
	if (dev.cur() == 1)
		getOption("device")(width=3, height=10)

	simData <- prepareSimData()
	
	while (simData$running)
	{
		sceneObjects <- collectSceneObjects(simData)
		state <- getStateDesc(sceneObjects)
		action <- selectAction(state, qmat)
		
		simData <- simulationStep(simData, action)
		simulationDraw(simData)	
	}

	as.numeric(simData$distance)
}

qlearning <- function(dimStateSpace, gamma = 0.9, maxtrials = 200, maxdistance = 100000)
{
	dimQ <- c(dimStateSpace, NUMACTIONS)
	Q <- array(0, dim=dimQ)

	alpha <- 1
	ntrials <- 0
 
	while (alpha > 0.1 && ntrials < maxtrials)
	{
		simData <- prepareSimData()
		sceneObjects <- collectSceneObjects(simData)
		curState <- getStateDesc(sceneObjects)

		while (simData$running && simData$distance < maxdistance)
		{
			action <- sample(1:5, 1, T)
                  simData <- simulationStep(simData, action)
			
			sceneObjects <- collectSceneObjects(simData)
			nextState <- getStateDesc(sceneObjects)
			reward <- getReward(nextState, action, simData$hitItems)

			curStatePos <- dim2sub(c(curState, action), dimQ)
			len <- length(nextState)
			if (len > 1)
			{
				n <- dim2sub(nextState, dimStateSpace)
			}
			else
			{
				n <- nextState[1]
			}
			vals <- apply(Q, len+1, '[', n)			

              Q[curStatePos] <- Q[curStatePos] + alpha * (reward + gamma * max(vals, na.rm = T) - Q[curStatePos])
              curState <- nextState
		}

		ntrials <- ntrials + 1
		alpha <- alpha * 0.999

		print(paste("trial",ntrials), quote=F)
		flush.console()
	}

	Q / max(Q)
}
