manhattanDistance <- function(A, B) {
  #Computes the manhattan distance between A and B
  #A and B each needs to be a list and have a 'x' and a 'y' coordinate members
  return(abs(A['x'] - B['x']) + abs(A['y'] - B['y']))
}

# <Waiting Optimization>
#
# nextMove <- function(car, history, goal, traffic) {
#   neighbourX = goal[['x']]
#   neighbourY = goal[['y']]
#   parentX = history[['parentXs']][neighbourX, neighbourY]
#   parentY = history[['parentYs']][neighbourX, neighbourY]
#   
#   if(goal['x'] == car['x'] && goal['y'] == car['y']){
#     nextMove = 5
#   }else{
#     while(parentX != car[['x']] || parentY != car[['y']]) {
#       neighbourX = parentX
#       neighbourY = parentY
#       parentX = history[['parentXs']][neighbourX, neighbourY]
#       parentY = history[['parentYs']][neighbourX, neighbourY]
#     }
#   }
#   
#   parentScore = history[['scores']][neighbourX, neighbourY]
#   neighbour = c(x=neighbourX, y=neighbourY)
#   carCoord = c(x = car[['x']],y = car[['y']])
#   waits = car$mem$wait
#   
#   #checks if you move away from the goal and if you've been waiting for more than a
#   #certain amount of turns for a better path.
#   if(waits<2 && manhattanDistance(neighbour,goal)>manhattanDistance(carCoord,goal)){
#     nextMove = 5
#     car$mem$wait = waits +1
#   }
#   #checks if the next move will be too costly and if you've been waiting for more than a
#   #certain amount of turns for the costs to go down.
#   else if(waits<2 && (mean(traffic[['vroads']]) + mean(traffic[['hroads']]) * 1.5) < parentScore){
#     nextMove = 5
#     car$mem$wait = waits + 1
#   }
#   else{
#     
#     if (neighbourY < car['y']) {
#       nextMove = 2
#     }
#     else if (neighbourY > car['y']) {
#       nextMove = 8
#     }
#     else {
#       if (neighbourX < car['x']) {
#         nextMove = 4
#       }
#       else if (neighbourX > car['x']) {
#         nextMove = 6
#       }
#       else {
#         print("Shouldn't be here")
#         nextMove = 5
#       }
#     }
#     car$mem$wait = 0
#   }
#   car['nextMove'] = nextMove
#   
#   return(car)
# }
#
# </Waiting Optimization>




nextMove <- function(car, history, goal) {
  neighbourX = goal['x']
  neighbourY = goal['y']
  parentX = history[['parentXs']][neighbourX, neighbourY]
  parentY = history[['parentYs']][neighbourX, neighbourY]
  
  if(goal['x'] == car['x'] && goal['y'] == car['y']){
    nextMove = 5
  }else(
    while(parentX != car[['x']] || parentY != car[['y']]) {
      neighbourX = parentX
      neighbourY = parentY
      parentX = history[['parentXs']][neighbourX, neighbourY]
      parentY = history[['parentYs']][neighbourX, neighbourY]
    }
  )
  
  if (neighbourY < car['y']) {
    nextMove = 2
  }
  else if (neighbourY > car['y']) {
    nextMove = 8
  }
  else {
    if (neighbourX < car['x']) {
      nextMove = 4
    }
    else if (neighbourX > car['x']) {
      nextMove = 6
    }
    else {
      nextMove = 5
    }
  }
  
  return(nextMove)
}

getNextPackageOrDelivery <- function(car, deliveries, size) {
  #Check if the car has a package loaded
  #If yes, the goal is the package's delivery point
  #If not, the goal is the closest unpicked package
  
  goal = c(x = 0, y = 0)
  
  if (car['load'] > 0) {
    #package loaded
    package = car[['load']]
    goal["x"] = deliveries[package,3]
    goal["y"] = deliveries[package,4]
  }
  else {
    #Look for the closest unpicked package in the deliveries list
    closestDistance = size * 2
    closest = 0
    for (i in 1:nrow(deliveries)) {
      if (deliveries[i,5] == 0) {
        #If the package has not been picked up yet
        carCoord = c(x = car[['x']],y = car[['y']])
        pickup = c(x = deliveries[i,1], y = deliveries[i,2])
        distance = manhattanDistance(carCoord, pickup)
        if (distance <= closestDistance) {
          closestDistance = distance
          closest = i
        }
      }
    }
    goal["x"] = deliveries[closest,1]
    goal["y"] = deliveries[closest,2]
  }
  
  #return a list of coordinates to the goal 
  return(goal)
}

computeAStarScore <- function(traffic, parent, child, goal) {
  #First computes the heuristic (manhattan distance) of the child
  heuri = manhattanDistance(child, goal)
  #Then finds the cost ot the movement from the parent to the child
  cost = 0
  if (parent['x'] == child['x']) {
    #vertical movement
    cost = traffic[['vroads']][min(parent['y'], child['y']), parent['x']]
  }
  else if (parent['y'] == child['y']) {
    #horizontal movement
    cost = traffic[['hroads']][parent['y'], min(parent['x'], child['x'])]
  }
  else {
    print("IMPOSSIBLEEEEEE")
  }
  
  return(heuri + cost)
}

addNeighboursToFrontier <- function(traffic, current, history, goal) {
  size = ncol(traffic[['vroads']])
  
  #For each possible neighbour of current
  for (x in (current['x'] - 1):(current['x'] + 1)) {
    for (y in (current['y'] - 1):(current['y'] + 1)) {
      # -check it's not the node itself
      if (x == current['x'] && y == current['y']) {
        next()
      }
      
      # -check it's a neighbour (and not a diagonal neighbour)
      if (x != current['x'] && y != current['y']) {
        next()
      }
      
      # -check if it exists
      if (x < 1 || x > size || y < 1 || y > size) {
        #If doesn't exist, next
        next()
      }
      
      # -check if it has been visited
      if (history[['scores']][x,y] == 0) {
        #If the node has never been visited, add it to the frontier
        neighbour = c(x = x, y = y)
        history[['scores']][x,y] = computeAStarScore(traffic, current, neighbour, goal)
        history[['parentXs']][x,y] = current['x']
        history[['parentYs']][x,y] = current['y']
      }
      else if (history[['scores']][x,y] == -1) {
        #If the node has already been explored, go to the next neighbour
        next()
      }
      else {
        #If the node is already in the frontier,
        #only replace the current occurence of the node if
        #the new score is better
        currentScore = history[['scores']][x,y]
        neighbour = c(x = x, y = y)
        newScore = computeAStarScore(traffic, current, neighbour, goal)
        if (newScore > currentScore) {
          history[['scores']][x,y] = newScore
          history[['parentXs']][x,y] = current['x']
          history[['parentYs']][x,y] = current['y']
        }
      }
    }
  }
  
  return(history)
}

aStarMain <- function(traffic, car, goal) {
  size = ncol(traffic[['vroads']])
  
  #We initialize current with the car position
  current = c(x = 0, y = 0)
  current['x'] = car[['x']]
  current['y'] = car[['y']]
  current['score'] = manhattanDistance(current, goal)
  
  #Matrices representing the grid, score matrix, parentX matrix and parentY matrix
  #score has value 0 if not visited, -1 if explored
  #If the node is in the frontier, its value is its corresponding score
  scores = matrix(nrow = size, ncol = size)
  parentXs = matrix(nrow = size, ncol = size)
  parentYs = matrix(nrow = size, ncol = size)
  
  for (i in 1:nrow(scores)) {
    for (j in 1:ncol(scores)) {
      scores[i,j] = 0
      parentXs[i,j] = 0
      parentYs[i,j] = 0
    }
  }
  history = list(scores = scores, parentXs = parentXs, parentYs = parentYs)
  
  #While we have not reached the goal
  while(current['x'] != goal['x'] || current['y'] != goal['y']) {
    
    #We add unvisited neighbours of current to the frontier
    history = addNeighboursToFrontier(traffic, current, history, goal)
    
    #We set the current node score to -1
    history[['scores']][current['x'], current['y']] = -1
    
    #We look for the lowest score to choose the next node to explore
    bestScore = size * size * 1000
    bestNode = c()
    for (i in 1:nrow(history[['scores']])) {
      for (j in 1:ncol(history[['scores']])) {
        if (history[['scores']][i,j] > 0) {
          #The node is in the frontier
          if (history[['scores']][i,j] < bestScore) {
            bestScore = history[['scores']][i,j]
            bestNode = c(x = i, y = j, parentX = history[['parentXs']][i,j], parentY = history[['parentYs']][i,j], score = bestScore)
          }
        }
      }
    }
    #Set current to the best node in the frontier
    current = bestNode
  }
  
  # <Waiting Optimizations>
  #return(nextMove(car,history,goal, traffic))
  # </Waiting Optimizations>
  
  return(nextMove(car, history, goal))
}


carAstar <- function(traffic, car, deliveries) {
  #traffic: list of two matrices giving the traffic conditions.
  #First one is 'hroads' and gives the traffic conditions for horizontal roads
  #Second on is 'vroads' and gives the traffic conditions for vertical roads
  #car: list of x and y coordinates (names 'x' and 'y')
  #and the package the car is carrying ('load') (0 if no package is carried)
  #and a list 'mem' to store info from one turn to another
  #and a field 'nextMove' to specify the move who want to make
  #(2 down, 4 left, 6 right, 8 up, 5 stay still)
  #deliveries: matrix with info on the packages to deliver,
  #one row per package,
  #first two columns are the x and y of the package,
  #next two columns are the x and y of the delivery point,
  #last column is the package status
  #(0 is not picked up, 1 is picked up but not delivered, 2 is delivered)
  
  #Function should return the car object with the nextMove specified
  
  # <Waiting Optimizations>
  #
  # if(length(car$mem)==0){
  #   car$mem$wait = 0
  # }
  # 
  # #Closest package + AStar
  # goal = getNextPackageOrDelivery(car, deliveries, ncol(traffic[['vroads']]))
  # car = aStarMain(traffic, car, goal)
  # return(car)
  #
  # </Waiting Optimizations>
  
  #Closest package + AStar
  goal = getNextPackageOrDelivery(car, deliveries, ncol(traffic[['vroads']]))
  car['nextMove'] = aStarMain(traffic, car, goal)
  
  return(car)
}