# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")


myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # finds the goal
  if(carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                   carInfo, 
                                   packageMatrix)
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }

  # finds the best path
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  
  
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[,1] - carInfo$x)) +
    abs((packageMatrix[,2] - carInfo$y))
  #+carInfo$mem$traffic;
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

# returns value of distance between 2 points
distance <- function(goal_coord, newcarInfo) {
    distance_value = abs((goal_coord[[1]] - newcarInfo[[1]])) +
      abs((goal_coord[[2]] - newcarInfo[[2]]));
    #print (goal_coord[2])
    return(distance_value)
}

# checks if in frontier, adds to frontier
addfrontier <- function(newcar, frontier, past, path, g1, h1, f1){
      in_loop = 0;  # if the value is in the frontier don't add to frontier
      
      if(length(past) != 0 ){
          for (i in 1:length(past)){
            if((newcar[[1]] == past[i][[1]][[1]]) && (newcar[[2]] == past[i][[1]][[2]])){
             return(frontier)
            }
          }
      }
        
      if(length(frontier) != 0){
        for (j in 1:length(frontier)){ # loop over frontier to see if the neighbour coord already there
          if ((newcar[[1]] == frontier[j][][[1]][1]) && (newcar[[2]] == frontier[j][][[1]][2])) {
            in_loop = 1
            if(f1 < frontier[j][][[1]][5]){
              # this if loop checks whether the x and y coord match to any in frontier, and compares the f values.
              frontier[j]$f = f1
              frontier[j]$p = list(path[[1]],path[[2]])
            }
          }
        }
        if (in_loop == 0){
          frontier <- c(frontier, list(list(x=newcar[[1]], y=newcar[[2]], g=g1, h=h1, f=f1, p=path)));
        }
      }
      else{ #when length frontier == 0
        frontier <- c(frontier, list(list(x=newcar[[1]], y=newcar[[2]], g=g1, h=h1, f=f1, p=path)))
      }
      return(frontier)
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
    frontier = list();
    current = list();
    past = list();
    path_list= list()    
    
    
    #current car position
    car_pos = list(carInfo$x, carInfo$y);

    goal_coord = list(carInfo$mem$goal[1], carInfo$mem$goal[2])
    distance_goal = distance(goal_coord,car_pos)
    
    g1=0;
    h1=distance_goal;
    f1=g1+h1;
    
    path = list(0,0)
    
    current = list(x=carInfo$x, y=carInfo$y, g=g1, h=h1, f=f1, p=path)
    
    if ((current[[1]] == goal_coord[[1]]) && (current[[2]] == goal_coord[[2]])){
      return(5)
    }
    
    # while the current coordinates is not the goal
    while ((current[[1]] != goal_coord[[1]]) || (current[[2]] != goal_coord[[2]])){
      
      path = list(current$x, current$y)
      if(current[[1]] > 1){ #look at left neighbour
          newcar1 = list((current[[1]]-1),current[[2]])
          
          g1= current$g + trafficMatrix$hroads[current[[1]]-1,current[[2]]];
          h1 = distance(goal_coord, newcar1);
          f1 = g1 +h1;
          frontier = addfrontier(newcar1, frontier, past, path, g1, h1, f1)
    }
      
      if(current[[1]] < 10){ #right neighbour
        newcar2 = list((current[[1]]+1),(current[[2]]))

        g1= current$g +trafficMatrix$hroads[current[[1]],current[[2]]];

        h1 = distance(goal_coord, newcar2)
        f1 = g1 + h1;

        frontier = addfrontier(newcar2, frontier, past, path, g1, h1, f1)
      }
      
      if(current[[2]] > 1){ #below neighbour
        newcar3 = list((current[[1]]),(current[[2]]-1))
        g1= current$g +trafficMatrix$vroads[current[[1]],current[[2]]-1]; 
        h1 = distance(goal_coord, newcar3)
        f1 = g1 + h1;

        frontier = addfrontier(newcar3, frontier, past, path, g1, h1, f1)
        }
      
      if(current[[2]] < 10){ #above neighbour
        newcar4 = list((current[[1]]),(current[[2]]+1))
        g1= current$g + trafficMatrix$vroads[current[[1]],current[[2]]]; 
        h1=distance(goal_coord, newcar4)
        f1 = g1 + h1;

        frontier = addfrontier(newcar4, frontier, past, path, g1, h1, f1)
      }
    
      f_value = 0;
      f_value = sapply(frontier,function(item)item$f);  # gathers all f values
      best_index=which.min(f_value); #returns index of minimum value
      
      past <- c(past, list(current)) #put current into past list
      
      current = frontier[[best_index]] #min value f now being expanded
      frontier[[best_index]] <- NULL #pop min f value out of frontier list, it's already expanded
 
    }
    #current value is the goal at this point   
    coord = current[[6]] # the coordinates in the path of the goal node gives previous node visited.
  
    past <- c(past, list(current)) #put current coord in past
    
    path_list <- c(path_list, list(list(current[[1]],current[[2]]))) #put current in path
    path_list <- c(path_list, list(list(coord[[1]],coord[[2]]))) #put current path in path initialise it
    
    if (coord[[1]] == 0){ # if coord already zero, goal was at point already at so return point
      new_move_x = carInfo$x
      new_move_y = carInfo$y
    }
    else{
      repeat{
        for (i in 1:length(past)){
          x1 = past[[i]][[1]]
          y1 = past[[i]][[2]]
          if(coord[[1]] == x1 
             && coord[[2]] == y1){
            coord = list(past[[i]][[6]][1],past[[i]][[6]][2])
            path_list <- c(path_list, list(list(coord[[1]],coord[[2]])))
          }
        }
        if (coord[[1]] == 0){
          break
        }
      }
      
      new_move_x = path_list[[length(path_list)-2]][[1]]
      new_move_y = path_list[[length(path_list)-2]][[2]]
    }
   # carInfo$mem$traffic=current$g;

  if(carInfo$x < new_move_x) {
    return(6)
  } else if (carInfo$x > new_move_x) {
    return(4)
  } else if (carInfo$y < new_move_y) {
    return(8)
  } else if (carInfo$y > new_move_y) {
    return(2)
  } else {
    return(5)
  }
    
}