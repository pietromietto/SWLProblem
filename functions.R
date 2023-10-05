# FUNCTION

# FUNCTION FOR INITIAL SOLUTION ----

## Generate initial solution ----
generate_initial_data_structure <- function(Z){
  
  H <- sample(1:(Z-1),1)
  X <- c(rep(1,H),rep(0,(Z-1)-H))
  c(sample(1:Z,Z, replace = FALSE), sample(X),rep(2, Z))
}

## Function for whether we use horizontal or vertical bay structure ----
### 1 -> horizontal
### 0 -> vertical
function_direction <- function(){
  direction <- sample(c(0,1),1)
  return(direction)
}

## Function for calculating each bay area ----
function_bay_area <- function(Bays,S_i,Solution,Z){
  
  Bays_number <- rep(1,Bays)
  Bays_area <- c()
  a <- 0
  Bays_area[1] <- S_i[Solution[1]] 
  
  for (i in 1:(Z-1)){
    if (Solution[Z+i] == 0){
      Bays_area[1+a] = Bays_area[1+a] + S_i[Solution[i+1]]
      Bays_number[1+a] <- Bays_number[1+a] + 1
      
    }
    if (Solution[Z+i] == 1){
      a = a + 1
      Bays_area[1+a] = S_i[Solution[i+1]]
      
    } 
  }
  return(list(Bays_number,Bays_area))
}

## Ceiling function with decimals
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

## Calculating the number aisles, length, x-dimensions, and y-dimensions ----
function_number_of_aisles2 <- function(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i){
  
  s <- 1
  n_i <- rep(0,Z)
  total_area <- sum(S_i)
  l <- rep(0,Z)
  y_dimension <- rep(0,Z)
  x_dimension <- rep(0,Z)
  feasibility_run <- 1
  
  if (direction == 1){
    Max_depth <- max(0,D - v_i*2*Current_Bays)
    if (Max_depth != 0){
      for (i in 1:Current_Bays){
        if (Current_Bays_Number[i] == 1){
          l[s] <- (Max_depth/total_area)*Bays_area[i]
          n_i[s] <- min(30,ceiling(S_i[New_data_structure[s]]/l[s]))
          l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
          x_dimension[s] <- n_i[s]*w_i
          y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
          s <- s + 1
        }
        else {
          for (d in 1:Current_Bays_Number[i]){
            l[s] <- (Max_depth/total_area)*Bays_area[i]
            n_i[s] <- min(30,ceiling(S_i[New_data_structure[s]]/l[s]))
            l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
            x_dimension[s] <- n_i[s]*w_i
            y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
            s <- s + 1
          }
        }
      }
    } else {
      feasibility_run <- 0
    }
  }
  else {
     for (i in 1:Current_Bays){
      if (Current_Bays_Number[i] == 1){
        l[s] <- D - v_i*2
        n_i[s] <- min(30,ceiling(S_i[New_data_structure[s]]/l[s]))
        l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
        x_dimension[s] <- n_i[s]*w_i
        y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
        s <- s + 1
      }
      else{
        for (d in 1:Current_Bays_Number[i]){
          l[s] <- ((D - v_i*2*Current_Bays_Number[i])/Bays_area[i])*S_i[New_data_structure[s]] 
          n_i[s] <- min(30,ceiling(S_i[New_data_structure[s]]/l[s]))
          l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
          x_dimension[s] <- n_i[s]*w_i
          y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
          s <- s + 1
        }
      }
    }
  }
  return(list(n_i,l,x_dimension,y_dimension,feasibility_run))
}

## Function for determining the starting points ----
Startingpoints <- function(Solution,x_dimension,y_dimension,direction,Bays_number,Z){
    
    startingpoint_x <- c()
    startingpoint_y <- c()
    p = 1
    r = 1
    s = 1
    
    startingpoint_x[1] <- 0
    startingpoint_y[1] <- 0

    if (direction == 1){
      for (i in 2:Z){
        if (Solution[Z+(i-1)] == 0){
          p = p + 1
          startingpoint_x[p] <- startingpoint_x[p-1] + x_dimension[p-1]
          startingpoint_y[p] <- startingpoint_y[p-1]
        }
        else{
          r = r + 1 
          p = p + 1
          s = s + 1
          if (Solution[Z+(i-2)] == 0){
            startingpoint_x[p] <- 0 
            startingpoint_y[p] <- startingpoint_y[p-Bays_number[s-1]] + max(y_dimension[(p-Bays_number[s-1]):(p-1)])
          }
          else {
            startingpoint_x[p] <- 0 
            startingpoint_y[p] <- startingpoint_y[p-1] + y_dimension[(p-1)]
          }
        }
      }  
    } else{ 
      for (i in 2:Z){

          if (Solution[Z+(i-1)] == 0){
          p = p + 1
          startingpoint_x[p] <- startingpoint_x[p-1] 
          startingpoint_y[p] <- startingpoint_y[p-1] + y_dimension[p-1]
        }
        else{
          r = r + 1 
          p = p + 1
          s = s + 1
          if (Solution[Z+(i-2)] == 0){
            startingpoint_x[p] <- startingpoint_x[p-Bays_number[s-1]] + max(x_dimension[(p-Bays_number[s-1]):(p-1)]) 
            startingpoint_y[p] <- 0 
          }
          else {
            startingpoint_x[p] <- startingpoint_x[p-1] + x_dimension[(p-1)]
            startingpoint_y[p] <- 0
          }
        } 
      }
    }  
    return(list(startingpoint_x, startingpoint_y))
}

# CHECKING FEASIBLE SOLUTION ----

## Feasibility Function ----
feasibility_function <- function(W,D,Bays,Bays_number,x_dimension,y_dimension,direction){
  
  x_total <- 0
  y_total <- 0
  e = 0
  feasibility_check <- 0
  Bays_x <- c()
  Bays_y <- c()

  if (direction == 0){
    for (i in 1:Bays){
      if (Bays_number[i] > 1){
        e = e + 1
        Bays_y[i] <- sum(y_dimension[i:(e+Bays_number[i]-1)])
        Bays_x[i] = max(x_dimension[i:(e+Bays_number[i]-1)])
        e = e + Bays_number[i] - 1
      }
      else {
        e = e + 1
        Bays_y[i] <- y_dimension[e]
        Bays_x[i] <- x_dimension[e]
      }
    }
    x_total = sum(Bays_x)
    y_total = max(Bays_y)
  }
  else {
    for (i in 1:Bays){
      if (Bays_number[i] > 1){
        e = e + 1
        Bays_y[i] <- max(y_dimension[i:(e+Bays_number[i]-1)])
        y_total = y_total + Bays_y[i]
        Bays_x[i] = sum(x_dimension[i:(e+Bays_number[i]-1)])
        e = e + Bays_number[i] - 1
      }
      else {
        e = e + 1
        Bays_y[i] <- y_dimension[e]
        y_total = y_total + y_dimension[e]
        Bays_x[i] = x_dimension[e]
      }
    }
    x_total = max(Bays_x)  
  }
  
  if (x_total <= W & y_total <= D){
    feasibility_check <- 1
  }
  return(feasibility_check)
}

## Graphical representation function ----
visualize <- function(W,D,startingpoint_x,x_dimension,startingpoint_y,y_dimension,Solution,Sol){
  plot(NULL, axes = FALSE, xlab = "", ylab = "", xlim = c(0, (W*1.1)), ylim = c(0, (D*1.1))) + 
    rect(xleft = 0, xright = W, ybottom = 0, ytop = D, border = "red", lwd = 2) 
  for (i in 1:Z){
    rect(xleft = startingpoint_x[i], xright = (startingpoint_x[i]+x_dimension[i]), ybottom = startingpoint_y[i], ytop = (startingpoint_y[i]+y_dimension[i]))
    center <- c(mean(c(startingpoint_x[i], (startingpoint_x[i]+x_dimension[i]))), mean(c(startingpoint_y[i], (startingpoint_y[i]+y_dimension[i]))))
    text(center[1], center[2], labels = Solution[i])
  }
  
}

# CALCULATE INITIAL SOLUTION ----

## Horizontal initial solution ----
Function_Initial_solution_horizontal <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i){
  # Feasible solution input
  run <- 0
  feasibility <- 0
  Instance <- c("")
  
  ## Repeat until feasibly starting solution
  while (feasibility != 1){
    
    ## Generate initial Solution
    Solution <- generate_initial_data_structure(Z)
    
    ## Determine the direction of the bay structure
    direction <- 1
    Solution[4*Z] <- direction
    
    ## Calculate the area of each bay
    Bays <- sum(Solution[(Z+1):(Z+Z-1)]) + 1
    Bay_area_results <- function_bay_area(Bays,S_i,Solution,Z)
    Bays_number <- Bay_area_results[[1]]
    Bays_area   <- Bay_area_results[[2]]
    
    ## Calculate the dimensions for each department
    Dimension_results <- function_number_of_aisles2(direction,D,v_i,Bays,Bays_number,Bays_area,S_i,Z,Solution,w_i)
    if (Dimension_results[[5]] != 0){
      n_i <- Dimension_results[[1]]
      Solution[(Z + (Z-1) + Z + 1):(Z + (Z-1) + Z + Z)] <- n_i
      l <- Dimension_results[[2]]
      x_dimension <- Dimension_results[[3]]
      y_dimension <- Dimension_results[[4]]
      
      ## Calculate Starting points
      Starting_point_results <- Startingpoints(Solution,x_dimension,y_dimension,direction,Bays_number,Z) 
      startingpoint_x <- Starting_point_results[[1]]
      startingpoint_y <- Starting_point_results[[2]]
      
      ## Feasibility Check
      feasibility <- feasibility_function(W,D,Bays,Bays_number,x_dimension,y_dimension,direction) 
    } else{
      feasibility <- 0
    }
    
    run <- run + 1
    
    if (run > 100000){
      Instance <- "difficult"
      break
    } 
  }
  
  if (Instance == "difficult"){
    
    while (feasibility != 1){
      
      run <- run + 1
      
      if (run > 200000){
        Objective <- -1
        break
      }
    }
    
    
  }
  
  if (feasibility == 1){
    # Calculation of Objective function
    Best_Objective_Complete <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, Solution, x_dimension, y_dimension)
    
    # Store best solution
    Best_Data_Structure      <- Best_Objective_Complete$Solution
    Best_Objective_Complete
    
    return(list(Best_Objective_Complete,Best_Data_Structure,direction))
  } else{
    n_obj <- Objective
    return(list(list(n_obj)))
  }
}

## Vertical Initial Solution ----
Function_Initial_solution_vertical <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i){
  # Feasible solution input
  run <- 0
  feasibility <- 0
  Instance <- c("")
  
  ## Repeat until feasibly starting solution
  while (feasibility != 1){
    
    ## Generate initial Solution
    Solution <- generate_initial_data_structure(Z)
    
    ## Determine the direction of the bay structure
    direction <- 0
    Solution[4*Z] <- direction
    
    ## Calculate the area of each bay
    Bays <- sum(Solution[(Z+1):(Z+Z-1)]) + 1
    Bay_area_results <- function_bay_area(Bays,S_i,Solution,Z)
    Bays_number <- Bay_area_results[[1]]
    Bays_area   <- Bay_area_results[[2]]
    
    
    ## Calculate the dimensions for each department
    Dimension_results <- function_number_of_aisles2(direction,D,v_i,Bays,Bays_number,Bays_area,S_i,Z,Solution,w_i)
    
    if (Dimension_results[[5]] != 0){
      n_i <- Dimension_results[[1]]
      Solution[(Z + (Z-1) + Z + 1):(Z + (Z-1) + Z + Z)] <- n_i
      l <- Dimension_results[[2]]
      x_dimension <- Dimension_results[[3]]
      y_dimension <- Dimension_results[[4]]
      
      ## Calculate Starting points
      Starting_point_results <- Startingpoints(Solution,x_dimension,y_dimension,direction,Bays_number,Z) 
      startingpoint_x <- Starting_point_results[[1]]
      startingpoint_y <- Starting_point_results[[2]]
      
      ## Feasibility Check
      feasibility <- feasibility_function(W,D,Bays,Bays_number,x_dimension,y_dimension,direction) 
    } else{
      feasibility <- 0
    }
    
    run <- run + 1
    
    if (run > 100000){
      Instance <- "difficult"
      break
    } 
  }
  
  if (Instance == "difficult"){
    
    while (feasibility != 1){
      
      run <- run + 1
      
      if (run > 200000){
        Objective <- -1
        break
      }
    }
    
    
  }
  
  if (feasibility == 1){
    # Calculation of Objective function
    Best_Objective_Complete <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, Solution, x_dimension, y_dimension)
    
    # Store best solution
    Best_Data_Structure      <- Best_Objective_Complete$Solution
    Best_Objective_Complete
    
    return(list(Best_Objective_Complete,Best_Data_Structure,direction))
  } else{
    n_obj <- Objective
    return(list(list(n_obj)))
  }
}




# OBJECTIVE FUNCTION ----
## Function for calculating T_i ----
Ti_fun <- function(Z, g, h, f, U, w_i, v_i, Matrixab){
  
  Matrixab <- as.data.frame(Matrixab)
  
  T_i <- rep(NA, Z)
  for(i in 1:Z){
    T_im <- 0
    for (m in 1:30){
      T_im <- T_im + as.numeric(U[i,m])*(Matrixab[i,2]*g[Matrixab[i,3],(Matrixab[i,6]-1),m] + w_i*h[Matrixab[i,3],(Matrixab[i,6]-1),m] + v_i*f[Matrixab[i,3],(Matrixab[i,6]-1),m])
    }
    T_i[i] <- T_im
  }
  return(T_i = T_i)
}

## Function for calculating R_i ----
Ri_fun <- function(Z, g, h, f, U, w_i, v_i, alpha_i, Matrixab){
  
  R_i <- rep(NA, Z)
  
  for(i in 1:Z){
    
    R_i[i] <- alpha_i[i]*(Matrixab[i,2]*g[Matrixab[i,3],(Matrixab[i,6]-1),1] + w_i*h[Matrixab[i,3],(Matrixab[i,6]-1),1] + v_i*f[Matrixab[i,3],(Matrixab[i,6]-1),1])  
    
  }
  
  return(R_i = R_i)
}

## Function for calculating U_i ----
Ui_fun <- function(Z, alpha_i, Matrixab){
  
  U_i <- rep(NA, Z)
  
  for(i in 1:Z){
    U_i[i] <- 2*Matrixab[i,4]*(1 + alpha_i[i])
  }
  
  return(U_i = U_i)
}

## Objective function ----
obj_fun1 <- function(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, Solution, x_dimension, y_dimension){
  
  # Put it in right order
  Solutionpart <- Solution[1:Z]
  k_i <- Solution[(2*Z):(3*Z-1)]
  Matrixa <- cbind(Solutionpart,l,n_i,startingpoint_y,startingpoint_x,k_i,x_dimension, y_dimension)
  Matrixab <- Matrixa[order(Matrixa[,1]),]
  
  T_i <- Ti_fun(Z, g, h, f, U, w_i, v_i, Matrixab = Matrixab)
  R_i <- Ri_fun(Z, g, h, f, U, w_i, v_i, alpha_i, Matrixab = Matrixab)
  U_i <- Ui_fun(Z, alpha_i, Matrixab = Matrixab)
  
  ## Combine
  n_obj <- sum(T_i) + sum(R_i) + sum(U_i)
  Objective <- rbind(Matrixab[,1], T_i, R_i, U_i)
  
  Results <- list(Solution = Solution, Out = Matrixab, Objective = Objective, n_obj = n_obj)
  
  return(Results)
}

# COMPARE RESULTS ----
## Function for Objective value comparison ----
comp_obj <- function(Current_Objective_Summary, Objective_Summary){
  
  a <- as.numeric(Current_Objective_Summary[[4]])
  b <- as.numeric(Objective_Summary[4])
  NEW_result <- 0
  
  if(a >= b){
    NEW_result <- 1
    return(NEW_result)
  }
  else{
    return(NEW_result)
  }
}


# HEURISTIC METHOD ----

# Destrow & Reapair method ----

## Department swap ----
# Destroy Heuristic Random
Destroy_Department <- function(Z){
  
  Number_Delete <- sample(2:Z,1)
  Position_Department_Delete <- sample(1:Z,Number_Delete, replace = FALSE)
  
  return(Position_Department_Delete)
}

# Repair Heuristic Random Insertion
Repair_Department <- function(Position_Department_Delete,Current_data_structure){
  
  Current_data_structure[Position_Department_Delete] <- sample(Current_data_structure[Position_Department_Delete])
  
  return(Current_data_structure)
}


## Department Neighborhood ----
# Destroy
Destroy_Department2 <- function(Z){
  
  Center_Delete <- sample(1:Z,1)
  Neighboor_Delete <- sample(1:(floor((Z-1)/2)),1)
  
  if (Center_Delete == 1){
    Position_Department_Delete <- c(1:(1+Neighboor_Delete))
  } else if (Center_Delete == Z){
    Position_Department_Delete <- c((Z-Neighboor_Delete):Z)
  } else {
    Position_Department_Delete <- c((max(1,Center_Delete-Neighboor_Delete)):(min(Center_Delete+Neighboor_Delete,Z)))
  }
  return(Position_Department_Delete)
}    


## Worst department removal ----
# Destroy
Destroy_Department3 <- function(Z,Current_Objective_Summary){
  
  # Calculate costs
  Costs <- matrix(data = NA, nrow = 2, ncol = Z)
  Z <- 4
  for (i in 1:Z){
    Costs[1,i] <- i
    Costs[2,i] <- sum(Current_Objective_Summary$Objective[2:4,i]) 
  }
  
  # Order the matrix
  Descending_order <- Costs[,order(-Costs[2,])]
  
  # Sample from min(Z,5) most expensive departments
  a <- min(Z, 5)
  
  # Sample how many of those most expensive departments has to be removed
  b <- sample(1:a, 1)
  
  # Sample which department has to be removed
  c <- sample(1:a, b)
  
  which_departments <- c()
  for (i in 1:length(c)){
    which_departments[i] <- Descending_order[1,c[i]]  
  }
  
  # CHECK WHICH POSITION OF DATA STRUCTURE THOSE DEPARTMENTS ARE
  Position_Department_Delete <- c()
  
  
  return(Position_Department_Delete)
}


## Bay Break Swap ----
# Destroy 
Destroy_Bay <- function(Z){
  
  Number_Delete <- sample(1:(Z-1),1)
  Position_Department_Delete <- sample((Z+1):(Z+Z-1),Number_Delete, replace = FALSE)
  
  return(Position_Department_Delete)
}

## Bay Neighborhood ----
Destroy_Bay2 <- function(Z){
  
  Center_Delete <- sample((Z+1):(Z+Z-1),1)
  Neighboor_Delete <- sample(1:(floor((Z-1)/2)),1)
  
  if (Center_Delete == (Z+1)){
    Position_Department_Delete <- c((Z+1):((Z+1)+Neighboor_Delete))
  } else if (Center_Delete == Z){
    Position_Department_Delete <- c(((Z+Z-1)-Neighboor_Delete):(Z+Z-1))
  } else {
    Position_Department_Delete <- c((max((Z+1),Center_Delete-Neighboor_Delete)):(min(Center_Delete+Neighboor_Delete,(Z+Z-1))))
  }
  return(Position_Department_Delete)
}  

# Repair 
# Heuristic Bay Break Interchange 0 --> 1 and viseversa
Repair_Bay <- function(Position_Department_Delete,Current_data_structure){
  
  for (i in 1:length(Position_Department_Delete)){
    if(Current_data_structure[Position_Department_Delete[i]] == 1){
      Current_data_structure[Position_Department_Delete[i]] <- 0
    } else{
      Current_data_structure[Position_Department_Delete[i]] <- 1
    }
  }
  return(Current_data_structure)
}


## Changing Cross-Aisles ----
# Destroy
destroy_cross <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction){
  
  result_cross <- c(0, Current_Objective_Summary$n_obj,Current_data_structure)
  
  r <- c(rep(1,Z),rep(-1,Z))
  d <- c(1:Z,1:Z)
  
  for(j in 1:(2*Z)){
    
    New_data_structure <- Current_data_structure
    
    New_data_structure[Z+(Z-1)+d[j]] <- Current_data_structure[Z+(Z-1)+d[j]] + r[j]
    New_data_structure[Z+(Z-1)+d[j]] <- max(New_data_structure[Z+(Z-1)+d[j]], 2)
    New_data_structure[Z+(Z-1)+d[j]] <- min(New_data_structure[Z+(Z-1)+d[j]], 10)
    
    ## Calculate the area of each bay
    Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
    Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
    Current_Bays_Number <- Bay_area_results[[1]]
    Bays_area   <- Bay_area_results[[2]]
    n_i <- New_data_structure[(Z+(Z)+Z):(Z+(Z-1)+(2*Z))]
    
    ## Calculate the dimensions for each department
    Dimension_results <- function_number_of_aisles3(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i)
    l <- Dimension_results[[1]]
    x_dimension <- Dimension_results[[2]]
    y_dimension <- Dimension_results[[3]]
    
    ## Calculate Starting points
    Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
    startingpoint_x <- Starting_point_results[[1]]
    startingpoint_y <- Starting_point_results[[2]]
    
    ## Calculate Objective value if feasible
    feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
    
    
    if (feasibility == 1){
      Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
      result_cross <- rbind(result_cross, c(d[j], Objective_Summary$n_obj, New_data_structure)) 
    }
  }
  
  result_cross
  if (length(result_cross) == (Z+(Z-1)+Z+Z+1+1)){
    return(result_cross[3:((Z + (Z-1) + Z + Z) + 2)])
  } else {
    
    mat_cross <- result_cross[order(result_cross[,1]),]
    a <- min(nrow(result_cross), 5)
    b <- sample(1:a, 1)
    mat_cross <- mat_cross[b,]
    
    New_data_structure2 = mat_cross[3:((Z + (Z-1) + Z + Z) + 3)]
    position_change = mat_cross[1] 
    
    return(list(New_data_structure2, position_change))}
}

# Repair cross
repair_cross <- function(New_data_structure2, position_change){
  if(position_change == 0){
    return(New_data_structure2)
  } else {
    a <- sample(1:3, 1)
    if(a == 1){
      New_data_structure2[Z+(Z-1)+position_change] = New_data_structure2[Z+(Z-1)+position_change] + 1
      New_data_structure2[Z+(Z-1)+position_change] = max(New_data_structure2[Z+(Z-1)+position_change], 2)
      New_data_structure2[Z+(Z-1)+position_change] = min(New_data_structure2[Z+(Z-1)+position_change], 10)
    } else if(a == 2){
      New_data_structure2[Z+(Z-1)+position_change] = New_data_structure2[Z+(Z-1)+position_change] - 1
      New_data_structure2[Z+(Z-1)+position_change] = max(New_data_structure2[Z+(Z-1)+position_change], 2)
      New_data_structure2[Z+(Z-1)+position_change] = min(New_data_structure2[Z+(Z-1)+position_change], 10)
    } else if(a == 2){
      New_data_structure2[Z+(Z-1)+position_change] = New_data_structure2[Z+(Z-1)+position_change] + 0
      New_data_structure2[Z+(Z-1)+position_change] = max(New_data_structure2[Z+(Z-1)+position_change], 2)
      New_data_structure2[Z+(Z-1)+position_change] = min(New_data_structure2[Z+(Z-1)+position_change], 10)
    }
    return(New_data_structure2)
  }
}

#destroy_cross(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction)

## Changing number of aisles ----
# Destroy aisles
destroy_aisles <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction){
  
  result_aisle <- c(0,Current_Objective_Summary$n_obj,Current_data_structure)
  
  r <- c(rep(1,Z),rep(-1,Z))
  d <- c(1:Z,1:Z)
  
  for(j in 1:(2*Z)){
    New_data_structure <- Current_data_structure
    
    New_data_structure[Z+(Z-1)+Z+d[j]] <- Current_data_structure[Z+(Z-1)+Z+d[j]] + r[j]
    New_data_structure[Z+(Z-1)+Z+d[j]] <- max(New_data_structure[Z+(Z-1)+Z+d[j]], 1)
    New_data_structure[Z+(Z-1)+Z+d[j]] <- min(New_data_structure[Z+(Z-1)+Z+d[j]], 30)
    
    ## Calculate the area of each bay
    Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
    Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
    Current_Bays_Number <- Bay_area_results[[1]]
    Bays_area   <- Bay_area_results[[2]]
    n_i <- New_data_structure[(Z+(Z)+Z):(Z+(Z-1)+(2*Z))]
    
    ## Calculate the dimensions for each department
    Dimension_results <- function_number_of_aisles3(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i)
    l <- Dimension_results[[1]]
    x_dimension <- Dimension_results[[2]]
    y_dimension <- Dimension_results[[3]]
    
    ## Calculate Starting points
    Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
    startingpoint_x <- Starting_point_results[[1]]
    startingpoint_y <- Starting_point_results[[2]]
    
    feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
    
    if (feasibility == 1){
      Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
      result_aisle <- rbind(result_aisle, c(d[j],Objective_Summary$n_obj, New_data_structure)) 
    }
  }
  
  if (length(result_aisle) == (Z+(Z-1)+Z+Z+2)){
    return(result_aisle[2:((Z + (Z-1) + Z + Z) + 2)])
  } else {
    
    mat_aisle <- result_aisle[order(result_aisle[,1]),]
    a <- min(nrow(mat_aisle), 5)
    b <- sample(1:a, 1)
    mat_aisle <- mat_aisle[b,]
    position_change = mat_aisle[1]
    
    New_data_structure2 = mat_aisle[3:((Z + (Z-1) + Z + Z) + 3)]
    
    return(list(New_data_structure2, position_change))}
}

# Repair aisles
repair_aisles <- function(New_data_structure2, position_change){
  if(position_change == 0){
    return(New_data_structure2)
  } else {
    a <- sample(1:3, 1)
    if(a == 1){
      New_data_structure2[Z+(Z-1)+Z+position_change] = New_data_structure2[Z+(Z-1)+Z+position_change] + 1
      New_data_structure2[Z+(Z-1)+Z+position_change] = max(New_data_structure2[Z+(Z-1)+Z+position_change], 1)
      New_data_structure2[Z+(Z-1)+Z+position_change] = min(New_data_structure2[Z+(Z-1)+Z+position_change], 30)
    } else if(a == 2){
      New_data_structure2[Z+(Z-1)+Z+position_change] = New_data_structure2[Z+(Z-1)+Z+position_change] - 1
      New_data_structure2[Z+(Z-1)+Z+position_change] = max(New_data_structure2[Z+(Z-1)+Z+position_change], 1)
      New_data_structure2[Z+(Z-1)+position_change] = min(New_data_structure2[Z+(Z-1)+position_change], 30)
    } else if(a == 2){
      New_data_structure2[Z+(Z-1)+Z+position_change] = New_data_structure2[Z+(Z-1)+Z+position_change] + 0
      New_data_structure2[Z+(Z-1)+Z+position_change] = max(New_data_structure2[Z+(Z-1)+Z+position_change], 1)
      New_data_structure2[Z+(Z-1)+Z+position_change] = min(New_data_structure2[Z+(Z-1)+Z+position_change], 30)
    }
    return(New_data_structure2)
  }
}

# FUNCTION HEURISTIC ----

## Feasibility calculation function for Heuristics ----
Feasibility_calculation_function <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, New_data_structure, Current_Objective_Summary, direction){
  
  ## Calculate the area of each bay
  Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
  Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
  Current_Bays_Number <- Bay_area_results[[1]]
  Bays_area   <- Bay_area_results[[2]]
  
  ## Calculate the dimensions for each department
  Dimension_results <- function_number_of_aisles2(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i)
  n_i <- Dimension_results[[1]]
  New_data_structure[(Z + (Z-1) + Z + 1):(Z + (Z-1) + Z + Z)] <- n_i
  l <- Dimension_results[[2]]
  x_dimension <- Dimension_results[[3]]
  y_dimension <- Dimension_results[[4]]
  
  ## Calculate Starting points
  Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
  startingpoint_x <- Starting_point_results[[1]]
  startingpoint_y <- Starting_point_results[[2]]
  
  ## Feasibility Check
  feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
  
  ## dep overlap
  overlap <- dep_overlap(New_data_structure, startingpoint_x, startingpoint_y, x_dimension, y_dimension, Z)
  
  ## Calculate Objective value if feasible
  if (feasibility == 1 && overlap == 1){
    Current_Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, 
                                          startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
  }
  return(Current_Objective_Summary)
}



## Function Heuristic ----

function_heuristic2 <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, 
                                Current_data_structure, Current_Objective_Summary, direction, heu_tab, heuristic_sample){
  
  ## ------- Generate Destroy Heuristic -------
  Destroy_Heuristsic <- sample(1:6, 1, prob = heuristic_sample)  
  
  ## ------- Generate Repair Heuristic, exclude impossible combinations -------
  if (Destroy_Heuristsic == 1){
    Repair_Heuristsic <- 1
  } else if (Destroy_Heuristsic == 2){
    Repair_Heuristsic <- 1
  } else if (Destroy_Heuristsic == 3){
    Repair_Heuristsic <- 2
  } else if (Destroy_Heuristsic == 4){
    Repair_Heuristsic <- 2
  } else if (Destroy_Heuristsic == 5) {
    Repair_Heuristsic <- 3
  } else if (Destroy_Heuristsic == 6){
    Repair_Heuristsic <- 4
  }
  Position_Department_Delete <- c()
  
  ## ------- DESTROY HEURISTICS ---------
  
  if (Destroy_Heuristsic == 1){ # Random removal of X departments
    Position_Department_Delete              <- Destroy_Department(Z) 
    heu_tab[1] <- heu_tab[1]  + 1
  } else if (Destroy_Heuristsic == 2){ # Cluster removal of X departments
    Position_Department_Delete              <- Destroy_Department2(Z)
    heu_tab[2] <- heu_tab[2]  + 1
  } else if (Destroy_Heuristsic == 3){ # Random removal of X bays
    Position_Department_Delete              <- Destroy_Bay(Z)
    heu_tab[3] <- heu_tab[3]  + 1
  } else if (Destroy_Heuristsic == 4){ # Cluster removal of X bays
    Position_Department_Delete              <- Destroy_Bay2(Z)
    heu_tab[4] <- heu_tab[4]  + 1
  } else if (Destroy_Heuristsic == 5){ # New data, Change cross-aisles
    Cross_change_info <- destroy_cross(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, 
                                                       Current_data_structure, Current_Objective_Summary, direction)
    heu_tab[5] <- heu_tab[5]  + 1
  } else if (Destroy_Heuristsic == 6){ # Change aisles
    Aisles_Change_info <- destroy_aisles(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, 
                                                     Current_data_structure, Current_Objective_Summary, direction)
    heu_tab[6] <- heu_tab[6]  + 1
  }
  
  
  
  ## ------- REPAIR HEURISTICS ---------
  
  if (Repair_Heuristsic == 1){ # Random repair of departments
    New_data_structure                  <- Repair_Department(Position_Department_Delete,Current_data_structure)
    heu_tab[7] <- heu_tab[7]  + 1
  } else if (Repair_Heuristsic == 2){ # Interchange the bays from 0 to 1 or visaversa
    New_data_structure                  <- Repair_Bay(Position_Department_Delete,Current_data_structure)
    heu_tab[8] <- heu_tab[8]  + 1
  } else if (Repair_Heuristsic == 3){ # Repair cross
    New_data_structure                  <- repair_cross(Cross_change_info[[1]],Cross_change_info[[2]])
    heu_tab[9] <- heu_tab[9]  + 1
  } else if (Repair_Heuristsic == 4){
    New_data_structure                  <- repair_aisles(Aisles_Change_info[[1]],Aisles_Change_info[[2]])
    heu_tab[10] <- heu_tab[10]  + 1
  }
  
  
  ## ------- Calculate Feasibility --------
  if (Destroy_Heuristsic <= 6){
    Current_Objective_Summary <- Feasibility_calculation_function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, 
                                                                  New_data_structure, Current_Objective_Summary, direction)
  }
  return(list(Current_Objective_Summary,heu_tab,Destroy_Heuristsic,Repair_Heuristsic))
}


# function_heuristic2(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, 
                    #Current_data_structure, Current_Objective_Summary, direction, heu_tab, heuristic_sample)



# # Change the number of cross-aisles 
# function_cross <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction){
#   
#   result_cross <- c(Current_Objective_Summary$n_obj,Current_data_structure)
#   
#   r <- c(rep(1,Z),rep(-1,Z))
#   d <- c(1:Z,1:Z)
#   
#   for(j in 1:(2*Z)){
#     
#     New_data_structure <- Current_data_structure
#     
#     New_data_structure[Z+(Z-1)+d[j]] <- Current_data_structure[Z+(Z-1)+d[j]] + r[j]
#     New_data_structure[Z+(Z-1)+d[j]] <- max(New_data_structure[Z+(Z-1)+d[j]], 2)
#     New_data_structure[Z+(Z-1)+d[j]] <- min(New_data_structure[Z+(Z-1)+d[j]], 10)
#     
#     ## Calculate the area of each bay
#     Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
#     Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
#     Current_Bays_Number <- Bay_area_results[[1]]
#     Bays_area   <- Bay_area_results[[2]]
#     n_i <- New_data_structure[(Z+(Z)+Z):(Z+(Z-1)+(2*Z))]
#     
#     ## Calculate the dimensions for each department
#     Dimension_results <- function_number_of_aisles3(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i)
#     l <- Dimension_results[[1]]
#     x_dimension <- Dimension_results[[2]]
#     y_dimension <- Dimension_results[[3]]
#     
#     ## Calculate Starting points
#     Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
#     startingpoint_x <- Starting_point_results[[1]]
#     startingpoint_y <- Starting_point_results[[2]]
#     
#     ## Calculate Objective value if feasible
#     feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
#     
#     
#     if (feasibility == 1){
#       Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
#       result_cross <- rbind(result_cross, c(Objective_Summary$n_obj, New_data_structure)) 
#     }
#   }
#   
#   if (length(result_cross) == (Z+(Z-1)+Z+Z+2)){
#      return(result_cross[2:((Z + (Z-1) + Z + Z) + 2)])
#   } else {
#   
#     mat_cross <- result_cross[order(result_cross[,1]),]
#     a <- min(nrow(result_cross), 5)
#     b <- sample(1:a, 1)
#     mat_cross <- mat_cross[b,]
#     
#     New_data_structure2 = mat_cross[2:((Z + (Z-1) + Z + Z) + 2)]
#     
#     return(New_data_structure2)}
# }

# Function_change_cross <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction){
#   ## Change cross-aisles procedure
#   New_data_structure <- function_cross(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction)
# 
#   ## Calculate the area of each bay
#   Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
#   Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
#   Current_Bays_Number <- Bay_area_results[[1]]
#   Bays_area   <- Bay_area_results[[2]]
#   n_i <- New_data_structure[(Z+(Z)+Z):(Z+(Z-1)+(2*Z))]
#   
#   ## Calculate the dimensions for each department
#   Dimension_results <- function_number_of_aisles3(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i)
#   l <- Dimension_results[[1]]
#   x_dimension <- Dimension_results[[2]]
#   y_dimension <- Dimension_results[[3]]
#   
#   ## Calculate Starting points
#   Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
#   startingpoint_x <- Starting_point_results[[1]]
#   startingpoint_y <- Starting_point_results[[2]]
#   
#   ## Calculate Objective value if feasible
#   feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
#   
#   if (feasibility == 1){
#     Current_Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
#   }
#   return(Current_Objective_Summary)
# }

add_n_i <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction){
  
  result_aisle <- c(Current_Objective_Summary$n_obj,Current_data_structure)
  
  r <- c(rep(1,Z),rep(-1,Z))
  d <- c(1:Z,1:Z)

  for(j in 1:(2*Z)){
    New_data_structure <- Current_data_structure
    
    New_data_structure[Z+(Z-1)+Z+d[j]] <- Current_data_structure[Z+(Z-1)+Z+d[j]] + r[j]
    New_data_structure[Z+(Z-1)+Z+d[j]] <- max(New_data_structure[Z+(Z-1)+Z+d[j]], 1)
    New_data_structure[Z+(Z-1)+Z+d[j]] <- min(New_data_structure[Z+(Z-1)+Z+d[j]], 30)
    
    ## Calculate the area of each bay
    Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
    Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
    Current_Bays_Number <- Bay_area_results[[1]]
    Bays_area   <- Bay_area_results[[2]]
    n_i <- New_data_structure[(Z+(Z)+Z):(Z+(Z-1)+(2*Z))]
    
    ## Calculate the dimensions for each department
    Dimension_results <- function_number_of_aisles3(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i)
    l <- Dimension_results[[1]]
    x_dimension <- Dimension_results[[2]]
    y_dimension <- Dimension_results[[3]]
    
    ## Calculate Starting points
    Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
    startingpoint_x <- Starting_point_results[[1]]
    startingpoint_y <- Starting_point_results[[2]]
    
    feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
    
    if (feasibility == 1){
      Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
      result_aisle <- rbind(result_aisle, c(Objective_Summary$n_obj, New_data_structure)) 
    }
  }
  
  if (length(result_aisle) == (Z+(Z-1)+Z+Z+2)){
    return(result_aisle[2:((Z + (Z-1) + Z + Z) + 2)])
  } else {
    
    mat_aisle <- result_aisle[order(result_aisle[,1]),]
    a <- min(nrow(mat_aisle), 5)
    b <- sample(1:a, 1)
    mat_aisle <- mat_aisle[b,]
    
    New_data_structure2 = mat_aisle[2:((Z + (Z-1) + Z + Z) + 2)]
    
    return(New_data_structure2)}
}

# Function_change_n_i <- function(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction){
#   
#   ## Change aisles procedure
#   New_data_structure <- add_n_i(Z, g, h, f, U, w_i, v_i, D, W, alpha_i, S_i, Current_data_structure, Current_Objective_Summary, direction)
#   
#   ## Calculate the area of each bay
#   Current_Bays <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
#   Bay_area_results <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)
#   Current_Bays_Number <- Bay_area_results[[1]]
#   Bays_area   <- Bay_area_results[[2]]
#   n_i <- New_data_structure[(Z+(Z)+Z):(Z+(Z-1)+(2*Z))]
#   
#   ## Calculate the dimensions for each department
#   Dimension_results <- function_number_of_aisles3(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i)
#   l <- Dimension_results[[1]]
#   x_dimension <- Dimension_results[[2]]
#   y_dimension <- Dimension_results[[3]]
#   
#   ## Calculate Starting points
#   Starting_point_results <- Startingpoints(New_data_structure,x_dimension,y_dimension,direction,Current_Bays_Number,Z) 
#   startingpoint_x <- Starting_point_results[[1]]
#   startingpoint_y <- Starting_point_results[[2]]
#   
#   ## Calculate Objective value if feasible
#   feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
#   
#   if (feasibility == 1){
#     Current_Objective_Summary <- obj_fun1(Z, g, h, f, U, n_i, w_i, v_i, alpha_i, l, startingpoint_y, startingpoint_x, New_data_structure, x_dimension, y_dimension)
#   }
#   return(Current_Objective_Summary)
# }

# Calculating the number aisles, length, x-dimensions, and y-dimensions
function_number_of_aisles3 <- function(direction,D,v_i,Current_Bays,Current_Bays_Number,Bays_area,S_i,Z,New_data_structure,w_i,n_i){
    s <- 1
  total_area <- sum(S_i)
  l <- rep(0,Z)
  y_dimension <- rep(0,Z)
  x_dimension <- rep(0,Z)
  
  if (direction == 1){
    Max_depth <- D - v_i*2*Current_Bays
    for (i in 1:Current_Bays){
      if (Current_Bays_Number[i] == 1){
        l[s] <- (Max_depth/total_area)*Bays_area[i]
        l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
        x_dimension[s] <- n_i[s]*w_i
        y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
        s <- s + 1
      }
      else {
        for (d in 1:Current_Bays_Number[i]){
          l[s] <- (Max_depth/total_area)*Bays_area[i]
          l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
          x_dimension[s] <- n_i[s]*w_i
          y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
          s <- s + 1
        }
      }
    }
  }
  else {
    for (i in 1:Current_Bays){
      if (Current_Bays_Number[i] == 1){
        l[s] <- D - v_i*2
        l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
        x_dimension[s] <- n_i[s]*w_i
        y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
        s <- s + 1
      }
      else{
        for (d in 1:Current_Bays_Number[i]){
          l[s] <- ((D - v_i*2*Current_Bays_Number[i])/Bays_area[i])*S_i[New_data_structure[s]]
          l[s] <- ceiling_dec(S_i[New_data_structure[s]]/n_i[s],2)
          x_dimension[s] <- n_i[s]*w_i
          y_dimension[s] <- l[s] + v_i*New_data_structure[2*Z-1+s]
          s <- s + 1
        }
      }
    }
  }
  return(list(l,x_dimension,y_dimension))
}


# Heuristic Sample probabilities
update_probabilities <- function(Obj_prev,Current_Objective_Summary,Heuristic,heuristic_sample){
  
  epsilon <- 0.001
  
  if(abs(Obj_prev -  Current_Objective_Summary$n_obj) < epsilon){
    
    heuristic_sample[Heuristic] <- heuristic_sample[Heuristic] * 0.7
    s_pi <- sum(heuristic_sample)
    # normalize the probabilities
    heuristic_sample <- heuristic_sample/s_pi
    
    if(heuristic_sample[Heuristic] < 0.05){
      heuristic_sample[Heuristic] = 0.05
      s_pi <- sum(heuristic_sample)
      heuristic_sample <- heuristic_sample/s_pi
    }
    
  } else{
    
    heuristic_sample[Heuristic] <- heuristic_sample[Heuristic] * 1.3
    s_pi <- sum(heuristic_sample)
    # normalize the probabilities
    heuristic_sample <- heuristic_sample/s_pi
    
    if(heuristic_sample[Heuristic] > 0.8){
      heuristic_sample[Heuristic] = 0.8
      s_pi <- sum(heuristic_sample)
      heuristic_sample <- heuristic_sample/s_pi
    }
    
  }
  return(heuristic_sample)
}

# check if dep are overlap

dep_overlap <- function(New_data_structure, startingpoint_x, startingpoint_y, x_dimension, y_dimension, Z){
  over <- 0
  matrixab <- matrix(NA, nrow = Z, ncol = 5)
  for(i in 1:Z){
    matrixab[i,] <- c(New_data_structure[i], startingpoint_x[i], startingpoint_y[i], x_dimension[i], y_dimension[i])
  }
  c <- 0
  epsilon <- 0.000001
  for(i in 1:Z){
    b <- 2
    c <- cbind(c, i)
    second_x <- matrixab[i,2] + matrixab[i,4] 
    second_y <- matrixab[i,3] + matrixab[i,5] 
    
    a <- c(1:Z)
    a <- a[-c]
    
    for(j in a){
      if(second_x > matrixab[j,2] + epsilon && second_y > matrixab[j,3] + epsilon){
        over <- over + 1
      } 
    }
  }
  if(over == 0)
    return(1)
  else
    return(-1)
  
}










dep_overlap2 <- function(New_data_structure, startingpoint_x, startingpoint_y, x_dimension, y_dimension, Z){
  over <- 0
  matrixab <- matrix(NA, nrow = Z, ncol = 5)
  for(i in 1:Z){
    matrixab[i,] <- c(i,startingpoint_x[i], startingpoint_y[i], x_dimension[i], y_dimension[i])
  }
  matrixab <- matrixab[match(New_data_structure[1:Z],matrixab[,1]),]
  
  c <- 0
  epsilon <- 0.000001
  for(i in 1:Z){
    b <- 2
    c <- cbind(c, i)
    second_x <- matrixab[i,2] + matrixab[i,4] 
    second_y <- matrixab[i,3] + matrixab[i,5] 
    
    a <- c(1:Z)
    a <- a[-c]
    
    for(j in a){
      if(second_x > matrixab[j,2] + epsilon && second_y > matrixab[j,3] + epsilon){
        over <- over + 1
      } 
    }
  }
  if(over == 0)
    return(1)
  else
    return(-1)
  
}

feasibility_check <- function(Optimal_Objective_Summary_best,Z,W,D,S_i){
  
  New_data_structure      <- Optimal_Objective_Summary_best[[1]]
  startingpoint_x         <- Optimal_Objective_Summary_best[[2]][,5]
  startingpoint_y         <- Optimal_Objective_Summary_best[[2]][,4]
  x_dimension             <- Optimal_Objective_Summary_best[[2]][,7]
  y_dimension             <- Optimal_Objective_Summary_best[[2]][,8]
  direction               <- Optimal_Objective_Summary_best[[1]][4*Z]
  Current_Bays            <- sum(New_data_structure[(Z+1):(Z+Z-1)]) + 1
  Current_Bays_Number     <- function_bay_area(Current_Bays,S_i,New_data_structure,Z)[[1]]
  
  ## Feasibility Check
  feasibility <- feasibility_function(W,D,Current_Bays,Current_Bays_Number,x_dimension,y_dimension,direction) 
  
  ## dep overlap
  overlap <- dep_overlap2(New_data_structure, startingpoint_x, startingpoint_y, x_dimension, y_dimension, Z)
  
  ## Calculate Objective value if feasible
  if (feasibility == 1 && overlap == 1){
    print("Feasible")
  }
  else{
    print("Infeasible")
  }
}







