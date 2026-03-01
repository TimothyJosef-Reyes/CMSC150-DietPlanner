# Author: Timothy Josef A. Reyes
# Simplex Function - Generates the table shown to be shown to the user and ensures that data inputted is viable to be shown
# Update date: December 5, 2024

ComputeSimplex <- function(foods, nutrition){ # function that computes for the minimized cost
  result <- list() # list for resulting tables
  
  minCalories <- c() # vector for each constraint
  maxCalories <- c()
  minCholesterol <- c()
  maxCholesterol <- c()
  minTotalFat <- c()
  maxTotalFat <- c()
  minSodium <- c()
  maxSodium <- c()
  minCarbohydrates <- c()
  maxCarbohydrates <- c()
  minDietaryFiber <- c()
  maxDietaryFiber <- c()
  minProtein <- c()
  maxProtein <- c()
  minVitaminA <- c()
  maxVitaminA <- c()
  minVitaminC <- c()
  maxVitaminC <- c()
  minCalcium <- c()
  maxCalcium <- c()
  minIron <- c()
  maxIron <- c()
  totalCost <- c()
  
  for (i in foods){ # implementation of each constraint
    minCalories <- c(minCalories, unname(unlist(nutrition[i,'Calories'])))
    maxCalories <- c(maxCalories, -unname(unlist(nutrition[i,'Calories'])))
    minCholesterol <- c(minCholesterol, unname(unlist(nutrition[i,'Cholesterol mg'])))
    maxCholesterol <- c(maxCholesterol, -unname(unlist(nutrition[i,'Cholesterol mg'])))
    minTotalFat <- c(minTotalFat, unname(unlist(nutrition[i,'Total_Fat g'])))
    maxTotalFat <- c(maxTotalFat, -unname(unlist(nutrition[i,'Total_Fat g'])))
    minSodium <- c(minSodium, unname(unlist(nutrition[i,'Sodium mg'])))
    maxSodium <- c(maxSodium, -unname(unlist(nutrition[i,'Sodium mg'])))
    minCarbohydrates <- c(minCarbohydrates, unname(unlist(nutrition[i,'Carbohydrates g'])))
    maxCarbohydrates <- c(maxCarbohydrates, -unname(unlist(nutrition[i,'Carbohydrates g'])))
    minDietaryFiber <- c(minDietaryFiber, unname(unlist(nutrition[i,'Dietary_Fiber g'])))
    maxDietaryFiber <- c(maxDietaryFiber, -unname(unlist(nutrition[i,'Dietary_Fiber g'])))
    minProtein <- c(minProtein, unname(unlist(nutrition[i,'Protein g'])))
    maxProtein <- c(maxProtein, -unname(unlist(nutrition[i,'Protein g'])))
    minVitaminA <- c(minVitaminA, unname(unlist(nutrition[i,'Vit_A IU'])))
    maxVitaminA <- c(maxVitaminA, -unname(unlist(nutrition[i,'Vit_A IU'])))
    minVitaminC <- c(minVitaminC, unname(unlist(nutrition[i,'Vit_C IU'])))
    maxVitaminC <- c(maxVitaminC, -unname(unlist(nutrition[i,'Vit_C IU'])))
    minCalcium <- c(minCalcium, unname(unlist(nutrition[i,'Calcium mg'])))
    maxCalcium <- c(maxCalcium, -unname(unlist(nutrition[i,'Calcium mg'])))
    minIron <- c(minIron, unname(unlist(nutrition[i,'Iron mg'])))
    maxIron <- c(maxIron, -unname(unlist(nutrition[i,'Iron mg'])))
    totalCost <- c(totalCost, unname(unlist(nutrition[i,'Price/Serving'])))
  }
  
  # setup of tableau, minimization
  tableau <- matrix(minCalories, nrow = 1) 
  tableau <- rbind(tableau, maxCalories)
  tableau <- rbind(tableau, minCholesterol)
  tableau <- rbind(tableau, maxCholesterol)
  tableau <- rbind(tableau, minTotalFat)
  tableau <- rbind(tableau, maxTotalFat)
  tableau <- rbind(tableau, minSodium)
  tableau <- rbind(tableau, maxSodium)
  tableau <- rbind(tableau, minCarbohydrates)
  tableau <- rbind(tableau, maxCarbohydrates)
  tableau <- rbind(tableau, minDietaryFiber)
  tableau <- rbind(tableau, maxDietaryFiber)
  tableau <- rbind(tableau, minProtein)
  tableau <- rbind(tableau, maxProtein)
  tableau <- rbind(tableau, minVitaminA)
  tableau <- rbind(tableau, maxVitaminA)
  tableau <- rbind(tableau, minVitaminC)
  tableau <- rbind(tableau, maxVitaminC)
  tableau <- rbind(tableau, minCalcium)
  tableau <- rbind(tableau, maxCalcium)
  tableau <- rbind(tableau, minIron)
  tableau <- rbind(tableau, maxIron)
  tableau <- rbind(tableau, totalCost)
  tableau <- cbind(tableau, c(-2000,2250,0,300,0,65,0,2400,0,300,-25,100,-50,100,-5000,50000,-50,20000,-800,1600,-10,30,0))
  
  # addition of magnitude constraints (serving cannot be more than 10)
  for(i in 1:length(foods)){
    magnitude_constraints <- rep(0, ncol(tableau))
    magnitude_constraints[i] <- -1
    magnitude_constraints[ncol(tableau)]<- 10
    tableau <- rbind(tableau, magnitude_constraints)
  }
  
  # transpose tableau to turn into maximization problem
  tableau <- t(tableau)
  
  # add columns for the variables (foods)
  for(i in 1:(length(foods)+1)){
    variables <- rep(0, length(foods)+1)
    variables[i] <- 1
    tableau <- cbind(tableau, variables)
  }
  
  # changes the location of the answer/total_cost column
  add <- tableau[,23]
  tableau <- tableau[,-23]
  tableau <- cbind(tableau, add)
  
  # remove column and row names, to be adjusted
  colnames(tableau) <- NULL
  rownames(tableau) <- NULL
  
  # values that holds the ncol and nrow for the simplex computation
  n <- nrow(tableau) # number of rows in the tableau
  m <- ncol(tableau) # number of columns in the tableau
  
  # new column names (slack variables)
  column_names <- paste0("S", 1:(22+length(foods)))

  # for each variable
  for(num in foods){
    column_names <- c(column_names, nutrition$Foods[num])
  }

  # final columns for Total_cost and Answer column
  column_names <- c(column_names, "Total Cost")
  column_names <- c(column_names, "Answer")
  
  colnames(tableau) <- column_names
  
  # Start of simplex computation
  iteration <- 0 # counts the itarations
  while (TRUE) { 
    lowest <- 0 # finds the pivot column
    pivotcol <- -1
    for (i in 1:(m - 1)) {
      if (tableau[n, i] < lowest) {
        lowest <- tableau[n, i]
        pivotcol <- i
      }
    }
    
    # basic solutions are the last row of the tableau
    # vector for the variable (foods) values 
    solutions <- tableau[n, ]
    finalSolutions <- c(solutions[-c(m-1)])
    variable_values <- finalSolutions[(m-length(foods)-1):(m-1)]
      
    # matrix for the basic solution
    basic_solution <- matrix(tableau[n,], nrow = 1)
    colnames(basic_solution) <- column_names
    basic_solution <- basic_solution[, -m]
    basic_solution <- t(as.matrix(basic_solution))
    basic_solution[1,m-1] <- tableau[n,m]
    
    if (pivotcol == -1) { # if there is no more negative numbers at the last row
      diet <- data.frame( # data frame for holding all of the final data
        Food = character(length(foods)),    
        Servings = numeric(length(foods)), 
        Amount = character(length(foods)), 
        Cost = numeric(length(foods)),      
        stringsAsFactors = FALSE            
      )
      
      for(i in 1:length(foods)) { # loops to add each final value of each food
        name <- nutrition[foods[i], 'Foods']  
        serving <- round(nutrition[foods[i], 'Serving Size'] * variable_values[i], 2)
        amount <- paste(serving, nutrition[foods[i], 'Serving Type']) 
        cost <- round(nutrition[foods[i], 'Price/Serving'] * variable_values[i], 2) 
        cost <- paste("$", cost)
        
        diet[i, ] <- list(name, round(variable_values[i], 2), amount, cost)
      }
      
      # final row for the total cost
      diet[length(foods)+1, ] <- list(" ", " ", "Total Cost", paste("$", round(unname(tableau[n, m]),2)))
      
      # adds into the result the final tableau, final basic solution, and final solution
      result[["Final Tableau"]] <- round(tableau, 2)
      result[["Final Basic Solution"]] <- round(basic_solution, 2)
      result[["Final Solutions"]] <- diet
      return(result) # returns the result
    }
    
    if(iteration == 0){ # if the iterations is zero, first tableau is indicated as initial tableau
      result[["Initial Tableau"]] <- round(tableau, 2)
      result[["Initial Basic Solution"]] <- round(basic_solution, 2)
    } else { # else they are numbered based on the number of iterations
      result[[paste("Tableau Iteration", iteration)]] <- round(tableau, 2)
      result[[paste("Basic Solution", iteration)]] <- round(basic_solution, 2)
    }
    
    # finds the test ratios to find the pivot row 
    test_ratios <- tableau[1:(n - 1), m] / tableau[1:(n - 1), pivotcol]
    # converts any negative test_ratio into infinity for easier acquirement of the pivot row
    test_ratios[tableau[1:(n - 1), pivotcol] <= 0] <- Inf 
    pivotrow <- which.min(test_ratios)
    
    # if the pivot row is not viable
    if (is.infinite(test_ratios[pivotrow])) {
      return(NULL)
    }
    
    # normalizes the pivot row
    tableau[pivotrow, ] <- tableau[pivotrow, ] / tableau[pivotrow, pivotcol] 
      
    # calculates each row
    for (j in 1:n) {
      if (j != pivotrow) {
        tableau[j, ] <- tableau[j, ] - tableau[pivotrow, ] * tableau[j, pivotcol]
      }
    }
     # increments iterations
    iteration <- iteration + 1
  }
}