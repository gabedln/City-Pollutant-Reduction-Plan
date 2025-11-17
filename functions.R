NegativeCheck = function(tableau){ # function to check the last row of tableau
  for(i in 1:ncol(tableau)){ # iterates through every element in the last row of tableau
    if(tableau[nrow(tableau),i]<0){ # if detects 0, then
      return(TRUE) # returns TRUE meaning there is still negative
    }
  }
  return(FALSE) # else returns false
}

FindMin = function(c){ # finds the minimum index in a vector (in this case, the col)
  min_index = 1 # Initial index
  min = c[1] # Sets initial minimum to first element
  for(i in 1:(length(c)-1)){ # Iterates through every element in vector
    if(c[i]<min){
      min = c[i]
      min_index = i 
    }
  }
  return(min_index) # Returns minimum index 
}

FindPivotRow = function(pcol_index, tableau){ # finds the pivot row in the tableau by calculating TR
  MinTR = Inf # initialization for comparison purposes only
  prow_index = -1 # similar to FindMin
  for(i in 1:(nrow(tableau)-1)){ # iterates through every row in the tableau
    if(tableau[i,pcol_index]<=0){ # Only looks at the elements in the pivot column
      next # If value is less than or equal to 0 moves to next iteration
    }
    currentTR = tableau[i,ncol(tableau)] / tableau[i,pcol_index] # Computes for the TR
    
    if(currentTR<0){ # If negative, moves on to the next iteration
      next
    }
    if(currentTR<MinTR){ # Compares TR, if less than, then sets new index
      MinTR = currentTR
      prow_index= i
    }
  }
  return(prow_index) # Returns index
}

BasicSolutionChecker = function(current_column){ # Checks for basic solution in maximization
  nonzero_values = c() # Initializes vector for nonzero values
  index = 0
  for(i in 1:length(current_column)){
    if(current_column[i] != 0){ # If value does not equal to zero, add it to vector
      nonzero_values = c(nonzero_values, current_column[i])
      index = i
    }
  }
  if(length(nonzero_values)>1){ # If length of that vector is greater than 1,
    return(-1) # returns false
  } else { # Else
    return(index) # Returns True
  }
}

Simplex = function(tableau, isMax){ # tableau is matrix of initial, isMax deterimines whether max or min
  basic_solution_history = list()
  tableau_history = list()
  while(NegativeCheck(tableau)){ # While there is still negative in the last row
    basic_solution = c() # For basic solution of that iteration
    pcol_index = FindMin(tableau[nrow(tableau),]) # Uses FindMin function
    prow_index = FindPivotRow(pcol_index, tableau) # Uses FindPivotRow function
    if(prow_index==-1){
      return (NA)
    }
    tableau[prow_index,] = tableau[prow_index,] / tableau[prow_index,pcol_index] # normalizes pivot row
    for(i in 1:nrow(tableau)){ #elimination
      if(i==prow_index){
        next
      }
      tableau[i,] = tableau[i,] - (tableau[prow_index,]*tableau[i,pcol_index])
    }
    tableau_history[[length(tableau_history)+1]] = tableau
    
    if(isMax){ # Way of determining basic solution for maximization
      for(i in 1:(ncol(tableau)-1)){
        index = BasicSolutionChecker(tableau[,i])
        if(index!=-1){ #Iterates through every column, if that column returns True
          basic_solution = c(basic_solution, tableau[index, ncol(tableau)])
        } else {
          basic_solution = c(basic_solution, 0) # If false (meaning more than 1 nonzero), add 0 to basic solution vector
        }
      }
      basic_solution_history[[length(basic_solution_history)+1]] = basic_solution
    } 
    
    else { # If minimization
      for(i in 1:ncol(tableau)){ # iterates through columns
        if(i==(ncol(tableau)-1)){ # if at Z column, skips iteration
          next
        }
        basic_solution = c(basic_solution, tableau[nrow(tableau), i]) # Adds the element of last row, and column i to basic solution
      }
      basic_solution_history[[(length(basic_solution_history)+1)]] = basic_solution
    }
    
  } # end of while loop bracket
  basicSolution = basic_solution_history[[length(basic_solution_history)]]
  labeled_list = list(finalTableau = tableau, basicSolution = basicSolution, Z = basicSolution[length(basicSolution)], tableau_history = tableau_history, basic_solution_history = basic_solution_history) # initializes labeled list
  return(labeled_list)
}