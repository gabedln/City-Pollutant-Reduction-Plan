source("simplex.R") # Since simplex function is separated
source("dataset.R") # Gets the R file containing the dataset for the projects


options(digits=10) # Printing and troubleshooting 
options(scipen=999) # To avoid scientific notation (+e, etc.)

buildTableau = function(index){
  matrix_data = c()
  j = 3 # We're going to start at index 3.
  for(i in 1:length(targetMinimum[,2])){ # Looping for each pollutant
    for(k in index){ # For each choice
      matrix_data = c(matrix_data, reductionsPerUnit[k,j]) # Insert into matrix_data 
    }
    matrix_data = c(matrix_data, targetMinimum[,2][i]) # Lastly in that row, inserts the required target
    j = j+1
  }
  
  # Here, we are adding the constraints where each unit should not be over 20 units.
  constraints = c() 
  for(i in 1:length(index)){ 
    for(j in 1:( length(index) + 1)){
      if(i==j){ 
        constraints = c(constraints,-1)
      } else if(j==length(index)+1){
        constraints = c(constraints,-20)
      } else { constraints = c(constraints, 0)}
    }
  } # Basically all this block does is create an identity matrix, then on the last column adds -20

  
  matrix_data = c(matrix_data, constraints) # Adds this into matrix_data
  
  for(i in index){ # Adds final row for objective function
    matrix_data = c(matrix_data, reductionsPerUnit[i,2])
  }
  matrix_data = c(matrix_data, 0) # Adds 0 for objetive function (Normally this should be 1, but because we are going to transpose it and move from RHS to LHS, we set it immediately to 0)
  tableau = matrix(data=matrix_data, byrow=TRUE, ncol=length(index)+1) # Puts all of matrix_data into a matrix
  tableau = t(tableau) # Transposes the tableau
  tableau[nrow(tableau),] = tableau[nrow(tableau),] * -1 # Multiplies the objective function to -1
  
  # Slack variables
  slack = c() #
  for(i in 1:(length(index)+1)){
    for(j in 1:(length(index)+1)){
      if(i==j){
        slack = c(slack, 1)
      } 
      else { slack = c(slack, 0)} # Identity matrix for slack variables
    }
  }
  slack_mat = matrix(data=slack, byrow=TRUE,ncol=length(index)+1) 
  tableau = cbind(tableau[,1:(ncol(tableau)-1)], slack_mat, tableau[,ncol(tableau)]) # Puts it in between Solution column and S1 to SN
  
  col_names = c(
    paste0("S",1:(10+length(index))),
    paste0("x",1:length(index)),
    "Z",
    "Solution"
  )
  colnames(tableau) = col_names
  
  outputList = list(tableau=tableau, choices=index) # Puts all of this into a list, and returns outputList. Choices is important for getting final answers.
  
  return(outputList)
}

getTable = function(outputList){ # Gets the table of units and cost.
  tableau = outputList$tableau # Gets the tableau from buildTabeleau function
  result = Simplex(tableau, FALSE) # Does the simplex method on it
  choices = outputList$choices # Gets indices of choices
  
  if(is.na(result$Z)){ # If Z is NA, returns result from Simplex method. Result includes tableau history and basic solution history up until it is not possible to continue anymore.
    return (result) 
  }
  
  startingIndex = nrow(tableau) + 10 # Since the rows is n projects + 1 for objective, then this is our startingIndex of our iteration.
  basicSolution = result$basicSolution # Gets basic solution of the result
  
  projects = c()
  units = c()
  cost = c()
  
  for(i in startingIndex:(ncol(tableau)-1)){ # Loops from the first up until the end
    
    if(i == (ncol(tableau)-1)){ # If at the Z solution part, returns final cost string formatted accordingly.
      final_cost = paste("$", basicSolution[i], sep="")
      next
    }
    
    if(basicSolution[i]!=0){ # If it is not equal to 0, meaning it has a value = an answer.
      ind = choices[i-(10+length(choices))] 
      # i is the current Iteration that we're on, the reason why it is subtracted to 10+length of choices is because of the way it is indexed.
      
      projects = c(projects, reductionsPerUnit[ind,1])
      units = c(units, basicSolution[i])
      project_cost = basicSolution[i] * reductionsPerUnit[ind,2]
      project_cost_string = paste("$", project_cost, sep="")
      cost = c(cost, project_cost_string)
      # Adds everything to our vectors that is used for final solution, and the things we need for our final output table.
    }
  }
  
  final_table = data.frame( # Puts all of it into a dataframe.
    Project = projects,
    Units = units,
    Cost = cost
  )
  
  output = list(Z=result$Z,final_table=final_table,final_cost=final_cost, tableau_history = result$tableau_history, basic_solution_history = result$basic_solution_history)
  return(output)
}
