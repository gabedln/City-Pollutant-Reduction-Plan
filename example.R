source("project_draft.R")
source("functions.R")

# x1, x2, x3... xn = unknown variables are the units for each project selected
# constraint is target minimum, as well as x1, x2, x3 ... xn <= 20.
# since minimization, need itranspose bago lagyan ng slack variables
options(digits=10)

buildTableau = function(index){
  matrix_data = c()
  j = 3 # We're going to start at index 3.
  for(i in 1:length(targetMinimum[,2])){
    for(k in index){
      matrix_data = c(matrix_data, reductionsPerUnit[k,j])
    }
    matrix_data = c(matrix_data, targetMinimum[,2][i])
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
  }
  matrix_data = c(matrix_data, constraints)
  
  for(i in index){
    matrix_data = c(matrix_data, reductionsPerUnit[i,2])
  }
  matrix_data = c(matrix_data, 0) 
  tableau = matrix(data=matrix_data, byrow=TRUE, ncol=length(index)+1)
  print(tableau)
  tableau = t(tableau)
  tableau[nrow(tableau),] = tableau[nrow(tableau),] * -1
  
  
  # Slack variables
  slack = c()
  for(i in 1:(length(index)+1)){
    for(j in 1:(length(index)+1)){
      if(i==j){
        slack = c(slack, 1)
      } else { slack = c(slack, 0)}
    }
  }
  slack_mat = matrix(data=slack, byrow=TRUE,ncol=length(index)+1)
  tableau = cbind(tableau[,1:(ncol(tableau)-1)], slack_mat, tableau[,ncol(tableau)])
  
  return(tableau)
}

allSelect = c()
for(i in 1:30){
  allSelect = c(allSelect, i)
}

minTableau = buildTableau(allSelect)
result = Simplex(minTableau, FALSE)

