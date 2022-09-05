# Ulala fossil game 'solver'
# Does not actually solve the game, but outputs something that I _think_
# is enough information to identify the optimal cell to uncover in any
# given situation.

# Game works as follows:
# You are shown a 6x6 grid of covered cells. Your goal is to reveal
# 4 tetris shapes that are located somewhere in the 6x6 grid in as few moves as
# possible.

# Solution concept:
# Given a 6x6 input matrix which identifies cells that cannot have a fossil under
# them (either through being covered or through having a fossil provably under
# them), feed said matrix to a solver function that finds all possible locations
# of a given tetris shape in that matrix, and assigns to each cell a percentage
# value that reflects the proportion of possible arrangements in which that cell
# uncovers a fossil.

# Do this for all the possible remaining fossils. Uncover the cell with the highest
# combined percentage value.

# This ignores the conditional nature of the arrangements - each solver implicitly
# acts as if there is only one possible fossil remaining.

# The input matrix is assumed to have a 0 for covered cells and a 1 for cells
# that have been revealed or are otherwise illegal.

# rotation function

rotate90 <- function(x) t(apply(x, 2, rev))

squareSolverOne <- function(inGrid) {
  
  # Identify the list of cells 
  legalStartingSpots <- c(NA,NA)
  
  for(c in 1:6) {
    # loop through columns
    for(r in 1:6) {
      # loop through rows
      
      # check if column c row r contains a 0 
      if(inGrid[r,c] == 0) {
        
        # add the r,c coordinates to the list of legal starting spots
        if(is.na(legalStartingSpots[1]) == TRUE) {
          # this indicates that the list is empty, so replace the elements
          legalStartingSpots <- c(r,c)
        } else {
          # the list already has elements, so rbind
          legalStartingSpots <- rbind(legalStartingSpots, c(r,c))
        }
        
      }
      
    }
    
  }
  
  # make intGrid for holding counts
  
  intGrid <- matrix(data = 0, nrow = 6, ncol = 6)
  
  # Try to place a square with the top left part in each starting location
  
  for(i in 1:nrow(legalStartingSpots)) {
    
    # Check if the square placed with it's top left part in legalStartingSpots[i,]
    # is legal
    
    # check if the cell to the RIGHT is legal
    # checking the left cell is unnecessary because we're starting from the top left
    if(legalStartingSpots[i,2]+1 > 6) {
      # illegal - cell to the right exceeds the bounds of the grid
      rightLegal <- 0
    } else {
      # potentially legal - check if there's a collision with starting spots
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rightLegal <- 1
      } else {
        # collision
        rightLegal <- 0
      }
    }
    
    # check if the cell DOWN is legal
    # checking the top cell is unnecessary because we're starting from the top left
    if(legalStartingSpots[i,1]+1 > 6) {
      # illegal - cell down exceeds the bounds of the grid
      downLegal <- 0
    } else {
      # potentially legal, check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]] == 0) {
        # no collision
        downLegal <- 1
      } else {
        # collision
        downLegal <- 0
      }
    }
    
    # check if the cell both RIGHT and DOWN is legal
    if(legalStartingSpots[i,1]+1 > 6 | legalStartingSpots[i,2]+1 > 6) {
      # illegal bounds
      rdLegal <- 0
    } else {
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rdLegal <- 1
      } else {
        rdLegal <- 0
      }
    }
    
    # check if all three cells are legal
    
    # Make an intermediate grid for counting
    
    if(rightLegal == 1 & downLegal == 1 & rdLegal == 1) {
      # the square with its top left part in the first row of legalStartingSpots
      # is legal. Increment the values in intGrid by 1 for that square
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1]+1
    }
    
  }
  
  return(intGrid)
  
}

LSolverOne <- function(inGrid) {
  
  # the L is the shape that looks like this:
  # 0 1 0
  # 0 1 0
  # 1 1 0
  
  # for the sake of always starting with a 1 in the top left, we're going to 
  # treat it as if it looks like this:
  # 1 0 0 
  # 1 1 1
  
  # Identify the list of cells 
  legalStartingSpots <- c(NA,NA)
  
  for(c in 1:6) {
    # loop through columns
    for(r in 1:6) {
      # loop through rows
      
      # check if column c row r contains a 0 
      if(inGrid[r,c] == 0) {
        
        # add the r,c coordinates to the list of legal starting spots
        if(is.na(legalStartingSpots[1]) == TRUE) {
          # this indicates that the list is empty, so replace the elements
          legalStartingSpots <- c(r,c)
        } else {
          # the list already has elements, so rbind
          legalStartingSpots <- rbind(legalStartingSpots, c(r,c))
        }
        
      }
      
    }
    
  }
  
  # make intGrid for holding counts
  
  intGrid <- matrix(data = 0, nrow = 6, ncol = 6)
  
  # Try to place a L with the top left part in each starting location
  
  for(i in 1:nrow(legalStartingSpots)) {
    # check legality
    
    # cell two to the right and one down
    if(legalStartingSpots[i,1]+1 > 6 | legalStartingSpots[i,2]+2 > 6 ) {
      # illegal - bounds
      rrdLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]+2] == 0) {
        # no collision
        rrdLegal <- 1
      } else {
        # collision
        rrdLegal <- 0
      }
    }
    
    # cell one to the right and one down
    if(legalStartingSpots[i,1]+1 > 6 | legalStartingSpots[i,2]+1 > 6 ) {
      # illegal - bounds
      rdLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rdLegal <- 1
      } else {
        # collision
        rdLegal <- 0
      }
    }
    
    # cell down
    if(legalStartingSpots[i,1]+1 > 6 ) {
      # illegal - bounds
      dLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]] == 0) {
        # no collision
        dLegal <- 1
      } else {
        # collision
        dLegal <- 0
      }
    }
  
    # increment intGrid appropriately
    if(rrdLegal == 1 & rdLegal == 1 & dLegal == 1) {
      # the square with its top left part in the first row of legalStartingSpots
      # is legal. Increment the values in intGrid by 1 for that square
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+2] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+2]+1
    }
    
  }
  
  # Return intgrid
  
  return(intGrid)
  
  
}

longSolverOne <- function(inGrid) {
  
  # the long is just:
  # 1 1 1 1 0
  # 0 0 0 0 0
  
  # Identify the list of cells 
  legalStartingSpots <- c(NA,NA)
  
  for(c in 1:6) {
    # loop through columns
    for(r in 1:6) {
      # loop through rows
      
      # check if column c row r contains a 0 
      if(inGrid[r,c] == 0) {
        
        # add the r,c coordinates to the list of legal starting spots
        if(is.na(legalStartingSpots[1]) == TRUE) {
          # this indicates that the list is empty, so replace the elements
          legalStartingSpots <- c(r,c)
        } else {
          # the list already has elements, so rbind
          legalStartingSpots <- rbind(legalStartingSpots, c(r,c))
        }
        
      }
      
    }
    
  }
  
  # make intGrid for holding counts
  
  intGrid <- matrix(data = 0, nrow = 6, ncol = 6)
  
  # Try to place a long with the top left part in each starting location
  
  for(i in 1:nrow(legalStartingSpots)) {
    # check legality
    
    # cell three to the right
    if(legalStartingSpots[i,2]+3 > 6) {
      # illegal - bounds
      rrrLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+3] == 0) {
        # no collision
        rrrLegal <- 1
      } else {
        # collision
        rrrLegal <- 0
      }
    }
    
    # cell two to the right
    
    if(legalStartingSpots[i,2]+2 > 6) {
      # illegal - bounds
      rrLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+2] == 0) {
        # no collision
        rrLegal <- 1
      } else {
        # collision
        rrLegal <- 0
      }
    }
    
    # cell right
    if(legalStartingSpots[i,2]+1 > 6 ) {
      # illegal - bounds
      rLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rLegal <- 1
      } else {
        # collision
        rLegal <- 0
      }
    }
    
    # increment intGrid appropriately
    if(rrrLegal == 1 & rrLegal == 1 & rLegal == 1) {
      # the square with its top left part in the first row of legalStartingSpots
      # is legal. Increment the values in intGrid by 1 for that square
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+2] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+2]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+3] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+3]+1
    }
    
  }
  
  #  Return intGrid
  
  return(intGrid)
  
}

sSolverOne <- function(inGrid) {
  
  # shape
  # 1 1 0
  # 0 1 1
  
  # Identify the list of cells 
  legalStartingSpots <- c(NA,NA)
  
  for(c in 1:6) {
    # loop through columns
    for(r in 1:6) {
      # loop through rows
      
      # check if column c row r contains a 0 
      if(inGrid[r,c] == 0) {
        
        # add the r,c coordinates to the list of legal starting spots
        if(is.na(legalStartingSpots[1]) == TRUE) {
          # this indicates that the list is empty, so replace the elements
          legalStartingSpots <- c(r,c)
        } else {
          # the list already has elements, so rbind
          legalStartingSpots <- rbind(legalStartingSpots, c(r,c))
        }
        
      }
      
    }
    
  }
  
  # make intGrid for holding counts
  
  intGrid <- matrix(data = 0, nrow = 6, ncol = 6)
  
  # Try to place a s with the top left part in each starting location
  
  for(i in 1:nrow(legalStartingSpots)) {
    # check legality
    
    # cell two to the right and one down
    if(legalStartingSpots[i,1]+1 > 6 | legalStartingSpots[i,2]+2 > 6) {
      # illegal - bounds
      rrdLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]+2] == 0) {
        # no collision
        rrdLegal <- 1
      } else {
        # collision
        rrdLegal <- 0
      }
    }
    
    # cell one right and one down
    
    if(legalStartingSpots[i,1]+1 > 6 | legalStartingSpots[i,2]+1 > 6 ) {
      # illegal - bounds
      rdLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rdLegal <- 1
      } else {
        # collision
        rdLegal <- 0
      }
    }
    
    # cell right
    if(legalStartingSpots[i,2]+1 > 6 ) {
      # illegal - bounds
      rLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rLegal <- 1
      } else {
        # collision
        rLegal <- 0
      }
    }
    
    # increment intGrid appropriately
    if(rrdLegal == 1 & rdLegal == 1 & rLegal == 1) {
      # the square with its top left part in the first row of legalStartingSpots
      # is legal. Increment the values in intGrid by 1 for that square
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+2] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+2]+1
    }
    
  }
  
  # return intGrid
  return(intGrid)
  
  
}


# 0 1 0
# 1 1 1

tSolverOne <- function(inGrid) {
  
  # shape
  # 1 1 1
  # 0 1 0
  
  # Identify the list of cells 
  legalStartingSpots <- c(NA,NA)
  
  for(c in 1:6) {
    # loop through columns
    for(r in 1:6) {
      # loop through rows
      
      # check if column c row r contains a 0 
      if(inGrid[r,c] == 0) {
        
        # add the r,c coordinates to the list of legal starting spots
        if(is.na(legalStartingSpots[1]) == TRUE) {
          # this indicates that the list is empty, so replace the elements
          legalStartingSpots <- c(r,c)
        } else {
          # the list already has elements, so rbind
          legalStartingSpots <- rbind(legalStartingSpots, c(r,c))
        }
        
      }
      
    }
    
  }
  
  # make intGrid for holding counts
  
  intGrid <- matrix(data = 0, nrow = 6, ncol = 6)
  
  # Try to place a T with the top left part in each starting location
  
  for(i in 1:nrow(legalStartingSpots)) {
    # check legality
    
    # cell two to the right
    if(legalStartingSpots[i,2]+2 > 6) {
      # illegal - bounds
      rrLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+2] == 0) {
        # no collision
        rrLegal <- 1
      } else {
        # collision
        rrLegal <- 0
      }
    }
    
    # cell one right
    
    if(legalStartingSpots[i,2]+1 > 6 ) {
      # illegal - bounds
      rLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1], legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rLegal <- 1
      } else {
        # collision
        rLegal <- 0
      }
    }
    
    # cell right and down
    if(legalStartingSpots[i,1]+1 > 6 |legalStartingSpots[i,2]+1 > 6 ) {
      # illegal - bounds
      rdLegal <- 0
    } else {
      # potentially legal - check if there's a collision
      if(inGrid[legalStartingSpots[i,1]+1, legalStartingSpots[i,2]+1] == 0) {
        # no collision
        rdLegal <- 1
      } else {
        # collision
        rdLegal <- 0
      }
    }
    
    # increment intGrid appropriately
    if(rrLegal == 1 & rLegal == 1 & rdLegal == 1) {
      # the square with its top left part in the first row of legalStartingSpots
      # is legal. Increment the values in intGrid by 1 for that square
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1] <- intGrid[legalStartingSpots[i,1]+1,legalStartingSpots[i,2]+1]+1
      intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+2] <- intGrid[legalStartingSpots[i,1],legalStartingSpots[i,2]+2]+1
    }
    
  }
  
  # return intGrid
  return(intGrid)
  
  
}


# all of the above functions return a single intGrid with the assumed starting position
# to get correct outgrids, it is insufficient to rotate intGrid, you must instead
# rotate _in_Grid and solve again (rotating the grid and rotating the piece are
# isomorphic transformations, but rotating intGrid is not)
# Rotating intGrid only works if inGrid is empty.

squareSolver <- function(inGrid) {
  
  # rotationally invariant
  
  sMat <- squareSolverOne(inGrid)
  
  return(sMat/sum(sMat))
  
}

LSolver <- function(inGrid) {
  
  # rotationally variant, perform 3 rotations
  
  lMat1 <- LSolverOne(inGrid)
  lMat2 <- LSolverOne(rotate90(inGrid))
  lMat3 <- LSolverOne(rotate90(rotate90(inGrid)))
  lMat4 <- LSolverOne(rotate90(rotate90(rotate90(inGrid))))
  
  # rotate the *result* grids back as many times as they were rotated to get the
  # correct orientation of the grid.
  
  lMatFinal <- lMat1 + rotate90(rotate90(rotate90(lMat2))) + rotate90(rotate90(lMat3)) + rotate90(lMat4)
  
  return(lMatFinal/sum(lMatFinal))
  
}

tSolver <- function(inGrid) {
  
  # rotationally variant, perform 3 rotations
  
  tMat1 <- tSolverOne(inGrid)
  tMat2 <- tSolverOne(rotate90(inGrid))
  tMat3 <- tSolverOne(rotate90(rotate90(inGrid)))
  tMat4 <- tSolverOne(rotate90(rotate90(rotate90(inGrid))))
  
  tMatFinal <- tMat1 + rotate90(rotate90(rotate90(tMat2))) + rotate90(rotate90(tMat3)) + rotate90(tMat4)
  
  return(tMatFinal/sum(tMatFinal))
  
}

longSolver <- function(inGrid) {
  
  # rotationally variant, perform 3 rotations
  
  longMat1 <- longSolverOne(inGrid)
  longMat2 <- longSolverOne(rotate90(inGrid))
  longMat3 <- longSolverOne(rotate90(rotate90(inGrid)))
  longMat4 <- longSolverOne(rotate90(rotate90(rotate90(inGrid))))
  
  longMatFinal <- longMat1 + rotate90(rotate90(rotate90(longMat2))) + rotate90(rotate90(longMat3)) + rotate90(longMat4)
  
  return(longMatFinal/sum(longMatFinal))
  
}

sSolver <- function(inGrid) {
  
  # rotationally variant, perform 3 rotations
  
  sMat1 <- sSolverOne(inGrid)
  sMat2 <- sSolverOne(rotate90(inGrid))
  sMat3 <- sSolverOne(rotate90(rotate90(inGrid)))
  sMat4 <- sSolverOne(rotate90(rotate90(rotate90(inGrid))))
  
  sMatFinal <- sMat1 + rotate90(rotate90(rotate90(sMat2))) + rotate90(rotate90(sMat3)) + rotate90(sMat4)
  
  return(sMatFinal/sum(sMatFinal))
  
}
