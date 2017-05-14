root <- function(data , features)
{
  if(length(features) == 1)
  {
    demo <- as.numeric(sum(data["type"] == "democrat"))
    repub <- as.numeric(sum(data["type"] == "republican"))
    if(demo > 0)
    {
      bestNode <- "democrat"
    }
    else
    {
      bestNode <- "republican"
    }
  }
  else
  {
    bestNode <- features[1]
  }
  if(bestNode == "democrat" || bestNode == "republican")
  {
    node <- Node$new(bestNode)
    return (node)
  }
  #print(bestNode)
  features <- features[!features %in% bestNode]
  root <- Node$new(bestNode)
  leftChild <- data
  leftChild <- leftChild[leftChild[bestNode] == "y",]
  #print(leftChild)
  rightChild <- data
  rightChild <- rightChild[rightChild[bestNode] == "n",]
  if(nrow(leftChild) == 0)
  {
    rightChild <-  rightChild[,!(names(rightChild) %in% c(bestNode))]
    rightNode <- root$AddChild(paste (bestNode , "no" ,sep=""))
    #print(leftChild)
    rightFeature <- rightNode$AddChildNode(root(rightChild , features))
  }
  else if(nrow(rightChild) == 0)
  {
    leftChild <-  leftChild[,!(names(leftChild) %in% c(bestNode))]
    leftNode <- root$AddChild(paste (bestNode , "yes" ,sep=""))
    #print(leftChild)
    leftFeature <- leftNode$AddChildNode(root(leftChild , features))
  }
  else
  {
    leftChild <-  leftChild[,!(names(leftChild) %in% c(bestNode))]
    rightChild <-  rightChild[,!(names(rightChild) %in% c(bestNode))]
    leftNode <- root$AddChild(paste (bestNode , "yes" ,sep=""))
    rightNode <- root$AddChild(paste (bestNode , "no" ,sep=""))
    #print(leftChild)
    leftFeature <- leftNode$AddChildNode(root(leftChild , features))
    rightFeature <- rightNode$AddChildNode(root(rightChild , features))  
  }
  
  return(root)
}

buildTree <- function(data)
{
  features <- c("f1" ,"f2" ,"f3" ,"f4" ,"f5" ,"f6" ,"f7" ,"f8" ,"f9" ,"f10" ,"f11" ,"f12" ,"f13" ,"f14" ,"f15" ,"f16" )
  bestNode <- entropy(data , features)
  features <- features[!features %in% bestNode]
  root <- Node$new(bestNode)
  leftChild <- data
  leftChild <- leftChild[leftChild[bestNode] == "y",]
  rightChild <- data
  rightChild <- rightChild[rightChild[bestNode] == "n",]
  leftChild <-  leftChild[,!(names(leftChild) %in% c(bestNode))]
  rightChild <-  rightChild[,!(names(rightChild) %in% c(bestNode))]
  leftNode <- root$AddChild(paste (bestNode , "yes" ,sep=""))
  rightNode <- root$AddChild(paste (bestNode , "no" ,sep=""))
  leftFeature <- leftNode$AddChildNode(root(leftChild , features))
  rightFeature <- rightNode$AddChildNode(root(rightChild , features))

  return (root)
  
}

entropy <- function(data,features)
{
  maxGain <- as.double(-1)
  bestNode <- ""
  #print(features)
  #print(features)
  #print(data)
  for(i in features)
  {
    #feature <- paste("f",i,sep = "")
    tempFeature <- data[c("type" ,i)]
    demo <- sum(tempFeature["type"] == "democrat")
    repub <- sum(tempFeature["type"] == "republican")
    entropy <- -demo/(demo+repub)*log2(demo/(demo+repub))-(repub/(demo+repub)*log2(repub/(demo+repub)))
    if( is.na(entropy))
    {
      entropy <- 0
    }
    #print(tempFeature)
    if(entropy == 0)
    {
      if(demo == 0)
      {
        #print("22222222222222222")
        return("republican")
      }
      else if(repub == 0)
      {
        #print("111111111111111")
        return("democrat")
      }
    
    }
    count <- 0
    child1 <- tempFeature
    child1 <- child1[child1[i]== "y",]
    demochild1 <- 0
    repubchild1 <- 0
    entropychild1 <- 0
    if(nrow(child1) == 0)
    {
      count <- 1
    }
    else
    {
      demochild1 <- sum(child1["type"] == "democrat")
      repubchild1 <- sum(child1["type"] == "republican")
      entropychild1 <- -demochild1/(demochild1+repubchild1)*log2(demochild1/(demochild1+repubchild1))-(repubchild1/(demochild1+repubchild1)*log2(repubchild1/(demochild1+repubchild1)))
      if(is.na(entropychild1) )
      {
        entropychild1 <- 0
      }
    }
    child2 <- tempFeature
    child2 <- child2[child2[i]== "n",]
    demochild2 <- 0
    repubchild2 <- 0
    entropychild2 <- 0
    if(nrow(child2) == 0)
    {
      count <- 2
    }
    else
    {
      demochild2 <- sum(child2["type"] == "democrat")
      repubchild2 <- sum(child2["type"] == "republican")
      entropychild2 <- -demochild2/(demochild2+repubchild2)*log2(demochild2/(demochild2+repubchild2))-(repubchild2/(demochild2+repubchild2)*log2(repubchild2/(demochild2+repubchild2)))  
      if(is.na(entropychild2) )
      {
        entropychild2 <- 0
      }
    }
    if(count == 0)
    {
      infoGain <- as.double((entropy - (((demochild1 +repubchild1 )/(demo + repub)) * entropychild1) - (((demochild2 + repubchild2)/(demo + repub))* entropychild2)))  
    }
    else if(count == 1)
    {
      infoGain <- as.double((entropy  - (((demochild2 + repubchild2)/(demo + repub))* entropychild2)))
      #print(1)
    }
    else if(count == 2)
    {
      #print("11111111111111111111111111111")
      #print(entropy)
      #print(entropychild1)
      infoGain <- as.double((entropy - (((demochild1 +repubchild1 )/(demo + repub)) * entropychild1)))
      #print(2)
      
    }
   #print(demo)
   #print(repub)
   #print(infoGain) 
    if(infoGain > maxGain)
    {
      bestNode <- i
      maxGain <- infoGain
      #print(maxGain)
      #print(bestNode)
    }
    
  }
  return(bestNode)
}

decisionTree <- function()
{   
  library(data.tree)
  data <- read.csv("house-votes-84.data.csv")
  features <- c("f1" ,"f2" ,"f3" ,"f4" ,"f5" ,"f6" ,"f7" ,"f8" ,"f9" ,"f10" ,"f11" ,"f12" ,"f13" ,"f14" ,"f15" ,"f16" )
  for(i in features)
  {
    tempCol <- data[i]
    sumy <- sum(tempCol == "y")
    sumn <- sum(tempCol == "n")
    j <- 1
    count <- length(tempCol[,1])
    while(j < count)
    {
      tempChar <- tempCol[j,]
      if (as.character(tempChar) == "?")
      {
        if(sumy > sumn)
        {
          tempCol[j,] <- "y"
        }
        else
        {
          tempCol[j,] <- "n"
        }
      }
      j <- j + 1
    }
    data[i] <- tempCol
  }
  accureacyPlot <- c()
  treeSizePlot <- c()
  #accuracy <- c(30)
  trainingSize <- c(30 , 40 , 50 , 60 , 70 )
  for(k in trainingSize)
  {
    accuracySize <- (k / (100)) * (nrow(data))
    maxTreeSize <- 0
    minTreeSize <- 1000
    meanTreeSize <- 0
    maxAccuracy <- 0
    minAccuracy <- 100
    meanAccuracy <- 0
    print("----------------------------------------")
    print(paste("for train size : " , k , sep = ""))
    print("----------------------------------------")
    for(l in 1:5)
    {
      tmpData <- data
      trainSet <- sample(1:nrow(data) ,accuracySize , replace = F)
      totalIndexes <- c(1:nrow(data))
      testSet <- setdiff(totalIndexes , trainSet)
      tmpData <- tmpData[-testSet,] 
        #x[,!(names(x) %in% c("f1"))]
      root <- buildTree(tmpData)
      currentTreeSize <- treeSize(root)
      tmpData <- data
      tmpData <- tmpData[-trainSet,]
      currentTreeAccuracy <- testTree(root , tmpData , testSet)
      meanTreeSize <- (meanTreeSize + currentTreeSize)
      meanAccuracy <- (meanAccuracy + currentTreeAccuracy)
      if(currentTreeSize > maxTreeSize)
      {
        maxTreeSize <- currentTreeSize
      }
      if(currentTreeSize < minTreeSize)
      {
        minTreeSize <- currentTreeSize
      }
      if(currentTreeAccuracy > maxAccuracy)
      {
        maxAccuracy <- currentTreeAccuracy
      }
      if(currentTreeAccuracy < minAccuracy)
      {
        minAccuracy <- currentTreeAccuracy
      }
      print(paste("trail number : " , l , sep = "") )
      print("----------------------------------------")
      print(paste("tree size :" , currentTreeSize , sep = ""))
      print(paste("tree accuracy :" , currentTreeAccuracy , sep = ""))
      #accureacyPlot <- append(accureacyPlot , currentTreeAccuracy)
      #treeSizePlot <- append(treeSizePlot , currentTreeSize)
    }
    print("----------------------------------------")
    print(paste("for size : ", k  ," final", sep = "") )
    print("----------------------------------------")
    print(paste("maximum tree size : " , maxTreeSize , sep = ""))
    print(paste("minimum tree size : " , minTreeSize , sep = ""))
    print(paste("mean tree size : " , meanTreeSize/5 , sep = ""))
    print(paste("maximum tree accuracy : " , maxAccuracy , sep = ""))
    print(paste("minimum tree accuracy : " , minAccuracy , sep = ""))
    print(paste("mean tree accuracy : " , meanAccuracy/5 , sep = ""))
    accureacyPlot <- append(accureacyPlot , meanAccuracy)
    treeSizePlot <- append(treeSizePlot , meanTreeSize)
  }
    treeSizes <- c(30,40,50,60,70)
    plot(treeSizes , accureacyPlot , xlab = "train set" ,ylab = "mean accuracy" )
    plot(treeSizes , treeSizePlot , xlab = "train set" ,ylab = "mean tree size" )
}
testTree <- function(root, data , testSet)
{
  #print(testSet)
  accuracy <- 0
  for(i in testSet)
  {
    n <- root
    #print(n)
    currentRow <- data[i,]
    while(TRUE)
    {
      featureName <- n$name
      if(featureName == "democrat" || featureName == "republican")
      {
        if(featureName == currentRow["type"])
        {
          accuracy <- accuracy +1 
        }
        break
      }
      featureNumber <- as.numeric(gsub("f","",featureName))
      featureNumber <- featureNumber+1
      choice <- currentRow[,featureNumber]
     # print(choice)
     # print(featureNumber)
     # print(featureName)
      if(n$count == 2)
      {
        if(!is.na(choice) && choice == "y")
        {
          #print("111111111111111")
          tmpNode <-  n$children[[1]]
          #print(tmpNode)
          if(regexpr("yes" , tmpNode$name) > 0)
          {
            n <- tmpNode$children[[1]]
          }
        }
        else if(!is.na(choice) && choice == "n")
        {
          
          tmpNode <-  n$children[[2]]
          if(regexpr("no" , tmpNode$name) > 0)
          {
            n <- tmpNode$children[[1]]
          }
        }
        else break
      }
      else
      {
        tmpNode <-  n$children[[1]]
        if(((!is.na(choice)) && choice == "y" && (regexpr("yes" , tmpNode$name) > 0)) || ((!is.na(choice)) &&choice == "n" && (regexpr("no" , tmpNode$name) > 0)))
        {
          n <- tmpNode$children[[1]]
        }
        else
        {
          break
        }
      }
    
    }
  }
  return(as.numeric((accuracy/nrow(data))*100))
}
treeSize<- function(root)
{
  return(root$totalCount)
}
