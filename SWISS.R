# function SWISS
# SWISS, Standarized WithIn class Sum of Sqaures
# Calculates SWISS and Permutation test for significant differences between    
#        sum of squares decomposition of data1 and data2
# This is R version of Cabanski, C. et al. (2009) SWISS MAKE: Standardized WithIn class 
#           Sum of Squares to evaluate Methodologies And Data set Elements. 
#           PloS one, 5(3), e9905.
# This R code is simply used for comparisation of only two classes. For more than
# it can be simply calculated by average pairwise SWISS score.
# Input:
# data1  (d1 * n)
#
# data2   (d2 * n)
#         if want to calculate only one data set, 
#         set data2 =c(), in which case teststat is only output
#
# groups  (n * 1) vector with labels (1,2,3,...,k) for each group
#
# opt     option for centering
#         1 - (default) ususal sum of squares decomposition (overall mean)
#         2 - uses mean of the group means (this weighs all groups equally)
#
# nsim -     Number of simulated relabellings to use (default is 1000)
#
# curve      0 (default) fit kde curve of simulated relabelling points
#            1 fit gaussian curve to simulated data (suggested when n is small)
#            2 no curve 
#
#icolor      0 fully black and white version (everywhere)
#            1 (default) ine color version (red for data set 1, blue for data set 2)
#
#dotcolor    0 black dots(default)
#            a string of one color for data (i.e.,'g' or 'm') or a 1*3 color matrix
#
#titlestr    string with title for left hand plot (showing projected data)
#            default is 'Sum of Squares Permutation Hypothesis Test'
#
#titlefontsize
#            font size of title (only have effect when titlestr is nonempty)
#            default is empty
#
#xlabelstr   string with x axis label 
#            default is empty string,'' for no xlabel
#
#ylabelstr   string with y axis label
#            default is empty string, '' for no ylabel
#
#data1str    string with data1 test stat label
#            default is 'SWISS 1'
#
#data2str    string with data2 test stat label default is 'SWISS 2'
#
#labelfontsize font size for axis labels (only has effect when plot is made
#              here, and when a label str is nonempty)
#              default is empty [], for Matlab default
#
#savestr     string controlling saving of output, either a full path, or a file prefix
#            to save in matlab's current directory  
#            Will add .ps and save as either color postscript(icolor != 0)
#            OR black&white postscript (when icolor = 0) unspecified: result only appear on screen
#
# Output:
#           Graphic in current Figure,
#                     showing population of simulated p values
#           When savestr exists, Postscript files saved in 
#                   'savestr'.ps (color postscript for icolor != 0)
#                    (B & W ostscript for icolor == 0)
#
#        teststat - test statistics based on ration of withingroup sum of 
#                   squares devided by total sum of squares
#        epval - empirical pvalue, based on simulated quantiles. 
#                 summarizing results of permutation test
#        ci - 95% confidence interval of permutaed population
opt_d <- 1
nsimd_d = 1000
seed_d = 1234
curve_d = 0


SWISS <- function(data1, data2, groups,...){
  if (length(as.list(match.call()))-1 < 3){# if the parameters 
    warning(cat('ERROR : Not enough inputs.\n Please at leased input following required parameters:\n
                mdata1,\n mdata2 (can be emptym i.e. = NULL),\n groups.'))
  }else{
      print('HERE WE GO')
      call <- match.call()
      data1 < -as.data.frame(data1)
      data2 <- as.data.frame(data2)
      if(is.vector(groups)&&is.numeric(groups[1])){
        groups <- groups
      }else{
        warning('groups should be a vector with n numerical values')
      }
      
      if(opt!=1 |opt != 2){
        warning('opt must be 1 or 2, and default value is 1')
        opt = 1
      }else if(opt==2){
        opt <-2
      }else{
        opt <- opt_d
      }
      
      if(is.integer(nsim)){nsim <- nsim}else{nsim <- nsimd_d}
      
      if(is.integer(seed)){seed<-seed}else{seed <- seed_d}
      
      if(curve!=0|curve!=1|curve!=2){
        warning('curve must be 0 or 1 or 2, and default value is 0')
        curve<-curve_d
        }else{
        curve<-curve  
        }
# end of parameter resetting      
      k = max(groups)
      if (min(group) != 1)
      {
        mat <- c()
        stop('Warning from SWISS: groups labeling does not starting at 1, returning empty matrix')
      }     
      
      n <- dim(data1)[2]
      d1 <- dim(data1)[1]
      
      if(is.null(data2)){
        gmeanA <- matrix(0,d1,k)
        tempn <- matrix(0,k,1)
# calculate group means(gmean), number of elements in each group
        for (i in 1:k){
          temp <- matrix(0,d1,1)
          for (j in 1:n){
            if(groups[j] == i){
              temp <- cbind(temp,data1[,j])
            }
          }
          tempn[i] <- dim(temp)[2] - 1
          rmeanA <- matrix(0,d1,1)
          for(m in 1:d1){
            rmeanA[m] <- 0
            for (j in 1:tempn[i]){
              rmeanA[m] <- rmeanA[m]+temp[m, j+1]
            }
            rmeanA[m] <- rmeanA[m] / tempn[i]
          }
          centdatA <- matrix(0,d1,tempn[i])
          for(m in 1:d1){
            for(j in 1:tempn[i]){
              centdatA[m,j] <- temp[m,j+1]- rmeanA(m)
            }
        }
      }
#calculate overall mean(center)
      omeanA <- matrix(0,d1,1)
      if(opt == 1){
        for(i in 1:d1){
          omeanA(i,1) <- sum(data1[i,])/n
        }
        }else if(opt == 2){
          for(i in 1:d1){
            omeanA(i,1) = sum(gmeanA[i,])/k
          }
        }else{
          mat = c()
          stop('Warning from SWISS: option does not equal 1, 2 or 3 returning empty matrix')
        }
# calculate distance matrices
      distA <- matrix(0,2,n)
      for(i in 1:n){
        g <- groups[i]
        distA[1,i] <- sum((data1[,i]-gmeanA[,g])^2)
        distA[2,i] <- sum((data1[,i]-omeanA)^2)
      }
# calculate ratio A
      RatioA <- sum(distA[1,])/sum(distA[2,])
      teststat <- c(RatioA)
      epval<-c()
      ci <- c()
      
      
      } # end of the condition no data2 input
      
      
      
      else{
        d2<-dim(data2)[1]
        
#calculate group means, number of elements in each group tempn
      gmeanA <- matrix(0,d1,k)
      tempn <- matrix(0,k,1)
      for (i in 1:k){
        temp <- matrix(0,d1,1)
        for (j in 1:n){
          if (groups[j] == i){
            temp<-cbind(temp,data1[,j])
          }
        }
        tempn[i] <-dim(temp)[2] -1
        rmeanA <- matrix(0,d1,1)
        for (m in 1:d1){
          rmeanA(m) <- 0
          for (j in 1:tempn[i]){
            rmean[m] <- rmean[m] + temp[m,j+1]
          }
          rmeanA[m] <- rmeanA[m] / tempn[i]
        }
        centdatA <- mat(0,d1,tempn[i])
        for (m in 1:d1){
          for(j in 1:tempn[i]){
            centdatA[m,j] <- temp[m,j+1] - rmeanA[m]
          }
        }
        gmeanA[,i] <- rmeanA
      }
      
      gmeanB <- matrix(0,d2,k)
      for (i in 1:k){
        temp <- matrix(0,d2,1)
        for (j in 1:n){
          if (groups[j] == i){
            temp<-cbind(temp,data2[,j])
          }
        }
        tempn[i] <-dim(temp)[2] -1
        rmeanB <- matrix(0,d2,1)
        for (m in 1:d2){
          rmeanB(m) <- 0
          for (j in 1:tempn[i]){
            rmean[m] <- rmean[m] + temp[m,j+1]
          }
          rmeanB[m] <- rmeanB[m] / tempn[i]
        }
        centdatB <- mat(0,d1,tempn[i])
        for (m in 1:d2){
          for(j in 1:tempn[i]){
            centdatB[m,j] <- temp[m,j+1] - rmeanB[m]
          }
        }
        gmeanB[,i] <- rmeanB
      }
# calculate overall mean(center)
      omeanA <- matrix(0,d1,1)
      omeanB <- matrix(0,d2,1)
      if(opt==1){
        for (i in 1:d1){
          omeanA[i,1] = sum(data1[i,])/n
        }
        for(i in 1:d2){
          omeanB[i,1]= sum(data2[i,])/n
        }
      }else if(opt ==2){
        for(i in 1:d1){
          omeanA[i,1] <-sum(gmeanA[i,])/k
        }
        for(i in 1:d2){
          omeanB[i,1] <-sum(gmeanB[i,])/k
        }
      }else{
        mat<-c()
        stop('Warning: option does not equal 1,2 or 3; returning empty matrix')
      } 
# calculate distance matrices
      distA <- matrix(0,2,n)
      distB <- matrix(0,2,n)
      for (i in 1:n){
        g = groups[i]
        distA[1,i] <-sum((data1[,i]-gmeanA[,g])^2)
        distB[1,i] <-sum((data2[,i]-gmeanB[,g])^2)
        distA[2,i] <-sum((data1[,i]-omeanA)^2)
        distB[2,i] <-sum((data2[,i]-omeanB)^2)
      }
      RatioA <- sum(distA[1,])/sum(distA[2,])
      RatioB <- sum(distB[1,])/sum(distB[2,])
      distA <- distA/sum(distA[2,])
      distB <- distB/sum(distB[2,])
      #calculate permuted population of ratios
      perm <- runif(nsim, n)
      ratio <- c()
      
      for (i in 1:nsim){
        pdata <-matrix(0,2,n)
        for(j in 1:n){
          if (perm[i,j]<0.5){
            pdata[,j] <- distA[,j]
          }else{
            pdata[,j] <- distB[,j]
          }
        }
        ratio[i] <- sum(pdata[1,])/sum(pdata[2,])
      }
      
    
      }
   }
}
      
 



