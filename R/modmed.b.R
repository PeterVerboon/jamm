
# This file is a generated template, your changes will not be overwritten

modmedClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "modmedClass",
  inherit = modmedBase,
  private = list(
    .run = function() {
      
      # get the data
      
      data <- self$data
      data <- na.omit(data)
      
      if (self$results$isFilled()) return()
      
      yvar  = self$options$dep
      mvars = self$options$meds
      xvar = self$options$pred
      xmmod = self$options$xmmod
      mymod = self$options$mymod
      cmvars = self$options$covsm
      cyvars = self$options$covsy
      plotIom = self$options$plotIom
      plotSS = self$options$plotSS
      paths = self$options$paths


      ready <- !is.null(self$options$dep) &&  !is.null(self$options$pred) &&  !is.null(self$options$meds)
      
      if (ready) {
        
        suppressWarnings({
          
           results <- moderatedMediationSem (
             data =  data,
             yvar  = self$options$dep,
             mvars = self$options$meds,
             xvar = self$options$pred,
             xmmod = self$options$xmmod,
             mymod = self$options$mymod,
             cmvars = self$options$covsm,
             cyvars = self$options$covsy,
             estMethod = self$options$estMethod,
             nboot = self$options$bootstrap)
             

        })   # suppressWarnings
        
        if (self$options$showFit){
          a <- lavaan::fitmeasures(results$intermediate$result)[c("cfi","tli","rmsea","chisq", "df")]
          self$results$text$setContent(round(a,3))
        }
        
        private$.populateRsqTable(results)
        private$.populateIndTable(results)
        private$.populateDirTable(results)
        private$.populateIndSTable(results)

        
  
        
        if (paths == TRUE) {
          private$.populateATable(results)
          private$.populateBTable(results)
        }
        
        if ((plotIom == TRUE)  | (plotSS == TRUE)) {

          ## test if moderator exists for x=m path and if it is dichotomous factor
          if (length(xmmod)) {
            noIomPlot <- ifelse(plotIom, noIomPlot <- FALSE, noIomPlot <- TRUE)
            noSSPlot <- ifelse(plotSS, noSSPlot <- FALSE, noSSPlot <- TRUE)
            xdichotomous <- FALSE
            if (is.factor(data[,xmmod])) {
              if (length(levels(data[,xmmod])) > 2) {
                message <- ("This function can not yet plot moderation with a moderator (x-m path) that is a factor with more than two levels.");
                noSSPlot <- TRUE
                noIomPlot <- TRUE
              }
              else {
                xmodLevels <- levels(data[,xmmod]);
                data[,xmmod] <- as.numeric(data[,xmmod]) - 1;
                xdichotomous <- TRUE;
                noIomPlot <- TRUE
              }
            }
          
            # index of moderated mediation
            
            if (noIomPlot == FALSE){
              
               parEst <- results$intermediate$parameterEstimates
               plotData <- private$.preparePlotIom(data=data, mod = xmmod, mvars = mvars, 
                                                parEst = parEst, vorw = "w",int = "im")
             
              image <- self$results$plotIMMx     # empty plot defined in r.yaml
              image$setState(plotData)           # construct plot
              
              }  # if noIomPLot
            
            
            
            if (noSSPlot == FALSE){
              
              parEst <- results$intermediate$parameterEstimates
              plotDat2 <- private$.prepareSimpleSlopes(data=data,xvar=xvar,yvar=yvar,mod=xmmod, mvars=mvars, 
                                             parEst=parEst, vorw="w", int = "im", vdichotomous = xdichotomous,
                                             modLevels=xmodLevels, path ="x-m") 
              image <- self$results$plotSSx     # empty plot defined in r.yaml
              image$setState(plotDat2)          # construct plot
              

              } # if noSSPLot

         }   # length(xmmod)
          
          
          if (length(mymod)) {
            noIomPlot <- ifelse(plotIom, noIomPlot <- FALSE, noIomPlot <- TRUE)
            noSSPlot <- ifelse(plotSS, noSSPlot <- FALSE, noSSPlot <- TRUE)
            xdichotomous <- FALSE
            if (is.factor(data[,mymod])) {
              if (length(levels(data[,mymod])) > 2) {
                message <- ("This function can not yet plot moderation with a moderator (m-y path) that is a factor with more than two levels.");
                noSSPlot <- TRUE
                noIomPlot <- TRUE
              }
              else {
                xmodLevels <- levels(data[,mymod]);
                data[,mymod] <- as.numeric(data[,mymod]) - 1;
                xdichotomous <- TRUE;
                noIomPlot <- TRUE
              }
            }
            
            # index of moderated mediation
            
            if (noIomPlot == FALSE){
              
              parEst <- results$intermediate$parameterEstimates
              plotData <- private$.preparePlotIom(data=data, mod = mymod, mvars = mvars, 
                                              parEst = parEst, vorw = "v",int = "iy")
              image <- self$results$plotIMMy     # empty plot defined in r.yaml
              image$setState(plotData)           # construct plot
              
            }  # if noIomPLot
            
            
            
            if (noSSPlot == FALSE){
              
              parEst <- results$intermediate$parameterEstimates
              plotDat2 <- private$.prepareSimpleSlopes(data=data,xvar=xvar,yvar=yvar,mod=mymod, mvars=mvars, 
                                                       parEst=parEst, vorw="v", int = "iy", vdichotomous = xdichotomous,
                                                       modLevels=xmodLevels, path ="m-y") 
              image <- self$results$plotSSy      # empty plot defined in r.yaml
              image$setState(plotDat2)           # construct plot
              
            } # if noSSPLot
            
          }   # length(mymod)
          
       }     # plots
      
      }      # ready
    },  # end run
    
    
    
    
      .populateRsqTable = function(results) {
      
      res <- results$output$Rsq
      table <- self$results$rsq           # empty table, defined in r.yaml
      
      for (rowKey in table$rowKeys) {
        
        table$setRow(rowKey=rowKey, values=list(
          value=res[rowKey])
        )
      }
      
      
    },
    
    .populateATable = function(results) {  
      
      res <- results$output$parameterEstimates.apath
      table <- self$results$apaths                   # empty table, defined in r.yaml
      mvars <- self$options$meds
      
      p <- 1
      if (length(self$options$xmmod)) p <- 3 
      
      for (rowNo in 1:(length(mvars)*p)) {
        
        table$setRow(rowNo=rowNo, values=list(
          varName = res[rowNo, "label"],
          est=res[rowNo, "est"],
          se =res[rowNo, "se"],
          z  =res[rowNo, "z"],
          p  =res[rowNo, "pvalue"],
          ci.lower =res[rowNo, "ci.lower"],
          ci.upper =res[rowNo, "ci.upper"]
        )
        )
      }
      
      
    },
    
    .populateBTable = function(results) {  
      
      res <- results$output$parameterEstimates.bpath
      table <- self$results$bpaths                   # empty table, defined in r.yaml
      mvars <- self$options$meds
      
      p <- 1
      if (length(self$options$mymod)) p <- 2 
      
      for (rowNo in 1:(p*length(mvars)+(p-1))) {
        
        table$setRow(rowNo=rowNo, values=list(
          varName = res[rowNo, "label"],
          est=res[rowNo, "est"],
          se =res[rowNo, "se"],
          z  =res[rowNo, "z"],
          p  =res[rowNo, "pvalue"],
          ci.lower =res[rowNo, "ci.lower"],
          ci.upper =res[rowNo, "ci.upper"]
        )
        )
      }
      
      
    },
    
    
      .populateIndTable = function(results) {
      
      res <- results$output$parameterEstimates.indirect.raw
      table <- self$results$indirectEffect                   # empty table, defined in r.yaml
      mvars <- self$options$meds
      
      for (rowNo in 1:(1+length(mvars))) {
        
        table$setRow(rowNo=rowNo, values=list(
          est=res[rowNo, "est"],
          se =res[rowNo, "se"],
          z  =res[rowNo, "z"],
          p  =res[rowNo, "pvalue"],
          ci.lower =res[rowNo, "ci.lower"],
          ci.upper =res[rowNo, "ci.upper"]
          )
        )
      }
      
      
    },
    
      .populateDirTable = function(results) {
      
      res <- results$output$parameterEstimates.direct
      
      table <- self$results$directEffect           # empty table, defined in r.yaml
      
        table$setRow(1, values=list(
          est=res[1, "est"],
          se =res[1,"se"],
          z  =res[1,"z"],
          p  =res[1,"pvalue"],
          ci.lower =res[1,"ci.lower"],
          ci.upper =res[1,"ci.upper"]
        )
        )
      
    },
    
      .populateIndSTable = function(results) {
      
      res <- results$output$parameterEstimates.indirect.standardized
      table <- self$results$indirectEffectStand           
      
      for (rowKey in table$rowKeys) {
        
        table$setRow(rowKey=rowKey, values=list(
          value=res[rowKey])
        )
      }
    },
      
       .preparePlotIom = function(data=data, mod , mvars = mvars,
                                 parEst, vorw , int )  {

        
        # compute parameters for index of mediation

        a   <- subset(parEst, grepl("a1", parEst$label))[,"est"]
        b   <- subset(parEst, grepl("b", parEst$label))[,"est"]
        vw  <- subset(parEst, grepl(vorw, parEst$label))[,"est"]
        int <- subset(parEst, grepl(int, parEst$label))[,c("ci.lower","est","ci.upper")]

        if (vorw == "v") vw <- rep(vw,length(mvars))
        
        plotData <- data.frame(yIom=numeric(), moderator = numeric(), mediator = factor())
        IMMplots <- list()
        moderator <- c(min(data[,mod]),max(data[,mod])) 

        for (i in seq_along(mvars)) {

          yIom <- a[i]*b[i] + vw[i]*int[i,2]*moderator
          mediator <- c(mvars[i],mvars[i])
          
          plotDat0 <- data.frame(yIom,moderator,mediator);
          plotData <- rbind(plotData,plotDat0)
         
        } #   end loop mvars
        
        names(plotData) <- c('IMM', mod, "mediator")
        attr(plotData, 'ylim') <- c(-1, 1);
        
        return(plotData)
        
        
      },  # end function preparePlotIom
      
      
      .plotIom = function(image, ...) {     # Function name corresponds to r.yaml definition
        if (self$options$plotIom == FALSE) return(FALSE)
        if (is.null(image$state) || is.na(image$state)) return(FALSE)
        plotData <- image$state
        imm <- names(plotData)[1]
        mod <- names(plotData)[2]
        med <- names(plotData)[3]
         plot <- ggplot(image$state, 
                        aes_string(x = mod, y = imm, colour = med)) +
          geom_point() + geom_line() +
          coord_cartesian(ylim=c(-1.0, 1.0)) +
          scale_y_continuous(breaks=seq(-1, 1, 0.2)) 

        print(plot)
        TRUE
      },
    
  
    
    .prepareSimpleSlopes = function(data,xvar,yvar,mod, mvars, 
                                    parEst, vorw, int, vdichotomous,
                                    modLevels, path = NULL) {
      
      xmin <- min(data[,xvar], na.rm = TRUE)
      xmax <- max(data[,xvar], na.rm = TRUE)
      miny <- min(data[,yvar], na.rm = TRUE)
      maxy <- max(data[,yvar], na.rm = TRUE)
      
      # compute simple slopes
      
      if (vdichotomous) {
        modmin <- 0;
        modmax <- 1
      } else {
        modmin <- mean(data[,mod]) - stats::sd(data[,mod]);
        modmax <- mean(data[,mod]) + stats::sd(data[,mod]);
        modmin <- round(modmin,1)
        modmax <- round(modmax,1)
      }
      
      a <- subset(parEst, grepl("a1", parEst$label))[,"est"]
      b <- subset(parEst, grepl("b", parEst$label))[,"est"]
      vw <- subset(parEst, grepl(vorw, parEst$label))[,"est"]
      int <- subset(parEst, grepl(int, parEst$label))[,c("ci.lower","est","ci.upper")]
      
      if (vorw == "v") vw <- rep(vw,length(mvars))
      
      if (vdichotomous) {
        legendLabel <- modLevels
        maxLab <- max(stringr::str_length(modLevels[1]),stringr::str_length(modLevels[2]))
        minLabel <- stringr::str_pad(modLevels[1],maxLab, pad = " ")
        maxLabel <- stringr::str_pad(modLevels[2],maxLab, pad = " ")
      }
      else {
        legendLabel <- c("1 SD below mean", "1 SD above mean")
        minLabel <- c("for 1 sd below mean of moderator: ")  # for table
        maxLabel <- c("for 1 sd above mean of moderator: ")  # for table
        data$moderator <- data[,mod]
      }
      
      title <- paste0("Simple slopes in ", path , " path for indirect effect ")
      
      plotDat2 <- data.frame(yv=numeric(), xv=numeric(), mov=numeric(),mev=factor())
      xv <- c(xmin,xmax,xmin,xmax)
      mov <- c(modmin, modmin, modmax, modmax)
      
      for (i in seq_along(mvars)) {
        
        incmin <- vw*modmin
        slopemin <- a[i]*b[i] + vw[i]*int[i,]*modmin
        incmax  <- vw*modmax
        slopemax <- a[i]*b[i] + vw[i]*int[i,]*modmax
        
        pred <- rep(0,4)
        pred[1] <- incmin  + slopemin[2] * xmin;
        pred[2] <- incmin + slopemin[2] * xmax;
        pred[3] <- incmax  + slopemax[2] * xmin;
        pred[4] <- incmax  + slopemax[2] * xmax
        pred <- unlist(pred)
        
        plotDat1 <- data.frame(cbind(yv = pred, xv = xv))
        plotDat1$mov <- as.factor(round(mov,1))                      
        plotDat1$mev <- as.factor(rep(mvars[i],4))
         
        plotDat2 <- rbind(plotDat2,plotDat1)
         
         
      }  # loop mvars
      
      
      
      colnames(plotDat2) <- c(yvar, xvar, mod, "mediator")
     
      attr(plotDat2, 'legendLabel') <- legendLabel;
      attr(plotDat2, 'ylim') <- c(miny, maxy);
      attr(plotDat2, 'title') <- title;
      plotDat2[,mod] <- as.factor(plotDat2[,mod])
      plotDat2[,yvar] <- as.numeric(plotDat2[,yvar])
      plotDat2[,xvar] <- round(plotDat2[,xvar],2)  
      
      
      # if (path == "x-m") image <- self$results$plotSSx     # empty plot defined in r.yaml
      # if (path == "m-y") image <- self$results$plotSSy     # empty plot defined in r.yaml
      #  
      # 
      # 
      # image$setState(plotDat2)        # construct plot
      # 
      return(plotDat2)
      
    }, # end function prepareSS
    
      
      .plotSS = function(image, legendLabel=legendLabel, ...) {     # Function name corresponds to r.yaml definition
        if (is.null(image$state) || is.na(image$state)) return(FALSE)
        if (ncol(image$state) != 4) return(FALSE);
        plotDat2 <- image$state
        xvar <- names(plotDat2)[2]
        yvar <- names(plotDat2)[1]
        mod <-  names(plotDat2)[3]
        med <-  names(plotDat2)[4]
        plot <- ggplot(plotDat2, aes_string(x=xvar,y=yvar,colour=mod)) +
          geom_point() + geom_line() +
          ylim(attr(plotDat2, 'ylim')) +
          scale_colour_discrete(name  = mod, labels=attr(plotDat2, 'legendLabel')) 
        plot <- plot + facet_grid(mediator ~ .) 
        
         
         print(plot)
         return(TRUE);
          
      }
      
  )
)
