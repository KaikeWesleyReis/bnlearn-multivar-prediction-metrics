# FUNCTION - Multi Variable Discrete prediction
bnMultiVarPrediction <- function(bnFit, trainSet, testSet, to_predict, to_evidence, nSamples = NULL, calcFunction = NULL){
  # Probabilities predictions for each sample
  pred_list_prob = list()
  
  # Dominant output predicted for each sample
  pred_list_domi = list()
  
  # Auxiliar variables
  N <- nrow(testSet) # Number of samples
  np <- length(to_predict) # Number of variables to predict
  
  # Loop into all possible variables to Generate our output format
  for(j in to_predict){
    # TARGET LEVELS (CATEGORIES)
    tv_lvls =  levels(trainSet[,j])
    # DATAFRAME - PROBABILITIES PREDICTIONS
    prob_pred <- setNames(data.frame(matrix(ncol = length(tv_lvls), nrow = N)), tv_lvls)
    # DATAFRAME - DOMINANT OUTCOME
    domi_pred <- setNames(data.frame(matrix(ncol = 1, nrow = N)), j)
    # LISTS - APPEND
    pred_list_prob[[j]] = prob_pred
    pred_list_domi[[j]] = domi_pred
  }
  
  # Multi Var Prediction :: PREDICT FUNCTION
  if(calcFunction == 'predict' || is.null(calcFunction)){
    for (i in 1:N){ 
      # Prediction process
      for(j in to_predict){
        predicted = predict(bnFit, j, testSet[i, names(testSet) %in% to_evidence], prob = TRUE, method = 'bayes-lw')
        ## TAKING IMPORTANT RESULTS
        dominant = as.character(predicted)
        probs = attr(predicted,'prob')
        ## DOMINANT OUTPUT
        pred_list_domi[[j]][i,j] = dominant
        ## PROBABILITY OUTPUT
        tv_lvls = colnames(pred_list_prob[[j]])
        ## LOOP INTO ALL LEVELS TO SAVE YOUR PROBABILITY
        for(k in tv_lvls){ 
          pred_list_prob[[j]][i,k] = as.numeric(probs[k,])
        }
      }
    }    
  }
  
  # Multi Var Prediction :: CPDIST FUNCTION
  else if(calcFunction == 'cpdist'){
    # nSamples Verification
    if(is.null(nSamples) || typeof(nSamples) != 'double'){ # Default value for N samples is 10.000 samples generated
      nSamples <- 10000
    }
    # Prediction process
    for (i in 1:N){
      predicted = cpdist(fitted = bnFit, nodes = to_predict, evidence = as.list(test[i, names(test) %in% to_evidence]), n = nSamples, method = 'lw')
      for(j in to_predict){ # Loop into all variables to be predicted
        # PROBABILITY OUTPUT
        probs = table(predicted[,j])/nrow(predicted)
        # DOMINANT OUTPUT
        pred_list_domi[[j]][i,j] = names(which.max(probs))
        # KEEP PROB VALUES
        tv_lvls = colnames(pred_list_prob[[j]])
        for(k in tv_lvls){ # Loop into all lvls to save the probability
          pred_list_prob[[j]][i,k] = as.numeric(probs[k])
        }
      }
    }    
  }
  
  # Turn dominant output in factors - help metrics CM
  for(j in to_predict){
    #levels(pred_list_domi[[j]]) <- levels(trainSet[,j])
    pred_list_domi[[j]] <- factor(x = unlist(pred_list_domi[[j]], use.names = F), levels = levels(trainSet[,j]))
  }
  # Returning
  ret_list <- list("probList" = pred_list_prob, "dominantList" = pred_list_domi)
  return(ret_list)
}

# FUNCTION - Metrics :: Confusion Matrix by OVA and Scoring Rules
bnMetricsMultiVarPrediction <- function(reference, prediction, predProbList){
  # AUXILIAR VARs
  np <- length(names(prediction))
  to_predict <- names(prediction)
  N <- nrow(reference)
  
  # METRICS DATASET CREATION
  metricsType <- c('accuracy','accuracyOVA','sensibilityOVA','specificityOVA','precisionOVA','f1-scoreOVA','mccOVA','sphericalPayoff','brierLoss','logLoss')
  metrics <- setNames(data.frame(matrix(ncol = length(metricsType), nrow = length(to_predict)), row.names = to_predict), metricsType)
  
  
  # CONFUSION MATRIX
  cm_list <- setNames(vector(mode = "list", length = np), to_predict)
  for(j in to_predict){
    cm_list[[j]] <- table(prediction[[j]], reference[[j]])
    
    #### PRINT - CM
    #cat(paste('CONFUSION MATRIX - ',j,' Variable:\n'))
    #print(cm_list[[j]])
    #cat('\n')
    
  }
  
  # ONE VS ALL (OVA) CALCULATION
  ova_list <- setNames(vector(mode = "list", length = np), to_predict)
  for(j in to_predict){
    # OVA INIT
    lvls_var = colnames(cm_list[[j]])
    ova_list[[j]] <- setNames(vector(mode = "list", length = length(lvls_var)), lvls_var)
    
    ### PRINT
    #cat(paste('ONE VS ALL - ',j,'Variable:\n\n'))
    
    # LOOP TO DEVELOP EACH v CLASS VS ALL MATRIX FOR A j VARIABLE
    for(v in lvls_var){
      ## MATRIX CREATION
      ova_lvl <- matrix(0,nrow = 2, ncol = 2)
      row.names(ova_lvl) = c(v,'All')
      colnames(ova_lvl) = c(v,'All')
      ## RENAME cm_list FROM OUR j VARIABLE TO BETTER CODE READING
      cm = cm_list[[j]]
      ## AUXILIAR VARIABLES
      rs = rowSums(cm) 
      cs = colSums(cm)
      n = sum(cm)
      
      ## TAKING BINARIES VALUES FROM A BINARY C.M.
      tp = cm[v,v]
      fn = rs[v] - cm[v,v]
      fp = cs[v] - cm[v,v]
      tn = n - rs[v] - cs[v] + cm[v,v]
      
      ## INSERT THE VALUES IN OVA C.M.
      ova_lvl[v,v] <- tp
      ova_lvl[v,'All'] <- fn
      ova_lvl['All',v] <- fp
      ova_lvl['All','All'] <- tn
      
      ## SAVE OVA INSIDE THE LIST FOR THAT j TARGET
      ova_list[[j]][[v]] <- ova_lvl
      
      #### PRINT - OVA
      #print(ova_list[[j]][[v]])
      #cat('\n')
      
    }
  }
  
  # CONFUSION MATRIX METRICS - ONE VS ALL :: SEN, SPECIFICITY, RECALL, ACCURACY, F1-SCORE, MCC, ACCURACY OVA
  for(j in to_predict){
    cm <- cm_list[[j]]
    # ACCURACY
    cmACC <- 100*(sum(diag(cm))/sum(cm))
    
    ### SAVE INTO DATAFRAME RESULT
    metrics[j,'accuracy'] <- round(cmACC,2)
    
    ### PRINT - ACCURACY MULTI-CLASS
    cat(paste('#### CONFUSION MATRIX METRICS - ',j,' Variable ####\n'))
    cat(paste('+ ACCURACY: ', round(cmACC,2),'\n'))
    
    ## OVA :: AUXILIAR VARIABLES - REPRESENT VECTOR FOR EACH v LEVELS VS ALL
    acc = c()
    sen = c()
    spe = c()
    pre = c()
    f1s = c()
    mcc = c()
    ## OVA :: AUXILIAR VARs
    lvls <- names(ova_list[[j]])
    len_lvls <- length(lvls)
    ## OVA :: LOOP INTO ALL l LEVELS FROM j VARIABLE TO CALCULATE METRICS FOR EACH LEVEL
    for(l in lvls){
      ## AUXILIAR VARIABLE TO IMPROVE CODE READING
      ovaCM <- ova_list[[j]][[l]]
      ## GETTING BINARY TERMS FROM ovaCM
      tp = ovaCM[l,l]
      fn = ovaCM[l,'All']
      fp = ovaCM['All',l]
      tn = ovaCM['All','All']
      
      ## CALCULATE METRICS FOR THAT LVL AND SUM WITH PREVIOUS PART 1
      acc = c(acc,((tp+tn)/(tp+tn+fp+fn)))
      sen = c(sen,(tp/(tp+fn)))
      spe = c(spe,(tn/(tn+fp)))
      pre = c(pre,(tp/(tp+fp)))
      
      # AUXILIAR CALCULATION FOR MCC AND F1S
      sen_unq = tp/(tp+fn)
      pre_unq = tp/(tp+fp)
      f1s_unq = 2*((sen_unq*pre_unq)/(sen_unq+pre_unq))
      mcc_num = (tp*tn) - (fp*fn)
      mcc_den = (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
      
      ## CALCULATE METRICS FOR THAT LVL AND SUM WITH PREVIOUS PART 2
      f1s = c(f1s,f1s_unq)
      mcc = c(mcc,(mcc_num/sqrt(mcc_den)))
    }
    ## OVA :: RESULT FOR EACH j VARIABLE BASED ON A MEAN FROM ALL OVA MATRIX FROM THAT VARIABLE i.e. ALL LEVELS OVA MATRIX
    cmACC_ova <- 100*(sum(acc, na.rm = T)/len_lvls)
    cmSEN_ova <- 100*(sum(sen, na.rm = T)/len_lvls)
    cmSPE_ova <- 100*(sum(spe, na.rm = T)/len_lvls)
    cmPRE_ova <- 100*(sum(pre, na.rm = T)/len_lvls)
    cmF1S_ova <- 100*(sum(f1s, na.rm = T)/len_lvls)
    cmMCC_ova <- 100*(sum(mcc, na.rm = T)/len_lvls)
    
    ### PRINT - OVA METRICS
    cat(paste('+ ACCURACY OVA: ', round(cmACC_ova,2),'\n'))
    cat(paste('+ SENSIBILITY OVA: ', round(cmSEN_ova,2),'\n'))
    cat(paste('+ SPECIFICITY OVA: ', round(cmSPE_ova,2),'\n'))
    cat(paste('+ PRECISION OVA: ', round(cmPRE_ova,2),'\n'))
    cat(paste('+ F1-SCORE OVA: ', round(cmF1S_ova,2),'\n'))
    cat(paste('+ MCC OVA: ', round(cmMCC_ova,2),'\n'))
    
    ### SAVE INTO DATAFRAME RESULT
    metrics[j,'accuracyOVA'] <- round(cmACC_ova,2)
    metrics[j,'sensibilityOVA'] <- round(cmSEN_ova,2)
    metrics[j,'specificityOVA'] <- round(cmSPE_ova,2)
    metrics[j,'precisionOVA'] <- round(cmPRE_ova,2)
    metrics[j,'f1-scoreOVA'] <- round(cmF1S_ova,2)
    metrics[j,'mccOVA'] <- round(cmMCC_ova,2)
    
  }
  
  # SCORING RULES CALC
  for(j in to_predict){
    ## AUXILIAR VARIABLES
    aux_prob <- predProbList[[j]]
    cor_test <- reference[j]
    states <- colnames(aux_prob)
    
    ## TERMS FOR THE FORMULAS :: init
    pc = 0
    pj_2 = 0
    sum_pj = 0
    
    ## TERMS FOR :: SPHERICAL PAYOFF
    smp_spher_payoff = 0
    srSP = 0
    
    ## TERMS FOR :: BRIER LOSS
    smp_brier_loss = 0
    srBL = 0
    
    ## TERMS FOR :: LOG LOSS
    smp_log_loss = 0
    srLL = 0
    
    ## LOOP THROUGH ALL THE N SAMPLES PREDICTED
    for(i in 1:N){
      correct_state = cor_test[i,]
      ## TERMS CALCULATION
      pc = aux_prob[i,correct_state]
      sum_pj = 0
      for(s in states){
        pj_2 = aux_prob[i,s] * aux_prob[i,s]
        sum_pj = sum_pj + pj_2
      }
      
      ## SCORING RULE CALC FOR EACH SAMPLE - SP
      smp_spher_payoff = pc/sqrt(sum_pj)
      srSP = srSP + smp_spher_payoff
      
      ## SCORING RULE CALC FOR EACH SAMPLE - BL 
      #smp_brier_loss = 1-(2*pc+sum_pj)
      smp_brier_loss = 2*pc-sum_pj
      srBL = srBL + smp_brier_loss   
      
      ## SCORING RULE CALC FOR EACH SAMPLE - LL
      smp_log_loss = (-log(pc))
      srLL = srLL + smp_log_loss
    }
    
    ## RESULT - SPHERICAL PAYOFF (MOAC)
    srSP = srSP/N
    srBL = srBL/N
    srLL = srLL/N
    
    ### PRINT - SCORING RULES
    cat(paste('#### SCORING RULES METRICS - ',j,' Variable ####\n'))
    cat(paste('+ SPHERICAL PAYOFF: ', round(srSP,2),'\n'))
    cat(paste('+ BRIER LOSS: ', round(srBL,2),'\n'))
    cat(paste('+ LOG LOSS: ', round(srLL,2),'\n'))
    
    ### SAVE INTO DATAFRAME RESULT
    metrics[j,'sphericalPayoff'] <- round(srSP,2)
    metrics[j,'brierLoss'] <- round(srBL,2)
    metrics[j,'logLoss'] <- round(srLL,2)
  }
  
  # RETURN
  ret_list <- list('cmList' = cm_list, 'ovaList' = ova_list, 'eval' = metrics)
  
}