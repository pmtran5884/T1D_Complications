Complication="Autonomic_Neuropathy3"
varstoassess<-c("Autonomic_Neuropathy3","Peripheral_Neuropathy3","DM_Retinopathy3","DM_Nephropathy3","HTN3","Dyslipidemia3","CAD3","Prior_TIA3","Prior_MI3","Prior_CVA3","Dr_Age","Dur","Hemoglobin","Albumin","avg.Creatinine","avg.Systolic","avg.BUN","avg.MicroAlb","AvgA1c","Sex","HDL","Max_HbA1c")

compare_3_models <- function(T1Dcomp_alldata,Complication,varstoassess){
# 1) DPN
dpn<-T1Dcomp_alldata[!is.na(T1Dcomp_alldata[,Complication]),
                     varstoassess]
dpn_idx = createDataPartition(dpn[,Complication], p = 0.75, list = FALSE)
dpn_trn = dpn[dpn_idx, ] 
dpn_tst = dpn[-dpn_idx, ]

##TRAIN Preprocessing
dpn_dmy <- dummyVars(~., dpn_trn)
x <- data.frame(predict(dpn_dmy, newdata = dpn_trn))[,-c(1,2)]

dpn_preobj <- preProcess(x, method = "knnImpute")
dt3 <- predict(dpn_preobj, x)

dpn_trn1<-as.matrix(dt3[,-findLinearCombos(dt3)$remove])
# Convert the outcome (class) to a numerical variable
dpn_trn_y <- ifelse(dpn_trn[,Complication] == "Yes", 1, 0)


##TEST Preprocessing
# Dumy code categorical predictor variables
x <- data.frame(predict(dpn_dmy, newdata = dpn_tst))[,-c(1,2)]
dt3 <- predict(dpn_preobj, x)
dpn_tst1<-as.matrix(dt3[,-findLinearCombos(dt3)$remove])


##LASSO
cv.lasso <- cv.glmnet(dpn_trn1, dpn_trn_y, alpha = 1, family = "binomial")
lasso <- glmnet(dpn_trn1, dpn_trn_y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

# Make predictions on the test data
dpn_lasso_test_prob = predict(lasso, newx = dpn_tst1, type = "response",na.action = na.pass)
dpn_lasso_test_roc = roc(dpn_tst[,Complication] ~ dpn_lasso_test_prob, plot = TRUE, print.auc = TRUE)


#########     RFE      ####################
glmFuncs=lmFuncs
glmFuncs$fit=function (x, y, first, last, ...)  {   
  tmp <- as.data.frame(x)   
  tmp$y <- y   
  glm(y ~ ., data = tmp, family=binomial(link='logit'))}

subsets <- c(1,5,7,8,10,13, 15, 18,21)

ctrl <- rfeControl(functions = glmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

rfelogit <- rfe(dpn_trn1, dpn_trn_y,
                sizes = subsets,
                rfeControl = ctrl)


dpn_rfelogit_test_prob = predict(rfelogit, newdata = dpn_tst1, type = "prob",na.action = na.pass)
dpn_rfelogit_test_roc = roc(dpn_tst[,Complication] ~ dpn_rfelogit_test_prob, plot = TRUE, print.auc = TRUE)

#### Manual model 
dpn_trn1_pred<-cbind.data.frame(dpn_trn[,Complication],dpn_trn1)
colnames(dpn_trn1_pred)[1]<-"Complication"
my_dpn_model <- train(
  form = Complication~Dr_Age+Dur+avg.Systolic+AvgA1c,
  method = "glm",
  family = "binomial",
  data = dpn_trn1_pred,
  trControl = trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5),
  na.action = na.pass
)

dpn_customlogit_test_prob = predict(my_dpn_model, newdata = dpn_tst1, type = "prob",na.action = na.pass)
dpn_customlogit_test_roc = roc(dpn_tst[,Complication] ~ dpn_customlogit_test_prob$Yes, plot = TRUE, print.auc = TRUE)


comp_obj<-list("trn"=dpn_trn1,
               "trn_y"=dpn_trn_y,
               "tst"=dpn_tst1,
               "tst_y"=dpn_tst$Complication,
               "lasso_mod"=lasso,
               "lasso_coef"=coef(lasso),
               "rfelogit_mod"=rfelogit,
               "rfelogit_coef"=rfelogit$fit,
               "customlogit_mod"=my_dpn_model,
               "customlogit_coef"=summary(my_dpn_model),
               "lasso_tst_prob"=dpn_lasso_test_prob,
               "lasso_tst_roc"=dpn_lasso_test_roc,
               "rfelogit_tst_prob"=dpn_rfelogit_test_prob,
               "rfelogit_tst_roc"=dpn_rfelogit_test_roc,
               "customlogit_tst_prob"=dpn_customlogit_test_prob,
               "customlogit_tst_roc"=dpn_customlogit_test_roc)

comp_obj
}
