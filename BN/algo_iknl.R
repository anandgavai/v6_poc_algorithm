library(bnlearn)
library(tidyverse)
library(graph)
library(gRbase)
library(gRain)

# This algorithm train takes teh data hosted at iknl data station node and returns a prediction value from it based on Bayesian network model

iknlbn<-function (){

    Data <-read_csv("./data/iknl_node/iknl_data.csv",show_col_types = FALSE)
    Data[is.na(Data)]<-"Missing data"

    set.seed(0)

    Sampled_rows<-sample(1:length(Data$Fraud),ceiling(length(Data$Fraud)*0.2),rep=FALSE)

    BNdata_train<-Data[-Sampled_rows,]

    BNdata_test<-Data[Sampled_rows,]


    BNdata_train <- as.data.frame(BNdata_train)
    BNdata_train[] <- lapply( BNdata_train, factor)
    BNdata_test <- as.data.frame(BNdata_test)
    BNdata_test[] <- lapply( BNdata_test, factor)


    #structure leaning
    BN_structure<- tree.bayes(BNdata_train,'Fraud')

    ## test other learning algorithms #

    #BN_structure<-gs(BNdata_train)  ### Grow-Shrink (GS) Constraint-based algorithm
    #BN_structure<-iamb(BNdata_train)### The incremental association Markov boundary algorithm
    #BN_structure<-hc(BNdata_train)### Hill climbing algorithm

  #  graphics::plot(BN_structure)
    #graphviz.plot(BN_structure, shape = "ellipse")


    BIC(BN_structure, BNdata_train)


    ## parameter learning
    BN_trained<-bn.fit(x=BN_structure,data=BNdata_train,method = "bayes")

    ## convert to gRain
    fitted.grain = as.grain(BN_trained)

    #Retrieve marginal probability distribution of each node
    Mprobs = querygrain(fitted.grain, type = "marginal")
   # Mprobs


    ## internal validation: confusion matrix
    pred_in <- predict(fitted.grain , response = c("Fraud"), newdata = BNdata_train, type = "class")

    pred_in <- as.data.frame(pred_in)
    table(BNdata_train$Fraud,pred_in$Fraud)
    Detection_probability_inter<-sum(diag(table(BNdata_train$Fraud,pred_in$Fraud)))/sum(table(BNdata_train$Fraud,pred_in$Fraud))
    # Detection_probability_inter


    ## external validation: confusion matrix
    pred_ex <- predict(fitted.grain, response = c("Fraud"), newdata=BNdata_test, type = "class")

    pred_ex <- as.data.frame(pred_ex)

    table(BNdata_test$Fraud,pred_ex$Fraud)
    Detection_probability_exter<-sum(diag(table(BNdata_test$Fraud,pred_ex$Fraud)))/sum(table(BNdata_test$Fraud,pred_ex$Fraud))
    # Detection_probability_exter

    pred <- list(
        description = "Bayesian Network model learned from data hosted at IKNL",
        accuracy = Detection_probability_exter
    )

    pred_value<-toJSON(pred,auto_unbox=TRUE)

    return (pred_value)

}









