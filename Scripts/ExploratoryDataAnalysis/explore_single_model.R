source("./Functions/lecospectR.R")
#Explore a single model

#Best adaboost model
#Model path but only useful for looking at model. Specify model in config.json
model_path <- "./test/f02a4a5d-beff-418b-8c13-20df941457c7/model.rda"
mod<-load_model(model_path) #%>% str
#load_model(model_path)$forest %>% str
load_model(model_path)$forest$independent.variable.names
tree1Info<-treeInfo(load_model(model_path), tree=1)
tree1Info$tree<-"tree1"
tree2Info<-treeInfo(load_model(model_path), tree=2)
tree2Info$tree<-"tree2"

bothtreeinfo<-rbind(tree1Info,tree2Info)

bothtreeinfo %>% group_by(tree,splitvarName, splitval) %>% tally() %>% pivot_wider(names_from = tree, values_from = n) %>% arrange(splitvarName, splitval) %>% print(n=500)
bothtreeinfo %>% group_by(tree,prediction) %>% tally() %>% pivot_wider(names_from = tree, values_from = n) %>% arrange(prediction) %>% print(n=500)

load_model(model_path) %>% treeInfo(tree=2) %>% group_by(splitvarName) %>% tally %>% print(n=80)
load_model("./test/3f02a4a5d-beff-418b-8c13-20df941457c7.rda")$call

tree1<-as.data.frame(load_model(model_path)$forest$split.varIDs[1])
tree2<-as.data.frame(load_model(model_path)$forest$split.varIDs[2])
colnames(tree1)<-"splitVarIDs"
colnames(tree2)<-"splitVarIDs"
  branch1<-load_model(model_path)$forest$split.values[1] %>% as.data.frame()
  colnames(branch1)<-"splitVals"
  branch2<-load_model(model_path)$forest$split.values[2] %>% as.data.frame()
  colnames(branch2)<-"splitVals"

  tree1.2<-cbind(tree1,branch1)
  tree2.2<-cbind(tree2,branch2)

  tree1.2$tree<-"1"
  tree2.2$tree<-"2"

  tree3<-rbind(tree1.2,tree2.2)
  head(tree3)
  tree3 %>% group_by(splitVarIDs,tree) %>% tally() %>% print(n=142)