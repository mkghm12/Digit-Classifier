# importing helper functions

source('./N_layer_model.R')
source('./confusionmatrix.R')
source('./displaydata.R')

# reading training,cross-validation and testing dataset of image-matrices of digits 
X=t(as.matrix(read.csv('./datamat_train.csv',header = FALSE)))
X_CV = t(as.matrix(read.csv('./mk_CV.csv',header = FALSE)))
X_test = t(as.matrix(read.csv('./mk_test.csv',header = FALSE)))
Y=t(as.matrix(read.csv('./datalabel_train.csv',header = FALSE)))
Y_CV=t(as.matrix(read.csv('./man_label_test.csv',header = FALSE)))
Y_test=t(as.matrix(read.csv('./man_label_CV.csv',header = FALSE)))

# display randomly selected data 100 images
index = sample((1:dim(X)[2]),100)
displaydata(X[,index])

#setting no. of iterations and learning_rate 
iterations = 3000
alpha = 1

# running neural network model and plotting cost vs itrations
p = N_layer_model(X,Y,c(400,25,10),learning_rate = alpha,num_iterations =iterations ,print_cost = TRUE,lambda = 1)
J_iter=p[[2]]

# plot cost vs iterations
plot(1:length(J_iter),J_iter,xlab = "no of iterations",ylab = "Cost",type = 'l',main = paste('Cost vs Iteration \n(learning rate = ',alpha,')\n(iterations = ',iterations,')'),col="red",ylim = c(0,max(J_iter)+1))
readline(prompt = "Press [enter] to continue")

# make predictions on cross-validation dataset using forward propagation on optimized parameters 
# and printing confusion matrix
optimized_parameters = p[[1]]
q=L_model_forward((X_CV),optimized_parameters)
AL=t(q$AL)
prediction=max.col(AL)
cm = confusionMatrix(Y_CV,prediction)
cat("\nconfusion matrix for cross-validation dataset = \n")
print(cm)
accuracy = mean(Y_CV==prediction)
cat("\naccuracy for validation dataset = ",accuracy*100,"%")
readline(prompt = "Press [enter] to continue")

# make predictions on testing dataset using forward propagation on optimized parameters 
# and printing confusion matrix
q=L_model_forward((X_test),optimized_parameters)
AL=t(q$AL)
prediction=max.col(AL)
cm = confusionMatrix(Y_test,prediction)
cat("\n\nconfusion matrix for testing dataset = \n")
print(cm)
accuracy = mean(Y_CV==prediction)
cat("\naccuracy for testing dataset = ",accuracy*100,"%\n")
readline(prompt = "Press [enter] to continue")
# prediction of a number by its image matrix
index_of_prediction = sample(c(1:dim(X_test)[2]),1)
displaydata(as.matrix(X_test[,index_of_prediction]))
q=L_model_forward(as.matrix(X_test[,index_of_prediction]),optimized_parameters)
AL=t(as.matrix(q$AL))
prediction=max.col(AL)
print("predicted number= ")
print(prediction%%10)

