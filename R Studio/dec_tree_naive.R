#DECISION TREE
library(rpart)			# Popular decision tree algorithm
library(rattle)			# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)			# Color selection for fancy tree plot
library(party)			# Alternative decision tree algorithm
library(partykit)			# Convert rpart object to BinaryTree
data(iris)				# Get some data
data <- iris

# Make big tree
form <- as.formula(Species ~ .)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))

plot(tree.1)			# Will make a mess of the plot
text(tree.1)

prp(tree.1,					# Will plot the tree
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    shadow.col="gray",   # shadows under the leaves
    branch.lty=3,        # draw branches using dotted lines
    branch=.5,           # change angle of branch lines
    faclen=0,            # faclen=0 to print full factor names
    trace=1,             # print the automatically calculated cex
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col='green', border.col='green',   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)              # round the split box corners a tad

# Interactively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj 	# interactively trim the tree
prp(new.tree.1)

tree.2 <- rpart(form,data)		# A more reasonable tree
prp(tree.2)                                 	# A fast plot													
fancyRpartPlot(tree.2)		# A fancy plot from rattle

#NAIVE BAYES
library(naivebayes)
comp <- read.csv("computer.csv",header = TRUE)
nb_comp<-naive_bayes(buys_computer~age+income+student+credit_rating, 
                     data = comp)

#predict new object
comp.test <- data.frame(t(c("<=30","medium","yes","Fair")))
predict(nb_comp,comp.test)
