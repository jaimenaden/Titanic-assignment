# Titanic-assignment
# In this report we explain the methods use to predict whether a passenger in the Titanic survived. 
# The sinking of RMS Titanic was popularized by the movie ”Titanic” which was released in 1997, 
# which starred Leonardo DiCaprio and Kate Winslet. The dataset was obtained from https://www.kaggle.com/c/titanic . 
# The file description and file format can be found below.

# titanic train: the training set 
# assign2 titanic test: the test set
#The training set contains 12 covariates with 900 observations and the training set contains 12 covariates with 409 onbservations.
# ID: ID of the passenger.
# pclass: Ticket class. A proxy for socio-economic status (SES). 1 = Upper, 2 = Middle, 3= Lower
# survived: Whether the passenger survived. 0 = No, 1 = Yes
# name: Name of the passenger
# sex : Gender of the passenger
# age: Age of the passenger. Age is fractional if less than 1. If the age is estimated, it is in the form of xx.5
# sibsp: Number of siblings / spouses aboard the Titanic. Sibling = brother, sister, stepbrother, stepsister; Spouse = husband, wife (mistresses and fiancs were ignored)
# parch: Number of parents / children aboard the Titanic. Parent = mother, father; Child = daugh- ter, son, stepdaughter, stepson. Some children travelled only with a nanny, therefore parch=0 for them.
# ticket: Ticket number
# fare: Passenger fare
# cabin: Cabin number
#embarked: Port of Embarkation, C = Cherbourg, Q = Queenstown, S = Southampton
