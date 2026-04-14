import numpy as np

"""4. Using Classes"""
# 4.1 Class Design: Using classes gives us the flexibility to grow our program’s
# capabilities. Start by looking at your game with functions and thinking about what sort of options you might add to
# expand what you have so far. For example:
# • Create “players” that will use different strategies.

p1 = Player("Chris")
p2 = Player("Jeff")

print(p1.name, p2.name)

class PlayerPlays():
    def _init_(self, plays, random_strategy):
        self.plays = plays, 
        self.random_strategy =  
        """Needs to create different strategies for players"""
    pass
    
    def plays()
        """"Needs to define potential plays, some sort of dictionary"""
    pass

    def random_strategy()
        """Needs to decide random strategies considering the different options available in the dictionary"""
    pass


# • Having “players” keep track of how many wins they have after playing repeatedly.



# • Design the “game” to interact two or more players N times and report results.
# • Many other possibilities! Before you continue, think for a minute about how you would
# implement these examples. Some of these we will implement in this assignment, while others
# are just thought exercises to help you understand planning a class. No answer needs to be
# submitted for this question.




# 4.2 The Player Class: First, create a class that plays a random choice with equal weight:
# • The initialization method should require
# • a name argument, which is assigned as an instance attribute [variable],
# • along with attributes to record its number of wins (starting at zero),and
# • the choices available to it to play (the standard three).
# • Move your ready set go function to a method of this class that accesses the instance attributes
# as required. No global variables should be referenced here.
# • Test your code using this Player class in place of the simple strings (e.g. p1 = "rock") we had
# been using.
# Step 1: add just the name argument and test!
# Step 2: add the non-argument (internal) instance variables and test!
# Step 3: add the ready_set_go() method and test!

# 4.3 Adding kwargs to the Player Class: Modify your class from Question 4.2 to take a key word
# argument that controls the playing behavior. It should default to the random equal-weight choice
# as before, but allow for different weights to be applied to each of the three choices when the kwarg
# is overridden. See the Numpy documentation for help.
# Step 1: add a string kwarg and test!
# Step 2: read the docuentation, add a kwarg of probabilities to the __init__, and test!
# Step 3: test adding kwarg of probabilities to the choice method outside of the class!
# Step 4: add kwarg of probabilities to ready_set_go() and test!

# 4.4 The Game Class: Finally, create a class that handles all of the setup and execution of the game:
# • The initialization method should have no arguments, but should set up attributes named p1
# and p2, which are each equal to one instance of Player with different names.
# • Move the find winner function into a method here and be sure it does not reference any
# globals.
# • Move the play once function into a method here. Modify it so that it updates each player
# instance’s number of wins after each game, and correctly reports the current winner and their
# total wins. Again, make sure no globals are referenced.
# • Hint: When testing this, make sure that you have restarted your Python interpreter since
# Part 2! You may inadvertently be referring to functions or globals you wrote earlier, without
# realizing you aren’t using your new methods properly.
# Step 1: draft the Game class!
# Step 2: add p1 & p2 (non-arg) instance variables, then test!
# Step 3: add the find_winner() method, then test!
# Step 4: add the play_once() method, then test!

# 4.5 Execution: Create an instance of your Game class, then call the play once method several times.
# Make sure your results are correct, and that players are updating their win to