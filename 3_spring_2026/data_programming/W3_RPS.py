import numpy as np
rng = np.random.default_rng()

# 1. Using Globals and Conditionals (No Functions)
# 1.1 Global Setup: Configure the following global variables:
# • p1 = "rock"
# • p2 = "paper"
# 1.2 Nested Conditionals: Set up nested if statements such that:
# • The outer level tests what p1 plays (three possibilities)
# • The inner level tests what p2 plays (three possibilities)
# • Inside each combination (9 possible outcomes), print the winner, player 1 or player 2.
# 1.3 Testing: Change the values of p1 and p2 a few times to make sure the result is always as expected.

p1 = "rock"
p2 = "paper"

if p1 == "rock":
    if p2 == "rock":
        print("tie!")
    elif p2 == "paper":
        print("p2 wins!")
    elif p2 == "scissors":
        print("p1 wins!")
elif p1 == "paper":
    if p2 == "rock":
        print("p1 wins!")
    elif p2 == "paper":
        print("tie!")
    elif p2 == "scissors":
        print("p2 wins!")
elif p1 == "scissors":
    if p2 == "rock":
        print("p2 wins!")
    elif p2 == "paper":
        print("p1 wins!")
    elif p2 == "scissors":
        print("tie!")

# 1.4 Dictionary: For simple conditionals where there is always a single value equated to a single result,
# you can often use a dictionary where the key is the testing condition! Later we will see that this
# is very common when, for example, renaming the columns of a dataframe. Rewrite your nested
# conditional with:
# • A dictionary equating the winning plays as keys, to the play that it beats as values (e.g.
# "rock":"scissors")
# • A single if statement that uses that dictionary to print the winner (only three conditions).

p1 = "scissors"
p2 = "rock"

rps_beats = {"rock": "scissors", "paper": "rock", "scissors": "paper"}

if p1 == p2:
    print("tie!")
elif p2 == rps_beats[p1]:
    print("p1 wins!")
elif p1 == rps_beats[p2]:
    print("p2 wins!")
else:    
    print("Something went wrong")

# 1.5 Adding Randomness: Replace your static variables p1 and p2 with randomly generated choices:
# • At the top of your file, write: import numpy as np
# • Below that, write: rng = np.random.default_rng()
# • Then to use the random selection (for example, to select one of “a” or “b”, you will write:
#   rng.choice(["a", "b"])
# • Look up the official documentation for the Numpy random.choice function for more details.
# • Use that to set p1 and p2 to random picks from a list of the three options in this game.
# • Test your game!

rng = np.random.default_rng()
p1 = rng.choice(["rock", "paper", "scissors"])
p2 = rng.choice(["rock", "paper", "scissors"])
print(f"p1 plays {p1}, p2 plays {p2}")


# 2. Using Functions
# 2.1 The find winner Function: Building on your code from Question 1, create a function that holds
# all of the logic to decide the winner of a single game:
# • Your function should have two arguments, one for player 1’s move, and one for payer 2’s move.1
# • Instead of printing the winner, instead pass the result out with a return statement.
# • Hint: Move your conditional into the function.


numbers = [55, 100, 5, 0, -5, 42]

result = []
for num in numbers:
    new_num = (num * 2) / 100
    result.append(new_num)

print(result)

# 2.2 The ready set go Function: Right now we are using random choices for each player, but we
# can imagine adopting many strategies! We might always play rock, or play scissors twice as often
# as the others. Move your decision for what to play into a function. Even though it is currently
# only a single line (returning the random choice) it is a place of organization that holds any future
# development related to choosing what to play. Set both p1 and p2 equal to the result of this
# function.
# • Hint: This function should not take any arguments. It just needs to return the random choice
# of rock, paper, or scissors.

#2.3 The play once Function: You will now create what is commonly described as a main function.
# This is a function that is responsible for calling all your other functions, with little or no code
# besides that. To create the play once function, you should:
# • Move the assignment of p1 and p2 into the function.
# • Using a format string, print out what each player chooses.
# • Call your find winner function.
# • Print the winner using a format string.
# • Finally, test your code by calling the play once function a few times.