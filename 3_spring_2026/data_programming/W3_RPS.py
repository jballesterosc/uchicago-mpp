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
p2 = "paper"

dic_plays = {"rock": "scissors", "paper": "rock", "scissors": "paper"}

if p1 == p2:
    print("tie!")
elif dic_plays[p1] == p2:
    print("p1 wins!")
else:    print("p2 wins!")

# 1.5 Adding Randomness: Replace your static variables p1 and p2 with randomly generated choices:
# • At the top of your file, write: import numpy as np
# • Below that, write: rng = np.random.default_rng()
# • Then to use the random selection (for example, to select one of “a” or “b”, you will write:
#   rng.choice(["a", "b"])
# • Look up the official documentation for the Numpy random.choice function for more details.
# • Use that to set p1 and p2 to random picks from a list of the three options in this game.
# • Test your game!



course = "programming"
grades = {"Zhixin": {"programming": 95, "basket weaving": 100}, 
          "Sarah": {"cooking": 100, "programming": 99}, 
          "Anthony": {"math": 58, "econ": 77}, 
          "Haoxuan": {"programming": 75, "econ": 100}, 
          "Angela ": {"programming": 88, "programming 2": 91}, 
          "Richard ": {"gardening": 60, "studying": 59}}

for name, courses in grades.items():
    if course in courses:
        score = courses[course]
        if score >= 60:
            result = "passed"
        else:
            result = "failed"
        print(f"The student {name} {result} the class with a score of {score}.")
    else:
        print(f"The student {name} did not take {course}.")
