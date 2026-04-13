# prisioner dilemma
import numpy as np
import pandas as pd
rng = np.random.default_rng()

# 1.1 The Standard Game: Create a simple game using conditionals, strings, and ints, with no func-
# tions or classes.
# • Make two variables representing strategies you set as a constant (e.g. p1 = "c").

p1 = "c"
p2 = "d"

# • Use a set of nested if statements to determine the payoffs given the classic Prisoner’s Dilemma
# results. Store the resulting payoffs as variables.

if p1 == "c" and p2 == "c":
    p1_payoff = 3
    p2_payoff = 3
elif p1 == "c" and p2 == "d":
    p1_payoff = 1
    p2_payoff = 5
elif p1 == "d" and p2 == "c":
    p1_payoff = 5
    p2_payoff = 1
elif p1 == "d" and p2 == "d":
    p1_payoff = 1
    p2_payoff = 1

# • Have them play once and print the results. Test again by changing the strategy constants to
# see that you get the expected scores. Show printed results for two more tested combinations
# (three total)

print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")

p1 = "c"
p2 = "c"

if p1 == "c" and p2 == "c":
    p1_payoff = 3
    p2_payoff = 3
elif p1 == "c" and p2 == "d":
    p1_payoff = 1
    p2_payoff = 5
elif p1 == "d" and p2 == "c":
    p1_payoff = 5
    p2_payoff = 1
elif p1 == "d" and p2 == "d":
    p1_payoff = 1
    p2_payoff = 1

print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")


p1 = "d"
p2 = "d"

if p1 == "c" and p2 == "c":
    p1_payoff = 3
    p2_payoff = 3
elif p1 == "c" and p2 == "d":
    p1_payoff = 1
    p2_payoff = 5
elif p1 == "d" and p2 == "c":
    p1_payoff = 5
    p2_payoff = 1
elif p1 == "d" and p2 == "d":
    p1_payoff = 1
    p2_payoff = 1

print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")

# 1.2 Dictionaries: Nested conditionals are confusing, hard to debug, and do not scale well! Repeat
# Step 1.1, but replace your payoff conditionals with a nested dictionary where the first key is what
# “you” play, and the second (inner) key is what your “opponent” plays, with inner values being
# the integer payouts. Make sure you test multiple combinations again, and print the results for two
# different combinations.

p1 = "c"
p2 = "d"

game_payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}

p1_payoff = game_payoffs[p1][p2]
p2_payoff = game_payoffs[p2][p1]

print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")

p1 = "c"
p2 = "c"

p1_payoff = game_payoffs[p1][p2]
p2_payoff = game_payoffs[p2][p1]

print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")

# 1.3. Creating Player Functions: Redo Step 1.1 again, but this time instead of representing players
# as variables with constants as their strategy, write functions to return strategies.
# • The first function should always return defect.
# • The second should randomly choose between returning cooperate and defect with equal weight.
# • Neither function should take any arguments.
# • Utilize the rest of your code from Step 1.2, including the payoff dictionary, to have them play
# once. Test several times to see the random choice strategy in action, and print the results of
# three runs of the model, to give randomness a chance to show.

def defect():
    return "d"

def random_strategy():
    return rng.choice(["c", "d"])

p1 = defect()
p2 = random_strategy()

game_payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}

p1_payoff = game_payoffs[p1][p2]
p2_payoff = game_payoffs[p2][p1]

print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")
p1 = defect()
p2 = random_strategy()
p1_payoff = game_payoffs[p1][p2]
p2_payoff = game_payoffs[p2][p1]
print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")

p1 = defect()
p2 = random_strategy()
p1_payoff = game_payoffs[p1][p2]
p2_payoff = game_payoffs[p2][p1]
print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")

p1 = defect()
p2 = random_strategy()
p1_payoff = game_payoffs[p1][p2]
p2_payoff = game_payoffs[p2][p1]
print(f"Game is ({p1},{p2}), so payoffs are ({p1_payoff},{p2_payoff})")


# 1.4. Creating a “main” Function: It is a common programming paradigm to organize every line of
# code into well-named functions, then create a single “main” function at the end that calls all of
# the functions in order. After the previous step, we still have code that is not in a function:
# • Write a third function that takes in two positional arguments, one for each strategy function,
# and returns two tuples in the form of (score, strategy played) for each player.
# • The only code that shouldn’t be part of a function is the calling of your final function, and a
# print statement showing the results.

def defect():
    return "d"

def random_strategy():
    return str(rng.choice(["c", "d"]))

def play_game(strategy1, strategy2):
    p1 = strategy1()
    p2 = strategy2()
    p1_payoff = game_payoffs[p1][p2]
    p2_payoff = game_payoffs[p2][p1]
    return (p1_payoff, p1), (p2_payoff, p2)

result1, result2 = play_game(defect, random_strategy)
print(f"Player 1: {result1}, Player 2: {result2}")


# 1.5 Reflection: Copy and edit your code from the earlier questions for the first part of this reflection,
# then answer the second part using comments:
# • The prisoner's dilemma is only one possible game we could play using game theory. Read a
# brief summary of the "chicken" game here. Change the values in your payoff dictionary to
# match this game. Replace your strategies with two strategies you might want to play in the
# chicken game, and run them and display the results.

game_payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 0}}

def defect():
    return "d"

def always_cooperate():
    return "c"

result1, result2 = play_game(defect, always_cooperate)
print(f"Player 1: {result1}, Player 2: {result2}")

# • Game theory and simulations can be an important tool for the study of policy. The current
# US administration is kicking off an international trade war through rampant tariffs that many
# consider senseless. Regardless of your personal opinion, you have been tasked with defending
# the administration's moves! Write no more than 3 sentences describing how a version of the
# chicken game might be used to explain their international strategy.

# the US administration stragegy can be interpreted as credibly commiting to impose high tariffs. 
# By doing this, the US pressures other countries into cooperating. Like in the chicken game, 
# the player that convinces the opponent they will never back down gains the advantage. 
# In this specific case, the risk is a trade war where both sides lose, but the strategy bets that 
# opponents will yield before that happens.

# 2.1 Time Reporting
# I spent about 10 hours on DAP and this pset during this week. 
