# 3.1.

import numpy as np
rng = np.random.default_rng(seed=50)

def cooperate():
    return "c"

def random_strategy():
    return rng.choice(["c", "d"])

def play_game(play1, play2):
    payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}
    p1_payoff = payoffs[play1][play2]
    p2_payoff = payoffs[play2][play1]
    return p1_payoff, p2_payoff

def game(strategy1, strategy2, n_rounds=50):
    total1 = 0
    total2 = 0
    for i in range(n_rounds):
        play1 = strategy1()
        play2 = strategy2()
        p1_payoff, p2_payoff = play_game(play1, play2)
        total1 += p1_payoff
        total2 += p2_payoff
    print(f"Player 1 total: {total1}, Player 2 total: {total2}")

game(cooperate, random_strategy)

# the random strategy wins because, on average, half the time it 
# defects against an always cooperate strategy, earning 5 points 
# while always-cooperate only gets 1.

# 3.2. 

class AlwaysCooperate:
    def __init__(self):
        self.name = "Always Cooperate"

    def strategy(self):
        return "c"

class RandomStrategy:
    def __init__(self):
        self.name = "Random Strategy"

    def strategy(self):
        return rng.choice(["c", "d"])

def play_round(play1, play2):
    payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}
    p1_payoff = payoffs[play1][play2]
    p2_payoff = payoffs[play2][play1]
    return p1_payoff, p2_payoff

def game(player1, player2, n_rounds=50):
    total1 = 0
    total2 = 0
    for i in range(n_rounds):
        play1 = player1.strategy()
        play2 = player2.strategy()
        p1_payoff, p2_payoff = play_round(play1, play2)
        total1 += p1_payoff
        total2 += p2_payoff
    print(f"{player1.name} had a total of {total1}, while {player2.name} had {total2}")


game(AlwaysCooperate(), RandomStrategy())

# 3.3.

class AlwaysCooperate:
    def __init__(self):
        self.name = "Always Cooperate"
        self.opponent_last_play = None

    def strategy(self):
        return "c"

    def opponent_played(self, play):
        self.opponent_last_play = play

class RandomStrategy:
    def __init__(self):
        self.name = "Random Strategy"
        self.opponent_last_play = None

    def strategy(self):
        return rng.choice(["c", "d"])

    def opponent_played(self, play):
        self.opponent_last_play = play

class TitForTat:
    def __init__(self):
        self.name = "TitForTat"
        self.opponent_last_play = None

    def strategy(self):
        if self.opponent_last_play is None:
            return "c"
        else:
            return self.opponent_last_play

    def opponent_played(self, play):
        self.opponent_last_play = play

def play_round(play1, play2):
    payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}
    p1_payoff = payoffs[play1][play2]
    p2_payoff = payoffs[play2][play1]
    return p1_payoff, p2_payoff

def game(player1, player2, n_rounds=50):
    total1 = 0
    total2 = 0
    for i in range(n_rounds):
        play1 = player1.strategy()
        play2 = player2.strategy()
        p1_payoff, p2_payoff = play_round(play1, play2)
        total1 += p1_payoff
        total2 += p2_payoff
        player1.opponent_played(play2)
        player2.opponent_played(play1)
    print(f"{player1.name} had a total of {total1}, while {player2.name} had {total2}")

game(TitForTat(), AlwaysCooperate())
game(TitForTat(), RandomStrategy())
game(TitForTat(), TitForTat())

# 3.4.

class Player:
    def __init__(self, name):
        self.name = name
        self.opponent_last_play = None

    def opponent_played(self, play):
        self.opponent_last_play = play

class AlwaysCooperate(Player):
    def __init__(self):
        super().__init__("Always Cooperate")

    def strategy(self):
        return "c"

class RandomStrategy(Player):
    def __init__(self):
        super().__init__("Random Strategy")

    def strategy(self):
        return rng.choice(["c", "d"])

class TitForTat(Player):
    def __init__(self):
        super().__init__("TitForTat")

    def strategy(self):
        if self.opponent_last_play is None:
            return "c"
        else:
            return self.opponent_last_play

def play_round(play1, play2):
    payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}
    p1_payoff = payoffs[play1][play2]
    p2_payoff = payoffs[play2][play1]
    return p1_payoff, p2_payoff

def game(player1, player2, n_rounds=50):
    total1 = 0
    total2 = 0
    for i in range(n_rounds):
        play1 = player1.strategy()
        play2 = player2.strategy()
        p1_payoff, p2_payoff = play_round(play1, play2)
        total1 += p1_payoff
        total2 += p2_payoff
        player1.opponent_played(play2)
        player2.opponent_played(play1)
    print(f"{player1.name} had a total of {total1}, while {player2.name} had {total2}")

game(TitForTat(), AlwaysCooperate())
game(TitForTat(), RandomStrategy())
game(TitForTat(), TitForTat())


# 3.5. 

class Player:
    def __init__(self, name):
        self.name = name
        self.opponent_last_play = None

    def opponent_played(self, play):
        self.opponent_last_play = play

class AlwaysCooperate(Player):
    def __init__(self):
        super().__init__("Always Cooperate")

    def strategy(self):
        return "c"

class RandomStrategy(Player):
    def __init__(self):
        super().__init__("Random Strategy")

    def strategy(self):
        return rng.choice(["c", "d"])

class TitForTat(Player):
    def __init__(self):
        super().__init__("TitForTat")

    def strategy(self):
        if self.opponent_last_play is None:
            return "c"
        else:
            return self.opponent_last_play

def play_round(play1, play2):
    payoffs = {"c": {"c": 3, "d": 1}, "d": {"c": 5, "d": 1}}
    p1_payoff = payoffs[play1][play2]
    p2_payoff = payoffs[play2][play1]
    return p1_payoff, p2_payoff

def game(player1, player2, n_rounds=50):
    total1 = 0
    total2 = 0
    results = "round,player,choice,payoff\n"
    for i in range(n_rounds):
        play1 = player1.strategy()
        play2 = player2.strategy()
        p1_payoff, p2_payoff = play_round(play1, play2)
        total1 += p1_payoff
        total2 += p2_payoff
        results += f"{i},{player1.name},{play1},{p1_payoff}\n"
        results += f"{i},{player2.name},{play2},{p2_payoff}\n"
        player1.opponent_played(play2)
        player2.opponent_played(play1)
    print(f"{player1.name} had a total of {total1}, while {player2.name} had {total2}")
    with open("results.csv", "w") as f:
        f.write(results)

game(TitForTat(), RandomStrategy())


# 4.1.
# this week I spent approximately 20 hours on DAP.