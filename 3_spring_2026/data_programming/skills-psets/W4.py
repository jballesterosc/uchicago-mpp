import numpy as np

# 4.2 
class Player():
    def __init__(self, name):
        self.name = name
        self.wins = 0
        self.choices = ["rock", "paper", "scissors"]

    def ready_set_go(self):
        return np.random.choice(self.choices)

p1 = Player("Jay")
print(p1.name)
print(p1.wins)
print(p1.ready_set_go())
print(p1.ready_set_go())

# 4.3 
class Player():
    def __init__(self, name, weights=None):
        self.name = name
        self.wins = 0
        self.choices = ["rock", "paper", "scissors"]
        self.weights = weights

    def ready_set_go(self):
        return np.random.choice(self.choices, p=self.weights)

p1 = Player("Random")
print(p1.ready_set_go())

p2 = Player("Always Scissors", weights=[0, 0, 1])
print(p2.ready_set_go())
print(p2.ready_set_go())
print(p2.ready_set_go())

# 4.4 
class Game():
    def __init__(self):
        self.p1 = Player("Jay")
        self.p2 = Player("Alejandro")

    def find_winner(self, choice1, choice2):
        rps_beats = {"rock": "scissors", "paper": "rock", "scissors": "paper"}
        if choice1 == choice2:
            return 0
        elif rps_beats[choice1] == choice2:
            return 1
        else:
            return 2

    def play_once(self):
        choice1 = self.p1.ready_set_go()
        choice2 = self.p2.ready_set_go()
        print(f"{self.p1.name} plays {choice1}, {self.p2.name} plays {choice2}")
        winner = self.find_winner(choice1, choice2)
        if winner == 0:
            print("tie!")
        elif winner == 1:
            self.p1.wins += 1
            print(f"{self.p1.name} wins. Total: {self.p1.wins}")
        else:
            self.p2.wins += 1
            print(f"{self.p2.name} wins. Total: {self.p2.wins}")

# 4.5 
game = Game()
game.play_once()
game.play_once()
game.play_once()

# 5.1 
class PlayerBomb(Player):
    def __init__(self, name):
        super().__init__(name)
        self.choices.append("bomb")

class Game():
    def __init__(self):
        self.p1 = Player("Jay")
        self.p2 = PlayerBomb("Diego")

    def find_winner(self, choice1, choice2):
        rps_beats = {"rock": "scissors", "paper": "rock", "scissors": "paper"}
        if choice1 == choice2:
            return 0
        elif choice1 == "bomb":
            return 1
        elif choice2 == "bomb":
            return 2
        elif rps_beats[choice1] == choice2:
            return 1
        else:
            return 2

    def play_once(self):
        choice1 = self.p1.ready_set_go()
        choice2 = self.p2.ready_set_go()
        print(f"{self.p1.name} plays {choice1}, {self.p2.name} plays {choice2}")
        winner = self.find_winner(choice1, choice2)
        if winner == 0:
            print("It's a tie!")
        elif winner == 1:
            self.p1.wins += 1
            print(f"{self.p1.name} wins. Total: {self.p1.wins}")
        else:
            self.p2.wins += 1
            print(f"{self.p2.name} wins. Total: {self.p2.wins}")

game = Game()
game.play_once()
game.play_once()
game.play_once()
game.play_once()
game.play_once()

# 5.2.
class Game():
    def __init__(self, n=20):
        self.p1 = Player("Jay")
        self.p2 = PlayerBomb("Diego")
        self.n = n

    def find_winner(self, choice1, choice2):
        rps_beats = {"rock": "scissors", "paper": "rock", "scissors": "paper"}
        if choice1 == choice2:
            return 0
        elif choice1 == "bomb":
            return 1
        elif choice2 == "bomb":
            return 2
        elif rps_beats[choice1] == choice2:
            return 1
        else:
            return 2

    def play_once(self):
        choice1 = self.p1.ready_set_go()
        choice2 = self.p2.ready_set_go()
        print(f"{self.p1.name} plays {choice1}, {self.p2.name} plays {choice2}")
        winner = self.find_winner(choice1, choice2)
        if winner == 0:
            print("Tie")
        elif winner == 1:
            self.p1.wins += 1
            print(f"{self.p1.name} wins. Total: {self.p1.wins}")
        else:
            self.p2.wins += 1
            print(f"{self.p2.name} wins. Total: {self.p2.wins}")

    def play_n(self):
        for _ in range(self.n):
            self.play_once()

game = Game(n=5)
game.play_n()

