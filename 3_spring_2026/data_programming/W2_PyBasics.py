# 2.1. Simple sum 
x =  10
y = 20
print (x + y)

# 2.2 Print the last item
my_list = [40 , 50 , 60 , 70 , 80 , 100 , 200 , 400]
my_list_len = len (my_list)
print (my_list[my_list_len - 1]) # answer : 400

# 2.3 Make the string all upper-case
my_string = " hello world "
print (my_string.upper()) # answer : " HELLO WORLD "


# 2.4 Add a value onto the end of the list
z = ["a", "b", "c"] + ["d"]  
print(z) # answer : [" a " , " b " , " c " , " d "]

# 3.1. Place your cursor at the end of the variable "text" below, then type a period. VS Code will pop
# up a list of all methods that this type of object knows (other interpreters may need you to push
# tab for this). Search through the list until you find one that seems like it will help you separate
# this single string into a list holding four strings, one for each state. Look up the documentation
# for this method if you have to!

text = " Illinois Indiana Michigan Ohio "
text.split() # <- type a period here

# 3.2. Using the same technique as above, find a string method that will tell us how many times the letter
# “A” appears in this string. Show two ways to handle the capitalization!

fruit = " Apples are Awesome ! "

fruit.count("A")
fruit.count("a")

# 3.3. Often times we create a base string with placeholders for information we want to fill in later. There
# are two common ways to do this in Python; look up “string formatting” in chapter 2 of the text
# book. Create a string named “name” equal to your name, and a string named “greeting” that
# says “Hello my name is , how are you?” and then, in a print function, replace the blank with the
# contents of the variable “name” using a string formatting method. Repeat with the second string
# formatting method.

name = "Jay"
greeting = f"Hello my name is {name}, how are you?"
print(greeting)

# 4.1. Make the entries in this list unique
schools = ["harris", "booth", "crown", "harris", "harris"]
unique_schools = set(schools)
print(unique_schools)

# 4.2. Change the “dog” entry to “cat”
animals = tuple(["bird", "horse", "dog", "fish"])
animals = list(animals)
animals[2] = "cat"
animals = tuple(animals)
print(animals)

# 4.3 Use indexing on the given dictionary to display the result of dividing the value stored in A by the
# value stored in B (expected result: 0.5).

my_dict = {"A": 10, "B": 20, "C": 0, "a": 2, "b": 4, "c": 1}
result = my_dict["A"] / my_dict["B"]
print(result)

# 5.1. Write a for-loop that iterates over both the keys and values of the given dictionary at the same
#   time (you may have to look up how to do that!), evaluates a student as passing if they scored a 60%
#   or above, then prints out a format string that says “The student < name > < passed/f ailed >
#   the class with a score of < score >.” for each student.

grades = {"Zhixin" :95 , "Sarah" :99 , "Anthony" :58 , "Haoxuan" :75 , "Angela " :88 , "Richard " :60}

for name in grades.keys():
    print(name)

for score in grades.values():
    print(score)


for name, score in grades.items():
    if score >= 60:
        result = "passed"
    else:
        result = "failed"
    print(f"The student {name} {result} the class with a score of {score}.")

# 5.2. Building on the previous question, write a for-loop that does the same thing, but only for scores in
#   the course named “programming”. If the student did not take that course, instead print something
#   stating that. Hint: Nested containers and loops like this can be challenging! Remember to write
#   and test small pieces at a time, and use intermediate output.

course = "programming"
grades = {"Zhixin": {"programming": 95, "basket weaving": 100}, 
          "Sarah": {"cooking": 100, "programming": 99}, 
          "Anthony": {"math": 58, "econ": 77}, 
          "Haoxuan": {"programming": 75, "econ": 100}, 
          "Angela ": {"programming": 88, "programming 2": 91}, 
          "Richard ": {"gardening": 60, "studying": 59}}

for name in grades.keys():
    print(name)

for course in grades.values():
    print(course)


for name, score in grades.items():
    if course in score:
        if score[course] >= 60:
            result = "passed"
        else:
            result = "failed"
        print(f"The student {name} {result} the class with a score of {score[course]}.")
    else:
        print(f"The student {name} did not take the {course} course.")

