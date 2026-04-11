# 1.1 When doing skills psets, I am allowed to look at my classmate’s code.
# Answer: True! It's possible. 
# 1.2 When doing applied psets, I am allowed to show others my code.
# Answer: False. Is not allowed.
# 1.3 The due date for all skills psets is Friday before midnight on Gradescope.
# Answer: True. 
# 1.4 The due date for all applied psets is Sunday before midnight on Gradescope.
# Answer: True.
# 1.5 If I need to turn in my applied pset late, I should email the professor.
# Answer: False

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
z = ["a", "b", "c"]
z.append("d")
print(z)

# 3.1. Place your cursor at the end of the variable "text" below, then type a period. VS Code will pop
# up a list of all methods that this type of object knows (other interpreters may need you to push
# tab for this). Search through the list until you find one that seems like it will help you separate
# this single string into a list holding four strings, one for each state. Look up the documentation
# for this method if you have to!

text = " Illinois Indiana Michigan Ohio "
text.split()

# 3.2. Using the same technique as above, find a string method that will tell us how many times the letter
# “A” appears in this string. Show two ways to handle the capitalization!

fruit = " Apples are Awesome ! "
fruit.lower().count("a")

# 3.3. Often times we create a base string with placeholders for information we want to fill in later. There
# are two common ways to do this in Python; look up “string formatting” in chapter 2 of the text
# book. Create a string named “name” equal to your name, and a string named “greeting” that
# says “Hello my name is , how are you?” and then, in a print function, replace the blank with the
# contents of the variable “name” using a string formatting method. Repeat with the second string
# formatting method.

name = "Jay"
greeting = f"Hello my name is {name}, how are you?"
print(greeting)

greeting = "Hello my name is {}, how are you?".format(name)
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

# 5.3 Take the following list of numbers and write a for-loop with an if-elif-else block that classifies each
# value as "small" (less than 10), "medium" (between 10 and 20), or "large" (greater than 20). Print
# out a string with each number and its classification.

numbers = [8, 10, 11, 34, 0, -2, 15]

for number in numbers:
    if number < 10:
        size = "small"
    elif number >= 10 and number <= 20:
        size = "medium"
    else:
        size = "large"
    print(f"The number {number} is {size}.")


# 5.4 Write a nested for-loop to iterate through each element in the matrix and print only the even
# numbers. Use an if statement to check if a number is even before printing it.

matrix = [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]]

for row in matrix:
    for number in row:
        if number % 2 == 0:
            print(number)


# 5.5 Write code that takes a list of numbers as input and uses a for-loop to find the sum of all positive
# numbers and the sum of all negative numbers in the list. Use if statements to distinguish between
# positive and negative numbers. Provide an example input list and the result.

numbers = [10, -5, 3, -8, 7, -2, 15]
positive_sum = 0
negative_sum = 0

for number in numbers:
    if number > 0:
        positive_sum = positive_sum + number
    elif number < 0:
        negative_sum = negative_sum + number

print(f"Sum of positive numbers: {positive_sum}")
print(f"Sum of negative numbers: {negative_sum}")


# 5.6 Write a for-loop to convert each temperature to Fahrenheit and then put the results in the
# appropriate list. 

celsius = [36.5, 37, 37.5, 38, 38.5, 39, 39.5]
farenheit = []

for temp in celsius:
    farenheit.append(temp * 9/5 + 32)

print(farenheit)


# 5.7 Write a for-loop that iterates over the values 0-99 using the range function, then square each value.
# The for-loop should exit when the squared value is greater than 40.

for number in range(100):
    squared = number ** 2
    if squared > 40:
        break
    print(squared)


# 6.1 Take the nested loop you wrote in question 5.4 and redo it here using a single list comprehension.
# Instead of printing the result, store it in a flat list, then print that results list.

matrix = [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]]

even_numbers = [number for row in matrix for number in row if number % 2 == 0]
print(even_numbers)


# 6.2 One issue that is commonly encountered when merging data from different sources is different
# conventions for column names. Use list comprehensions to standardize them.

columns_df1 = ["state", "year", "gdp", "unemp_rate", "pop"]
columns_df2 = ["State", "Year", "Industrial Production", "Business Formation"]
columns_df3 = ["STATE", "YEAR", "ConsumerPriceIndex"]

columns_df2 = [col.lower().replace(" ", "_") for col in columns_df2]
columns_df3 = [col.lower().replace("consumerpriceindex", "consumer_price_index") for col in columns_df3]

print(columns_df1)
print(columns_df2)
print(columns_df3)


# 6.3 Look up how to use the built-in zip function, then use zip with a single dictionary comprehension
# so that the cities are the keys and the universities are the values.

cities = ["Chicago", "Ann Arbor", "Los Angeles", "Washington, DC", "St. Louis"]
universities = ["University of Chicago", "University of Michigan", "UCLA", "Georgetown", "Washington University"]

city_university = {city: university for city, university in zip(cities, universities)}
print(city_university)


# 6.4 Using a dictionary comprehension, drop all of the candy that has the None object as the value.

my_dict = {"chocolate": 8, "gummy worms": None, "snickers": 1, "cinnamon candy": 5,
           "lemon candy": None, "nerds": 9, "mints": 3, "candy cane": 4,
           "cookies": 6, "licorice": None}

my_dict = {candy: count for candy, count in my_dict.items() if count is not None}
print(my_dict)


# 7.1 Use ChatGPT to help you build a dictionary where the keys are the entries in cities_left,
# and the values are the entry they should match with in cities_right. Any unmatched keys should
# have a value of None.

# I asked to ChatGPT to: compare both lists and match cities that refer to the same place. 
# Create a python a dictionary mapping each variable in cities_left to its best match in cities_right. 
# If no match, then "None".

cities_left = ["Chicago, IL", "Toronto, Canada", "Bogota, Columbia", "Beijing, China",
               "Abuja, Nigeria", "Cape Town, South Africa", "Mumbai, India", "Seoul, South Korea",
               "Los Angeles, California, USA", "Mexico City, Mexico", "Tokyo, Japan",
               "London, England", "Berlin, Germany", "Riyadh, Saudi Arabia",
               "Yamoussoukro, Cote d'Ivoire", "Paris, France"]

cities_right = ["Tokyo (Honshu) Japan", "Los Angeles, CA (USA)", "Toronto, Ontario, CA",
                "Capital city Berlin, Germany", "Abuja Nigeria", "Seoul (South Korea)",
                "Mumbai (Bombay) India", "Beijing (Peking), China", "Riyadh, Riyadh Province, Saudi Arabia",
                "Chicago, IL, USA", "Mexico City, MX", "Cape Town, SA",
                "Yamoussoukro, Cote dIvoire", "Bogota, CO", "London, United Kingdom"]

city_mapping = {
    "Chicago, IL": "Chicago, IL, USA",
    "Toronto, Canada": "Toronto, Ontario, CA",
    "Bogota, Columbia": "Bogota, CO",
    "Beijing, China": "Beijing (Peking), China",
    "Abuja, Nigeria": "Abuja Nigeria",
    "Cape Town, South Africa": "Cape Town, SA",
    "Mumbai, India": "Mumbai (Bombay) India",
    "Seoul, South Korea": "Seoul (South Korea)",
    "Los Angeles, California, USA": "Los Angeles, CA (USA)",
    "Mexico City, Mexico": "Mexico City, MX",
    "Tokyo, Japan": "Tokyo (Honshu) Japan",
    "London, England": "London, United Kingdom",
    "Berlin, Germany": "Capital city Berlin, Germany",
    "Riyadh, Saudi Arabia": "Riyadh, Riyadh Province, Saudi Arabia",
    "Yamoussoukro, Cote d'Ivoire": "Yamoussoukro, Cote dIvoire",
    "Paris, France": "None"
}

print(city_mapping)