
{quiz, id: quiz_name_here, attempts: 10}

## Template quiz

Put any other instructions your quiz takers need to know here like, Choose the best answer.

{choose-answers: 4}
? First question to ask goes here. (Note- you need a question mark at end like this. Just one is required if using a question mark in your question field)?

C) One correct answer here marked with a "C"
C) A second correct answer here
m) Mandatory incorrect answers have an "m"
m) A second mandatory incorrect answer
o) An optional incorrect answer here marked with an "o"
o) A second optional incorrect answer here

? Example without choose answers

a) A first wrong answer
B) The correct answer which is capitalized
c) Some wrong answer
d) Some other wrong answer

{choose-answers: 4}
? Question example with just a question mark?

C) One correct answer here marked with a "C"
C) A second correct answer here
m) Mandatory incorrect answers have an "m"
m) A second mandatory incorrect answer
o) An optional incorrect answer here marked with an "o"
o) A second optional incorrect answer here

{choose-answers: 4}
? A more complicated example. Note the question mark at the end of the options. Which of the following are correct...
1. An option described
2. A second option
3. A third option
4. A fourth option
5. A fifth option ?

C) All of the examples listed except 5
C) All of the listed examples except 5
m) 1, 3, and 5
m) 1, 2, and 3
o) All of the examples except 1 and 5
o) All of the examples listed

? Here's an example of a text question. And we don't care what the answer is with the regex we've used below. 

! /(.*\?)/i

{/quiz}
