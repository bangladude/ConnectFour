ConnectFour
===========
by Saajid Moyen

This is the game ConnectFour written in Haskell. It is played from the terminal by typing commands. Currently the working version is in ConnectFour.hs and is a two player game. MinimaxCF.hs contains an in-progress version of ConnectFour with an AI that selects moves using the "negamax" version of the minimax algorithm. 



INSTRUCTIONS:

To play the game, type "runhaskell ConnectFour.hs" or load the file into the Haskell interpreter and type "main"

The game will prompt each user for his or her move. Valid inputs are integers between 1 and 7 inclusive, which correspond to the number of the column from left to right.

To play again, use the same command as before to run the file again.



THE PROCESS:

I first had to figure out a way to represent the board. I started with lists, then tried arrays, and finally settled on maps. We hadn't used maps in class, so this was something new for me. Also, I had to figure out a way to write an instance of Show for the Board data type that I wrote, which I haven't had to do since we usually can just write deriving Show. Writing the rest of the game was just a matter of pulling out the rows, columns, and diagonals and writing functions to check for winners or a full board. To play the game, I wrote a few functions that call each other after each turn to allow the game to go on until it is done (either someone won or the game board is full). This was a new control structure than the ones we had used in the IO segment of the class. I also had to figure out how to read user input as well as how to handle bad input without crashing the program. 

I also have a nearly-functional AI written so that users can play against a computer. I was trying to implement the negamax version of the minimax algorithm to use for the computer's moves. To do that, I had to generate all the possible boards that could result and then evaluate them using some heuristic. The evaluation of boards gives very large scores to four-in-a-row and, at this point, can also count the number of four-space-segments with three pieces of the same type and no other pieces, since these are potential winning segments. There is currently a bug in which the computer does not act to prevent the player from winning when there are three pieces in a row, despite the evaluation that gives winning boards a very high value. I also included, in comments, another version of minimax that I was writing. 