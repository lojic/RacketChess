# RacketChess
This is currently a work in progress, but it plays fairly strong chess already - it can consistently beat human players above 1,900 in 15+10 or 10+5 games if I manage the time well manually :)

To play, `cd` to the `RacketChess/src` directory and run `rlwrap racket chess.rkt`, then enter a `w` or a `b` to indicate whether the computer should play white or black. Moves are entered in pgn fashion, e.g. e5, Qd4, Rae1, O-O, O-O-O, etc. After the move, you can optionally add a space and a number of minutes to think. That think time will "stick" until changed. Look in `chess.rkt` for the initial think time.

The engine currently has:

* alpha beta pruning
* quiescence search
* iterative deepening
* piece square table
* simple MVV-LVA move ordering
* transposition table

There are a few more features I'd like to add, then I'll refactor the code to (hopefully) be a great example of both Racket code and chess code. It's currently quite ugly as I was in the mode of "get it to work", and I've yet to find any good examples of beautiful chess code - most of what I've seen has tons of global variables, huge function sizes, etc., which make it very difficult to follow.