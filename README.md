# RacketChess
This is currently a work in progress, but it plays fairly strong chess already - it can consistently beat human players above 1,900 in 15+10 or 10+5 games if I manage the time well manually :)

To play, `cd` to the `RacketChess/src` directory and run `rlwrap racket chess.rkt`, then enter a `w` or a `b` to indicate whether the computer should play white or black. Moves are entered in pgn fashion, e.g. e5, Qd4, Rae1, O-O, O-O-O, etc. After the move, you can optionally add a space and a number of minutes to think. That think time will "stick" until changed. Look in `chess.rkt` for the initial think time.

The engine currently does alpha beta pruning, simple MVV-LVA move ordering, and it uses a transposition table. After I add a few more features, I'll refactor the code to (hopefully) be a great example of both Racket code and chess code.