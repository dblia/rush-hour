rush-hour
=========

This is a Rush Hour game solver written in Erlang.

For more information about the Rush Hour game, visit:

  http://en.wikipedia.org/wiki/Rush_Hour_%28board_game%29

How to run
==========

1.  To start Erlang/OTP on a Unix system you execute in a Unix shell:

$> erl

2. Compile the program:

1> c(rush_hour).

3. Run a testcase:

2> rush_hour:solve(6,6,{2,0},[{0,{2,3},{2,4}},{1,{0,0},{2,0}},{2,{4,0},{4,1}},{3,{5,1},{5,3}},{4,{1,2},{3,2}},{5,{0,4},{0,5}},{6,{1,5},{3,5}},{7,{4,5},{5,5}}]).

where you provide the board dimensions, the target exit hole, and the 
positions of the cars. Note that the red car must always be the zero
indexed one.
