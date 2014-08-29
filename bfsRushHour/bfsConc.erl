-module(bfsConc).

-export([solve/4]).
-export([master/7, bfs_workers/6]). % for spawning

-define(OPTIONS, [named_table, public, {read_concurrency,true}, {write_concurrency, true}]).

%% Function for input transformation depending on the way i make my checks.
%% (eg. i want to have: {Car, {1, 2}, {2, 2}} and not {Car, {2, 2}, {1, 2}} )
initBoard([]) -> [];
initBoard([{Car, {SX, SY}, {FX, SY}}|XS]) when SX > FX -> 
  [{Car, {FX, SY}, {SX, SY}} | initBoard(XS)];
initBoard([{Car, {SX, SY}, {SX, FY}}|XS]) when SY > FY -> 
  [{Car, {SX, FY}, {SX, SY}} | initBoard(XS)];
initBoard([N|XS]) -> [N | initBoard(XS)].

% Check if the red car, (zero indexed), reached the exit.
wins([], _Goal) -> false;
wins([{0, {SX, SY}, {FX, FY}}|_], {GoalX, GoalY})
when (GoalX == SX orelse GoalX == FX) andalso (GoalY == SY orelse GoalY == FY) -> true;
wins([_|Rest], Goal) -> wins(Rest, Goal).

% Check if the car is blocked by another one or not.
canMove([], _Target) -> false;
canMove([{_Car, {SX, SY}, {FX, FY}}|_], {TarX, TarY}) 
when TarX >= SX andalso TarY >= SY andalso TarX =< FX andalso TarY =< FY -> true;
canMove([_|Rest], Target) -> canMove(Rest, Target).

% Replace the new car move in the state given.
replaceCar([], _Car, Acc) -> Acc;
replaceCar([{C, _, _}|Rest], Car={C, _, _}, Acc) -> 
  lists:reverse(Acc, [Car|Rest]);
replaceCar([C|Rest], Car, Acc) ->
  replaceCar(Rest, Car, [C|Acc]).

% Check if the move given has already been marked.
insertNew(State, Acc) ->
  case ets:insert_new(marked, {State, null}) of
    true -> [State | Acc];
    false -> Acc
  end.

% Every one of the four functions below return a list of the possible
% movements, in every direction, for every car of the given state.
moveLeft({_C, {0, Y}, {_FX, Y}}, _State, Acc) -> Acc;
moveLeft({C, {SX, Y}, {FX, Y}}, State, Acc) ->
  case canMove(State, {SX-1, Y}) of
    true -> Acc;
    false -> 
      NewMove = {C, {SX-1, Y}, {FX-1, Y}},
      moveLeft(NewMove, State, [NewMove|Acc])
  end;
moveLeft(_Car, _State, Acc) -> Acc.

moveUp({_C, {X, 0}, {X, _FY}}, _State, Acc) -> Acc;
moveUp({C, {X, SY}, {X, FY}}, State, Acc) ->
  case canMove(State, {X, SY-1}) of
    true -> Acc;
    false -> 
      NewMove = {C, {X, SY-1}, {X, FY-1}},
      moveUp(NewMove, State, [NewMove|Acc])
  end;
moveUp(_Car, _State, Acc) -> Acc.

moveRight({_C, {_SX, Y}, {BoundX, Y}}, _State, BoundX, Acc) -> Acc;
moveRight({C, {SX, Y}, {FX, Y}}, State, BoundX, Acc) ->
  case canMove(State, {FX+1, Y}) of
    true -> Acc;
    false -> 
      NewMove = {C, {SX+1, Y}, {FX+1, Y}},
      moveRight(NewMove, State, BoundX, [NewMove|Acc])
  end;
moveRight(_Car, _State, _BoundX, Acc) -> Acc.

moveDown({_C, {X, _SY}, {X, BoundY}}, _State, BoundY, Acc) -> Acc;
moveDown({C, {X, SY}, {X, FY}}, State, BoundY, Acc) ->
  case canMove(State, {X, FY+1}) of
    true -> Acc;
    false -> 
      NewMove = {C, {X, SY+1}, {X, FY+1}},
      moveDown(NewMove, State, BoundY, [NewMove|Acc])
  end;
moveDown(_Car, _State, _BoundY, Acc) -> Acc.

% Returns a list of all the possible states derived from the given one.
neighbors(State, BoundX, BoundY) ->
  neighbors(State, State, BoundX, BoundY, []).

neighbors([], _State, _BoundX, _BoundY, Acc) -> Acc;
neighbors([C|Rest], State, BoundX, BoundY, Acc) ->
  MoveHoriz = moveLeft(C, State, []) ++ moveRight(C, State, BoundX, []),
  MoveVertical = moveUp(C, State, []) ++ moveDown(C, State, BoundY, []),
  MovesAll = [replaceCar(State, Car, []) || Car <- (MoveHoriz ++ MoveVertical)],
  Pred = fun(Move, Board) -> insertNew(Move, Board) end,
  NewMoves = lists:foldl(Pred, Acc, MovesAll),
  neighbors(Rest, State, BoundX, BoundY, NewMoves).

% Function that coordinates the processes
master(SolverPid, 0, [], _Goal, _BoundX, _BoundY, _Step) -> 
  SolverPid ! {answer, -1}; % there is no solution
master(SolverPid, 0, States, {GoalX, GoalY}, BoundX, BoundY, Step) ->
  Slaves = [spawn(?MODULE, bfs_workers, [self(), Board, {GoalX, GoalY}, BoundX, BoundY, Step]) 
    || Board <- States], % spawn every board in the queue
  master(SolverPid, length(Slaves), [], {GoalX, GoalY}, BoundX, BoundY, Step+1);
master(SolverPid, Workers, OldStates, {GoalX, GoalY}, BoundX, BoundY, Step) ->
  receive
    {winner, Ans} -> % answer found
      SolverPid ! {answer, Ans}; 
    {no_winner, NewStates} -> % current thread found all it's neighbors
      master(SolverPid, Workers-1, NewStates ++ OldStates, {GoalX, GoalY}, BoundX, BoundY, Step)
  end.

% BFS tasks
bfs_workers(ParentPid, State, {GoalX, GoalY}, BoundX, BoundY, Step) ->
  case wins(State, {GoalX, GoalY}) of
    true ->  % notify parent of winning state
      ParentPid ! {winner, Step};
    false -> % compute new unexplored neighbour states
      Neighbors = neighbors(State, BoundX, BoundY),
      ParentPid ! {no_winner, Neighbors}
  end.

%% Main Function
%% try-catch used for multiple calls in ets without problems
solve(BoundX, BoundY, {GoalX, GoalY}, StartState) ->
  try ets:delete(marked) of
    _ -> ets:new(marked, ?OPTIONS)
  catch
    error:_Error -> % badarg exception
      ets:new(marked, ?OPTIONS)
  end,
  S=initBoard(lists:usort(StartState)),
  ets:insert_new(marked, {S, null}),
  spawn(?MODULE, master, [self(), 0, [S], {GoalX,GoalY}, BoundX-1, BoundY-1, 0]),
  receive
    {answer, Ans} -> 
      Ans
  end.
