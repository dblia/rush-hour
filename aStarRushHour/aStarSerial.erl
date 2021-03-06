-module(aStarSerial).
-export([solve/4]).
-define(OPTIONS, [named_table]).

% Functions that finds the heuristic value depending on the car's position. 
heurHorizRight(State=[Car|_]) -> heurHorizRight(State, Car, 0).

heurHorizRight([], _, Acc) -> Acc;
heurHorizRight([{_, {SX, SY}, {SX, FY}}|XS],  G={0, {_, GoalSY}, {GoalFX, _}},  Acc)  
when (SX > GoalFX andalso SY=<GoalSY andalso FY>=GoalSY) -> 
  heurHorizRight(XS,  G,  Acc+1);
heurHorizRight([_|XS],  G,  Acc) ->
  heurHorizRight(XS,  G,  Acc).

heurHorizLeft(State=[Car|_]) -> heurHorizLeft(State, Car, 0).

heurHorizLeft([], _, Acc) -> Acc;
heurHorizLeft([{_, {SX, SY}, {SX, FY}}|XS],  G={0, {GoalSX, GoalSY}, {_, _}},  Acc)  
when (SX < GoalSX andalso SY=<GoalSY andalso FY>=GoalSY) -> 
  heurHorizLeft(XS,  G,  Acc+1);
heurHorizLeft([_|XS],  G,  Acc) ->
  heurHorizLeft(XS,  G,  Acc).

heurVerticalUp(State=[Car|_]) -> heurVerticalUp(State, Car, 0).

heurVerticalUp([], _, Acc) -> Acc;
heurVerticalUp([{_, {SX, SY}, {FX, SY}}|XS],  G={0, {GoalSX, GoalSY}, {_, _}},  Acc)  
when (SY < GoalSY andalso SX=<GoalSX andalso FX>=GoalSX) -> 
  heurVerticalUp(XS,  G,  Acc+1);
heurVerticalUp([_|XS],  G,  Acc) ->
  heurVerticalUp(XS,  G,  Acc).

heurVerticalDown(State=[Car|_]) -> heurVerticalDown(State, Car, 0).

heurVerticalDown([], _, Acc) -> Acc;
heurVerticalDown([{_, {SX, SY}, {FX, SY}}|XS],  G={0, {GoalSX, _}, {_, GoalFY}},  Acc)  
when (SY > GoalFY andalso SX=<GoalSX andalso FX>=GoalSX) -> 
  heurVerticalDown(XS,  G,  Acc+1);
heurVerticalDown([_|XS],  G,  Acc) ->
  heurVerticalDown(XS,  G,  Acc).

% Function that finds the appropriate heuristic function depending on the car's 
% and EXIT's position. 
heuristic(State=[C|_], {GoalX,GoalY}, BoundX, BoundY) ->
  case C of
    {0, {_, SY}, {_, SY}} -> % Horizonal position
      case {GoalX,GoalY} of
        {BoundX,SY} -> heurHorizRight(State);
        {0,SY} -> heurHorizLeft(State);
        _ -> exit("Wrong EXIT position.")
      end;
    {0, {SX, _}, {SX, _}} -> % Vertical position
      case {GoalX,GoalY} of
        {BoundY,SX} -> heurVerticalDown(State);
        {SX,0} -> heurVerticalUp(State);
        _ -> exit("Wrong EXIT position")
      end;
    _ -> 
      exit("The Red Car (zero indexed) haven't found") 
  end.

% Function for input transformation depending on the way i make my checks.
% (eg. i want to have: {Car, {1, 2}, {2, 2}} and not {Car, {2, 2}, {1, 2}} )
initBoard([]) -> [];
initBoard([{Car, {SX, SY}, {FX, SY}}|XS]) when SX > FX -> 
  [{Car, {FX, SY}, {SX, SY}} | initBoard(XS)];
initBoard([{Car, {SX, SY}, {SX, FY}}|XS]) when SY > FY -> 
  [{Car, {SX, FY}, {SX, SY}} | initBoard(XS)];
initBoard([N|XS]) -> [N | initBoard(XS)].

% Checks if the car is blocked by another one or not.
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

% Checks if the move given has already been marked.
insertNew(State, Acc) ->
  case ets:insert_new(closedSet, {State, none}) of
    true -> [State | Acc];
    false -> Acc
  end.

% Every one of the four functions below return a list of the possible
% movements, in every directions, for every car of the given state.
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

% Returns the dictionaries and the openset with the updated values.
update(_, [], _, _, _,_) -> ok;
update(Y, [X|XS], {GoalX, GoalY}, BoundX, BoundY, Tentative_g) ->
  case ets:lookup(dict, X) of 
    [] -> % the state isn't in the openset
      H = heuristic(X, {GoalX, GoalY}, BoundX, BoundY),
      ets:insert_new(dict, {X, H, Tentative_g}),
      ets:insert_new(openSet, {{Tentative_g+H, X}, null}),
      update(Y, XS, {GoalX, GoalY}, BoundX, BoundY, Tentative_g);
    [{_, H, G}] -> % the state is already in the openset
      case G =< Tentative_g of
        true -> % the current move is worst than the previous one (in the openset)
          update(Y, XS, {GoalX,GoalY}, BoundX, BoundY,Tentative_g);
        false -> % we found a better solution so we update it's values
          ets:insert(dict, {X, H, Tentative_g}),
          ets:insert_new(openSet, {{Tentative_g+H, X}, null}),
          update(Y, XS, {GoalX, GoalY}, BoundX, BoundY, Tentative_g)
      end
  end.

% Main Algorithm
aStar(StartState, {GoalX,GoalY}, BoundX, BoundY) -> 
  HStart = heuristic(StartState, {GoalX, GoalY}, BoundX, BoundY), 
  ets:insert_new(openSet, {{1+HStart, StartState}, null}),
  ets:insert_new(dict, {StartState, HStart, 1}),
  aStarStep(0, {GoalX,GoalY}, BoundX,  BoundY).

aStarStep(Goal, {GoalX,GoalY},  BoundX,  BoundY) ->
  case ets:first(openSet) of
    {Value, State} ->
      ets:delete_object(openSet, {{Value, State},null}),
      [{_, H, G}] = ets:lookup(dict, State),
      case H of 
        Goal -> G;
        _ -> 
          ets:insert_new(closedSet, {State, null}),
          Neighbors = neighbors(State, BoundX, BoundY),  % list of States (list of lists)
          update(State, Neighbors, {GoalX,GoalY}, BoundX, BoundY, G+1), 
          aStarStep(Goal, {GoalX,GoalY}, BoundX,  BoundY)
      end;
    _ -> -1
  end.

% Main Function
solve(BoundX, BoundY, {GoalX, GoalY}, State) ->
  ets:new(dict, ?OPTIONS),
  ets:new(closedSet, ?OPTIONS), 
  ets:new(openSet, [ordered_set | ?OPTIONS]),
  S = lists:usort(initBoard(State)), 
  Result = aStar(S, {GoalX,GoalY}, BoundX-1, BoundY-1),
  ets:delete(dict),
  ets:delete(closedSet),
  ets:delete(openSet),
  Result.

