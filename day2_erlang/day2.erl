-module(day2).

-export([part1/0, part2/0]).

-record(sub, {horPos, depth, aim}).

initSub() ->
  #sub{horPos = 0,
       depth = 0,
       aim = 0}.

product(Sub) ->
  Sub#sub.horPos * Sub#sub.depth.

part1() ->
  Input =
    case file:read_file("input") of
      {ok, Content} ->
        Content;
      {error, _} ->
        throw("Could not read input file")
    end,
  Lines =
    string:split(
      string:trim(Input), "\n", all),
  string:split(
    string:trim(Input), "\n", all),
  FinalSub =
    lists:foldl(fun(Line, #sub{horPos = HorPos, depth = Depth} = Sub) ->
                   NextSub =
                     case string:split(Line, " ", all) of
                       [<<"up">>, N] -> Sub#sub{depth = Depth - binary_to_integer(N)};
                       [<<"down">>, N] -> Sub#sub{depth = Depth + binary_to_integer(N)};
                       [<<"forward">>, N] -> Sub#sub{horPos = HorPos + binary_to_integer(N)};
                       _ -> throw(error("invalid line: " ++ string:split(Line, " ", all)))
                     end,
                   NextSub
                end,
                initSub(),
                Lines),
  io:fwrite("~w\n", [product(FinalSub)]).

part2() ->
  Input =
    case file:read_file("input") of
      {ok, Content} ->
        Content;
      {error, _} ->
        throw("Could not read input file")
    end,
  Lines =
    string:split(
      string:trim(Input), "\n", all),
  string:split(
    string:trim(Input), "\n", all),
  FinalSub =
    lists:foldl(fun(Line,
                    #sub{horPos = HorPos,
                         depth = Depth,
                         aim = Aim} =
                      Sub) ->
                   NextSub =
                     case string:split(Line, " ", all) of
                       [<<"up">>, N] -> Sub#sub{aim = Aim - binary_to_integer(N)};
                       [<<"down">>, N] -> Sub#sub{aim = Aim + binary_to_integer(N)};
                       [<<"forward">>, N] ->
                         Num = binary_to_integer(N),
                         Sub#sub{horPos = HorPos + Num, depth = Depth + Aim * Num};
                       _ -> throw(error("invalid line: " ++ string:split(Line, " ", all)))
                     end,
                   NextSub
                end,
                initSub(),
                Lines),
  io:fwrite("~w\n", [product(FinalSub)]).
