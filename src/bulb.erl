%%%-------------------------------------------------------------------
%%% @author Lukasz
%%% Created : 29. gru 2019 12:39
%%%-------------------------------------------------------------------
-module(bulb).
-compile(export_all).

clear_board() ->
  io:format(os:cmd(clear)).

main() ->
  Main_PID = spawn(bulb, finished, []),
  Main_Room_PID = spawn(bulb, move, [0]),

  Light_1 = spawn(bulb, listen, []),
  Light_2 = spawn(bulb, listen, []),
  Light_3 = spawn(bulb, listen, []),
  Light_4 = spawn(bulb, listen, []),
  Light_5 = spawn(bulb, listen, []),

  Error_Handler_PID = spawn(bulb, handle, []),
  Light_Level_PID = spawn(bulb, start_brightness, []),
  Halt_PID = spawn(bulb, handle_halt, []),

  ets:new(pids, [set, named_table]),
  ets:new(var, [set, named_table, public]),
  ets:insert(pids, [{l1pid, Light_1}, {mpid, Main_PID}, {mrpid, Main_Room_PID}, {error_pid, Error_Handler_PID}, {light_level_pid, Light_Level_PID}, {halt_pid, Halt_PID},
    {l2pid, Light_2}, {l3pid, Light_3}, {l4pid, Light_4}, {l5pid, Light_5}]),

  ets:insert(var, [{brightness_1, 0}]),
  ets:insert(var, [{brightness_2, 0}]),
  ets:insert(var, [{brightness_3, 0}]),
  ets:insert(var, [{brightness_4, 0}]),
  ets:insert(var, [{brightness_5, 0}]),

  [{mpid, Main_PID}] = ets:lookup(pids, mpid),
  clear_board(),
  io:format("\nLight simulator launched!"),
  timer:sleep(2000),
  Light_Level_PID ! {start}.

start_brightness() ->
  [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  [{halt_pid, Halt_PID}] = ets:lookup(pids, halt_pid),
  receive
    {start} ->
      clear_board(),
      InputBrightness = io:fread("\nEnter maximum brightness level for your simulation: ","~d"),
      case InputBrightness of
        {ok, [Max_Brightness]} ->
          if
            Max_Brightness =< 0 ->
              ets:insert(var, [{max_brightness, 100}]),
              Halt_PID ! {wait_for_halt},
              automatic_input();
            true ->
              ets:insert(var, [{max_brightness, Max_Brightness}]),
              handle_input()
          end;
        {error, _} ->
          Error_Handler_PID ! {bad_input_init},
          start_brightness()
      end
  end.

automatic_input()->
  [{l1pid, Listen1_PID}] = ets:lookup(pids, l1pid),
  [{l2pid, Listen2_PID}] = ets:lookup(pids, l2pid),
  [{l3pid, Listen3_PID}] = ets:lookup(pids, l3pid),
  [{l4pid, Listen4_PID}] = ets:lookup(pids, l4pid),
  [{l5pid, Listen5_PID}] = ets:lookup(pids, l4pid),
  N1 = rand:uniform(90),
  N2 = N1 + 10,
  Listen1_PID ! {here, N1, N2},
  Listen2_PID ! {here, N1, N2},
  Listen3_PID ! {here, N1, N2},
  Listen4_PID ! {here, N1, N2},
  Listen5_PID ! {here, N1, N2}.

handle_halt() ->
  receive
    {wait_for_halt} ->
      Halt = io:fread("Type anything to quit.", "~s"),
      case Halt of
        {ok, _ } ->
          halt_sim();
        {error, _} ->
          halt_sim()
      end
  end.

halt_sim() ->
  clear_board(),
  io:format("Terminating lights\n"),
  halt().

handle_input() ->
  [{l1pid, Listen_PID}] = ets:lookup(pids, l1pid),
  [{l2pid, Listen_PID}] = ets:lookup(pids, l2pid),
  [{l3pid, Listen_PID}] = ets:lookup(pids, l3pid),
  [{l4pid, Listen_PID}] = ets:lookup(pids, l4pid),
  [{l5pid, Listen_PID}] = ets:lookup(pids, l5pid),

  [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  [{max_brightness, Max_Brightness}] = ets:lookup(var, max_brightness),

  InputLightNumber = io:fread("Enter light number. If you wish to end simulation enter 1q: ","~d~s"),

  case InputLightNumber of
    {ok, [LightNumber, Char1]} ->
      if Char1 =:= "q" ->
        halt_sim();
        true ->
          InputBrightness = io:fread("Enter requested brightess: ","~d"),
          case InputBrightness of
            {ok, [RequestedBrightness]} ->
              if
               (LightNumber < 0) or (RequestedBrightness > Max_Brightness) or (RequestedBrightness < 0) ->
                  Error_Handler_PID ! {invalid_brightness},
                  handle_input();
                true ->
                  Listen_PID ! {here, LightNumber, RequestedBrightness},
                  handle_input()
              end;
            {error, _ } ->
              Error_Handler_PID ! {bad_input}
          end
      end;
    {error, _ } ->
      Error_Handler_PID ! {bad_input}
  end.


listen() ->
  receive
    {here, Number, Brightness} ->
      change_brightness(Number, Brightness),
      listen()
  end.


change_brightness(LightNumber, RequestedBrightness) ->
  {mrpid, Main_Room_PID} = ets:lookup(pids, mrpid),
  if
    LightNumber =:= 1 ->
      ets:delete(var, brightness_1),
      ets:insert(var, [{brightness_1, RequestedBrightness}]),
      Main_Room_PID ! {success, LightNumber};
    LightNumber =:= 2 ->
      ets:delete(var, brightness_2),
      ets:insert(var, [{brightness_2, RequestedBrightness}]),
      Main_Room_PID ! {success, LightNumber};
    LightNumber =:= 3 ->
      ets:delete(var, brightness_3),
      ets:insert(var, [{brightness_3, RequestedBrightness}]),
      Main_Room_PID ! {success, LightNumber};
    LightNumber =:= 4 ->
      ets:delete(var, brightness_4),
      ets:insert(var, [{brightness_4, RequestedBrightness}]),
      Main_Room_PID ! {success, LightNumber};
    LightNumber =:= 5 ->
      ets:delete(var, brightness_5),
      ets:insert(var, [{brightness_5, RequestedBrightness}]),
      Main_Room_PID ! {success, LightNumber}
  end.

handle() ->
  [{mrpid, Main_Room_PID}] = ets:lookup(pids, mrpid),
  receive
    {bad_input} ->
      clear_board(),
      io:format("\nInvalid input\n"),
      handle_input(),
      handle();
    {bad_input_init} ->
      Main_Room_PID ! {start},
      handle();
    {invalid_light} ->
      clear_board(),
      io:format("\nThere is no light like that!\n"),
      handle()
  end.


changeBrightness(LightNumber) ->
  if
    LightNumber =:= 1 -> [{brightness_1, Brightness}] = ets:lookup(var,brightness_1);
    LightNumber =:= 2 -> [{brightness_2, Brightness}] = ets:lookup(var,brightness_2);
    LightNumber =:= 3 -> [{brightness_3, Brightness}] = ets:lookup(var,brightness_3);
    LightNumber =:= 4 -> [{brightness_4, Brightness}] = ets:lookup(var,brightness_4);
    LightNumber =:= 5 -> [{brightness_5, Brightness}] = ets:lookup(var,brightness_5)
  end,

  timer:sleep(1000),
  clear_board(),
  draw_Light(LightNumber, Brightness).


move() ->
  receive
    {success, LightNumber} ->
      changeBrightness(LightNumber);
    {failure, Brightness} ->
      io:format("\Something went wrong!")
  end.

draw_Light(LightNumber, Brightness) ->
  %timer:sleep(100),
  io:format("\n---\n"),
  io:format(LightNumber),
  io:format(Brightness),
  io:format("\n---\n"),
  if
    Brightness =:= 0 ->  io:format("0");
    Brightness =< 10 ->  io:format("|");
    Brightness > 10 ->  draw_lines(Brightness)
  end.

draw_lines(Brightness) ->
  if
    Brightness > 10 ->
      io:format("|"),
      Brightness = Brightness - 10,
      draw_lines(Brightness)
  end.



