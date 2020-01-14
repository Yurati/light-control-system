%%%-------------------------------------------------------------------
%%% @author Lukasz
%%% Created : 29. gru 2019 12:39
%%%-------------------------------------------------------------------
-module(bulb).
-compile(export_all).

clear_board() ->
  io:format(os:cmd(clear)).

main() ->
  Main_Room_PID = spawn(bulb, move, []),
  Controller = spawn(bulb, listen, []),
  Light_1 = spawn(bulb, light1, []),
  Light_2 = spawn(bulb, light2, []),
  Light_3 = spawn(bulb, light3, []),
  Light_4 = spawn(bulb, light4, []),
  Light_5 = spawn(bulb, light5, []),

  Error_Handler_PID = spawn(bulb, handle, []),
  Set_Up_PID = spawn(bulb, start_brightness, []),
  Halt_PID = spawn(bulb, handle_halt, []),

  ets:new(pids, [set, named_table]),
  ets:new(var, [set, named_table, public]),
  ets:insert(pids, [{l1pid, Light_1}, {l2pid, Light_2}, {l3pid, Light_3}, {l4pid, Light_4}, {l5pid, Light_5},
    {controller, Controller}, {mrpid, Main_Room_PID}, {error_pid, Error_Handler_PID}, {light_level_pid, Set_Up_PID}, {halt_pid, Halt_PID}]),

  ets:insert(var, [{brightness_1, 0}]),
  ets:insert(var, [{brightness_2, 0}]),
  ets:insert(var, [{brightness_3, 0}]),
  ets:insert(var, [{brightness_4, 0}]),
  ets:insert(var, [{brightness_5, 0}]),

  clear_board(),
  io:format("\nLight simulator launched!"),
  timer:sleep(2000),
  Set_Up_PID ! {start}.

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
              handle_input();
            true ->
              ets:insert(var, [{max_brightness, Max_Brightness}]),
              handle_input()
          end;
        {error, _} ->
          Error_Handler_PID ! {bad_input_init},
          start_brightness()
      end
  end.

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
  [{controller, Controller_PID}] = ets:lookup(pids, controller),
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
                  LightToBeChanged = get_light_by_number(LightNumber),
                  Controller_PID ! {here, LightToBeChanged, RequestedBrightness},
                  handle_input()
              end;
            {error, _ } ->
              Error_Handler_PID ! {bad_input}
          end
      end;
    {error, _ } ->
      Error_Handler_PID ! {bad_input}
  end.

get_light_by_number(Number) ->
  [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  if
    Number =:= 1 ->
      [{_, Light_PID}] = ets:lookup(pids, ldpi1),
      Light_PID;
    Number =:= 2 ->
      [{_, Light_PID}] = ets:lookup(pids, ldpi2),
      Light_PID;
    Number =:= 3 ->
      [{_, Light_PID}] = ets:lookup(pids, ldpi3),
      Light_PID;
    Number =:= 4 ->
      [{_, Light_PID}] = ets:lookup(pids, ldpi4),
      Light_PID;
    Number =:= 5 ->
      [{_, Light_PID}] = ets:lookup(pids, ldpi5),
      Light_PID;
    true -> Error_Handler_PID ! {invalid_light}
  end.

get_light_by_pid(Pid) ->
  [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  [{_, Light1_PID}] = ets:lookup(pids, ldpi1),
  [{_, Light2_PID}] = ets:lookup(pids, ldpi2),
  [{_, Light3_PID}] = ets:lookup(pids, ldpi3),
  [{_, Light4_PID}] = ets:lookup(pids, ldpi4),
  [{_, Light5_PID}] = ets:lookup(pids, ldpi5),

  if
    Pid =:= Light1_PID -> Light1_PID;
    Pid =:= Light2_PID -> Light2_PID;
    Pid =:= Light3_PID -> Light3_PID;
    Pid =:= Light4_PID -> Light4_PID;
    Pid =:= Light5_PID -> Light5_PID;
    true ->
      Error_Handler_PID ! {invalid_light}
  end.

get_brightness_by_pid(Pid) ->
  [{_, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  [{_, Brightness1}] = ets:lookup(var,brightness_1),
  [{_, Brightness2}] = ets:lookup(var,brightness_2),
  [{_, Brightness3}] = ets:lookup(var,brightness_3),
  [{_, Brightness4}] = ets:lookup(var,brightness_4),
  [{_, Brightness5}] = ets:lookup(var,brightness_5),

  [{_, Light1_PID}] = ets:lookup(pids, ldpi1),
  [{_, Light2_PID}] = ets:lookup(pids, ldpi2),
  [{_, Light3_PID}] = ets:lookup(pids, ldpi3),
  [{_, Light4_PID}] = ets:lookup(pids, ldpi4),
  [{_, Light5_PID}] = ets:lookup(pids, ldpi5),

  if
    Pid =:= Light1_PID -> Brightness1;
    Pid =:= Light2_PID -> Brightness2;
    Pid =:= Light3_PID -> Brightness3;
    Pid =:= Light4_PID -> Brightness4;
    Pid =:= Light5_PID -> Brightness5;
    true ->
      Error_Handler_PID ! {invalid_light}
  end.

listen() ->
  receive
    {here, Light , Brightness} ->
      Light ! {change, Brightness},
      listen()
  end.

light1() ->
  receive
    {change, Brightness} ->
      Changer = spawn(bulb, change_brightness, []),
      Changer ! {self(), Brightness},
      light1()
  end.

light2() ->
  receive
    {change, Brightness} ->
      Changer = spawn(bulb, change_brightness, []),
      Changer ! {self(), Brightness},
      light2()
  end.

light3() ->
  receive
    {change, Brightness} ->
      Changer = spawn(bulb, change_brightness, []),
      Changer ! {self(), Brightness},
      light3()
  end.

light4() ->
  receive
    {change, Brightness} ->
      Changer = spawn(bulb, change_brightness, []),
      Changer ! {self(), Brightness},
      light4()
  end.

light5() ->
  receive
    {change, Brightness} ->
      Changer = spawn(bulb, change_brightness, []),
      Changer ! {self(), Brightness},
      light5()
  end.

change_brightness() ->
  {mrpid, Main_Room_PID} = ets:lookup(pids, mrpid),
  [{_, Light1_PID}] = ets:lookup(pids, ldpi1),
  [{_, Light2_PID}] = ets:lookup(pids, ldpi2),
  [{_, Light3_PID}] = ets:lookup(pids, ldpi3),
  [{_, Light4_PID}] = ets:lookup(pids, ldpi4),
  [{_, Light5_PID}] = ets:lookup(pids, ldpi5),
  receive
    {Light_PID, Brightness} ->
      if
        Light_PID =:= Light1_PID ->
          ets:delete(var, brightness_1),
          ets:insert(var, [{brightness_1, Brightness}]),
          Main_Room_PID ! {success, Light_PID};
        Light_PID =:= Light2_PID ->
          ets:delete(var, brightness_2),
          ets:insert(var, [{brightness_2, Brightness}]),
          Main_Room_PID ! {success, Light_PID};
        Light_PID =:= Light3_PID ->
          ets:delete(var, brightness_3),
          ets:insert(var, [{brightness_3, Brightness}]),
          Main_Room_PID ! {success, Light_PID};
        Light_PID =:= Light4_PID ->
          ets:delete(var, brightness_4),
          ets:insert(var, [{brightness_4, Brightness}]),
          Main_Room_PID ! {success, Light_PID};
        Light_PID =:= Light5_PID ->
          ets:delete(var, brightness_5),
          ets:insert(var, [{brightness_5, Brightness}]),
          Main_Room_PID ! {success, Light_PID};
        true ->
          Main_Room_PID ! {failure}
      end
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
      handle();
    {failure} ->
      clear_board(),
      io:format("\nUnexpected error\n"),
      handle()
  end.


draw_light(Light_PID) ->
  Brightness = get_brightness_by_pid(Light_PID),
  timer:sleep(1000),
  clear_board(),
  draw_brightness(Light_PID, Brightness).


move() ->
  receive
    {success, Light} ->
      draw_light(Light);
    {failure, Light} ->
      io:format("\Something went wrong!")
  end.

draw_brightness(LightNumber, Brightness) ->
  %timer:sleep(100),
  io:format("\n---\n"),
  io:format(LightNumber),
  io:format(Brightness),
  io:format("\n---\n"),
  if
    Brightness =:= 0 ->  io:format("0");
    Brightness =< 10 ->  io:format("|");
    Brightness > 10 ->  draw_brightness(Brightness)
  end.

draw_brightness(Brightness) ->
  if
    Brightness > 10 ->
      io:format("|"),
      Brightness = Brightness - 10,
      draw_brightness(Brightness)
  end.



