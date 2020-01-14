%%%-------------------------------------------------------------------
%%% @author Lukasz
%%% Created : 29. gru 2019 12:39
%%%-------------------------------------------------------------------
-module(bulb).
-compile(export_all).

clear_board() ->
  io:format(os:cmd(clear)).

main() ->
  Main_Room_PID = spawn(bulb, draw, []),
  Controller = spawn(bulb, listen, []),
  Light_1 = spawn(bulb, light1, []),
  Light_2 = spawn(bulb, light2, []),
  Light_3 = spawn(bulb, light3, []),
  Light_4 = spawn(bulb, light4, []),
  Light_5 = spawn(bulb, light5, []),
  Changer = spawn(bulb, change_brightness, []),
  Error_Handler_PID = spawn(bulb, handle, []),
  Set_Up_PID = spawn(bulb, start_brightness, []),
  Halt_PID = spawn(bulb, handle_halt, []),
  Drawer = spawn(bulb, draw_all_lights, []),

  ets:new(pids, [set, named_table]),
  ets:new(var, [set, named_table, public]),
  ets:insert(pids, [{l1pid, Light_1}, {l2pid, Light_2}, {l3pid, Light_3}, {l4pid, Light_4}, {l5pid, Light_5},
    {controller, Controller}, {mrpid, Main_Room_PID}, {changer, Changer}, {drawer, Drawer},
    {error_pid, Error_Handler_PID}, {light_level_pid, Set_Up_PID},
    {halt_pid, Halt_PID}]),

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
      [{_, Light_PID}] = ets:lookup(pids, l1pid),
      Light_PID;
    Number =:= 2 ->
      [{_, Light_PID}] = ets:lookup(pids, l2pid),
      Light_PID;
    Number =:= 3 ->
      [{_, Light_PID}] = ets:lookup(pids, l3pid),
      Light_PID;
    Number =:= 4 ->
      [{_, Light_PID}] = ets:lookup(pids, l4pid),
      Light_PID;
    Number =:= 5 ->
      [{_, Light_PID}] = ets:lookup(pids, l5pid),
      Light_PID;
    true -> Error_Handler_PID ! {invalid_light}
  end.

get_light_by_pid(Pid) ->
  [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  [{_, Light1_PID}] = ets:lookup(pids, l1pid),
  [{_, Light2_PID}] = ets:lookup(pids, l2pid),
  [{_, Light3_PID}] = ets:lookup(pids, l3pid),
  [{_, Light4_PID}] = ets:lookup(pids, l4pid),
  [{_, Light5_PID}] = ets:lookup(pids, l5pid),

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
  [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
  [{brightness_1, Brightness1}] = ets:lookup(var,brightness_1),
  [{brightness_2, Brightness2}] = ets:lookup(var,brightness_2),
  [{brightness_3, Brightness3}] = ets:lookup(var,brightness_3),
  [{brightness_4, Brightness4}] = ets:lookup(var,brightness_4),
  [{brightness_5, Brightness5}] = ets:lookup(var,brightness_5),

  [{_, Light1_PID}] = ets:lookup(pids, l1pid),
  [{_, Light2_PID}] = ets:lookup(pids, l2pid),
  [{_, Light3_PID}] = ets:lookup(pids, l3pid),
  [{_, Light4_PID}] = ets:lookup(pids, l4pid),
  [{_, Light5_PID}] = ets:lookup(pids, l5pid),

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
  [{_, Changer}] = ets:lookup(pids, changer),
  receive
    {change, Brightness} ->
      Changer ! {light1, Brightness},
      light1()
  end.

light2() ->
  [{_, Changer}] = ets:lookup(pids, changer),
  receive
    {change, Brightness} ->
      Changer ! {light2, Brightness},
      light2()
  end.

light3() ->
  [{_, Changer}] = ets:lookup(pids, changer),
  receive
    {change, Brightness} ->
      Changer ! {light3, Brightness},
      light3()
  end.

light4() ->
  [{_, Changer}] = ets:lookup(pids, changer),
  receive
    {change, Brightness} ->
      Changer ! {light4, Brightness},
      light4()
  end.

light5() ->
  [{_, Changer}] = ets:lookup(pids, changer),
  receive
    {change, Brightness} ->
      Changer ! {light5, Brightness},
      light5()
  end.

change_brightness() ->
  [{mrpid, Main_Room_PID}] = ets:lookup(pids, mrpid),
  receive
    {light1, Brightness} ->
      io:format("\nChanging brightness 1\n"),
      [{_, Light_PID}] = ets:lookup(pids, l1pid),
      ets:delete(var, brightness_1),
      ets:insert(var, [{brightness_1, Brightness}]),
      Main_Room_PID ! {success, Light_PID},
      change_brightness();
    {light2, Brightness} ->
      [{_, Light_PID}] = ets:lookup(pids, l2pid),
      ets:delete(var, brightness_2),
      ets:insert(var, [{brightness_2, Brightness}]),
      Main_Room_PID ! {success, Light_PID},
      change_brightness();
    {light3, Brightness} ->
      [{_, Light_PID}] = ets:lookup(pids, l3pid),
      ets:delete(var, brightness_3),
      ets:insert(var, [{brightness_3, Brightness}]),
      Main_Room_PID ! {success, Light_PID},
      change_brightness();
    {light4, Brightness} ->
      [{_, Light_PID}] = ets:lookup(pids, l4pid),
      ets:delete(var, brightness_4),
      ets:insert(var, [{brightness_4, Brightness}]),
      Main_Room_PID ! {success, Light_PID},
      change_brightness();
    {light5, Brightness} ->
      [{_, Light_PID}] = ets:lookup(pids, l5pid),
      ets:delete(var, brightness_5),
      ets:insert(var, [{brightness_5, Brightness}]),
      Main_Room_PID ! {success, Light_PID},
      change_brightness()
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

draw() ->
  [{_, Drawer}] = ets:lookup(pids, drawer),
  receive
    {success, Light} ->
      draw_light(Light),
      Drawer ! {draw},
      draw();
    {failure, Light} ->
      io:format("\Something went wrong!\n"),
      halt_sim()
  end.

draw_light(Light_PID) ->
  Brightness = get_brightness_by_pid(Light_PID),
  timer:sleep(1000),
  clear_board(),
  draw_brightness(Light_PID, Brightness).

draw_brightness(LightNumber, Brightness) ->
  io:format("\n---\n"),
  io:format("Light: ~w~n", [LightNumber]),
  io:format("Brightness: ~w~n", [Brightness]),
  if
    Brightness =:= 0 ->  io:format("\n---\n");
    Brightness =< 10 ->  io:format("|");
    Brightness > 10 ->  draw_brightness(Brightness)
  end.

draw_brightness(Brightness) ->
  if
    Brightness > 10 ->
      io:format("|"),
      NewBrightness = Brightness - 10,
      draw_brightness(NewBrightness);
    true ->
      io:format("\n---\n")
  end.

draw_all_lights() ->
  [{_, Light1_PID}] = ets:lookup(pids, l1pid),
  [{_, Light2_PID}] = ets:lookup(pids, l2pid),
  [{_, Light3_PID}] = ets:lookup(pids, l3pid),
  [{_, Light4_PID}] = ets:lookup(pids, l4pid),
  [{_, Light5_PID}] = ets:lookup(pids, l5pid),

  receive
    {draw} ->
      draw_light(Light1_PID),
      draw_light(Light2_PID),
      draw_light(Light3_PID),
      draw_light(Light4_PID),
      draw_light(Light5_PID),
      draw_all_lights()
  end.


