-module(hw1_game_test).
-author("kfiros").
-export([run/0]).


test_game_inputs() ->
	% Test inputs
	erlang:display("[*] Test_1: Input tests for canWin"),
	try game:canWin(-10) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error0 -> erlang:display({error, caught, Error0})
	end,
	try game:canWin(0) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error1 -> erlang:display({error, caught, Error1})
	end,
	try game:canWin(-1.5) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error2 -> erlang:display({error, caught, Error2})
	end,
	try game:canWin(0.2) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error3 -> erlang:display({error, caught, Error3})
	end,
	erlang:display("[*] Test_1: Pass"),

	erlang:display("[*] Test_2: Input tests for nextMove"),
	try game:nextMove(-10) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error4 -> erlang:display({error, caught, Error4})
	end,
	try game:nextMove(0) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error5 -> erlang:display({error, caught, Error5})
	end,
	try game:nextMove(-1.5) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error6 -> erlang:display({error, caught, Error6})
	end,
	try game:nextMove(0.2) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error7 -> erlang:display({error, caught, Error7})
	end,
	erlang:display("[*] Test_2: Pass"),

	erlang:display("[*] Test_3: Input tests for explanation"),
	try game:explanation(-10) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error8 -> erlang:display({error, caught, Error8})
	end,
	try game:explanation(0) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error9 -> erlang:display({error, caught, Error9})
	end,
	try game:explanation(-1.5) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error10 -> erlang:display({error, caught, Error10})
	end,
	try game:explanation(0.2) of
		_ -> erlang:display("Error - no exception thrown")
	catch
		error:Error11 -> erlang:display({error, caught, Error11})
	end,
	erlang:display("[*] Test_3: Pass").

test_game_logic() ->
	erlang:display("[*] Test_1: Test canWin Logic"),
	true = game:canWin(1),
	true = game:canWin(2),
	false = game:canWin(3),
	true = game:canWin(4),
	true = game:canWin(5),
	false = game:canWin(6),
	true = game:canWin(7),
	true = game:canWin(8),
	false = game:canWin(9),
	true = game:canWin(10),
	true = game:canWin(11),
	false = game:canWin(12),
	true = game:canWin(13),
	true = game:canWin(14),
	false = game:canWin(15),
	erlang:display("[*] Test_1: Pass"),

	erlang:display("[*] Test_2: Test nextMove Logic"),
	false = game:nextMove(3),
	false = game:nextMove(6),
	false = game:nextMove(9),
	false = game:nextMove(12),
	false = game:nextMove(15),

	
	{true, 1} = game:nextMove(1),
	{true, 2} = game:nextMove(2),
	{true, 1} = game:nextMove(4),
	{true, 2} = game:nextMove(5),
	{true, 1} = game:nextMove(7),
	{true, 2} = game:nextMove(8),
	{true, 1} = game:nextMove(10),
	{true, 2} = game:nextMove(11),
	
	erlang:display("[*] Test_2: Pass"),

	erlang:display("[*] Test_3: Test explanation Logic"),
	erlang:display(game:explanation()),
	erlang:display("[*] Test_3: Pass").
run() ->
	% Compile files
	erlang:display("[*] Test_1: Compile game.erl"),
	compile:file(game),
	erlang:display("[*] Test_1: Pass"),
	erlang:display("[*] Testing game module"),
	test_game_inputs(),
	test_game_logic(),
	erlang:display("[*] Success").
