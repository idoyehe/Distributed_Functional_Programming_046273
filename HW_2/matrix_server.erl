-module(matrix_server).
-author("Ido Yehezkel & Ohad Zohar").

%% API
-export([mult/2, get_version/0, shutdown/0, start_server/0, explanation/0, matrix_server/0]).

% generate a matrix with X rows and Y columns with zeros
getZeroMat(X, Y) ->
  list_to_tuple([list_to_tuple([0 || _Y <- lists:seq(1, Y)]) || _X <- lists:seq(1, X)]).

% return the ROW row of a Matrix in a tuple format
getRow(Mat, Row) ->
  element(Row, Mat).

% return the COL col of a Matrix in a tuple format
getCol(Mat, Col) ->
  list_to_tuple([element(Col, ColData) || ColData <- tuple_to_list(Mat)]).

% return a new Matrix which is a copy of OldMat with a NewVal as the value of Row,Col
setElementMat(Row, Col, OldMat, NewVal) ->
  setelement(Row, OldMat, setelement(Col, element(Row, OldMat), NewVal)).

% convert tuple to list
tuple2list(Tuple) when is_tuple(Tuple) -> [element(I, Tuple) || I <- lists:seq(1, tuple_size(Tuple))].

% return the dot product of 2 tuples
dotProduct(V1, RowIndex, V2, ColIndex, Pid) when
  is_tuple(V1) andalso
    is_tuple(V2) andalso
    tuple_size(V1) == tuple_size(V2) ->
  Pid ! {RowIndex, ColIndex, lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, tuple2list(V1), tuple2list(V2)))}.

% creating new process for each Col to multiplied by given Row
colLoop(_, _, _, ColIndex, _) when ColIndex =< 0 -> ok;
colLoop(Row, RowIndex, Matrix2, ColIndex, Pid) when ColIndex > 0 ->
  spawn(fun() -> dotProduct(Row, RowIndex, getCol(Matrix2, ColIndex), ColIndex, Pid) end),
  colLoop(Row, RowIndex, Matrix2, ColIndex - 1, Pid).


% loop each Row in Matrix1 and call colLoop
rowLoop(_, RowIndex, _, _) when RowIndex =< 0 -> ok;
rowLoop(Matrix1, RowIndex, Matrix2, Pid) when RowIndex > 0 ->
  colLoop(getRow(Matrix1, RowIndex), RowIndex, Matrix2, tuple_size(element(1, Matrix2)), Pid),
  rowLoop(Matrix1, RowIndex - 1, Matrix2, Pid).

% collecting all spawned processes into the matrix
matrixMulAux(0, TemplateMat) -> TemplateMat;
matrixMulAux(State, TemplateMat) ->
  receive
    {RowIndex, ColIndex, NewVal} -> % dut product processing
      matrixMulAux(State - 1, setElementMat(RowIndex, ColIndex, TemplateMat, NewVal))
  end.


% starting the loop, generating the template and collect all results into matrix
matricesMultiply(Matrix1, Matrix2, ClientPid, MsgRef) ->
  rowLoop(Matrix1, tuple_size(Matrix1), Matrix2, self()),
  TemplateMat = getZeroMat(tuple_size(Matrix1), tuple_size(element(1, Matrix2))),
  Total = tuple_size(Matrix1) * tuple_size(element(1, Matrix2)),
  ResMatrix = matrixMulAux(Total, TemplateMat),
  ClientPid ! {MsgRef, ResMatrix}.


matrix_server() ->
  receive
    {Pid, MsgRef, {multiple, Mat1, Mat2}} -> % multiple message processing
      spawn(fun() -> matricesMultiply(Mat1, Mat2, Pid, MsgRef) end),
      matrix_server();
    {Pid, MsgRef, get_version} -> % get_version message processing
      Pid ! {MsgRef, version_1},
      matrix_server();
    sw_upgrade -> % sw_upgrade message processing
      ?MODULE:matrix_server();
    shutdown -> % do cleanup here
      cleanup(),
      exit(shutdown);
    _ -> % default message processing, prevent mailbox explosion
      matrix_server()
  end.

cleanup() -> % cleanup here procedure
  unregister(matrix_server).

shutdown() -> % sending shutdown to server
  matrix_server ! shutdown.

mult(Matrix1, Matrix2) ->
  MsgRef = make_ref(),
  try matrix_server ! {self(), MsgRef, {multiple, Matrix1, Matrix2}} of
    _ ->
      receive
        {MsgRef, ResMatrix} -> ResMatrix
      end
  catch % retry
    error:_ ->
      exit(error)
  end.


get_version() ->
  MsgRef = make_ref(),
  try matrix_server ! {self(), MsgRef, get_version} of
    _ ->
      receive
        {MsgRef, Version} -> Version
      end
  catch % retry
    error:_ ->
      exit(error)
  end.

start_server() ->
  spawn(server_supervisor, restarter, []).

explanation() ->
  io:format("The supervisor code and the server code should be in separated moduls because
  we would like to compile and link the upgraded server module as many times as we want without killing the server,
  becuase Erlang can hold only two versions of code for a single module, but the supervisor process is waiting
  in a receive it can prevent us to upgrade the server code more than one time if not seperated.
  When seperating, other alive modules (supervisor) don't distrub upgrading the server module many times.").