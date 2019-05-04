-module(matrix).

-export([matrixMul/4]).

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
dotProduct(Vect1, RowIndex, Vect2, ColIndex, Pid) when
  is_tuple(Vect1) andalso
    is_tuple(Vect2) andalso
    tuple_size(Vect1) == tuple_size(Vect2) ->
  Pid ! {RowIndex, ColIndex, lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, tuple2list(Vect1), tuple2list(Vect2)))}.

% creating new process for each Col to multiplied by given Row
colLoop(Row, RowIndex, Matrix2, ColIndex, Pid) ->
  case ColIndex > 0 of
    true ->
      spawn(fun() -> dotProduct(Row, RowIndex, getCol(Matrix2, ColIndex), ColIndex, Pid) end),
      colLoop(Row, RowIndex, Matrix2, ColIndex - 1, Pid);
    false -> ok
  end.

% loop each Row in Matrix1 and call colLoop
rowLoop(Matrix1, RowIndex, Matrix2, Pid) ->
  case RowIndex > 0 of
    true ->
      colLoop(getRow(Matrix1, RowIndex), RowIndex, Matrix2, tuple_size(element(1, Matrix2)), Pid),
      rowLoop(Matrix1, RowIndex - 1, Matrix2, Pid);
    false -> ok
  end.

% collecting all spawned processes into the matrix
matrixMulAux(0, TemplateMat) -> TemplateMat;
matrixMulAux(State, TemplateMat) ->
  receive
    {RowIndex, ColIndex, NewVal} -> % dut product processing
      matrixMulAux(State - 1, setElementMat(RowIndex, ColIndex, TemplateMat, NewVal))
  end.


% starting the loop, generating the template and collect all results into matrix
matrixMul(Matrix1, Matrix2, Pid, MsgRef) ->
  rowLoop(Matrix1, tuple_size(Matrix1), Matrix2, self()),
  TemplateMat = getZeroMat(tuple_size(Matrix1), tuple_size(element(1, Matrix2))),
  Total = tuple_size(Matrix1) * tuple_size(element(1, Matrix2)),
  Pid ! {MsgRef, matrixMulAux(Total, TemplateMat)}.
