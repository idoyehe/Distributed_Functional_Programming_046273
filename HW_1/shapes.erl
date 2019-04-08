-module(shapes).
-author("idoYe & ohadZ").


-export([shapesArea/1, shapesFilter/1, shapesFilter2/1, squaresArea/1, trianglesArea/1]).

%%validate all dimensions are positive
dimValidator(D1, D2) ->
  case D1 > 0 andalso D2 > 0 of
    true -> true;
    false -> erlang:error("Not a valid dimensions shape object")
  end.

%%validate all dimensions are positive and the pattern matching
shapeValidatorInner({rectangle, {dim, D1, D2}}) -> dimValidator(D1, D2);
shapeValidatorInner({triangle, {dim, D1, D2}}) -> dimValidator(D1, D2);
shapeValidatorInner({ellipse, {radius, D1, D2}}) -> dimValidator(D1, D2).

shapeValidator({shapes, []})->true;%%empty list is valid
shapeValidator({shapes, [H | T]}) -> shapeValidator({shapes, [H | T]}, true).%%tail recursion
shapeValidator({shapes, []}, ACC) -> ACC;
shapeValidator({shapes, [H | T]}, ACC) -> shapeValidator({shapes, T}, ACC andalso shapeValidatorInner(H)).

%%area calculations functions
area({rectangle, {dim, Width, Height}}) -> Width * Height;
area({triangle, {dim, Base, Height}}) -> Base * Height * 0.5;
area({ellipse, {radius, Radius1, Radius2}}) -> math:pi() * Radius1 * Radius2.

shapesArea({shapes, []}) -> 0;
%%first checking if object is valid
shapesArea({shapes, [H | T]}) ->
  case shapeValidator({shapes, [H | T]}) of
    true -> shapesArea({shapes, [H | T]}, 0)
  end.
shapesArea({shapes, []}, SUM) -> SUM;
shapesArea({shapes, [H | T]}, SUM) -> shapesArea({shapes, T}, SUM + area(H)).

%% calculate all squares areas by filter shapes list and then call "shapesArea"
squaresArea({shapes, [H | T]}) -> shapesArea(filterSquare({shapes, [H | T]})).
%% calculate all triangles areas by filter shapes list and then call "shapesArea"
trianglesArea({shapes, [H | T]}) -> shapesArea(filterTriangle({shapes, [H | T]})).

filterSquare({shapes, []}) -> {shapes, []};
filterSquare({shapes, [H | T]}) ->
  case shapeValidator({shapes, [H | T]}) of
    true -> {shapes, [{rectangle, {dim, D1, D1}} || {rectangle, {dim, D1, D1}} <- [H | T]]}
  end.


filterCircle({shapes, []}) ->{shapes, []};
filterCircle({shapes, [H | T]}) ->
  case shapeValidator({shapes, [H | T]}) of
    true -> {shapes, [{ellipse, {radius, D1, D1}} || {ellipse, {radius, D1, D1}} <- [H | T]]}
  end.

filterRectangle({shapes, []}) ->{shapes, []};
filterRectangle({shapes, [H | T]}) ->
  case shapeValidator({shapes, [H | T]}) of
    true -> {shapes, [{rectangle, {dim, D1, D2}} || {rectangle, {dim, D1, D2}} <- [H | T]]}
  end.

filterEllipse({shapes, []}) ->{shapes, []};
filterEllipse({shapes, [H | T]}) ->
  case shapeValidator({shapes, [H | T]}) of
    true -> {shapes, [{ellipse, {radius, D1, D2}} || {ellipse, {radius, D1, D2}} <- [H | T]]}
  end.

filterTriangle({shapes, []}) ->{shapes, []};
filterTriangle({shapes, [H | T]}) ->
  case shapeValidator({shapes, [H | T]}) of
    true -> {shapes, [{triangle, {dim, D1, D2}} || {triangle, {dim, D1, D2}} <- [H | T]]}
  end.


shapesFilter(rectangle) -> fun filterRectangle/1;
shapesFilter(ellipse) -> fun filterEllipse/1;
shapesFilter(triangle) -> fun filterTriangle/1.

shapesFilter2(rectangle) -> shapesFilter(rectangle);
shapesFilter2(ellipse) -> shapesFilter(ellipse);
shapesFilter2(triangle) -> shapesFilter(triangle);
shapesFilter2(square) -> fun filterSquare/1;
shapesFilter2(circle) -> fun filterCircle/1.