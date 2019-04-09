-module(shapes).
-author("idoYe & ohadZ").

-export([shapesArea/1, shapesFilter/1, shapesFilter2/1, squaresArea/1, trianglesArea/1]).

%%area calculations functions
%%validate all dimensions are positive by guards
area({rectangle, {dim, Width, Height}}) when Width > 0 andalso Height > 0 -> Width * Height;
area({triangle, {dim, Base, Height}}) when Base > 0 andalso Height > 0 -> Base * Height * 0.5;
area({ellipse, {radius, Radius1, Radius2}}) when Radius1 > 0 andalso Radius2 > 0 -> math:pi() * Radius1 * Radius2.

%%area of empty list is 0
shapesArea({shapes, []}) -> 0;
shapesArea({shapes, [H | T]}) -> shapesArea({shapes, [H | T]}, 0).
shapesArea({shapes, []}, SUM) -> SUM;
shapesArea({shapes, [H | T]}, SUM) -> shapesArea({shapes, T}, SUM + area(H)).

%% calculate all squares areas by filter shapes list and then call "shapesArea"
squaresArea({shapes, [H | T]}) -> shapesArea(filterSquare({shapes, [H | T]})).
%% calculate all triangles areas by filter shapes list and then call "shapesArea"
trianglesArea({shapes, [H | T]}) -> shapesArea(filterTriangle({shapes, [H | T]})).

predSquare({Shape, {Dim, D1, D2}}) when D1 > 0 andalso D2 > 0 andalso (Shape == triangle orelse Shape == ellipse orelse Shape == rectangle) ->
  Shape == rectangle andalso Dim == dim andalso D1 =:= D2.
filterSquare({shapes, []}) -> {shapes, []};
filterSquare({shapes, [H | T]}) -> {shapes, lists:filter(fun predSquare/1, [H | T])}.


predCircle({Shape, {Dim, R1, R2}}) when R1 > 0 andalso R2 > 0 andalso (Shape == triangle orelse Shape == ellipse orelse Shape == rectangle) ->
  Shape == ellipse andalso Dim == radius andalso R1 =:= R2.
filterCircle({shapes, []}) -> {shapes, []};
filterCircle({shapes, [H | T]}) -> {shapes, lists:filter(fun predCircle/1, [H | T])}.

predRectangle({Shape, {Dim, D1, D2}}) when D1 > 0 andalso D2 > 0 andalso (Shape == triangle orelse Shape == ellipse orelse Shape == rectangle) ->
  Shape == rectangle andalso Dim == dim.
filterRectangle({shapes, []}) -> {shapes, []};
filterRectangle({shapes, [H | T]}) -> {shapes, lists:filter(fun predRectangle/1, [H | T])}.


predEllipse({Shape, {Dim, R1, R2}}) when R1 > 0 andalso R2 > 0 andalso (Shape == triangle orelse Shape == ellipse orelse Shape == rectangle) ->
  Shape == ellipse andalso Dim == radius.
filterEllipse({shapes, []}) -> {shapes, []};
filterEllipse({shapes, [H | T]}) -> {shapes, lists:filter(fun predEllipse/1, [H | T])}.


predTriangle({Shape, {Dim, B, H}}) when B > 0 andalso H > 0 andalso (Shape == triangle orelse Shape == ellipse orelse Shape == rectangle) ->
  Shape == triangle andalso Dim == dim.
filterTriangle({shapes, []}) -> {shapes, []};
filterTriangle({shapes, [H | T]}) -> {shapes, lists:filter(fun predTriangle/1, [H | T])}.


shapesFilter(rectangle) -> fun filterRectangle/1;
shapesFilter(ellipse) -> fun filterEllipse/1;
shapesFilter(triangle) -> fun filterTriangle/1.

shapesFilter2(rectangle) -> shapesFilter(rectangle);
shapesFilter2(ellipse) -> shapesFilter(ellipse);
shapesFilter2(triangle) -> shapesFilter(triangle);
shapesFilter2(square) -> fun filterSquare/1;
shapesFilter2(circle) -> fun filterCircle/1.
