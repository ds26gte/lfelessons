% first change 2013-03-03
% last change 2015-07-11

% You can run these tests in either the lfe or the erl shell.
%
% In lfe, type
%              (eunit:test 'testall)
%
% In erl, type
%              eunit:test(testall).

-module(testall).
-include_lib("eunit/include/eunit.hrl").

within(V,M,N) ->
    ?assert(M < V andalso V < N).

montyhall_test() ->
    within(montyhall:run(), 0.63, 0.69).

guess_test_help(Run) ->
    within(Run(24), 0.9, 1.1).

guess_test() ->
    guess_test_help(fun guess:run/1).

guess2_test() ->
    guess_test_help(fun guess2:run/1).

craps_test() ->
    within(craps:run(), 0.46, 0.53).

elevator_test_help(Run) ->
    within(Run(1), 0.8, 0.85),
    within(Run(2), 0.7, 0.75),
    within(Run(3), 0.62, 0.67).

elevator_test() ->
    elevator_test_help(fun elevator:run/1).

elevatorf_test() ->
    elevator_test_help(fun elevatorf:run/1).
