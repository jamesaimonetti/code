-module(p45).

-export([answer/0]).

%% Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
%% Triangle   T(n)=n(n+1)/2   1, 3, 6, 10, 15, ...
%% Pentagonal   P(n)=n(3n−1)/2   1, 5, 12, 22, 35, ...
%% Hexagonal   H(n)=n(2n−1)   1, 6, 15, 28, 45, ...
%% It can be verified that T(285) = P(165) = H(143) = 40755.
%% Find the next triangle number that is also pentagonal and hexagonal.

-compile(export_all).

answer() ->
    Ts = t_list(286),
    Ps = p_list(166),
    Hs = h_list(144),
    next_convergence(Ts, Ps, Hs).

next_convergence([A|_Ts], [A|_Ps], [A|_Hs]) -> A;
next_convergence([T|Ts], [P|Ps], Hs) when T > P ->
    next_convergence([T|Ts], Ps(), Hs);
next_convergence([T|Ts], Ps, [H|Hs]) when T > H ->
    next_convergence([T|Ts], Ps, Hs());
next_convergence([_T|Ts], Ps, Hs) ->
    next_convergence(Ts(), Ps, Hs).

t_list(N) -> [N * (N+1) div 2 | fun() -> t_list(N+1) end].
p_list(N) -> [N * (3*N-1) div 2 | fun() -> p_list(N+1) end].
h_list(N) -> [N * (2 * N - 1) | fun() -> h_list(N+1) end].
