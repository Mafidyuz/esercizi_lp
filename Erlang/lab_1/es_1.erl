-module(es1).
-export([is_palindrome/1, is_an_anagram/2, is_prime/1, factors/1, perfect/1, is_proper/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%is_palindrome: string → bool that checks if the string given as input is palindrome, 
%a string is palindrome when the represented sentence can be read the same way in either directions in spite of spaces, 
%punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...;
is_char(C) -> ((C >= $A) and (C =< $Z)) or ((C >= $a) and (C =< $z)).

is_palindrome(S) -> STR = string:casefold(lists:filter(fun(C) -> is_char(C) end, S)), STR == string:reverse(STR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%is_an_anagram : string → string list → boolean that given a dictionary of strings, 
%checks if the input string is an anagram of one or more of the strings in the dictionary;
contains([HD|TL], X) -> (HD == X) or contains(TL, X);
contains([], _) -> false.

is_an_anagram(S, LS) -> contains( lists:map(fun(X) -> lists:sort(X) end, LS) ,lists:sort(S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%factors: int → int list that given a number calculates all its prime factors;
is_prime(N) -> length([X||X <- lists:seq(2,round(math:sqrt(N))), N rem X == 0]) == 0. %non richiesto dall'esercizio

factors_aux(N, DIV) -> 
    case N rem DIV of
        0 when DIV =< N -> [DIV] ++ factors_aux(round(N/DIV), 2);
        _ when DIV < N -> factors_aux(N, DIV+1);
        _ when DIV >= N -> []
    end.

factors(N) -> factors_aux(N, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%is_proper: int → boolean that given a number calculates if it is a perfect number or not, where a perfect number is a 
%positive integer equal to the sum of its proper positive divisors (excluding itself), e.g., 6 is a perfect number 
%since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;
perfect(1) -> 1;
perfect(N) -> N + perfect(N-1).

is_proper(N) -> contains([es1:perfect(X) || X <- lists:seq(1,10), perfect(X) =< N], N).
