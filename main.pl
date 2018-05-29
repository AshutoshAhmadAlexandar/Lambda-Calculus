% Lambda Calculus Simulator
% author: Bharat Jayaraman
% date: March 31, 2018


:- dynamic fun/2.
:- dynamic reduce/1.

:- include(grammar).
:- include(lambda).


go :- write('Default strategy is to reduce the leftmost redex. Use \'reduce normal\''), nl,
      write('command to change to call-by-value reduction. Commands:'), nl,
      write('          norm Term'), nl,
      write('          derive Term'), nl,
      write('          subst Term1 [Var <- Term2]'), nl,
      write('          let Name = Term'), nl,
      write('          reduce beta'), nl,
      write('          reduce normal'), nl,
      write('          end'), nl,
      nl,
      asserta(reduce(beta)),
      go2.

go2 :-
	read_line_to_codes(user_input, C),
	(C = []
	   -> go2
	   ;  tokenize(C, Tokens),
	     (Tokens == [end]
	        -> nl
	        ; (go(ParseTree,Tokens,[])
		      ->  process(ParseTree)
		      ;  write('syntax error - please re-enter'), nl
		  ),
	          go2)
	),
	!.


go(let(N,T)) --> [let], [id(N)], ['='], term(T).

go(subst(T1,V,T2)) --> [subst], term(T1), ['['], var(V), ['<-'], term(T2), [']'].

go(derive(T)) --> [derive], term(T).

go(norm(T)) --> [norm], term(T).

go(alpha(T1, T2)) --> [alpha], term(T1), ['='], term(T2).  % not done

go(reduce(beta)) --> [reduce, beta].

go(reduce(normal)) --> [reduce, normal].


% ===========================================================


process(norm(T)) :-
	reduce(T, R, []),
	(T == R                    % must do alpha(T, R) test instead
	      -> (reducible(T)
	             -> write('=> ... nontermination'), nl
	              ; write('=> '), pretty_print(T), nl
	         )
              ; process(norm(R))
        ).


process(derive(T)) :-
       reduce(T, R, []),
       (T == R                   % must do alpha(T, R) test instead
          -> (reducible(T)
	         -> write('=> ... nontermination'), nl
	          ; nl
	     )
          ; write('=> '), pretty_print(R), nl,
	    process(derive(R))
       ).

process(subst(T1,V,T)) :-
       substitute(T1,V,T,T2),
       pretty_print(T2),
       nl.

process(let(N, T)) :-
	asserta(fun(N,T)),
	write('Adding '), write(N), write(' to database.'), nl.


process(reduce(beta)) :-
	retractall(reduce(_)),
	asserta(reduce(beta)),
	write('Changing to beta reduction of leftmost redex.'), nl.

process(reduce(normal))   :-
	retractall(reduce(_)),
	asserta(reduce(normal)),
	write('Changing to normal reduction.'), nl.




% ===============


pretty_print(X) :-
    atomic(X),
    write(X).
pretty_print(l(V,T)) :-
    write('L'),write(V),write('.'),
    pretty_print(T).
pretty_print(a(T1,T2)) :-
    write('('), pretty_print(T1),
    write('  '), pretty_print(T2),
    write(')').



