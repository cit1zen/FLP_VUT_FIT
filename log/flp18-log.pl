%% Projekt: Rubikova kostka
%% Login: xorman00
%% Meno: Adam Ormandy

start :-
	read_lines(RAW),
	mergeLines(RAW, [], MERGED),
	filter_list(MERGED, FILTERED),
	%% SANITATION
	list_tuple(FILTERED, PREPARED),	
	assert(root(PREPARED)),
	assert(edge((0), PREPARED)),
	search,
	print_solution,
	halt.

print_solution :-
	edge(_,	
		(	X,X,X,
			X,X,X,
			X,X,X,
			A,A,A, B,B,B, C,C,C, D,D,D,
			A,A,A, B,B,B, C,C,C, D,D,D,
			A,A,A, B,B,B, C,C,C, D,D,D,
			Y,Y,Y,
			Y,Y,Y,
			Y,Y,Y
		)),
	write_path((
			X,X,X,
			X,X,X,
			X,X,X,
			A,A,A, B,B,B, C,C,C, D,D,D,
			A,A,A, B,B,B, C,C,C, D,D,D,
			A,A,A, B,B,B, C,C,C, D,D,D,
			Y,Y,Y,
			Y,Y,Y,
			Y,Y,Y
		)).

list_tuple([A,B|L], (A,R)) :- list_tuple([B|L], R).
list_tuple([A,B], (A,B)).

%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

mergeLines([],Acc,Acc).
mergeLines([H|T],Acc,Res) :-
	append(Acc,H,IntRes),
	mergeLines(T,IntRes,Res).

characters_i_hate(X) :-
    ' ' == X.

filter_list(In, Out) :-
    exclude(characters_i_hate, In, Out).


expand(X) :- 
	rotate_clock_1(X),
	rotate_counter_1(X),
	rotate_clock_2(X),
	rotate_counter_2(X),
	rotate_clock_3(X),
	rotate_counter_3(X),
	rotate_clock_4(X),
	rotate_counter_4(X),
	rotate_clock_5(X),
	rotate_counter_5(X),
	rotate_clock_6(X),
	rotate_counter_6(X),
	rotate_clock_x(X),
	rotate_counter_x(X),
	rotate_clock_y(X),
	rotate_counter_y(X),
	rotate_clock_z(X),
	rotate_counter_z(X).

search :- 
	forall(edge(_, X), expand(X)),
	(	not(found_solution)
	->	search
	;	true
	).

%% %% 43252003274489856000

found_solution :- 
	edge(_,	
		(	X,X,X,
			X,X,X,
			X,X,X,
			A,A,A, B,B,B, C,C,C, D,D,D,
			A,A,A, B,B,B, C,C,C, D,D,D,
			A,A,A, B,B,B, C,C,C, D,D,D,
			Y,Y,Y,
			Y,Y,Y,
			Y,Y,Y
		)).

write_path(X) :-
	(	edge(Y, X), not(root(X))
	-> 	write_path(Y),
		write_cube(X),
		write('\n')
	;	write_cube(X),
		write('\n')
	).

%% TODO
write_cube((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	) :-
	format("~a~a~a~n", [A5,B5,C5]),
	format("~a~a~a~n", [D5,E5,F5]),
	format("~a~a~a~n", [G5,H5,I5]),
	format("~a~a~a ~a~a~a ~a~a~a~n", [A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4]),
	format("~a~a~a ~a~a~a ~a~a~a~n", [D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4]),
	format("~a~a~a ~a~a~a ~a~a~a~n", [G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4]),
	format("~a~a~a~n", [A6,B6,C6]),
	format("~a~a~a~n", [D6,E6,F6]),
	format("~a~a~a~n", [G6,H6,I6]).


%% Nasleduje implementacia roznych rotacii kocky
%% Rotacia okolo stredu 1
rotate_clock_1((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		D5,E5,F5,
		I4,F4,C4,

		G1,D1,A1, G5,B2,C2, A3,B3,C3, A4,B4,A6,
		H1,E1,B1, H5,E2,F2, D3,E3,F3, D4,E4,B6, 
		I1,F1,C1, I5,H2,I2, G3,H3,I3, G4,H4,C6, 

		G2,D2,A2,
		D6,E6,F6,
		G6,H6,I6)
		)).

rotate_counter_1((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		D5,E5,F5,
		A2,D2,G2,

		C1,F1,I1, C6,B2,C2, A3,B3,C3, A4,B4,I5, 
		B1,E1,H1, B6,E2,F2, D3,E3,F3, D4,E4,H5, 
		A1,D1,G1, A6,H2,I2, G3,H3,I3, G4,H4,G5, 

		C4,F4,I4,
		D6,E6,F6,
		G6,H6,I6)
		)).


rotate_clock_2((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C1,
		D5,E5,F1,
		G5,H5,I1,

		A1,B1,C6, G2,D2,A2, I5,B3,C3, A4,B4,C4, 
		D1,E1,F6, H2,E2,B2, F5,E3,F3, D4,E4,F4, 
		G1,H1,I6, I2,F2,C2, C5,H3,I3, G4,H4,I4, 

		A6,B6,G3,
		D6,E6,D3,
		G6,H6,A3)
		)).

rotate_counter_2((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,G3,
		D5,E5,D3,
		G5,H5,A3,

		A1,B1,C5, C2,F2,I2, I6,B3,C3, A4,B4,C4, 
		D1,E1,F5, B2,E2,H2, F6,E3,F3, D4,E4,F4, 
		G1,H1,I5, A2,D2,G2, C6,H3,I3, G4,H4,I4, 

		A6,B6,C1,
		D6,E6,F1,
		G6,H6,I1)
		)).


rotate_clock_3((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		C2,F2,I2,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,I6, G3,D3,A3, C5,B4,C4, 
		D1,E1,F1, D2,E2,H6, H3,E3,B3, B5,E4,F4, 
		G1,H1,I1, G2,H2,G6, I3,F3,C3, A5,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G4,D4,A4)
		)).

rotate_counter_3((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		G4,D4,A4,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,A5, C3,F3,I3, G6,B4,C4, 
		D1,E1,F1, D2,E2,B5, B3,E3,H3, H6,E4,F4, 
		G1,H1,I1, G2,H2,C5, A3,D3,G3, I6,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		I2,F2,C2)
		)).

rotate_clock_4((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		I3,B5,C5,
		F3,E5,F5,
		C3,H5,I5,

		A5,B1,C1, A2,B2,C2, A3,B3,G6, G4,D4,A4, 
		D5,E1,F1, D2,E2,F2, D3,E3,D6, H4,E4,B4, 
		G5,H1,I1, G2,H2,I2, G3,H3,A6, I4,F4,C4, 

		A1,B6,C6,
		D1,E6,F6,
		G1,H6,I6)
		)).

rotate_counter_4((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A1,B5,C5,
		D1,E5,F5,
		G1,H5,I5,

		A6,B1,C1, A2,B2,C2, A3,B3,G5, C4,F4,I4, 
		D6,E1,F1, D2,E2,F2, D3,E3,D5, B4,E4,H4, 
		G6,H1,I1, G2,H2,I2, G3,H3,A5, A4,D4,G4, 

		I3,B6,C6,
		F3,E6,F6,
		C3,H6,I6)
		)).


rotate_clock_5((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		G5,D5,A5,
		H5,E5,B5,
		I5,F5,C5,

		A2,B2,C2, A3,B3,C3, A4,B4,C4, A1,B1,C1, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6)
		)).

rotate_counter_5((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		C5,F5,I5,
		B5,E5,H5,
		A5,D5,G5,

		A4,B4,C4, A1,B1,C1, A2,B2,C2, A3,B3,C3, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6)
		)).

rotate_clock_6((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G4,H4,I4, G1,H1,I1, G2,H2,I2, G3,H3,I3, 

		G6,D6,A6,
		H6,E6,B6,
		I6,F6,C6)
		)).

rotate_counter_6((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G2,H2,I2, G3,H3,I3, G4,H4,I4, G1,H1,I1, 

		G6,D6,A6,
		H6,E6,B6,
		I6,F6,C6)
		)).


rotate_clock_x((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D4,E4,F4, D1,E1,F1, D2,E2,F2, D3,E3,F3,
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6)
		)).

rotate_counter_x((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D2,E2,F2, D3,E3,F3, D4,E4,F4, D1,E1,F1,
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6)
		)).


rotate_clock_y((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B3,C5,
		D5,E3,F5,
		G5,H3,I5,

		A1,B5,C1, A2,B2,C2, A3,B6,C3, A4,B4,C4, 
		D1,E5,F1, D2,E2,F2, D3,E6,F3, D4,E4,F4, 
		G1,H5,I1, G2,H2,I2, G3,H6,I3, G4,H4,I4, 

		A6,B1,C6,
		D6,E1,F6,
		G6,H1,I6)
		)).

rotate_counter_y((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B1,C5,
		D5,E1,F5,
		G5,H1,I5,

		A1,B6,C1, A2,B2,C2, A3,B5,C3, A4,B4,C4, 
		D1,E6,F1, D2,E2,F2, D3,E5,F3, D4,E4,F4, 
		G1,H6,I1, G2,H2,I2, G3,H5,I3, G4,H4,I4, 

		A6,B3,C6,
		D6,E3,F6,
		G6,H3,I6)
		)).


rotate_clock_z((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		B2,E2,H2,
		G5,H5,I5,

		A1,B1,C1, A2,F6,C2, A3,B3,C3, A4,F5,C4, 
		D1,E1,F1, D2,E6,F2, D3,E3,F3, D4,E5,F4, 
		G1,H1,I1, G2,D6,I2, G3,H3,I3, G4,D5,I4, 

		A6,B6,C6,
		B4,E4,H4,
		G6,H6,I6)
		)).

rotate_counter_z((
	A5,B5,C5,
	D5,E5,F5,
	G5,H5,I5,

	A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
	D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
	G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

	A6,B6,C6,
	D6,E6,F6,
	G6,H6,I6)
	):-
	assert(edge((
		A5,B5,C5,
		D5,E5,F5,
		G5,H5,I5,

		A1,B1,C1, A2,B2,C2, A3,B3,C3, A4,B4,C4, 
		D1,E1,F1, D2,E2,F2, D3,E3,F3, D4,E4,F4, 
		G1,H1,I1, G2,H2,I2, G3,H3,I3, G4,H4,I4, 

		A6,B6,C6,
		D6,E6,F6,
		G6,H6,I6
		),
		(
		A5,B5,C5,
		B4,E4,H4,
		G5,H5,I5,

		A1,B1,C1, A2,F5,C2, A3,B3,C3, A4,F6,C4, 
		D1,E1,F1, D2,E5,F2, D3,E3,F3, D4,E6,F4, 
		G1,H1,I1, G2,D5,I2, G3,H3,I3, G4,D6,I4, 

		A6,B6,C6,
		B2,E2,H2,
		G6,H6,I6)
		)).