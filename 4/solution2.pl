count_valid_passwords(Low, High, Count) :- count_valid_passwords(Low, High, Count, _).
count_valid_passwords(Low, High, Count, Results) :-
  findall(Password, valid_password(Low, High, Password), PossiblePasswords),
  sort(PossiblePasswords, Results),
  length(Results, Count).
  
valid_password(Low, High, Value) :-
  between(Low, High, Value),
  number_codes(Value, Digits),
  has_exactly_two_adjacent_symbols(Digits),
  symbols_never_decrease(Digits).

has_exactly_two_adjacent_symbols([A, A, C, _, _, _]) :-
  dif(A, C).
has_exactly_two_adjacent_symbols([A, B, B, D, _, _]) :-
  dif(A, B),
  dif(B, D).
has_exactly_two_adjacent_symbols([_, B, C, C, E, _]) :-
  dif(B, C),
  dif(C, E).
has_exactly_two_adjacent_symbols([_, _, C, D, D, F]) :-
  dif(C, D),
  dif(D, F).
has_exactly_two_adjacent_symbols([_, _, _, D, E, E]) :-
  dif(D, E).

symbols_never_decrease([]).
symbols_never_decrease([_]).
symbols_never_decrease([H, N | T]) :-
  N >= H,
  symbols_never_decrease([N | T]).
