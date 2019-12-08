count_valid_passwords(Low, High, Count) :- count_valid_passwords(Low, High, Count, _).
count_valid_passwords(Low, High, Count, Results) :-
  findall(Password, valid_password(Low, High, Password), PossiblePasswords),
  sort(PossiblePasswords, Results),
  length(Results, Count).
  
valid_password(Low, High, Value) :-
  between(Low, High, Value),
  number_codes(Value, Digits),
  has_two_adjacent_symbols(Digits),
  symbols_never_decrease(Digits).

has_two_adjacent_symbols([H, H | _]).
has_two_adjacent_symbols([_ | T]) :-
  has_two_adjacent_symbols(T).

symbols_never_decrease([]).
symbols_never_decrease([_]).
symbols_never_decrease([H, N | T]) :-
  N >= H,
  symbols_never_decrease([N | T]).
