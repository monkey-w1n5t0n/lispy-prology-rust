#!/usr/bin/env swipl

% Token and AST representation
:- module(parser, [tokenize/2, parse_tokens/2]).

% Dynamic predicates for token stream
:- dynamic token/4.

% Tokenizer
tokenize(String, Tokens) :-
    string_chars(String, Chars),
    scan_tokens(Chars, 1, 1, [], Tokens), !.

scan_tokens([], _, _, Acc, Tokens) :- !,
    reverse(Acc, Tokens).
scan_tokens(Chars, Line, Col, Acc, Tokens) :-
    skip_whitespace(Chars, Line, Col, NewLine, NewCol, RestChars),
    (RestChars = [] ->
        reverse(Acc, Tokens)
    ;
        scan_token(RestChars, NewLine, NewCol, Token, AfterCol, RemainingChars),
        scan_tokens(RemainingChars, NewLine, AfterCol, [Token|Acc], Tokens)
    ).

% Skip whitespace and return next position
skip_whitespace([], Line, Col, Line, Col, []) :- !.
skip_whitespace([C|Cs], Line, Col, NewLine, NewCol, RestChars) :-
    (is_whitespace(C) ->
        next_pos(C, Line, Col, TempLine, TempCol),
        skip_whitespace(Cs, TempLine, TempCol, NewLine, NewCol, RestChars)
    ;
        NewLine = Line,
        NewCol = Col,
        RestChars = [C|Cs]
    ).

% Scan a single token
scan_token(['"'|Cs], Line, Col, Token, NewCol, RestChars) :- !,
    read_string(Cs, StringChars, RestChars),
    string_chars(String, StringChars),
    Token = token(string, String, Line, Col),
    string_length(String, Len),
    NewCol is Col + Len + 1.  % Only add 1 since we're at the closing quote position
scan_token(['('|Cs], Line, Col, token(lparen, '(', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
scan_token([')'|Cs], Line, Col, token(rparen, ')', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
scan_token(['['|Cs], Line, Col, token(lbracket, '[', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
scan_token([']'|Cs], Line, Col, token(rbracket, ']', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
scan_token([C|Cs], Line, Col, Token, NewCol, RestChars) :-
    char_type(C, digit), !,
    read_number([C|Cs], NumChars, RestChars),
    atom_chars(NumAtom, NumChars),
    atom_number(NumAtom, Num),
    Token = token(number, Num, Line, Col),
    length(NumChars, Len),
    NewCol is Col + Len.
scan_token([C|Cs], Line, Col, Token, NewCol, RestChars) :-
    (char_type(C, alpha) ; special_char(C)), !,
    read_word([C|Cs], WordChars, RestChars),
    atom_chars(Word, WordChars),
    (is_in_vector_context(Cs) -> Type = symbol ; is_atom(Word) -> Type = atom ; Type = symbol),
    Token = token(Type, Word, Line, Col),
    length(WordChars, Len),
    NewCol is Col + Len.

% Context detection
is_in_vector_context(Chars) :-
    skip_whitespace(Chars, 1, 1, _, _, CleanChars),
    (CleanChars = [']'|_] ;
     append(_, [']'|_], CleanChars)).

% Helper predicates
special_char('+').
special_char('-').
special_char('!').
special_char('_').

% Atom detection
is_atom(Word) :-
    atom_chars(Word, [C|_]),
    (char_type(C, lower) ;     % Lowercase start = atom
     member(C, ['+', '-'])).   % Operators are atoms

% Whitespace handling
is_whitespace(' ').
is_whitespace('\n').
is_whitespace('\t').

next_pos(' ', Line, Col, Line, NextCol) :- 
    NextCol is Col + 1, !.
next_pos('\n', Line, _, NextLine, 1) :- 
    NextLine is Line + 1, !.
next_pos('\t', Line, Col, Line, NextCol) :- 
    NextCol is Col + 8, !.  % Standard tab width

% Reading helpers
read_number([], [], []).
read_number([C|Cs], [C|NumChars], Rest) :-
    char_type(C, digit), !,
    read_number(Cs, NumChars, Rest).
read_number(Rest, [], Rest).

read_string([], [], []) :- !.
read_string(['"'|Rest], [], Rest) :- !.
read_string([C|Cs], [C|Chars], Rest) :-
    read_string(Cs, Chars, Rest).

read_word([], [], []).
read_word([C|Cs], [C|WordChars], Rest) :-
    (char_type(C, alpha) ; char_type(C, digit) ; special_char(C)), !,
    read_word(Cs, WordChars, Rest).
read_word(Rest, [], Rest).

% Parser
parse_tokens([], []).
parse_tokens([Token|Rest], AST) :-
    parse_expr([Token|Rest], AST, []), !.
parse_expr([], _, _) :- fail.
parse_expr([token(lparen, _, Line, Col)|Rest], sexpr(Elements, Line, Col), Remaining) :-
    parse_list_elements(Rest, Elements, Remaining).
parse_expr([token(lbracket, _, Line, Col)|Rest], vector(Elements, Line, Col), Remaining) :-
    parse_vector_elements(Rest, Elements, Remaining).
parse_expr([token(atom, Value, Line, Col)|Rest], atom(Value, Line, Col), Rest).
parse_expr([token(symbol, Value, Line, Col)|Rest], symbol(Value, Line, Col), Rest).
parse_expr([token(number, Value, Line, Col)|Rest], number(Value, Line, Col), Rest).
parse_expr([token(string, Value, Line, Col)|Rest], string(Value, Line, Col), Rest).

parse_list_elements([token(rparen, _, _, _)|Rest], [], Rest) :- !.
parse_list_elements(Tokens, [Element|Elements], Remaining) :-
    parse_expr(Tokens, Element, AfterElement),
    parse_list_elements(AfterElement, Elements, Remaining).
parse_list_elements(_, _, _) :-
    throw(error(syntax_error("Unmatched parenthesis"), _)).

parse_vector_elements([token(rbracket, _, _, _)|Rest], [], Rest) :- !.
parse_vector_elements(Tokens, [Element|Elements], Remaining) :-
    parse_expr(Tokens, Element, AfterElement),
    parse_vector_elements(AfterElement, Elements, Remaining).
parse_vector_elements(_, _, _) :-
    throw(error(syntax_error("Unmatched bracket"), _)).