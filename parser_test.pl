#!/usr/bin/env swipl

:- use_module(parser).

:- begin_tests(tokenizer).

% Debug predicate
debug_tokens(Expected, Got) :-
    writeln('Expected tokens:'),
    maplist(writeln, Expected),
    writeln('Got tokens:'),
    maplist(writeln, Got).

test(tokenize_empty) :-
    tokenize("", Got),
    debug_tokens([], Got),
    Got = [].

test(tokenize_single_atom) :-
    tokenize("hello", Got),
    debug_tokens([token(atom, hello, 1, 1)], Got),
    Got = [token(atom, hello, 1, 1)].

test(tokenize_number) :-
    tokenize("42", Got),
    debug_tokens([token(number, 42, 1, 1)], Got),
    Got = [token(number, 42, 1, 1)].

% Break down simple list test into smaller parts
test(tokenize_lparen) :-
    tokenize("(", Got),
    debug_tokens([token(lparen, '(', 1, 1)], Got),
    Got = [token(lparen, '(', 1, 1)].

% Add new granular list structure tests
test(tokenize_single_element_list) :-
    tokenize("(x)", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, x, 1, 2),
        token(rparen, ')', 1, 3)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_two_element_list) :-
    tokenize("(x y)", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, x, 1, 2),
        token(atom, y, 1, 4),
        token(rparen, ')', 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_list_with_number) :-
    tokenize("(+ 1)", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, '+', 1, 2),
        token(number, 1, 1, 4),
        token(rparen, ')', 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_rparen) :-
    tokenize(")", Got),
    debug_tokens([token(rparen, ')', 1, 1)], Got),
    Got = [token(rparen, ')', 1, 1)].

test(tokenize_operator) :-
    tokenize("+", Got),
    debug_tokens([token(atom, '+', 1, 1)], Got),
    Got = [token(atom, '+', 1, 1)].

test(tokenize_simple_list_structure) :-
    tokenize("(+ 1 2)", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, '+', 1, 2),
        token(number, 1, 1, 4),
        token(number, 2, 1, 6),
        token(rparen, ')', 1, 7)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

% Break down nested list tests
test(tokenize_brackets) :-
    tokenize("[]", Got),
    Expected = [
        token(lbracket, '[', 1, 1),
        token(rbracket, ']', 1, 2)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_symbol_in_brackets) :-
    tokenize("[x]", Got),
    Expected = [
        token(lbracket, '[', 1, 1),
        token(symbol, x, 1, 2),
        token(rbracket, ']', 1, 3)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_nested_list_structure) :-
    tokenize("(defn add [x y] (+ x y))", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, defn, 1, 2),
        token(atom, add, 1, 7),
        token(lbracket, '[', 1, 11),
        token(symbol, x, 1, 12),
        token(symbol, y, 1, 14),
        token(rbracket, ']', 1, 15),
        token(lparen, '(', 1, 17),
        token(atom, '+', 1, 18),
        token(symbol, x, 1, 20),
        token(symbol, y, 1, 22),
        token(rparen, ')', 1, 23),
        token(rparen, ')', 1, 24)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

% Break down string tests
test(tokenize_empty_string) :-
    tokenize("\"\"", Got),
    Expected = [token(string, "", 1, 1)],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_simple_string) :-
    tokenize("\"Hello\"", Got),
    Expected = [token(string, "Hello", 1, 1)],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_string_in_list) :-
    tokenize("(println! \"Hello, world!\")", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, 'println!', 1, 2),
        token(string, "Hello, world!", 1, 11),
        token(rparen, ')', 1, 25)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

% Add new whitespace and position tests after debug_tokens predicate
test(tokenize_whitespace) :-
    tokenize("   ", Got),
    debug_tokens([], Got),
    Got = [].

test(tokenize_multiple_spaces_between_atoms) :-
    tokenize("foo   bar", Got),
    Expected = [
        token(atom, foo, 1, 1),
        token(atom, bar, 1, 7)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_after_whitespace) :-
    tokenize("  foo", Got),
    Expected = [token(atom, foo, 1, 3)],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_after_atom) :-
    tokenize("foo bar", Got),
    Expected = [
        token(atom, foo, 1, 1),
        token(atom, bar, 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_after_number) :-
    tokenize("42 foo", Got),
    Expected = [
        token(number, 42, 1, 1),
        token(atom, foo, 1, 4)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_in_sequence) :-
    tokenize("a b c", Got),
    Expected = [
        token(atom, a, 1, 1),
        token(atom, b, 1, 3),
        token(atom, c, 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_after_lparen) :-
    tokenize("(a", Got),
    Expected = [
        token(lparen, '(', 1, 1),
        token(atom, a, 1, 2)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_before_rparen) :-
    tokenize("a)", Got),
    Expected = [
        token(atom, a, 1, 1),
        token(rparen, ')', 1, 2)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_around_brackets) :-
    tokenize("a[b]c", Got),
    Expected = [
        token(atom, a, 1, 1),
        token(lbracket, '[', 1, 2),
        token(symbol, b, 1, 3),
        token(rbracket, ']', 1, 4),
        token(atom, c, 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_in_multiword_string) :-
    tokenize("\"foo bar\"", Got),
    Expected = [token(string, "foo bar", 1, 1)],
    debug_tokens(Expected, Got),
    Got = Expected.

% Add position tracking specific tests
test(tokenize_position_increment_after_atom) :-
    tokenize("ab", Got),
    Expected = [token(atom, ab, 1, 1)],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_multichar_atom_position) :-
    tokenize("foo bar", Got),
    Expected = [
        token(atom, foo, 1, 1),
        token(atom, bar, 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_position_after_number_length) :-
    tokenize("123 x", Got),
    Expected = [
        token(number, 123, 1, 1),
        token(atom, x, 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_simple_vector) :-
    tokenize("[x]", Got),
    Expected = [
        token(lbracket, '[', 1, 1),
        token(symbol, x, 1, 2),
        token(rbracket, ']', 1, 3)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

test(tokenize_vector_with_spaces) :-
    tokenize("[ x ]", Got),
    Expected = [
        token(lbracket, '[', 1, 1),
        token(symbol, x, 1, 3),
        token(rbracket, ']', 1, 5)
    ],
    debug_tokens(Expected, Got),
    Got = Expected.

:- end_tests(tokenizer).

:- begin_tests(parser).

test(parse_empty) :-
    parse_tokens([], []).

test(parse_single_atom) :-
    parse_tokens([token(atom, hello, 1, 1)], AST),
    AST = atom(hello, 1, 1).

test(parse_number) :-
    parse_tokens([token(number, 42, 1, 1)], AST),
    AST = number(42, 1, 1).

test(parse_simple_list) :-
    parse_tokens([
        token(lparen, '(', 1, 1),
        token(atom, '+', 1, 2),
        token(number, 1, 1, 4),
        token(number, 2, 1, 6),
        token(rparen, ')', 1, 7)
    ], AST),
    AST = sexpr([atom('+', 1, 2), number(1, 1, 4), number(2, 1, 6)], 1, 1).

test(parse_nested_list) :-
    parse_tokens([
        token(lparen, '(', 1, 1),
        token(atom, defn, 1, 2),
        token(atom, add, 1, 7),
        token(lbracket, '[', 1, 11),
        token(symbol, x, 1, 12),
        token(symbol, y, 1, 14),
        token(rbracket, ']', 1, 15),
        token(lparen, '(', 1, 17),
        token(atom, '+', 1, 18),
        token(symbol, x, 1, 20),
        token(symbol, y, 1, 22),
        token(rparen, ')', 1, 23),
        token(rparen, ')', 1, 24)
    ], AST),
    AST = sexpr([
        atom(defn, 1, 2),
        atom(add, 1, 7),
        vector([symbol(x, 1, 12), symbol(y, 1, 14)], 1, 11),
        sexpr([atom('+', 1, 18), symbol(x, 1, 20), symbol(y, 1, 22)], 1, 17)
    ], 1, 1), !.

test(parse_string) :-
    parse_tokens([
        token(lparen, '(', 1, 1),
        token(atom, 'println!', 1, 2),
        token(string, "Hello, world!", 1, 11),
        token(rparen, ')', 1, 25)
    ], AST),
    AST = sexpr([atom('println!', 1, 2), string("Hello, world!", 1, 11)], 1, 1), !.

% Error cases
test(parse_unmatched_paren, [throws(error(syntax_error("Unmatched parenthesis"), _))]) :-
    parse_tokens([token(lparen, '(', 1, 1)], _).

test(parse_unmatched_bracket, [throws(error(syntax_error("Unmatched bracket"), _))]) :-
    parse_tokens([token(lbracket, '[', 1, 1)], _).

:- end_tests(parser).

% Main test runner
:- initialization((run_tests, halt), main).