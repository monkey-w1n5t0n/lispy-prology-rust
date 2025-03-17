# Risper-to-Rust Transpiler: Prolog Implementation Plan

## 1. Overall Architecture

```
┌─────────────┐     ┌───────────┐     ┌──────────────┐     ┌───────────────┐     ┌──────────────┐
│  Tokenizer  │────▶│  Parser   │────▶│     AST      │────▶│  Transformer  │────▶│ Code Generator│
│  (lexing)   │     │  (syntax) │     │ Representation│     │  (semantics)  │     │  (rust code) │
└─────────────┘     └───────────┘     └──────────────┘     └───────────────┘     └──────────────┘
```

## 2. Tokenization and Parsing

### 2.1. Token Representation

```prolog
% Token types
% token(Type, Value, Line, Column)
:- dynamic token/4.

% Example tokens
% token(lparen, '(', 1, 1).
% token(atom, 'defn', 1, 2).
% token(symbol, 'add', 1, 7).
% token(rparen, ')', 1, 10).
```

### 2.2. Tokenizer Implementation

```prolog
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

% Special token handlers
scan_token(['"'|Cs], Line, Col, Token, NewCol, RestChars) :- !,
    read_string(Cs, StringChars, RestChars),
    string_chars(String, StringChars),
    Token = token(string, String, Line, Col),
    string_length(String, Len),
    NewCol is Col + Len + 2.  % Include quotes in length calculation
    
scan_token(['('|Cs], Line, Col, token(lparen, '(', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
    
scan_token([')'|Cs], Line, Col, token(rparen, ')', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
    
scan_token(['['|Cs], Line, Col, token(lbracket, '[', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
    
scan_token([']'|Cs], Line, Col, token(rbracket, ']', Line, Col), NewCol, Cs) :- !,
    NewCol is Col + 1.
    
% Numbers and identifiers
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

% Context detection and helper predicates
is_in_vector_context(Chars) :-
    skip_whitespace(Chars, 1, 1, _, _, CleanChars),
    (CleanChars = [']'|_] ; append(_, [']'|_], CleanChars)).

is_atom(Word) :-
    atom_chars(Word, [C|_]),
    (char_type(C, lower) ; member(C, ['+', '-'])).  % Lowercase or operators
```

### 2.3. S-expression Parser

```prolog
parse_tokens(Tokens, AST) :-
    parse_sexpr(Tokens, AST, []).

parse_sexpr([token(lparen, _, Line, Col)|Rest], SExpr, Remaining) :-
    parse_list_elements(Rest, Elements, AfterList),
    SExpr = sexpr(Elements, Line, Col),
    Remaining = AfterList.
parse_sexpr([token(atom, Value, Line, Col)|Rest], atom(Value, Line, Col), Rest).
parse_sexpr([token(symbol, Value, Line, Col)|Rest], symbol(Value, Line, Col), Rest).
parse_sexpr([token(number, Value, Line, Col)|Rest], number(Value, Line, Col), Rest).
% Additional parsing rules for strings, keywords, etc.

parse_list_elements([token(rparen, _, _, _)|Rest], [], Rest) :- !.
parse_list_elements(Tokens, [Element|Elements], Remaining) :-
    parse_sexpr(Tokens, Element, AfterElement),
    parse_list_elements(AfterElement, Elements, Remaining).
```

## 3. AST Representation

```prolog
% AST node types
% let_binding(Name, Value, Mutable)
% function_def(Name, Params, ReturnType, Body)
% function_call(Name, Args)
% struct_def(Name, Fields)
% enum_def(Name, Variants)
% trait_def(Name, Methods)
% impl_block(Trait, Type, Methods)
% etc.
```

## 4. Semantic Analysis

### 4.1. Type Analysis

```prolog
% Define type rules
% infer_type(AST, Context, Type)
infer_type(number(Value, _, _), _, 'i32').
infer_type(string_lit(Value, _, _), _, 'String').
infer_type(symbol(Name, _, _), Context, Type) :-
    lookup_variable(Context, Name, Type).
infer_type(sexpr([symbol('+', _, _), Left, Right], _, _), Context, 'i32') :-
    infer_type(Left, Context, LeftType),
    infer_type(Right, Context, RightType),
    compatible_types(LeftType, RightType, 'i32').
% Add more type inference rules
```

### 4.2. Borrow Checking

```prolog
% Track ownership and references
% check_borrows(AST, OwnershipContext, NewContext, Errors)
check_borrows(sexpr([symbol('let', _, _), symbol(Name, _, _), Value], _, _), Context, NewContext, Errors) :-
    infer_type(Value, Context, Type),
    check_borrows(Value, Context, TempContext, Errors),
    add_ownership(TempContext, Name, Type, NewContext).

check_borrows(sexpr([symbol('&', _, _), symbol(Name, _, _)], _, _), Context, Context, Errors) :-
    is_borrowed(Context, Name, Errors).
% Additional borrow-checking rules...
```

## 5. Transformation Rules

### 5.1. Variable Bindings

```prolog
transform(sexpr([symbol('let', _, _), symbol(Name, _, _), Value], _, _), RustCode) :-
    transform(Value, ValueCode),
    format_to_string(RustCode, "let ~w = ~w;", [Name, ValueCode]).

transform(sexpr([symbol('let', _, _), symbol('mut', _, _), symbol(Name, _, _), Value], _, _), RustCode) :-
    transform(Value, ValueCode),
    format_to_string(RustCode, "let mut ~w = ~w;", [Name, ValueCode]).

transform(sexpr([symbol('set!', _, _), symbol(Name, _, _), Value], _, _), RustCode) :-
    transform(Value, ValueCode),
    format_to_string(RustCode, "~w = ~w;", [Name, ValueCode]).
```

### 5.2. Function Definitions

```prolog
transform(sexpr([symbol('defn', _, _), symbol(Name, _, _), ParamList, ReturnType, Body], _, _), RustCode) :-
    transform_params(ParamList, ParamsCode),
    transform_type(ReturnType, RetTypeCode),
    transform(Body, BodyCode),
    format_to_string(RustCode, "fn ~w(~w) -> ~w {\n    ~w\n}", [Name, ParamsCode, RetTypeCode, BodyCode]).

transform_params(sexpr([Param1, Param2|Rest], _, _), ParamsCode) :-
    transform_params(sexpr([Param1], _, _), Param1Code),
    transform_params(sexpr([Param2|Rest], _, _), RestCode),
    format_to_string(ParamsCode, "~w, ~w", [Param1Code, RestCode]).
% Additional function parameter transformation rules...
```

### 5.3. Control Flow

```prolog
transform(sexpr([symbol('if', _, _), Condition, ThenExpr, ElseExpr], _, _), RustCode) :-
    transform(Condition, CondCode),
    transform(ThenExpr, ThenCode),
    transform(ElseExpr, ElseCode),
    format_to_string(RustCode, "if ~w {\n    ~w\n} else {\n    ~w\n}", [CondCode, ThenCode, ElseCode]).

transform(sexpr([symbol('while', _, _), Condition, Body], _, _), RustCode) :-
    transform(Condition, CondCode),
    transform(Body, BodyCode),
    format_to_string(RustCode, "while ~w {\n    ~w\n}", [CondCode, BodyCode]).
```

### 5.4. Data Structures

```prolog
transform(sexpr([symbol('defstruct', _, _), symbol(Name, _, _) | Fields], _, _), RustCode) :-
    transform_struct_fields(Fields, FieldsCode),
    format_to_string(RustCode, "struct ~w {\n~w\n}", [Name, FieldsCode]).

transform_struct_fields([sexpr([symbol(FieldName, _, _), Type], _, _)|Rest], FieldsCode) :-
    transform_type(Type, TypeCode),
    transform_struct_fields(Rest, RestCode),
    format_to_string(FieldsCode, "    ~w: ~w,\n~w", [FieldName, TypeCode, RestCode]).
transform_struct_fields([], "").
```

## 6. Code Generation

```prolog
generate_rust_code(AST, RustCode) :-
    transform(AST, TransformedCode),
    format_code(TransformedCode, RustCode).

% Format the code with proper indentation
format_code(Code, FormattedCode) :-
    % Apply formatting rules
    % ...
    FormattedCode = Code.  % Simplified for now
```

## 7. Error Handling

```prolog
% Error representation
% error(Type, Message, Line, Column)

validate_ast(AST, Errors) :-
    check_undefined_variables(AST, [], E1),
    check_type_errors(AST, [], E2),
    check_borrow_violations(AST, [], E3),
    append(E1, E2, TempErrors),
    append(TempErrors, E3, Errors).
```

## 8. Full Pipeline

```prolog
risper_to_rust(RisperCode, RustCode) :-
    tokenize(RisperCode, Tokens),
    parse_tokens(Tokens, AST),
    validate_ast(AST, Errors),
    (Errors = [] ->
        generate_rust_code(AST, RustCode)
    ;
        format_errors(Errors, ErrorMsg),
        throw(error(compilation_error, ErrorMsg))
    ).
```

## 9. Testing Strategy

### 9.1. Unit Testing

```prolog
:- begin_tests(tokenizer).
test(tokenize_let) :-
    tokenize("(let x 5)", Tokens),
    Tokens = [
        token(lparen, '(', 1, 1),
        token(atom, 'let', 1, 2),
        token(symbol, 'x', 1, 6),
        token(number, 5, 1, 8),
        token(rparen, ')', 1, 9)
    ].
:- end_tests(tokenizer).

:- begin_tests(parser).
test(parse_let) :-
    parse_tokens([
        token(lparen, '(', 1, 1),
        token(atom, 'let', 1, 2),
        token(symbol, 'x', 1, 6),
        token(number, 5, 1, 8),
        token(rparen, ')', 1, 9)
    ], AST),
    AST = sexpr([symbol('let', 1, 2), symbol('x', 1, 6), number(5, 1, 8)], 1, 1).
:- end_tests(parser).
```

### 9.2. Integration Testing

```prolog
:- begin_tests(transpiler).
test(simple_let) :-
    risper_to_rust("(let x 5)", RustCode),
    RustCode = "let x = 5;".

test(simple_function) :-
    risper_to_rust("(defn add [x:i32 y:i32] -> i32 (+ x y))", RustCode),
    RustCode = "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}".
:- end_tests(transpiler).
```

## 10. Implementation Plan

1. **Phase 1**: Implement tokenizer and parser
2. **Phase 2**: Implement basic transformations for variables and expressions
3. **Phase 3**: Add support for functions, control flow
4. **Phase 4**: Implement data structures (structs, enums)
5. **Phase 5**: Add ownership and borrowing semantics
6. **Phase 6**: Support for traits and implementations
7. **Phase 7**: Handle advanced features (macros, closures)
8. **Phase 8**: Optimization and testing

## 11. Sample Usage

```prolog
% Sample usage
main :-
    read_file('sample.risper', RisperCode),
    risper_to_rust(RisperCode, RustCode),
    write_file('output.rs', RustCode).

% Example command line interface
:- initialization(main, main).
```

## 12. Bugs and Their Solutions

### 12.1. Current Failed Tests

Based on the test failures, we have identified three issues in the tokenizer implementation:

1. **tokenizer:tokenize_nested_list_structure**
   - **Issue**: Incorrect identification of atom vs symbol tokens within nested lists
   - **Expected vs Actual**:
     ```
     Expected: token(atom,defn,1,2), token(atom,add,1,7)
     Got: token(symbol,defn,1,2), token(symbol,add,1,7)
     ```
     And:
     ```
     Expected: token(symbol,x,1,20), token(symbol,y,1,22) 
     Got: token(atom,x,1,20), token(atom,y,1,22)
     ```
   - **Fix Plan**: 
     1. Update the `is_atom` predicate to better identify atoms in different contexts
     2. Improve the context detection to consider parent expressions when determining token types
     3. Add special handling for identifiers within function parameter lists

2. **tokenizer:tokenize_string_in_list**
   - **Issue**: Incorrect position calculation for closing parenthesis after a string
   - **Expected vs Actual**:
     ```
     Expected: token(rparen,),1,25)
     Got: token(rparen,),1,26)
     ```
   - **Fix Plan**:
     1. Fix the string length calculation in `read_string` to correctly account for quotes
     2. Adjust the position tracking in `scan_token` for string tokens

3. **tokenizer:tokenize_position_around_brackets**
   - **Issue**: Incorrect identification of "a" as a symbol instead of an atom
   - **Expected vs Actual**:
     ```
     Expected: token(atom,a,1,1)
     Got: token(symbol,a,1,1)
     ```
   - **Fix Plan**:
     1. Refine rules for atom vs symbol identification based on context
     2. Add special handling for single-character identifiers
     3. Consider the position relative to brackets when determining token types

### 12.2. Implementation Strategy

1. **Enhanced Context Tracking**:
   ```prolog
   % Add contextual information during tokenization
   scan_tokens(Chars, Line, Col, Context, Acc, Tokens) :-
       % Track parent contexts (list, vector, etc.)
       % Use this to inform atom/symbol decisions
   ```

2. **Improved Type Detection Rules**:
   ```prolog
   determine_token_type(Word, Context, Type) :-
       % More sophisticated rules based on:
       % - Position within expression
       % - First character properties
       % - Parent context type
       % - Special cases for common atoms/symbols
   ```

3. **Position Calculation Fix**:
   ```prolog
   % Fix string length calculation
   scan_token(['"'|Cs], Line, Col, Token, NewCol, RestChars) :- !,
       read_string(Cs, StringChars, RestChars),
       string_chars(String, StringChars),
       Token = token(string, String, Line, Col),
       string_length(String, Len),
       NewCol is Col + Len + 2.  % +2 for the quotes
   ```