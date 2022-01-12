%----Propositional
cnf(propositional,axiom,
    ( p0
    | ~ q0
    | r0
    | ~ s0 )).

%----First-order
cnf(first_order,axiom,
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

%----Equality
cnf(equality,axiom,
    ( f(Y) = g(X,f(Y),Z)
    | f(f(f(b))) != a
    | X = f(Y) )).

%----True and false
cnf(true_false,axiom,
    ( $true
    | $false )).

%----Quoted symbols
cnf(single_quoted,axiom,
    ( 'A proposition'
    | 'A predicate'(Y)
    | p('A constant')
    | p('A function'(a))
    | p('A \'quoted \\ escape\'') )).

%----Connectives - seen them all already

%----Annotated formula names
cnf(123,axiom,
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

%----Roles - seen axiom already
cnf(role_hypothesis,hypothesis,
    p(h)).

cnf(role_negated_conjecture,negated_conjecture,
    ~ p(X)).

%----Include directive
include('Axioms/SYN000-0.ax').

%----Comments
/* This
   is a block
   comment.
*/
