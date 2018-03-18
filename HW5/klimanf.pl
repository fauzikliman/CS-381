% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.

% X = child
% Y = parent of X
child(X, Y) :- parent(Y, X).

% 2. Define two predicates `isMother/1` and `isFather/1`.

% X = person to check if a parent
isMother(X) :- parent(X, _), female(X).
isFather(X) :- parent(X, _), male(X).

% 3. Define a predicate `grandparent/2`.

% X = grandparent
% Y = grandchild of X
% Z = child of X and parent of Y
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.

% X = sibling of Y
% Y = sibling of X
% Z = parent of both X and Y
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.

% X = brother of Y
% Y = sister of X
brother(X, Y) :- sibling(X, Y), male(X).

% X = sister of Y
% Y = brother of X
sister(X, Y) :- sibling(X, Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.

% X = sibling of Z with spouse Y
% Y = spouse of Z with sibling X
% Z = sibling of X and spouse of Y
siblingInLaw(X, Y) :- sibling(X, Z), married(Z, Y).

% X = spouse of Z with sibling Y
% Y = sibling of Z with spouse X
% Z = spouse of Z and sibling of Y
siblingInLaw(X, Y) :- married(X, Z), sibling(Z, Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.

% X = aunt of Y, sibling/sibling-in-law of Z
% Y = niece/nephew of X, child of Z
% Z = parent of Y, sibling/sibling-in-law of X
aunt(X, Y) :- sibling(X, Z), parent(Z, Y), female(X).
aunt(X, Y) :- siblingInLaw(X, Z), parent(Z, Y), female(X).

% X = uncle of Y, sibling/sibling-in-law of Z
% Y = niece/nephew of X, child of Z
% Z = parent of Y, sibling/sibling-in-law of X
uncle(X, Y) :- sibling(X, Z), parent(Z, Y), male(X).
uncle(X, Y) :- siblingInLaw(X, Z), parent(Z, Y), male(X).

% 8. Define the predicate `cousin/2`.

% X = cousin of Y, child of Z
% Y = cousin of X, child of W
% Z = parent of X, sibling of W
% W = parent of Y, sibling of Z
cousin(X, Y) :- parent(Z, X), parent(W, Y), sibling(Z, W).

% 9. Define the predicate `ancestor/2`.

% X = ancestor of Y
% Y = descendant of X
% Z = descendant of X and ancestor of Y
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Extra credit: Define the predicate `related/2`.


%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

cmd(add, [A|[B|C]], S2) :- R is A + B, cmd(R, C, S2).
cmd(lte, [A|[B|C]], S2) :- A =< B, cmd(t, C, S2).
cmd(lte, [A|[B|C]], S2) :- A > B, cmd(f, C, S2).
cmd(if(P1, _), [A|B], S2) :- A = t, prog(P1, B, S2).
cmd(if(_, P2), [A|B], S2) :- A = f, prog(P2, B, S2).
cmd(X, S1, [X|S1]).

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

prog([P|PS], S1, S2) :- cmd(P, S1, S3), prog(PS, S3, S2).
prog([], S1, S1).
