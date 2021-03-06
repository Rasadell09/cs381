% Name: Yunfan Li
% Class: CS381-001
% Onid: liyunf@onid.oregonstate.edu
% OSU ID: 932530195

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
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_).
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X),parent(Z,Y), X\=Y.

% 5. Define two predicates `brother/2` and `sister/2`.
sister(X,Y) :- female(X), parent(Z,X), parent(Z,Y), X\=Y.
brother(X,Y) :- male(X), parent(Z,X), parent(Z,Y), X\=Y.

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X), sibling(X,W), child(Y,W).
aunt(X,Y) :- female(X), married(X,W), sibling(W,Z), child(Y,Z).
uncle(X,Y) :- male(X), sibling(X,W), child(Y,W).
uncle(X,Y) :- male(X), married(X,W), sibling(W,Z), child(Y,Z).
    
% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(Z,X), sibling(Z,W), parent(W,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y); parent(X,Z), parent(Z,Y).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. List manipulation
%%

% 10. Define a predicate `rdup(L,M)` that removes adjacent duplicate elements
%     from the list `L`. The resulting list should be bound to `M`. It's OK if
%     this function loops on queries where `L` is not provided.
rdup([],[_]).
rdup([X|T],[Y|R]) :- X=Y, rdup(T,[Y|R]).
rdup([X|T],[Y|R]) :- not(X=Y), rdup([X|T],R).
