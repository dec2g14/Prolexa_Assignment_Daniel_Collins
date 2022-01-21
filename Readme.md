[![Read Assignment Description](https://img.shields.io/badge/assignment-description-blue)](assignment.md)  

# COMSM0022 Assignment: Extending the Reasoning Capabilities of Prolexa Q&A Assistant #
## Introduction ##
This repository contains my modifications to the Prolog code for the [Prolexa question-answering assistant](https://github.com/simply-logical/ComputationalLogic), to add functionality for reasoning with logical negation.

The modified Prolog code is found within the following files: 
-`prolexa/prolog/prolexa.pl`, 
-`prolexa/prolog/prolexa_grammar.pl`
-`prolexa/prolog/prolexa_engine.pl`. 

The top-level module is `prolexa/prolog/prolexa.pl`, which should be run in the command line. 

This work does not integrate with Prolexa-plus. 


---
## Method ##
Before adding any new code, I spent time learning how the Prolexa system handles different queries using the [graphical debugger built into SWI Prolog](https://www.swi-prolog.org/pldoc/doc_for?object=section(2,%273.5%27,swi(%27/doc/Manual/guitracer.html%27)).

I set a ["spy point"](https://www.swi-prolog.org/pldoc/man?predicate=spy/1) on the predicate 'handle_utterance' in the top-level code. This is effectively the starting point of the system, from which Prolexa determines how to deal with the users input.

This allowed me to trace each step of the computation when querying a specific goal (e.g. "is peter human"), or finding all stored rules ("tell me everything"), to best determine which parts of the system I needed to change to extend the reasoning capabilities.

---
## Adding Logical Negation ##

I wanted to extend Prolexa to be able to handle reasoning patterns of the following form, as described in the [Assignment Description](assignment.md):

> Every teacher is happy. 
> Donald is not happy. 
> Therefore, Donald is not a teacher.

Using the existing functionality, I first added a similar rules “every teacher is happy” and "donald is happy", as well aspredicates for the words “teacher”, “happy” and “donald”:

```
%% Stored rule added to prolexa.pl
% New stored rule for "every teacher is happy".
stored_rule(1,[(happy(X):-teacher(X))]).
% New stored rule for "donald is happy".
stored_rule(1,[(teacher(donald):-true)]).


%% Grammar updates added to prolexa_grammar.pl
% New predicates for the adjective "happy" and noun "teacher".
pred(happy, 1, [a/happy]). 
pred(teacher, 1, [n/teacher]). 

% New proper_noun terminal for "donald"
proper_noun(s,donald) --> [donald].
```

I then traced the query "explain why donald is happy" to see how it would be processed. The output of the system in this case is a response showing logical deduction of an undefined rule based on the stored rules:

> Every teacher is happy
> Donald is a teacher
> Therefore, Donald is happy

By looking at the graphical tracer, I could see specifically which grammar rules I would need to update for negation. Further, this highlighted that logical deduction was achieved using the predicate "prove_rb", which would therefore need to be extended to be able to deduce negated rules.  

---
## New Rules and Grammar ##
The built in meta-predicate “not” in Prolog (”\+”) implements negation as failure, where "not(X)" fails (is False) if X is provable true, and succeeds (is True) otherwise. Here, “not/1” simply means that the goal specified by the argument is not provable. This could be interpreted as “if we cannot prove X is True from the knowledge base, then X must be False”. 

This is not the same as logical negation, in which "not(X)" would mean "the opposite of X". This is the desired form of negation for the reasoning task. Logical negation should give a rule such as  "X:- not(not(Y))" the same meaning as "X:- Y", however; under negation by failure,  "X:- not(not(Y))" would be interpreted as “X is true if it can’t be proved that it can’t be proved that Y is true”.  

Whilst the built-in "not" meta-predicate can be used to acheive negation in some cases, it will not always give the intended result, depending on whether the argument contains variables, and the ordering of literals in a clause. Further, the meta-predicate uses a procedural method that prevents the semantic analysis needed for Prolexa to be able to interpret and describe it's reasoning.

To implement logical negation in Prolexa, I have added the user defined prefix operator from [Simply Logical Chapter 8.1](https://too.simply-logical.space/src/text/3_part_iii/8.1.html), where it has been used to handle negation in exception cases for default rules:

```prolog
:-op(900,fy,not).
```
This operator replaces the built-in meta-predicate not/1 such that “not” is now an arbitrary functor, where “not y” is equivalent to "not(y)",. This operator allows associativity such that “not not y” or “not(not(y))” can be used to apply the functor twice. I have elected to continue writing negation statements in closed brackets in the code, however this is a personal preference for legibility.

This operator was used to formulate a rule corresponding to the meaning of "donald is not happy": 

```prolog
% New negative store rule: "Donald is not happy"
stored_rule(1,[(not(happy(donald)):-true)]). 
```

The operator does not specify negation by definition, and this rule can only interpreted as intended by adding a new set of grammar rules to handle negative verb phrases containing “not” separately from normal verb phrases, and corresponding sentence structure rules for the negative verb phrases. I included new rules adapter from each of the existing grammar rules, within which the negated property is wrapped by the "not" infix operator, and negative verb meanings are mapped to negative meanings of sentences:


```prolog
% New negative store rule: "Donald is not happy"
stored_rule(1,[(not(happy(donald)):-true)]). 

% Normal verb phrase structure i.e. something "is mortal". 
verb_phrase(s,M) --> [is],property(s,M).
% Interpret ["not"] as a separate verb phrase structure, mapping meaning M to not(M).
verb_phrase(s,not(M)) --> [is],[not],property(s,M).
verb_phrase(s,not(M)) --> [not],property(s,M). 

% Normal sentence structure i.e. for "Peter is mortal".
sentence1([(L:-true)]) --> proper_noun(N,X),verb_phrase(N,X=>L).
% Apply "not" opporator to the left-hand side of the predicate
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).

```

I then added a new question rule for every existing question rule, to handle queries which ask if the opposite of something is true:

```prolog
% Standard: e.g. "who is mortal"
question1(Q) --> [who],verb_phrase(s,_X=>Q).
% Negated: e.g. "who is not mortal"
question1(not(Q)) --> [who],verb_phrase(s,not(_X=>Q))
% Standard: e.g. "is peter mortal"
question1(Q) --> [is], proper_noun(N,X),property(N,X=>Q).
% Negated: e.g. "is peter not mortal"
question1(not(Q)) --> [is],proper_noun(N,X),verb_phrase(N,not(X=>Q)).
% Standard: e.g. "does tweety fly".
question1(Q) --> [does],proper_noun(_,X),verb_phrase(_,X=>Q).
% Negated: e.g. "does tweety not fly".
question1(not(Q)) --> [does],proper_noun(_,X),verb_phrase(_,not(X=>Q)).
```

At this stage, if I input the utterance “is donald not happy”, Prolexa is able to tell me “donald is not happy”, but if I ask “is donald a teacher”, Prolexa will tell me  “Sorry, I don't think this is the case”, indicating that it could not be proven that donald is a teacher. To have fully explainable negation, the “prove_rb” part of the meta-interpreter must also be extended:


```prolog
% Standard 
% e.g. for "explain why peter is mortal"
% We have the rule: "mortal(X):- human(X)", where "human(peter):-True" corresponds to the body B.
% We want to use these to prove the rule corresponding to the head A: "mortal(peter):-True". 

prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
		prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

% Negation
% e.g. for explain why "donald is not a teacher" 
% We have the rule: "happy(X):- teacher(X)", where "not(happy(donald)):-True" corresponds to the negation of the head A.
% We want to use these to prove the rule corresponding to the negation of the body B: "not(teacher(donald)):-True". 

prove_rb(not(B),Rulebase,P0,P):- 
  	find_clause((A:-B),Rule,Rulebase), 
	prove_rb(not(A),Rulebase,[p(not(B),Rule)|P0],P). 
```

Now, the input utterance “tell me everything” generates a response indicating the following stored rules:
- “every teacher is happy”
- “Donald is not happy”

For the input “is donald a teacher”, Prolexa can deduce that donald can’t be a teacher because he’s not happy, and is able to explain the logic:

```prolog
prolexa> "is donald a teacher".
*** utterance(is donald a teacher)
*** query(teacher(donald))
*** answer(donald is not a teacher)
donald is not a teacher
prolexa> "explain why donald is not a teacher".
*** utterance(explain why donald is not a teacher)
*** goal(explain_question(not(teacher(donald)),_11814,_11802))
*** answer(donald is not happy; every teacher is happy; therefore donald is not a teacher)
donald is not happy; every teacher is happy; therefore donald is not a teacher
prolexa>
```

Similarly, in response to the input "explain why donald is not a teacher", we get:

```prolog
prolexa> "explain why donald is not a teacher".
*** utterance(explain why donald is not a teacher)
*** goal(explain_question(not(teacher(donald)),_50810,_50798))
*** answer(donald is not happy; every teacher is happy; therefore donald is not a teacher)
donald is not happy; every teacher is happy; therefore donald is not a teacher
prolexa> 
```

## Discussion
In the above section, logical negation has been succesfully implemented.

An additional case has been added which can be looked at. 

```prolog
stored_rule(1,[(mortal(X):-human(X))]).
stored_rule(1,[(immortal(X):-god(X))]).

stored_rule(1,[(god(odin):-true)]).
stored_rule(1,[(not(mortal(odin)):-true)]).
```

Giving:

```prolog
prolexa> "explain why odin is not human".
*** utterance(explain why odin is not human)
*** goal(explain_question(not(human(odin)),_9522,_9510))
*** answer(odin is not mortal; every human is mortal; therefore odin is not human)
odin is not mortal; every human is mortal; therefore odin is not human
```
As well as:

```prolog
prolexa> "explain why peter is not god".
*** utterance(explain why peter is not god)
*** goal(explain_question(not(god(peter)),_7734,_7722))
*** answer(peter is not immortal; every god is immortal; therefore peter is not god)
peter is not immortal; every god is immortal; therefore peter is not god
prolexa> 
```

I had hoped to find a way to add rules such as "not(mortal(X)):-god(X)" for the statement "god is not mortal", and mapping between "immortal" and "mortal" as the negation of one another, so that they could be used interchangeably to query the same rules, however this would require further modifications to the grammar and meta-interpreter, and is not possible with the described changes.

I also started to explore implementing default rules fully by adapting the meta-interpreter from [Simply Logical Chapter 8.1](https://too.simply-logical.space/src/text/3_part_iii/8.1.html) to match the the form of the Prolexa "prove_rb" method.  

Currently, Prolexa can handle rules without exception such as "all humans are mortal".  A default rule such as "birds fly" can only be aplied when they do not lead to inconsistency, which would occur if an exception is added to the knowledge base for example, "penguins are birds" and "penguins don't fly".

It should be possible to get Prolexa  to handle such cases by extending the syntax of clausal logic, similar to how negation was added, to distinguish between rules and default rules, and check for inconsistency.  


```prolog
% Tweety is a penguin
stored_rule(1,[(penguin(tweety):- true)]).
% Opus is a bird
stored_rule(1,[(bird(opus):- true)]).

% A penguin is a bird
stored_rule(1,[(bird(X):-penguin(X))]).

% A default rule "birds fly".
stored_rule(1,[(default(fly(X)):-bird(X))].

% An exception to the default rule, "penguins don't fly"
stored_rule(1,[(not(fly(X)):-penguin(X))]).
```

In order to explain this, the “prove_rb” meta-interpreter must be extended for handling clauses such that it contain a separate part for applying a default rule whenever it does not lead to a contradiction.  

The following was tried, but was not successful:

```prolog
% Original sections from "Simply Logical" Ch 8.1 have been adapted for Prolexa.
:-op(900,fy,not).

% Original: explain(F,E) <- E explains F from rules and defaults
explain(F,E):-
    explain(F,[],E).

% Adapted: 
% We already have a top-level that ignores proof
prove_rb(Q,RB):-
	prove_rb(Q,RB,[],_P).
% We can add new top-level for rules and defaults
explain(Q,RB):-
	explain(Q,RB,[],_P).

% Original: meta-interpreter for rules and defaults
explain(true,E,E):-!.
explain((A,B),E0,E):-!,
    explain(A,E0,E1),
    explain(B,E1,E).
explain(A,E0,E):-
    prove_e(A,E0,E).         % explain by rules only
explain(A,E0,[default((A:-B))|E]):-
    default((A:-B)),
    explain(B,E0,E),
    not contradiction(A,E).  % A consistent with E

% Adapted: meta-interpreter for rules and defaults
explain(true,_Rulebase,P,P):-!.
explain((A,B),Rulebase,P0,P):-!,
	explain(A, Rulebase,P0,P1),
	explain(B, Rulebase, P1, P ).
explain((A,Rulebase,P0,P):-
	prove_rb(A, Rulebase,P0,P). % Explain using normal rules from rulebase
explain(A,Rulebase,P0,[default((A:-B))|P]):-
	find_clause(deafault(A:-B)),
	explain(B, Rulebase, P0, P),
  not(contradiction(A,Rulebase,P)).  % Check A is constistent with the rulebase (see below)

% Original: meta-interpreter for rules
prove_e(true,E,E):-!.
prove_e((A,B),E0,E):-!,
    prove_e(A,E0,E1),
    prove_e(B,E1,E).
prove_e(A,E0,[rule((A:-B))|E]):-
    rule((A:-B)),
    prove_e(B,E0,E).

% Adapted: We already have prove_rb to handle the above cases!
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
		find_clause((A:-C),Rule,Rulebase),
		conj_append(C,B,D),
	  prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
	  find_clause((A:-B),Rule,Rulebase),
		prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
prove_rb(not(B),Rulebase,P0,P):- % TEST 3 
  	find_clause((A:-B),Rule,Rulebase), % TEST 3 
		prove_rb(not(A),Rulebase,[p(not(B),Rule)|P0],P). % TEST 3

% Original: check contradiction against rules
contradiction(not A,E):-!,
    prove_e(A,E,_E1).
contradiction(A,E):-
    prove_e(not A,E,_E1).

% Adapted: check contradiction agaisnt rulebase with prove_rb
contradiction(not(A),Rulebase,P):-!,
	prove_rb(A,Rulebase,P,_P1).
contradiction(A,Rulebase,P):-
	prove_rb(not(A),Rulebase,P,_P1).

```


Further testing would be needed to check where the system is not functioning as expected. A new section may be required in the Prolexa method for checking if rules can be deduced from stored rules, to allow deduction from non contradicting default rules. This would mean extending the following code:

```prolog
'%%% test if a rule can be deduced from stored rules %%%
known_rule([Rule],SessionId):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	try((numbervars(Rule,0,_),
	     Rule=(H:-B),
	     add_body_to_rulebase(B,Rulebase,RB2),
	     prove_rb(H,RB2)
	   )).
```
