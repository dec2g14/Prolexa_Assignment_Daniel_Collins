:-module(prolexa,
	[
		prolexa_cli/0,			% run prolexa on the command line
		prolexa/1,				% HTTP server for prolexa (for Heroku)
		mk_prolexa_intents/0	% dump all possible Alexa intents in prolexa_intents.json
	]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:-use_module(library(theme/dark)).

:- consult(prolexa_engine).		% meta-interpreter
:- consult(prolexa_grammar).	% NLP grammar

:- dynamic stored_rule/2.	% to record additions to Rulebase in a session

:-prompt(_Old,'prolexa> ').

:-op(900, fy, not). % Test 3: New function
%some intial stored rules
stored_rule(1,[(immortal(X):-god(X))]). % Test 2: all gods are immortal
stored_rule(1,[(mortal(X):-human(X))]).
stored_rule(1,[(cool(X):-human(X))]). % Test 1: Adding another universal statement.
stored_rule(1,[(happy(X):-teacher(X))]). % Test 3

stored_rule(1,[(human(peter):-true)]).
stored_rule(1,[(penguin(tweety):- true)]).
stored_rule(1,[(god(odin):-true)]). % Test 2: Odin is a god
stored_rule(1,[(not(mortal(odin)):-true)]). %Test 2: Odin is not mortal
stored_rule(1,[(not(immortal(peter)):-true)]).
%stored_rule(1,[(teacher(donald):-true)]). % Test 1b: Works the same as peter is human therefore mortal
%stored_rule(1,[(happy(donald):-true)]). % Test 1b: Rules don't work both ways, can't deduce that donald is a teacher from this.
stored_rule(1,[(not(happy(donald)):-true)]). %Test 3: 
%TEST 4 Tweety is a penguin
%stored_rule(1,[(penguin(tweety):- true)]).
%TEST 4 Opus is a bird
%stored_rule(1,[bird(opus):- true)]).
%TEST 4 A penguin is a bird
%stored_rule(1,[(bird(X):-penguin(X))]).
%TEST 4 Default rule with exeption, "bird flies"
%stored_rule(1,[(default(flies(X):-bird(X)))]). %TEST 4
%TEST 4 Exception to the default rule, 
%stored_rule(1,[(not flies(X)):-penguin(X))]).



%%% Prolexa Command Line Interface %%%

% Utterances need to be typed as strings, e.g. "Every human is mortal".
prolexa_cli:-
	read(Input),
	( Input=stop -> true
	; otherwise ->
		handle_utterance(1,Input,Output),
		writeln(Output),
		prolexa_cli
	).

% Main predicate that uses DCG as defined in prolexa_grammar.pl 
% to distinguish between sentences, questions and commands
handle_utterance(SessionId,Utterance,Answer):-
	write_debug(utterance(Utterance)),
	% normalise Utterance into list of atoms
	split_string(Utterance," ","",StringList),	% tokenize by spaces
	maplist(string_lower,StringList,StringListLow),	% all lowercase
	maplist(atom_string,UtteranceList,StringListLow),	% strings to atoms
% A. Utterance is a sentence
	( phrase(sentence(Rule),UtteranceList),
	  write_debug(rule(Rule)),
	  ( known_rule(Rule,SessionId) -> % A1. It follows from known rules
			atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	  ; otherwise -> % A2. It doesn't follow, so add to stored rules
%		  remove_conflicting_rules(Rule), % Test 3 Added. emove
			assertz(prolexa:stored_rule(SessionId,Rule)),
			atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	  )
% B. Utterance is a question that can be answered
	; phrase(question(Query),UtteranceList),
	  write_debug(query(Query)),
	  prove_question(Query,SessionId,Answer) -> true
% C. Utterance is a command that succeeds
	; phrase(command(g(Goal,Answer)),UtteranceList),
	  write_debug(goal(Goal)),
	  call(Goal) -> true
% D. None of the above
	; otherwise -> atomic_list_concat(['I heard you say, ',Utterance,', could you rephrase that please?'],' ',Answer)
	),
	write_debug(answer(Answer)).

write_debug(Atom):-
	write(user_error,'*** '),writeln(user_error,Atom),flush_output(user_error).


%%%%% the stuff below is only relevant if you want to create a voice-driven Alexa skill %%%%%


%%% HTTP server %%%

prolexa(Request):-
	http_read_json_dict(Request,DictIn),
	RequestType = DictIn.request.type,
	( RequestType = "LaunchRequest" -> my_json_answer("I am Minerva, how can I help?",_DictOut)
    ; RequestType = "SessionEndedRequest" -> my_json_answer("Goodbye",_DictOut)
	; RequestType = "IntentRequest" -> 	IntentName = DictIn.request.intent.name,
										handle_intent(IntentName,DictIn,_DictOut)
	).

my_json_answer(Message,DictOut):-
	DictOut = _{
	      response: _{
	      				outputSpeech: _{
	      								type: "PlainText", 
	      								text: Message
	      							},
	      				shouldEndSession: false
	      			},
              version:"1.0"
	     },
	reply_json(DictOut).


handle_intent("getANewFact",_,DictOut):-	% for testing
	random_fact(Fact),
	my_json_answer(Fact,DictOut).
handle_intent("utterance",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Utterance=DictIn.request.intent.slots.utteranceSlot.value,
	handle_utterance(SessionId,Utterance,Answer),
	my_json_answer(Answer,DictOut).
handle_intent(_,_,DictOut):-
	my_json_answer('Please try again',DictOut).


%%% generating intents from grammar %%%
% Run this if you want to test the skill on the 
% Alexa developer console

mk_prolexa_intents:-
	findall(
			_{
				%id:null,
				name:
					_{
						value:SS,
						synonyms:[]
					}
			},
		( phrase(utterance(_),S),
		  atomics_to_string(S," ",SS)
		),
		L),
	% Stream=current_output,
	open('prolexa_intents.json',write,Stream,[]),
	json_write(Stream,
				_{
				    interactionModel: _{
        				languageModel: _{
            				invocationName: minerva,
            				intents: [
								_{
								  name: 'AMAZON.CancelIntent',
								  samples: []
								},
								_{
								  name: 'AMAZON.HelpIntent',
								  samples: []
								},
								_{
								  name: 'AMAZON.StopIntent',
								  samples: []
								},
								_{
								  name: utterance,
								  samples: [
									'{utteranceSlot}'
								  ],
								  slots: [
									_{
									  name: utteranceSlot,
									  type: utteranceSlot,
									  samples: []
									}
								  ]
							  }
							  ],
							  types: [
									_{
										name:utteranceSlot,
										values:L
									}
								]
							}
						}
				}
			   ),
		close(Stream).
