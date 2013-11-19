% Clue Game %

% TODO LIST %
% The players turn loop %
% The opponents turn %
% The guess! %

% First call to start the Clue Helper %
clue :- init.

% The dynamic lists the will be changing throughout the game %
:- dynamic numPlayers/1.

:- dynamic unknownWeapons/1.
:- dynamic unknownCharacters/1.
:- dynamic unknownRooms/1.

:- dynamic knownWeapons/1.
:- dynamic knownCharacters/1.
:- dynamic knownRooms/1.

% Pieces and Cards %

% List of all possible characters %
character(scarlett).
character(mustard).
character(white).
character(green).
character(peacock).
character(plum).

% List of all possible weapons %
weapon(candlestick).
weapon(dagger).
weapon(pipe).
weapon(revolver).
weapon(rope).
weapon(wrench).

% List of all possible rooms %
room(kitchen).
room(ballroom).
room(conservatory).
room(diningroom).
room(billiard).
room(library).
room(lounge).
room(hall).
room(study).

% Initalize the game state %
init :- clear_game_state,
		init_game_state,
		get_num_players,
		get_cards.

% Get the number of players playing %
get_num_players :- write('How many players are there? Example "6." \n'),
				   read(NumPlayers),
				   set_players(NumPlayers).

% Set the number of players playing %
set_players(NumPlayers) :- assert(numPlayers(NumPlayers)).


% Get all the cards player is holder %
get_cards :- write('Please state the cards you are holding. Example "candlestick.", if you have entered all cards type "done." \n' ),
			 read(Card),
			 set_card(Card).

%------ Check to see the type of card or if we are done ------%
set_card(done).

set_card(Card) :- weapon(Card),
				   set_weapon(Card),
				   get_cards.

set_card(Card) :- character(Card),
				   set_character(Card),
				   get_cards.

set_card(Card) :- room(Card),
				   set_room(Card),
				   get_cards.				   

set_weapon(Card) :- assert(knownWeapons(Card)),
				    retract(unknownWeapons(Card)).

set_character(Card) :- assert(knownCharacters(Card)),
				       retract(unknownCharacters(Card)).

set_room(Card) :- assert(knownRooms(Card)),
				  retract(unknownRooms(Card)).


%------ Initalize all cards to unknown ------%
init_game_state :- init_characters, 
				  init_weapons, 
				  init_rooms.

% Initalize all characters to unknown %
init_characters :- assert(unknownCharacters(scarlett)),
				   assert(unknownCharacters(mustard)),
				   assert(unknownCharacters(white)),
				   assert(unknownCharacters(green)),
				   assert(unknownCharacters(peacock)),
				   assert(unknownCharacters(plum)).

% Initalize all weapons to unknown %
init_weapons :- assert(unknownWeapons(candlestick)),
				assert(unknownWeapons(dagger)),
				assert(unknownWeapons(pipe)),
				assert(unknownWeapons(revolver)),
				assert(unknownWeapons(rope)),
				assert(unknownWeapons(wrench)).

% Initalize all rooms to unknown %
init_rooms :- assert(unknownRooms(kitchen)),
			  assert(unknownRooms(ballroom)),
			  assert(unknownRooms(conservatory)),
			  assert(unknownRooms(diningroom)),
			  assert(unknownRooms(billiard)),
			  assert(unknownRooms(library)),
			  assert(unknownRooms(lounge)),
			  assert(unknownRooms(hall)),
			  assert(unknownRooms(study)).

clear_game_state :- retractall(unknownWeapons(_)),
					retractall(unknownCharacters(_)),
					retractall(unknownRooms(_)),
					retractall(knownWeapons(_)),
					retractall(knownCharacters(_)),
					retractall(knownRooms(_)).