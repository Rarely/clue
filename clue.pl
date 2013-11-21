% Clue Game %

% TODO LIST %
% Show a card when opponent guesses 
% Refactor Possible into Unknown, they do the same thing! 
% Beginning known cards to loop for only a known number 
% Error handling: Loop Exit and Re-entry
	     	    % invalid entries

% First call to start the Clue Helper %
clue :- init.

% The dynamic lists the will be changing throughout the game %
:- dynamic numPlayers/1.
:- dynamic currentRoom/1.

:- dynamic unknownWeapons/1.
:- dynamic unknownCharacters/1.
:- dynamic unknownRooms/1.

:- dynamic knownWeapons/1.
:- dynamic knownCharacters/1.
:- dynamic knownRooms/1.

:- dynamic possibleWeapons/2.
:- dynamic possibleCharacters/2.
:- dynamic possibleRooms/2.

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
get_cards :- write('Please state the cards you are holding.\n Example "candlestick.", if you have entered all cards type "done." \n' ),
			 read(Card),
			 set_card(Card).

% Find which players turn it is and 
% prompt the user accordingly.
find_turn :- write('Is it our turn?\n(y/n)'),
             read(OurTurn),
             is_our_turn(OurTurn).

is_our_turn(y) :- my_turn.
is_our_turn(n) :- write('Which player\'s turn, from your left, is it?\n(0..Number of Players - 1)\n'),
                  read(OpponentsTurn),
                  opponents_turn(OpponentsTurn).
             
%------ Check to see the type of card or if we are done ------%
set_card(done) :- !, find_turn.

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
				    retractall(unknownWeapons(Card)),
				    retractall(possibleWeapons(Card,_)).

set_character(Card) :- assert(knownCharacters(Card)),
				       retractall(unknownCharacters(Card)),
				       retractall(possibleCharacters(Card,_)).

set_room(Card) :- assert(knownRooms(Card)),
				  retractall(unknownRooms(Card)),
				  retractall(possibleRooms(Card,_)).

set_possible(Card) :- knownCharacters(Card).
set_possible(Card) :- knownRooms(Card).
set_possible(Card) :- knownWeapons(Card). 

set_possible(Card) :- weapon(Card),
					  unknownWeapons(Card),
					  set_possible_weapon(Card).

set_possible(Card) :- character(Card),
					  unknownCharacters(Card),
				      set_possible_character(Card).

set_possible(Card) :- room(Card),
					  unknownRooms(Card),
				      set_possible_room(Card).

%----- Set Possible Characters -----%
set_possible_character(Card) :- possibleCharacters(Card,X),
							    Y is X + 1,
							    retractall(possibleCharacters(Card,_)),
							    assert(possibleCharacters(Card, Y)).

%----- Set Possible Rooms -----%
set_possible_room(Card) :- possibleRooms(Card,X),
						   Y is X + 1,
						   retractall(possibleRooms(Card,_)),
						   assert(possibleRooms(Card, Y)).

%----- Set Possible Weapons -----%
set_possible_weapon(Card) :- possibleWeapons(Card,X),
							 Y is X + 1,
							 retractall(possibleWeapons(Card,_)),
							 assert(possibleWeapons(Card, Y)).


%------ Initalize all cards to unknown ------%
init_game_state :- init_characters, 
				   init_weapons, 
				   init_rooms.

% Initalize all characters to unknown and possible %
init_characters :- assert(unknownCharacters(scarlett)),
				   assert(unknownCharacters(mustard)),
				   assert(unknownCharacters(white)),
				   assert(unknownCharacters(green)),
				   assert(unknownCharacters(peacock)),
				   assert(unknownCharacters(plum)),
                   assert(possibleCharacters(scarlett, 0)),
				   assert(possibleCharacters(mustard, 0)),
				   assert(possibleCharacters(white, 0)),
				   assert(possibleCharacters(green, 0)),
				   assert(possibleCharacters(peacock, 0)),
				   assert(possibleCharacters(plum, 0)).

% Initalize all weapons to unknown and possible %
init_weapons :- assert(unknownWeapons(candlestick)),
				assert(unknownWeapons(dagger)),
				assert(unknownWeapons(pipe)),
				assert(unknownWeapons(revolver)),
				assert(unknownWeapons(rope)),
				assert(unknownWeapons(wrench)),
				assert(possibleWeapons(candlestick, 0)),
				assert(possibleWeapons(dagger, 0)),
				assert(possibleWeapons(pipe, 0)),
				assert(possibleWeapons(revolver, 0)),
				assert(possibleeapons(rope, 0)),
				assert(possibleWeapons(wrench, 0)).

% Initalize all rooms to unknown and possible %
init_rooms :- assert(unknownRooms(kitchen)),
			  assert(unknownRooms(ballroom)),
			  assert(unknownRooms(conservatory)),
			  assert(unknownRooms(diningroom)),
			  assert(unknownRooms(billiard)),
			  assert(unknownRooms(library)),
			  assert(unknownRooms(lounge)),
			  assert(unknownRooms(hall)),
			  assert(unknownRooms(study)),
              assert(possibleRooms(kitchen, 0)),
			  assert(possibleRooms(ballroom, 0)),
			  assert(possibleRooms(conservatory, 0)),
			  assert(possibleRooms(diningroom, 0)),
			  assert(possibleRooms(billiard, 0)),
			  assert(possibleRooms(library, 0)),
			  assert(possibleRooms(lounge, 0)),
			  assert(possibleRooms(hall, 0)),
			  assert(possibleRooms(study, 0)).

clear_game_state :- retractall(numPlayers(_)),
					retractall(unknownWeapons(_)),
					retractall(unknownCharacters(_)),
					retractall(unknownRooms(_)),
					retractall(knownWeapons(_)),
					retractall(knownCharacters(_)),
					retractall(knownRooms(_)),
					retractall(possibleWeapons(_,_)),
					retractall(possibleCharacters(_,_)),
					retractall(possibleRooms(_,_)).

%------ Our turn ------%
my_turn :- write('Are you in a room?\n(y/n)'),
           read(InRoom),
           in_room_handler(InRoom).

in_room_handler(n) :- closest_room_interface.
in_room_handler(y) :- which_room_interface. 

which_room_interface :- write('Which room? (Ex: hall.) \n'),
                      read(Room),
                      which_room_handler(Room).

which_room_handler(Room) :- knownRooms(Room), already_seen_room_interface.  
which_room_handler(Room) :- unknownRooms(Room), exit_room_interface.

already_seen_room_interface :- write('We\'ve already seen this room \n'), 
							   closest_room_interface.

closest_room_interface :- write('What is the closest room?\n'),
                        read(ClosestRoom),
                        closest_room_handler(ClosestRoom).

closest_room_handler(ClosestRoom) :- knownRooms(ClosestRoom), !, loop_next_closest_room_interface.

closest_room_handler(ClosestRoom) :- unknownRooms(ClosestRoom), !, 
									 retractall(currentRoom(_)),
									 assert(currentRoom(ClosestRoom)),
									 go_to_room_interface.


loop_next_closest_room_interface :- write('We\'ve already seen this room \nWhat is the next closest room?\n'),
                                    read(ClosestRoom),
                                    closest_room_handler(ClosestRoom).

go_to_room_interface :- write('Go there... did you make it?\n(y/n)'),
                        read(ToRoom),
                        go_to_room_handler(ToRoom).

go_to_room_handler(n) :- end_turn_interface.
go_to_room_handler(y) :- suspect_this.

suspect_this :- the_guess,
                did_you_win_interface.

did_you_win_interface :- write('Did you win?\n(y/n)'),
                        read(Win),
                        did_you_win_handler(Win).

did_you_win_handler(n) :- input_card_interface.
did_you_win_handler(y) :- write('Yipee! B-)'), fail.

input_card_interface :- write('Input shown card:\n'),
                        read(Card),
                        input_card_handler(Card).

input_card_handler(Card) :- weapon(Card), set_weapon(Card), end_turn_interface.
input_card_handler(Card) :- character(Card), set_character(Card), end_turn_interface.
input_card_handler(Card) :- room(Card), set_room(Card), end_turn_interface.

end_turn_interface :- write('Our turn is over; opponents\' turn now...\n'), opponents_turn.

exit_room_interface :- write('Exit the room, but stay close!\n'),
                       end_turn_interface.

%------ The Guess ------%
the_guess :- guess_character(X),
			 guess_room(Y),
			 guess_weapon(Z),
			 write('Guess the following: \n'),
			 print_guesses(X,Y,Z).

print_guesses(X,Y,Z) :- write('Character: '), write(X), write('\n'),
					    write('Room: '), write(Y), write('\n'),
					    write('Weapon: '), write(Z), write('\n').

guess_room(Room) :- currentRoom(Room).

guess_character(Char) :- findall(X, possibleCharacters(_,X),L),
						 max(L,Max),
						 possibleCharacters(Char,Max).

guess_weapon(Weapon) :- findall(X, possibleWeapons(_,X),L),
						 max(L,Max),
						 possibleWeapons(Weapon,Max).
max([X],X).
max([X|Xs],X):- max(Xs,Y), X >=Y.
max([X|Xs],N):- max(Xs,N), N > X.

%------ Opponents turns ------%
opponents_turn :- opponents_turn(0).

opponents_turn(X) :- numPlayers(X), % Its now your turn again %
					 write('\nIt is now your turn to play again!\n'),
					 my_turn.

opponents_turn(X) :- write('\n\nIt is the '), write(X), write(' opponent\'s turn\n'),
					 Z is X + 1,
					 opponent_win,
				     opponent_ask,
				     opponents_turn(Z). 

opponent_win :- write('Did your opponent win?\n(y/n)'),
				read(Win),
				game_over(Win).

opponent_ask :- write('Did your opponent make a guess?\n(y/n)'),
				read(Guess),
				opponent_guess(Guess).

opponent_guess(n).

opponent_guess(y) :- write('What was your opponent\'s character guess?\n'),
					 read(Guess),
					 set_possible(Guess),
					 write('What was their room guess? \n'),
					 read(Guess2),
					 set_possible(Guess2),
					 write('What was their weapon guess? \n'),
					 read(Guess3),
					 set_possible(Guess3).

game_over(n).
game_over(y):- write('That sucks! \nBetter luck next time. \nPlease don\'t blame me.'), fail.