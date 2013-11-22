% Clue Game %

% TODO LIST % 
% There is an issue that if you get pulled into a room you
% were heading to, it will not allow you to make a move in that room
% Comment code
% Error handling: 
%---- Loop Exit and Re-entry
%---- Invalid entries
% It seems like if you exit when an opponent wins it is getting
% caught up in the our turn still and asking if we won.
% maybe there is a call there that is waiting and shouldn't be?

% First call to start the Clue Helper 
clue :- init.

% The dynamic lists the will be changing throughout the game 
:- dynamic numPlayers/1.
:- dynamic currentRoom/1.

:- dynamic yourCharacters/1.
:- dynamic yourWeapons/1.
:- dynamic yourRooms/1.

:- dynamic unknownWeapons/2.
:- dynamic unknownCharacters/2.
:- dynamic unknownRooms/2.

:- dynamic knownWeapons/1.
:- dynamic knownCharacters/1.
:- dynamic knownRooms/1.

%------ Pieces and Cards ------%

% List of all possible characters
character(scarlett).
character(mustard).
character(white).
character(green).
character(peacock).
character(plum).

% List of all possible weapons
weapon(candlestick).
weapon(dagger).
weapon(pipe).
weapon(revolver).
weapon(rope).
weapon(wrench).

% List of all possible rooms
room(kitchen).
room(ballroom).
room(conservatory).
room(diningroom).
room(billiard).
room(library).
room(lounge).
room(hall).
room(study).

% Initalize the game state
init :- clear_game_state,
		init_game_state,
		get_num_players,
		get_cards.

%------ Initalize all cards to unknown ------%
init_game_state :- init_characters, 
				   init_weapons, 
				   init_rooms.

% Initalize all characters to unknown
init_characters :- assert(unknownCharacters(scarlett, 0)),
				   assert(unknownCharacters(mustard, 0)),
				   assert(unknownCharacters(white, 0)),
				   assert(unknownCharacters(green, 0)),
				   assert(unknownCharacters(peacock, 0)),
				   assert(unknownCharacters(plum, 0)).

% Initalize all weapons to unknown 
init_weapons :- assert(unknownWeapons(candlestick, 0)),
				assert(unknownWeapons(dagger, 0)),
				assert(unknownWeapons(pipe, 0)),
				assert(unknownWeapons(revolver, 0)),
				assert(unknownWeapons(rope, 0)),
				assert(unknownWeapons(wrench, 0)).

% Initalize all rooms to unknown
init_rooms :- assert(unknownRooms(kitchen, 0)),
			  assert(unknownRooms(ballroom, 0)),
			  assert(unknownRooms(conservatory, 0)),
			  assert(unknownRooms(diningroom, 0)),
			  assert(unknownRooms(billiard, 0)),
			  assert(unknownRooms(library, 0)),
			  assert(unknownRooms(lounge, 0)),
			  assert(unknownRooms(hall, 0)),
			  assert(unknownRooms(study, 0)).

% Clear the game state
clear_game_state :- retractall(numPlayers(_)),
					retractall(yourCharacters(_,_)),
					retractall(yourWeapons(_,_)),
					retractall(yourRooms(_,_)),
					retractall(knownCharacters(_)),
					retractall(knownWeapons(_)),
					retractall(knownRooms(_)),
					retractall(unknownCharacters(_,_)),
					retractall(unknownWeapons(_,_)),
					retractall(unknownRooms(_,_)).

%------ Start of Game information ------%

% Get the number of players playing 
get_num_players :- write('\nHow many players are there?\nExample "6." \n'),
				   read(NumPlayers),
				   set_players(NumPlayers).

% Set the number of players playing
set_players(NumPlayers) :- assert(numPlayers(NumPlayers)).

% Get all the cards player is holder
get_cards :- write('\nPlease state the cards you are holding.\n Example "candlestick."\n If you have entered all cards type "done." \n' ),
			 read(Card),
			 update_card(Card).

% Find which players turn it is and 
% prompt the user accordingly.
find_turn :- write('\nIs it our turn?\n(y/n)'),
             read(OurTurn),
             is_our_turn(OurTurn).

is_our_turn(y) :- my_turn.
is_our_turn(n) :- write('\nWhich player\'s turn, from your left, is it?\n(0..Number of Players - 1)\n'),
                  read(OpponentsTurn),
                  opponents_turn(OpponentsTurn).
             
%------ Update Cards ------%

%Update the cards until the user types done
update_card(done) :- find_turn.

update_card(Card) :- character(Card),
					 assert(yourCharacters(Card)),
				   	 assert(knownCharacters(Card)),
				     retractall(unknownCharacters(Card,_)),
				   	 get_cards.

update_card(Card) :- weapon(Card),
					 assert(yourWeapons(Card)),
				   	 assert(knownWeapons(Card)),
				     retractall(unknownWeapons(Card,_)),
				   	 get_cards.

update_card(Card) :- room(Card),
					 assert(yourRooms(Card)),
				   	 assert(knownRooms(Card)),
				  	 retractall(unknownRooms(Card,_)),
				   	 get_cards.				   

%Update the unknown cards
update_unknown(Card) :- knownCharacters(Card).
update_unknown(Card) :- knownWeapons(Card). 
update_unknown(Card) :- knownRooms(Card).

update_unknown(Card) :- character(Card),
					  	unknownCharacters(Card,_),
				      	update_unknown_char(Card).

update_unknown(Card) :- weapon(Card),
					 	unknownWeapons(Card,_),
					  	update_unknown_weap(Card).

update_unknown(Card) :- room(Card),
					  	unknownRooms(Card,_),
				      	update_unknown_room(Card).

update_unknown_char(Card) :- unknownCharacters(Card,X),
							 Y is X + 1,
							 retractall(unknownCharacters(Card,_)),
							 assert(unknownCharacters(Card, Y)).

update_unknown_weap(Card) :- unknownWeapons(Card,X),
							 Y is X + 1,
							 retractall(unknownWeapons(Card,_)),
							 assert(unknownWeapons(Card, Y)).

update_unknown_room(Card) :- unknownRooms(Card,X),
						     Y is X + 1,
						     retractall(unknownRooms(Card,_)),
						     assert(unknownRooms(Card, Y)).


%------ Our turn ------%

my_turn :- write('\nAre you in a room?\n(y/n)'),
           read(InRoom),
           in_room_handler(InRoom).

in_room_handler(n) :- closest_room_interface.
in_room_handler(y) :- which_room_interface. 

which_room_interface :- write('\nWhich room? (Ex: hall.) \n'),
                        read(Room),
                        which_room_handler(Room).

which_room_handler(Room) :- knownRooms(Room), already_seen_room_interface.  
which_room_handler(Room) :- unknownRooms(Room,_), exit_room_interface.

already_seen_room_interface :- write('\nWe\'ve already seen this room \n'), 
							   closest_room_interface.

closest_room_interface :- write('\nWhat is the closest room?\n'),
                          read(ClosestRoom),
                          closest_room_handler(ClosestRoom).

closest_room_handler(ClosestRoom) :- knownRooms(ClosestRoom), !, loop_next_closest_room_interface.

closest_room_handler(ClosestRoom) :- unknownRooms(ClosestRoom,_), !, 
									 retractall(currentRoom(_)),
									 assert(currentRoom(ClosestRoom)),
									 go_to_room_interface.


loop_next_closest_room_interface :- write('\nWe\'ve already seen this room \nWhat is the next closest room?\n'),
                                    read(ClosestRoom),
                                    closest_room_handler(ClosestRoom).

go_to_room_interface :- write('\nGo there... did you make it?\n(y/n)'),
                        read(ToRoom),
                        go_to_room_handler(ToRoom).

go_to_room_handler(n) :- end_turn_interface.
go_to_room_handler(y) :- suspect_this.

suspect_this :- the_guess,
                did_you_win_interface.

did_you_win_interface :- write('\nDid you win?\n(y/n)'),
                        read(Win),
                        did_you_win_handler(Win).

did_you_win_handler(n) :- input_card_interface.
did_you_win_handler(y) :- write('Yipee! B-)'),!, false.

input_card_interface :- write('\nInput shown card:\n'),
                        read(Card),
                        update_input_card(Card).

update_input_card(Card) :- character(Card),
				   	 	   assert(knownCharacters(Card)),
				     	   retractall(unknownCharacters(Card,_)),
				     	   end_turn_interface.

update_input_card(Card) :- weapon(Card),
				   	 	   assert(knownWeapons(Card)),
				     	   retractall(unknownWeapons(Card,_)),
				     	   end_turn_interface.

update_input_card(Card) :- room(Card),
				   	 	   assert(knownRooms(Card)),
				  	 	   retractall(unknownRooms(Card,_)),
				  	 	   end_turn_interface.	

end_turn_interface :- write('\nOur turn is over; opponents\' turn now...\n'), opponents_turn.

exit_room_interface :- write('\nExit the room, but stay close!\n'),
                       end_turn_interface.

%------ The Guess ------%
the_guess :- guess_char(X),
			 guess_room(Y),
			 guess_weap(Z),
			 write('\nGuess the following: \n'),
			 print_guesses(X,Y,Z).

print_guesses(X,Y,Z) :- write('Character: '), write(X), write('\n'),
					    write('Room: '), write(Y), write('\n'),
					    write('Weapon: '), write(Z), write('\n').

guess_room(Room) :- currentRoom(Room).

guess_char(Char) :- findall(X, unknownCharacters(_,X),L),
				    max(L,Max),
					unknownCharacters(Char,Max).

guess_weap(Weap) :- findall(X, unknownWeapons(_,X),L),
					max(L,Max),
					unknownWeapons(Weap,Max).
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

opponent_win :- write('\nDid your opponent win?\n(y/n)'),
				read(Win),
				game_over(Win).

opponent_ask :- write('\nDid your opponent make a guess?\n(y/n)'),
				read(Guess),
				opponent_guess(Guess).

opponent_guess(n).

opponent_guess(y) :- write('What was your opponent\'s character guess?\n'),
					 read(Guess),
					 update_unknown(Guess),
					 write('What was their room guess? \n'),
					 read(Guess2),
					 update_unknown(Guess2),
					 write('What was their weapon guess? \n'),
					 read(Guess3),
					 update_unknown(Guess3),
					 show_card(Guess, Guess2, Guess3).


show_card(Guess,_,_):- yourCharacters(Guess),
				   				   write('\nIf you are asked, show them this Character: '),
				                   write(Guess).

show_card(_,Guess2,_):- yourWeapons(Guess2),
				   				   write('\nIf you are asked, show them this Weapon: '),
				   				   write(Guess2).

show_card(_,_,Guess3):- yourRooms(Guess3),
				  				   write('\nIf you are asked, show them this Room: '),
				   				   write(Guess3).

show_card(_,_,_).

game_over(n).
game_over(y):- write('\nThat sucks!\nBetter luck next time.\nPlease don\'t blame me.'),!, false.

fact(0,1).
fact(N,F) :- N > 0,N1 is N - 1, fact(N1,F1),F is F1 * N.