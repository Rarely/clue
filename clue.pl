
/******************************

    Program:
    	Clue Board Game Advisor

	Authors:
		Cody Robinson
		Gurkaran Poonia

	Date:
		November 25, 2013

******************************/

%=================================================================================================
% First call to start the Clue Helper 
%=================================================================================================
clue :- init.
re_enter :- find_turn.

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

%=================================================================================================
%------ Pieces and Cards ------%
%=================================================================================================

% List of all possible characters
character(mustard).
character(scarlett).
character(green).
character(white).
character(plum).
character(peacock).

% List of all possible weapons
weapon(dagger).
weapon(candlestick).
weapon(rope).
weapon(revolver).
weapon(wrench).
weapon(pipe).

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

%=================================================================================================
% Initalize the game state
%=================================================================================================
init :- clear_game_state,
		init_game_state,
		starting_info,
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
					retractall(yourCharacters(_)),
					retractall(yourWeapons(_)),
					retractall(yourRooms(_)),
					retractall(knownCharacters(_)),
					retractall(knownWeapons(_)),
					retractall(knownRooms(_)),
					retractall(unknownCharacters(_,_)),
					retractall(unknownWeapons(_,_)),
					retractall(unknownRooms(_,_)).

%=================================================================================================
%------ Start of Game information ------%
%=================================================================================================

starting_info :- write('Welcome! I will be advising you on how to win Clue!\nFollow my steps and give me the correct information and let us crush your opponents!\n'),
				 write('At anytime if you need to see a list of helpful information, please type "help."'), nl,
				 help_menu,nl,nl,
				 write('Let us BEGIN!'), nl.

help_menu :- nl,
			 write('=====List of Characters=====\n'),
			 findall(X1,unknownCharacters(X1,_),L1),
			 print_all(L1),
			 write('========List of Rooms=======\n'),
			 findall(X2,unknownRooms(X2,_),L2),
			 print_all(L2),
			 write('======List of Weapons======\n'),
			 findall(X3,unknownWeapons(X3,_),L3),
			 print_all(L3).


print_all([]).
print_all([H|T]) :- write(H), nl,
				    print_all(T).
			 
% Get the number of players playing 
get_num_players :- write('\nHow many oppenents are there?\nExample "5." \n'),
				   read(NumPlayers),
				   set_players(NumPlayers).

% Set the number of players playing
set_players(help) :- help_menu, get_num_players.
set_players(NumPlayers) :- assert(numPlayers(NumPlayers)).

% Get all the cards player is holder
get_cards :- write('\nPlease state the cards you are holding.\n Example "candlestick."\n If you have entered all cards type "done." \n' ),
			 read(Card),
			 update_card(Card).

% Find which players turn it is and 
% Prompt the user accordingly.
find_turn :- write('\nIs it our turn?\n(y/n)'),
             read(OurTurn),
             is_our_turn(OurTurn).

is_our_turn(help) :- help_menu, find_turn.
is_our_turn(y)    :- my_turn.
is_our_turn(n)    :- write('\nWhich player\'s turn, from your left, is it?\n(0..Number of Opponents - 1)\n'),
                     read(OpponentsTurn),
                     opponents_turn(OpponentsTurn).
is_our_turn(_)    :- write('Please answer with y or n only. Try again:\n'), find_turn.

%=================================================================================================
%------ Update Cards ------%
%=================================================================================================

% Update the cards until the user types done
update_card(done) :- find_turn.
update_card(help) :- help_menu, get_cards.

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

update_card(_) :- write('That\'s not even a card bro... try again:\n'), 
                  read(Card), update_card(Card).

% These update_unknown functions are used to help us keep track of what other
% players are guessing. Everytime they guess we increment the value of the card
% If the card is already known, we do not need to update it
update_unknown(Card) :- knownCharacters(Card).
update_unknown(Card) :- knownWeapons(Card). 
update_unknown(Card) :- knownRooms(Card).

% If a card is unknown, update its value
update_unknown(Card) :- character(Card),
					  	unknownCharacters(Card,_),
				      	update_unknown_char(Card).

update_unknown(Card) :- weapon(Card),
					 	unknownWeapons(Card,_),
					  	update_unknown_weap(Card).

update_unknown(Card) :- room(Card),
					  	unknownRooms(Card,_),
				      	update_unknown_room(Card).

update_unknown(_) :- write('That\'s not even a card bro... try again:\n'), 
                     read(Card), update_unknown(Card).

% Update the value of a card
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

%=================================================================================================
%------ Our turn ------%
%=================================================================================================

% Main player turn interface procedure
my_turn :- write('\nAre you in a room?\n(y/n)'),
           read(InRoom),
           in_room_handler(InRoom).

% Calls the appropriate interface procedure based on
% whether or not the player is in a room.
in_room_handler(help) :- help_menu, my_turn.
in_room_handler(n)    :- closest_room_interface.
in_room_handler(y)    :- which_room_interface. 
in_room_handler(_)    :- write('Please answer with y or n only. Try again:\n'), my_turn.

% Interface procedure to determine which room
% the player is currently in.
which_room_interface :- write('\nWhich room? (Ex: hall.) \n'),
                        read(Room),
                        which_room_handler(Room).

% If Room is already known, then find which room
% player should go to, otherwise prompt to find out
% whether or not player can guess from this room.
which_room_handler(help) :- help_menu, which_room_interface.
which_room_handler(Room) :- knownRooms(Room), already_seen_room_interface.
which_room_handler(Room) :- unknownRooms(Room,_),
							write('Did you get pulled into this room?\n(y/n.)\n'),
							read(Pulled),
							pulled_into_room(Room, Pulled).
which_room_handler(_)    :- write('That is not a room bro, Try again:\n'), which_room_interface.

% Ask if the opponent was pulled because the currentRoom will need to be updated.
pulled_into_room(Room,help) :- help_menu, which_room_handler(Room).
pulled_into_room(Room,y)    :- retractall(currentRoom(_)),
							   assert(currentRoom(Room)),
							   suspect_this.
pulled_into_room(_,n) 	    :- exit_room_interface.

% Interface procedure to find closest room when we have already
% seen current room. This procedure initiates a loop that loops
% until its passed a room that's unknown.
already_seen_room_interface :- write('\nWe\'ve already seen this room \n'), 
							   closest_room_interface.

% Interface procedure to determine which room
% is closest to the player. This gets called
% in a loop to find closest room.
closest_room_interface :- write('\nWhat is the closest room?\n'),
                          read(ClosestRoom),
                          closest_room_handler(ClosestRoom).

% This procedure is part of a loop that determines the closest
% room to the player. When it gets passed an unknown room, it
% will prompt the user to go there.
closest_room_handler(help)		  :- help_menu, closest_room_interface.
closest_room_handler(ClosestRoom) :- knownRooms(ClosestRoom), loop_next_closest_room_interface.

closest_room_handler(ClosestRoom) :- unknownRooms(ClosestRoom,_), 
									 retractall(currentRoom(_)),
									 assert(currentRoom(ClosestRoom)),
									 go_to_room_interface.

closest_room_handler(_)    :- write('That is not a room bro, Try again:\n'), closest_room_interface.

% Calls the closest_room_handler in a loop to find the closest
% room to the player.
loop_next_closest_room_interface :- write('\nWe\'ve already seen this room \nWhat is the next closest room?\n'),
                                    read(ClosestRoom),
                                    closest_room_handler(ClosestRoom).

% Interface procedure that prompts player to go to a room.
go_to_room_interface :- write('\nGo there... did you make it?\n(y/n)'),
                        read(ToRoom),
                        go_to_room_handler(ToRoom).

% Ends turn if player didn't get to the room in the current
% turn, otherwise advises player to guess some cards.
go_to_room_handler(help) :- help_menu, go_to_room_interface.
go_to_room_handler(n) 	 :- end_turn_interface.
go_to_room_handler(y) 	 :- suspect_this.
go_to_room_handler(_) 	 :- write('Please answer with y or n only. Try again:\n'), go_to_room_interface.
% Initiates the guess/suspect logic and prompts player
% to determine if we won.
suspect_this :- the_guess,
                did_you_win_interface.

% prompts player to determine if we won.
did_you_win_interface :- write('\nDid you win?\n(y/n)'),
                         read(Win),
                         did_you_win_handler(Win).

% If player wins, then celebrate, otherwise prompts player
% to enter the card they were shown.
did_you_win_handler(help) :- help_menu, did_you_win_interface.
did_you_win_handler(n) 	  :- input_card_interface.
did_you_win_handler(y) 	  :- write('Yipee! B-)'), nl, 
							 write('Do you wish to play again?'),nl, write('(y/n)'),
				   			 read(Play),!,
				   			 play_again(Play).
did_you_win_handler(_) 	  :- write('Please answer with y or n only. Try again:\n'), did_you_win_interface.

% Prompts player to enter the card they were shown.
input_card_interface :- write('\nInput shown card:\n'),
                        read(Card),
                        update_input_card(Card).

% Determines the type of card the player was shown
% and updates known card appropriately.
update_input_card(help) :- help_menu, input_card_interface.

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

update_input_card(_) :- write('That\'s not even a card bro... try again:\n'),
                           input_card_interface.

% Tells users that our turn is over and initiates the
% opponent turn loop.
end_turn_interface :- write('\nOur turn is over; opponents\' turn now...\n'), opponents_turn.

% Promts user to exit current room but stay close 
% because we will need to re-enter it in the next turn.
exit_room_interface :- write('\nExit the room, but stay close!\n'),
                       end_turn_interface.

%=================================================================================================
%------ The Guess ------%
%=================================================================================================

% Print out the guess for the player
the_guess :- guess_char(X),
			 guess_room(Y),
			 guess_weap(Z),
			 write('\nGuess the following: \n'),
			 print_guesses(X,Y,Z).

% Helper procedure that prints out guess neatly.
print_guesses(X,Y,Z) :- write('Character: '), write(X), write('\n'),
					    write('Room: '),      write(Y), write('\n'),
					    write('Weapon: '),    write(Z), write('\n').

% Always print the current room the player is in
guess_room(Room) :- currentRoom(Room).

% Find out what highest valued character and guess that
guess_char(Char) :- findall(X, unknownCharacters(_,X),L),
				    max(L,Max),
					unknownCharacters(Char,Max).

% Find out the highest valued weapon and guess that
guess_weap(Weap) :- findall(X, unknownWeapons(_,X),L),
					max(L,Max),
					unknownWeapons(Weap,Max).

% Helper function to return the max value in a list					
max([X],X).
max([X|Xs],X):- max(Xs,Y), X >=Y.
max([X|Xs],N):- max(Xs,N), N > X.

%=================================================================================================
%------ Opponents turns ------%
%=================================================================================================

% Loop through the functionality for each opponent asking if:
% Did they make a guess?
% Did they win?
opponents_turn    :- opponents_turn(0).
opponents_turn(X) :- numPlayers(X), % Its now your turn again %
					 write('\nIt is now your turn to play again!\n'),
					 my_turn.
opponents_turn(X) :- write('\n\nIt is the '), write(X), write(' opponent\'s turn\n'),
					 Z is X + 1,
					 opponent_ask,
				     opponent_win,!,
				     opponents_turn(Z). 

% Ask if the opponent made a guess or not
opponent_ask :- write('\nDid your opponent make a guess?\n(y/n)'),
				read(Guess),
				opponent_guess(Guess).

% Ask to see if the opponent won the game or not
opponent_win :- write('\nDid your opponent win?\n(y/n)'),
				read(Win), !,
				game_over(Win).

% If the opponent made a guess, if we have a card that they guessed
% show them card, if we have 2 of those cards, always show them in 
% order of character > weapon > room
opponent_guess(n).
opponent_guess(help) :- help_menu, opponent_ask.
opponent_guess(y) 	 :- write('What was your opponent\'s character guess?\n'),
					  	read(Guess1),
					 	update_unknown(Guess1),
					 	write('What was their room guess? \n'),
					 	read(Guess2),
					 	update_unknown(Guess2),
					 	write('What was their weapon guess? \n'),
					 	read(Guess3),
					 	update_unknown(Guess3),
					 	show_card(Guess1, Guess2, Guess3).

% Output the card a player should show if they have a card
show_card(Guess1,_,_):- yourCharacters(Guess1),
		   				write('\nIf you are asked, show them this Character: '),
		                write(Guess1), nl.

show_card(_,Guess2,_):- yourRooms(Guess2),
					    write('\nIf you are asked, show them this Room: '),
						write(Guess2), nl.

show_card(_,_,Guess3):- yourWeapons(Guess3),
				  		write('\nIf you are asked, show them this Weapon: '),
				   		write(Guess3), nl.

show_card(_,_,_) :- write('You do not have any cards to show'), nl.

% If the game has ended, display a message and stop the game
game_over(n).
game_over(help) :- help_menu, opponent_win.
game_over(y)	:- write('\nThat sucks!\nBetter luck next time.\nPlease don\'t blame me'), nl,
				   write('Do you wish to play again?'),nl, write('(y/n)'),
				   read(Play),!, play_again(Play).

% See if the plaers wants to play again, or close the window
play_again(y) :- nl,clue.
play_again(n) :- halt.
play_again(_) :- write('Wrong choose, please type y. or n.'). 