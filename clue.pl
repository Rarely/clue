% Clue Game %

clue :- init.

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

init :- get_num_players.

get_num_players:- write('How many players are there? '),
				  read(NumPlayers),
				  set_players(NumPlayers).

set_players(NumPlayers) :- assert(numPlayers(NumPlayers)).

