:- use_module(native).

bird(laysan_albatross) :-
    family(albatross),
    color(white).
bird(black_footed_albatross) :-
    family(albatross),
    color(dark).
bird(whistling_swan) :-
    family(swan),
    voice(muffled_musical_whistle).
bird(trumpeter_swan) :-
    family(swan),
    voice(loud_trumpeting).

family(albatross) :-
    order(tubenose),
    size(large),
    wings(long_narrow).
family(swan) :-
    order(waterfowl),
    neck(long),
    color(white),
    flight(ponderous).

order(tubenose) :-
    nostrils(external_tubular),
    live(at_sea),
    bill(hooked).
order(waterfowl) :-
    feet(webbed),
    bill(flat).

flight(X) :- ask(flight, X).
nostrils(X) :- ask(nostrils, X).
live(X) :- ask(live, X).
size(X) :- ask(size, X).
bill(X) :- ask(bill, X).
eats(X) :- ask(eats, X).
feet(X) :- ask(feet, X).
wings(X) :- ask(winds, X).
neck(X) :- ask(neck, X).
color(X) :- ask(color, X).
voice(X) :- ask(voice, X).

