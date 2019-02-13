:- consult(oops_ops).

initial_data(
    [ goal(place_furniture)
    , not_end_yet
    , legal_furniture([couch, chair, table_lamp, end_table, coffee_table, tv, standing_lamp, end])
    , opposite(north, south)
    , opposite(south, north)
    , opposite(east, west)
    , opposite(west, east)
    , right(north, west)
    , right(west, south)
    , right(south, east)
    , right(east, north)
    , left(north, east)
    , left(east, south)
    , left(south, west)
    , left(west, north)
    ]).

rule 1:
    [ 1: goal(place_furniture)
    , 2: legal_furniture(LF)
    ]
    ==>
    [ retract(1)
    , cls, nl
    , write("Enter an item of furniture at each prompt."), nl
    , write("Include the width in feet of each item."), nl
    , write("The format is Item#Length."), nl
    , write("The legal values are:" ), nl
    , write(LF), nl, nl
    , write("When there is no more furniture, enter end:end."), nl
    , assert(goal(read_furniture))
    ].

rule 2:
    [ 1: furniture(end,end)
    , 2: goal(read_furniture)
    ]
    ==>
    [ retract(all)
    , assert(goal(read_walls))
    ].

rule 3:
    [ goal(read_furniture)
    , legal_furniture(LF)
    ]
    ==>
    [ prompt("furniture> ", F:L)
    , write(blargh), nl
    , member(F, LF)
    , write(narf), nl
    , assert(furniture(F,L))
    ].

rule 4:
    [ 1: goal(read_furniture) 
    , 2: legal_furniture(LF)
    ]
    ==>
    [ write("Unknown piece of furniture. Must be one of:"), nl
    , write(LF), nl
    ].
