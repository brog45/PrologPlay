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
 
 rule 5:
    [ 1: goal(read_walls)
    ]
    ==>
    [ retract(1)
    , prompt("What is the length of the north and south sides?", LengthNS)
    , prompt("What is the length of the east and west sides?", LengthEW)
    , assert(wall(north, LengthNS))
    , assert(wall(south, LengthNS))
    , assert(wall(east, LengthEW))
    , assert(wall(west, LengthEW))
    , assert(goal(find_door))
    ].

rule 6:
    [ 1: goal(find_door)
    ]
    ==>
    [ retract(1)
    , prompt("Which wall has the door?", DoorWall)
    , prompt("What is the width of the door?", DoorWidth)
    , retract(wall(DoorWall, X))
    , NewWidth = X - DoorWidth
    , assert(wall(DoorWall, NewWidth))
    , assert(position(door, DoorWall))
    , assert(goal(find_plugs))
    , write('Which walls have plugs? "end" when no more plugs'), nl
    ].

rule 7:
    [ 1: goal(find_plugs)
    , 2: position(plug, end)
    ]
    ==> 
    [ retract(all)
    ].

rule 8:
    [ goal(find_plugs)
    ]
    ==>
    [ prompt("Side: ", Wall)
    , assert(position(plug, Wall))
    ].

% furtniture rules

% Start with the couch. The couch should either be opposite the door or to its right.

rule f1:
    [ 1: furniture(couch, LenC)
    ,    position(door, DoorWall)
    ,    opposite(DoorWall, OW)
    , 2: right(DoorWall, RW)
    ,    wall(OW, LenOW)
    ,    wall(RW, LenRW)
    ,    LenOW >= LenRW
    ,    LenC =< LenOW
    ]
    ==>
    [ retract(1)
    , assert(position(couch, OW))
    , retract(2)
    , NewSpace = LenOW - LenC
    , assert(wall(OW, NewSpace))
    ].

rule f1:
    [ 1: furniture(couch, LenC)
    ,    position(door, DoorWall)
    ,    opposite(DoorWall, OW)
    ,    right(DoorWall, RW)
    , 2: wall(OW, LenOW)
    ,    wall(RW, LenRW)
    ,    LenOW =< LenRW
    ,    LenC =< LenRW
    ]
    ==>
    [ retract(1)
    , assert(position(couch, RW))
    , retract(2)
    , NewSpace = LenRW - LenC
    , assert(wall(RW, NewSpace))
    ].

% The TV should be opposite the couch.

rule f3:
    [ 1: furniture(tv, LenTV)
    ,    position(couch, CW)
    ,    opposite(CW, W)
    , 2: wall(W, LenW)
    ]
    ==>
    [ retract(1)
    , assert(position(tv, W))
    , retract(2)
    , NewSpace = LenW - LenTV
    , assert(wall(W, NewSpace))
    ].
