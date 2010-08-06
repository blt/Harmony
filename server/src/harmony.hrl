-type(id() :: integer()).
-type(time() :: {integer(), integer(), integer()}).

time_ge({Msec0, Sec0, Usec0}, {Msec1, Sec1, Usec1}) ->
    T0 = Msec0*1000000 + Sec0 + Usec0/1000000,
    T1 = Msec1*1000000 + Sec1 + Usec1/1000000,
    T0 >= T1.

-record(star, {id      :: id(),
               xpos    :: integer(),
               ypos    :: integer(),
               key     :: integer(),
               created=erlang:now() :: time()
              }).

-record(planet, {id        :: id(),
                 radius    :: integer(),
                 speed     :: integer(),
                 angle     :: integer(),
                 note      :: integer(),
                 created=erlang:now() :: time()
                }).

-record(in_orbit, {planet_id :: id(),
                   star_id   :: id()
                  }).

-record(system,   {star          :: #star{},
                   planets       :: [#planet{}]}).
-record(universe, {time        :: time(),
                   stars       :: [#system{}]}).
