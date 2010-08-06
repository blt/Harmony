-type(id() :: integer()).
-type(time() :: {integer(), integer()}).

uninow() ->
    {Msec, Sec, Usec} = erlang:now(),
    {Msec*1000000+Sec, Usec}.

time_ge({Sec0, Msec0}, {Sec1, Msec1}) ->
    T0 = Sec0 + Msec0/1000000,
    T1 = Sec1 + Msec1/1000000,
    T0 >= T1.

-record(star, {id      :: id(),
               xpos    :: integer(),
               ypos    :: integer(),
               key     :: integer(),
               created=uninow() :: time()
              }).

-record(planet, {id        :: id(),
                 radius    :: integer(),
                 speed     :: integer(),
                 angle     :: integer(),
                 note      :: integer(),
                 created=uninow() :: time()
                }).

-record(in_orbit, {planet_id :: id(),
                   star_id   :: id()
                  }).

-record(system,   {star          :: #star{},
                   planets       :: [#planet{}]}).
-record(universe, {time        :: time(),
                   stars       :: [#system{}]}).
