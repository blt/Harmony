-type(id() :: integer()).
-type(time() :: {integer(), integer(), integer()}).

-record(star, {id      :: id(),
               xpos    :: integer(),
               ypos    :: integer()
              }).

-record(planet, {id        :: id(),
                 radius    :: integer(),
                 speed     :: integer(),
                 angle     :: integer()
                }).

-record(in_orbit, {planet_id :: id(),
                   star_id   :: id()
                  }).

-record(system,   {star          :: #star{},
                   planets       :: [#planet{}]}).
-record(universe, {time        :: time(),
                   stars       :: [#system{}]}).
