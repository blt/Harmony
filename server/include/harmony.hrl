-type(id() :: integer()).
-type(time() :: {integer(), integer(), integer()}).

-record(star, {id=null :: id(),
               xpos    :: integer(),
               ypos    :: integer(),
               key     :: integer(),
               modified=erlang:now() :: time()
              }).

-record(planet, {id=null   :: id(),
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

-record(counter, {key   :: atom(),
                  value :: integer()}).

-define(TIMEOUT, 150).
