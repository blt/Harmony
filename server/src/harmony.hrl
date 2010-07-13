-record(star, {star_id,
               xpos,
               ypos}).

-record(planet, {planet_id,
                 radius,
                 speed,
                 angle}).

-record(in_orbit, {planet_id,
                   star_id}).
