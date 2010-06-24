/*
   This an ancient game. Have a gander at this:

   10 REM *** CONVERTED FROM THE ORIGINAL FOCAL PROGRAM AND MODIFIED
   20 REM *** FOR EDUSYSTEM 70 BY DAVID AHL, DIGITAL
   30 REM *** MODIFIED FOR 8K MICROSOFT BASIC BY PETER TURNBULL

   Well, now it has been transliterated from the BASIC to C.

   Copyright (c) 2010 Brian L. Troutwine

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated
   documentation files (the "Software"), to deal in the
   Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to
   do so, subject to the following conditions:

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>

#define RAND(N) (int)((double)rand() / ((double)RAND_MAX + 1) * N)

int main(void) {
  puts("Try your hand at governing Ancient Sumeria\n"
       "successfully for a 10 year term of office.\n");

  int d1 = 0;
  int p1 = 0;

  int z = 0; // year
  int p = 95; // population
  int s = 2800; // bushels
  int h = 3000;
  int e = h - s; // rats ate
  int y = 3; // trading bushels per acre
  int a = (int) h/y; // acres owned
  int i = 5; // immigrants
  int d = 0; // starved
  int l, c, val = 1;
  const int inp_size = 10;
  char input[inp_size];
  char *endptr;

  ++z;
  printf("Hamurabi: I beg to report to you,\nin year %d, ", z);
  printf("%d people starved %d came to the city.\n", d, i);
  p = p+i;

  if (val <= 0) {
    p = (int) p/2;
    puts("A horrible plague struck! Half the people died.\n");
  }
  printf("\nPopulation is now %d\n", p);
  printf("The city now owns %d acres\n", a);
  printf("You have harvested %d bushels per acre.\n", y);
  printf("Rats ate %d bushels.\n\n", e);
  if (z >= 11)
    goto game_end;

  c = RAND(10);
  y = c+17;

  printf("Land is trading at %d bushels per acre.\n", y);

 buy_acres:
  printf("How many acres do you wish to buy/sell?\n");
  assert( fgets(input, inp_size, stdin) );
  errno = 0;
  val = (int) strtol(input, &endptr, inp_size);
  if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
      || (errno != 0 && val == 0)) {
    perror("strtol");
    exit(EXIT_FAILURE);
  }
  if (endptr == input) {
    fprintf(stderr, "No digits were found\n");
    exit(EXIT_FAILURE);
  }
  if (y*val <= s) {
    a = a+val;
    s = s-y*val;
    c = 0;
  }
  if (val >= a) {
    printf("Hamurabi: Think again. You only own %d acres. Now then,", a);
    goto buy_acres;
  }

 feed_people:
  printf("How many bushels do you wish to feed your people?\n");
  assert( fgets(input, inp_size, stdin) );
  errno = 0;
  val = (int) strtol(input, &endptr, inp_size);
  if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
      || (errno != 0 && val == 0)) {
    perror("strtol");
    exit(EXIT_FAILURE);
  }
  if (endptr == input) {
    fprintf(stderr, "No digits were found\n");
    exit(EXIT_FAILURE);
  }
  if (val < 0) goto storm_out;
  if (val > s) {
    printf("Hamurabi: Think again. We only have %d bushels. Now then,", s);
    goto feed_people;
  }
  s = s-val;
  c = 1;

 plant_seed:
  printf("How many bushels do you wish to plant with seed?\n");
  assert( fgets(input, inp_size, stdin) );
  errno = 0;
  val = (int) strtol(input, &endptr, inp_size);
  if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
      || (errno != 0 && val == 0)) {
    perror("strtol");
    exit(EXIT_FAILURE);
  }
  if (endptr == input) {
    fprintf(stderr, "No digits were found\n");
    exit(EXIT_FAILURE);
  }
  if (val == 0)
    c = RAND(5)+1;
  if (val < 0) goto storm_out;
  if (d <= a) {
    if (((int) d/2) < s) {
      if (d >= 10*p) {
        s = s - ((int) d/2);
        c = RAND(5)+1;
      } else {
        printf("But you only have %d people to tend the fields. Now then,\n",
               p);
        // FIXME: finish game
        goto storm_out;
        //goto plant_seed;
      }
    }
  } else {
    printf("Hamurabi: Thing again. You own only %d acres. Now then,\n", a);
    goto plant_seed;
  }

 game_end:
  printf("In your 10-year term of office %d percent of the\n", p1);
  printf("population starved per year on average, i.e., a total of\n");
  printf("%d people died!!\n\n", d1);
  printf("You started with 10 acres per person and ended with\n");
  l = (int) a/p;
  printf("%d acres per person.\n\n", l);

  if ((p1>33) || (l<7)) goto ejected;
  if ((p1>10) || (l<9)) goto nero_end;
  if ((p1>3)  || (l<10)) goto not_so_bad_end;

 ejected:
  printf("Due to this extreme mismanagement you have not only\n");
  printf("been impeached and thrown out of office but you have\n");
  printf("also been declared 'National Fink' !!\n");
  goto curtain_call;

 nero_end:
  printf("Your heavy handed performance smacks of Nero and Ivan IV.\n");
  printf("The people (remaining) find you an unpleasant ruler, and,\n");
  printf("frankly, hate your guts!\n");
  goto curtain_call;

 not_so_bad_end:
  printf("Your performance could have been somewhat better, but\n");
  printf("really wasn't too bad at all. ");
  printf("%d people would ", RAND(p));
  printf("dearly like to see you assassinated but we all have our\n");
  printf("trivial problems.\n");
  goto curtain_call;

 storm_out:
  printf("Hamurabi: I cannot do what you wish.\n");
  printf("Get yourself another steward!!!!!\n");
  goto curtain_call;

 curtain_call:
  puts("\nSo long for now.");
  exit(EXIT_SUCCESS);
}
