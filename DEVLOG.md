--------------------------------------------------------------------------------
-- DEVELOPMENT NOTES
--------------------------------------------------------------------------------


[2013.07.01] {First Performance Notes}
--------------------------------------

Ran for the first time on the 557Mb file 3Dgrid_J_10000000.
On my laptop it took 26.4 seconds sequential and 9.6 seconds on four cores.
Productivity was 62% in the latter case.  CPU usage stayed pegged at 400%.

Aha, there is some screwup in the parallel stage, the sequential routine itself
(readNatsPartial, t3) only takes 4.5 seconds of realtime to read the whole file
(97.6% productivity).  Actually... that's better than the sequential time of the PBBS
C++, I believe.

NFData may be the culprit...


[2013.07.01] {Mysterious}
-------------------------

I'm trying to lock down where this huge perf difference comes from, but it's getting
stranger and stranger.  Even when I launch ONE parallel chunk it is still slow.  Even
when I dispense with the parallel libraries and SEQUENTIALLY launch a single
chunk... t2 is still very slow (and yet it should be doing the SAME thing as t3).

I rebuilt t3 to check again... still fast.  Compiling with -threaded ... still
fast. Ok, what about the obvious thing.  Maybe readNatsPartial is not as strict as I
think it is (though that should apply to BOTH the parallel and sequential tests, as
long as NFData isn't used...).  Ok, so I did the heavy handed thing and added a
deepseq to t3... it's STILL FAST.  What gives?

Ok, I'm worried that there are some weird effects with the mmap-based file reading.
I'm trying with simple, strict, readFile instead.  Here's the thing... It only takes
0.303837s to read the 500M file on my laptop.  The rest of the time is all parsing
and should be scalable.
  I introduced t3B to use the sequential routine + readFile and, yep, it's STILL FAST.
(and t4 is still slow).

Ok, let's take a look at the actual output sizes:

    $ time ./t3B_use_readFile.exe +RTS -N1 -s
    Sequential version + readFile
    Time to read file sequentially: 0.330334s
    Result: 1 segments of output
     <segment, length 69,568,627>

vs. 

    $ time ./t4_use_readFile.exe +RTS -N1 -s
    Using parReadNats + readFile
    Time to read file sequentially: 0.312601s
    Sequential debug version running on sizes: [557968893]
    (SEQUENTIAL) Launching chunk of 557968893
    Result: 1 segments of output
     <segment, length 69,568,627>

But the first takes 4.5 seconds and the second takes 25.4 seconds!!

Ok, well let's make them IDENTICAL... and then back down from there.  Eek, I added a
fourth case to the #if above, getting rid of "loop" and "splitAt" so that the
"parallel" version *literally* just calls getNumCapabilities and then the sequential.
It STILL takes 25 seconds.

Well, let's take away the last thing distinguishing them... getNumCapabilities.  THAT
WAS IT!  Taking that away makes them both drop to 4.5 seconds.  If I call
getNumCapabilities, even if I don't use the resulting value it criples the program to
take >5X longer.

This is on Mac OS GHC 7.6.2.  Let's try on linux and see if the same bug exists.

Whoa, wait, when I try to package this for reproduction, changing the module name and
not using -main-is .... that seems to make the bug vanish!!

With proper parallelism:
------------------------

If I simply avoid that call to the offending getNumCapabilities, hardcoding the
number of threads, I actually see quite nice parallel performance.

  * 1.7 seconds with readFile / monad-par, 4x overpartition (16 chunks)
  * 1.4 seconds with mmap / monad-par, 4x overpartition
