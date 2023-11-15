## Goal: coerce FromMidi2's magical Music output into LilyPond representation

-----
### preamble

First, to get music types from MIDI files, we should use fromMidi2 (need to run "import Euterpea.IO.MIDI.FromMidi2").
This is the more up-to-date version of fromMidi.

Audio-wise, the output of this function makes sense when played using the play :: Music a -> IO () function.
I've also written 

However, the analogous functions writeMidi, toMidi, etc. do NOT always do a good job reconstructing the MIDI file.
In fact, sometimes the outputs aren't even close! One way I've found that usually allows these functions to work is to first
import the midi file that we wanna mess around with into https://onlinesequencer.net/, and then export it again and use the exported MIDI file instead.

These issues are probably caused by different methods of MIDI formatting.

-----
### fromMidi2 shenanigans

From here, I'm trying to get an idea of how FromMidi2 works. I've constructed [resources/weird.mid], which is only three
measures long, and contains overlapping notes, silences, and long notes. Please look at https://onlinesequencer.net/2426997 for what it looks like MIDI-wise. I'm not sure what this actually would look like in LilyPond, but I think if we can figure out what its sheet music looks like, then we can reverse-engineer to figure out what the LilyPond code would look like.

Here comes to hard part:

FromMidi2 (and fromMidi, as a matter of fact) is implemented in a rather "hacky" way:
    At a glance, it looks like the transformation from MIDI to Music uses a kind of greedy approach.
    - It tries to convert most of the notes into a single sequential composition
    - For the remaining notes, it repeats this, placing the output at the correct offset using a variably long rest

For example, when we convert weird.mid into EMusic, it becomes:
((Prim (Note (1 % 4) (B,3)) :+: (Prim (Note (1 % 4) (G,3)) :+: (Prim (Note (1 % 2) (A,3)) :+: Prim (Note (3 % 4) (A,2))))) :+: (Prim (Rest (1 % 4)) :+: (Prim (Note (1 % 16) (F,3)) :+: (Prim (Rest (3 % 16)) :+: (Prim (Note (1 % 16) (A,3)) :+: (Prim (Rest (3 % 16)) :+: (Prim (Note (1 % 16) (B,3)) :+: (Prim (Rest (3 % 16)) :+: Prim (Note (1 % 4) (G,3)))))))))) :=: (Prim (Rest (3 % 4)) :+: Prim (Note (1 % 2) (F,3)))

Let's break this down.

This becomes:
{
    (b3 1/4) :+:
    (g3 1/4) :+:
    (a3 1/2) :+: 
    (a2 3/4) :+: 
    (rest 1/4) :+: 
    (f3 1/16) :+: 
    (rest 3/16) :+: 
    (a3 1/16) :+: 
    (rest 3/16) :+: 
    (b3 1/16) :+: 
    (rest 3/16) :+: 
    (g3 1/4)
} :=: {
    (rest 3/4) :+:
    (f3 1/2)
}

-----
### our job
I think we can leverage this in our conversion from Music to LilyPond.

LilyPond allows for a similar type of "parallel" construction using the "<< >>" environment.
Specifically, in the "<< >>" environment, multiple series of notes (notes surrounded by braces) can be placed in parallel using the "\\\" (two slashes) operator.

Note that there we always want to use the \score environment and wrap our << >> environment in another brace. See resources/weird_lily.ly for an example.