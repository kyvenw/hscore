# hscore
> Midi to Sheet Music Transcription (552 Final Project by Kyven Wu, Sherry Shi)

## Purpose
hscore reads MIDI files using [Euterpea](https://github.com/Euterpea/Euterpea2) and outputs graphical sheet music by generating [LilyPond code](http://lilypond.org/text-input.html).

## Usage
`stack run {midi filepath} {optional config filepath}`

e.g.

`stack run demo/dream.mid demo/dream.yaml`

or, interactively:

`stack run`

### IMPORTANT
hscore needs two things to successfully generate a score:

1. The input midi must only contain notes that can be written as a regular note, a dotted note, or a double-dotted note.

2. The input midi must be 120bpm. you can import your file into [Online Sequencer](https://onlinesequencer.net/) and redownload it at 120bpm. This constraint is inherited from Euterpea's MIDI parsing code.

## Configuration
Configuration is in YAML format and is done on the per-input level.

A sample configuration is
```
title: "And Your Dream Comes True - The Beach Boys"
adjustAccidentals : True
sections:
  - key:
      keyPitch : Df
      keyMode: Major
    time:
      numerator: 2
      denominator: 4
    measures: 6
  - key:
      keyPitch : C
      keyMode: Major
    time:
      numerator: 4
      denominator: 4
midi:
  tempo: 120
```

Every key is optional -- the default (implicit) configuration looks like:
```
adjustAccidentals : True
sections:
  - key:
      keyPitch : C
      keyMode: Major
    time:
      numerator: 4
      denominator: 4
```
Also, note that if the measure key is unspecified, the section will last for the rest of the music.

### Using configurations
By default, when hscore is given `test.mid`, it will look for `test.yaml` to find configuration.
Alternatively, you can pass in a configuration filepath when running hscore:
`stack run test.mid otherconfig.yaml`

## Useful references
- [Euterpea quick reference](https://www.euterpea.com/wp-content/uploads/2016/12/Euterpea_Quick_Reference.pdf)

## Module organization
- Functional Modules
    - [EuterpeaConverter](src/EuterpeaConverter.hs)
    - [LilyPondPostProcess](src/LilyPondPostProcess.hs)
    - [LilyPondPrinter](src/LilyPondPrinter.hs)
- Abstraction Modules
    - [LilyPondSyntax](src/LilyPondSyntax.hs)
    - [EuterpeaTypes](src/EuterpeaTypes.hs)
    - [MusicTypes](src/MusicTypes.hs)
- IO-Related Modules
    - [Main](app/Main.hs)
    - [HScoreConfiguration](src/HScoreConfiguration)
    - [MidiMusicProcessing](src/MidiMusicProcessing)

## Building, running, and testing

This project compiles with `stack build`. 

You can add any needed library dependencies and update project metadata in [package.yaml](package.yaml). You should NOT edit the `.cabal` file. It is automatically generated by running Stack; see [this article](https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/) for more information.

You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Lastly, you can start a REPL with `stack ghci`.


## Notable Features 
### MVP 

1. Haskell Translation From Euterpea syntax to LilyPond music text syntax
    - Create a LilyPond Haskell Representation [LilyPondSyntax.hs](src/LilyPondSyntax.hs)
    - Implement Pretty Printer to translate LilyPond Haskell to LilyPond text [LilyPondPrinter.hs](src/LilyPondPrinter.hs)
    - Translate Duration of notes from Haskell rational duration to LilyPond text duration (non-trivial, see [formatDuration](src/LilyPondPrinter.hs))
2. Basic sequential, one-line music conversion from MIDI -> Sheet Music
3. Basic parallel, multi-staff music conversion from MIDI -> Sheet Music 
    - Aligning parallel notes in exactly the right places (non-trivial, see [getRawStaves](src/EuterpeaConverter.hs))
4. Integrate entire MIDI->PDF pipeline, including the "lilypond" terminal command into one main Function

### Bonus Features

5. Create staves of sections of measures accurately according to key signature and time signature
    - Break notes up into different measures as needed with a tie
6. Polish sheet music to be better style by musical conventions! 
    - Auto-adjust note accidentals according to the key signature (Ex: c# in a Db key becomes a Db so the music doesn't print the accidental)
    - Auto-guess the clef of a staff based on how many notes are above or below center C
    - Clean up rests (separate, consecutive $ rests within a measure get merged into one)
    - Sort the staves with the highest first note is at the top, with the tie-breaker sort being the staff with more notes goes higher
7. Make the # of parts in our output sheet music true to the correct number of parts as shown in the "gold standard" sheet music
    - First sort the staves in descending order of how many notes are in the staff
    - Merge upwards all combinations of parallel measures that can be joined together like puzzle non-trivial, see [tryCombineMeasures](src/LilyPondPostProcess.hs))
    - Clean staves so there are no empty staves  
8. Read and implement user-specified music configurations through a .yaml file 
    -  Adjust music according to user specified Time and Key signatures for different parts of the music
    - Allow users to specify whether they want auto-adjusted accidentals 
    - Adjust music according to the user specifies that there is a pick-up measure 

## Testing 
### End-to-end Testing 

1. ./demo/aunque.mid (Sequential, time/key signature, tying notes)
    - [aunque.mid MIDI sequencer](https://onlinesequencer.net/2464575)
2. ./demo/les_mis.mid (Parallel, squishing staves, tying notes, ranking staves)
    - [les_mis.mid MIDI sequencer](https://onlinesequencer.net/2464577)
3. ./demo/dream.mid (Parallel, squishing staves, accidental adjustment, multiple time/key signature configs, ranking staves)
    - [dream.mid MIDI sequencer](https://onlinesequencer.net/2464576)
4. ./demo/mozart_beginning.mid (Parallel, guess clef, rank staves)
    - [mozart.mid MIDI sequencer](https://onlinesequencer.net/2457146)
5. ./demo/memories.mid (Paralle, squish staves, key config, adjust accidentals)
    - [memories.mid MIDI sequencer](https://onlinesequencer.net/2464578)
6. ./demo/canon.mid (Completely random MIDI, parallel, guess clef, accidentals adjustment, rank staves)
    - [Pachelbel Canon MIDI sequencer](https://onlinesequencer.net/2463352)

### Unit Testing 

- Unit Testing 
  - [EuterpeaConverter](test/EuterpeaConverterTest.hs)
    - getRawStaves
    - appendRests
    - prependRests
    - joinStaves
    - testLengthEMusic
  - [LilyPondPostProcess](test/LilyPondPostProcessTest.hs)
    - postProcessTimeKeyTest
    - breakNoteIntoTieTest
    - postProcessAccidentalsTest
    - guessCleffTest
    - mergeRestsTest
    - sortStavesByPitchAndNoteCountTest
    - sortStavesByNoteCountTest
    - tryCombineMeasuresTest
  - LilyPondPrinter
    - encapsulated in end-to-end testing

- QuickCheck
  - [MusicGen](src/MusicGen.hs)
  - [EuterpeaConverter](test/EuterpeaConverterTest.hs)
    - Length of Euterpea music preserved after Lilypond conversion
    - Length of two sequential Euterpea music equals duration of their respective Lilypond staves joined
    - Euterpea-to-Lilypond converter finishes each measure in full
  - [LilyPondPostProcess](test/LilyPondPostProcessTest.hs)
    - Note breaks into two notes connected by a tie (when needed)