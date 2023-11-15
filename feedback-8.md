CIS 552 Final Project, Fall 2021

Project name: hscore
Github repo: https://github.com/cis552/project_t8-project/
Group members: Kyven Wu, Run Shi
Mentor: Stephanie Weirich

NOTE: after CIS 552 is over, we will be removing all repositories from github. If you want to make your project public, you should move it to your own github account.

# Comments:

Glad to see that you added a configuration file!  And several different post processing tracks. You have come a long way since your second checkpoint. Great job overall!

# Score: 99/100

## Proposal          5/5
## CP #1             10/10
## CP #2             10/10

## Correctness       25/25
## Design            29/30 

Good use of functional programming. You have cleanly isolated the 
IO monad from most of your code. 

This really shows up well with the "pipeline" oriented implementation of your
toLilyPond function! Nice!

There are several calls to "error" in your code base. Perhaps you could 
restructure your code or type structure to avoid these? For example 
the Data.List.NonEmpty type can record when you want to work with 
lists that can never be empty. 

A small note. The 

  join :: (Monoid a, Semigroup a) => (a, a) -> (a, a) -> (a, a)
  
operation is not needed --- the tuple instance for semigroup already 
has this behavior. You can just use `(<>)` instead.

## Testing           15/15
## Style             5/5


