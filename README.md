# ICFP 2015 Programming Contest

This is our submission to the ICFP 2015 Programming Contest. 

### Aside

We realize that this is not the way that inputs were deemed to be accepted by
the judges. We toyed briefly with the idea of using nodejs to take these
arguments, but that never happened. To be honest, even our current implementation 
is not particularly fast (you'll see what we mean), and we decided that going for
performance points was out of the question early on. As such, we hope that you
enjoy our work for what it is: a showcase of some of the features and promise of the
Elm language.

## Running The Application

To run the application, you have two options:

1. `elm-make` - You can use the elm compiler to build a website that you can then
    view. To do this, try something like `elm-make elm_main.elm --output index.html`
2. `elm-reactor` - If you start the elm reactor in this directory, then you can
   just point your browser at `elm_main.elm`.

During running, you may notice what appear to be lock-ups. The program will 
eventually resume. We are still troubleshooting the cause of this behavior.

## Adding New Tests (and other Stuff)

The test cases provided during the contest are all contained in `Tests.elm`. They're each a Json string bound to a variable. Adding more tests:

    testN = """"
      <string in full>
     """"

You'll also want to add the test to the drop-down, so you'll need to edit `Viewer.elm`. 
There's a big list of tuples right at the top, and it goes in there.

It's that easy!


## Output

The output is all conveniently printed on the left of the screen!

## Files

- `DataStructs.elm` - all of the data structures for the program
- `Engine.elm` - code for interacting with the game state
- `Hex.elm` - code for dealing with hexes and grid cells
- `Init.elm` - initialization code
- `ListDict.elm` - dictionary implementation that doesn't require comparable
- `PowerWords.elm` - power word finder algorithm
- `Rand.elm` - random number generator 
- `Search.elm` - search algorithm
- `Tests.elm` - tests from the contest website
- `Update.elm` - game model updater code 
- `Util.elm` - utility code and small functions for general use
- `Viewer.elm` - the code for rendering the model to the screen
- `io.elm` - Json IO tools 
