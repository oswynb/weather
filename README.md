# weather

## Building/Installing

    - Get Haskell! [The Haskell Platform](https://www.haskell.org/platform/) works well.
    - `git clone git@github.com:oswynb/weather.git`
    - `cabal sandbox init`
    - `cabal install --only-dependencies`
    - `cabal build`
    - The binaries will be in dist/build

## Running

### Common Options Stuff

    * Supported Temperature UOMs (-t)
        - `Celsius:   C`
        - `Kelvin:    K`
        - `Fahrenheit F`

    * Supported Distance UOMs (-d)
        - `Kilometre: km`
        - `Mile:      mi`
        - `Metre:     m`

### Normalise

    * Takes in an input file (-i) and an output file (-o)
    * Also takes in UOMs as detailed above

### Stats
    
    * Takes in an input file (-i)
    * Also takes in UOMs as detailed above

### Generate

    * Takes in an output file (-f)
    * As well as many simulation configuration options (They have sensible defaults)
        - Start Time (-s): The earliest timestamp in the output data
        - Time Step (-t): The time between balloon broadcasts in minutes
        - X and Y drift (-x, -y): The average direction the balloon is drifting in metres per time step
        - Temp Variance (-p): The rate at which temperature fluctuates about the baloon.
        - Failure Rate (-r): The rate at which observatories will produce invalid output
        - No. lines (-n): The number of lines to output
