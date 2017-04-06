# hascat

It's `cat` but with rainbows and monads.

## Dependencies

- [A terminal emulator that supports truecolor](https://gist.github.com/XVilka/8346728#now-supporting-truecolour)
- [ghc](https://www.haskell.org/downloads/linux) (build)
- [System.Random](https://hackage.haskell.org/package/random) (build)
- [Options.Applicative](https://hackage.haskell.org/package/optparse-applicative) (build)
- [Data.Semigroup](https://hackage.haskell.org/package/semigroups) (build)

## Installation

- clone this repository
- `make`
- copy `hascat` to somewhere in your $PATH

## Usage

    hascat [-h|--help] [-v|--version] [-F|--freq <f>] [-S|--seed <s>] [FILE]
    
    Concatenate files or standard input to standard output.
    With no FILE, read standard input.
    The FILE "-" represents stardard input.

      -F, --freq <f>      Ranbow frequency (default: 2)
      -S, --seed <s>      RNG seed, 0 means random (default: 0)
      -h, --help          Print this help message
      -v, --version       Print version information

## Todo

- Support non-truecolor terminals
- Support inverting the color
- Support different rainbow directions
