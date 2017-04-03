# hascat

It's `cat` but with rainbows and monads.

## Dependencies

- [A terminal emulator that supports truecolor](https://gist.github.com/XVilka/8346728#now-supporting-truecolour)
- [ghc](https://www.haskell.org/downloads/linux) (build)
- [System.Random](https://hackage.haskell.org/package/random) (build)

## Installation

- clone this repository
- `make`
- copy `hascat` to somewhere in your $PATH

## Usage

    hascat [OPTION]... [FILE]...
    
    Concatenate files or standard input to standard output.
    With no FILE, read standard input.

    -F <f>              Ranbow frequency (default: 2)
    -h, --help          Print this help message
    -v, --version       Print version information

## Todo

- Improve command line argument parsing
- Support non-truecolor terminals
- Support inverting the color
- Support fixed seeds
- Support different rainbow directions
