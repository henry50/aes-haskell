# AES Haskell
An implementation of the Advanced Encryption Standard in Haskell.

## Usage
If you don't have Stack installed, find out how to get it [here](https://docs.haskellstack.org/en/stable/README/).

Run the program with `stack run --`, followed by your options.

```
Usage: aes-exe (-k|--key KEY) ((-f|--file FILENAME) | (-r|--raw DATA)) 
               [-d|--decrypt] [-s]
  128, 192 and 256-bit ECB AES implementation.

Available options:
  -h,--help                Show this help text
  -k,--key KEY             Key as a hexadecimal string
  -f,--file FILENAME       Input file path
  -r,--raw DATA            Input as hexadecimal string
  -d,--decrypt             Decrypt
  -s                       Output as text
```

### Example

To encrypt the hexadecimal `3243f6a8885a308d313198a2e0370734` with key `2b7e151628aed2a6abf7158809cf4f3c` (taken from Appendix B of the standard), you would run
```
stack run -- -k "2b7e151628aed2a6abf7158809cf4f3c" -r "3243f6a8885a308d313198a2e0370734"
```
which should give `3925841d02dc09fbdc118597196a0b32`. To decrypt this, run
```
stack run -- -k "2b7e151628aed2a6abf7158809cf4f3c" -r "3925841d02dc09fbdc118597196a0b32" -d
```
which should give back the original.