# metalparsec

- idea is similar to [flatparse](https://github.com/AndrasKovacs/flatparse)
    - unboxed internals
    - failure/error distinction instead of consume/nonconsume
    - no incremental parsing
- differences
    - parsec -> megaparsec
      flatparse -> metalparsec
    - use custom streams, such as `Text`, `ByteArray`, `Vector a`
    - parse arbitrary tokens, such as tokens from [alex](https://github.com/haskell/alex)
    - custom position updating, such as byte offset position or line and column positions
    - first class support for text
      - parsing `ByteString` must copy to `ByteArray` first
    - because it has more features, it is slightly slower than `flatparse`
    - 6-8 times faster than `attoparsec` and `megaparsec`
    - less unsafe things exposed
