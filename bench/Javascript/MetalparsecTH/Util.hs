
keyword :: String -> Code Q (Parser ())
keyword s = [||$$(string s) `notFollowedBy` satisfyChar jsIdentLetter *> whitespace||]
