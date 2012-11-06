import Data.List
    
countNucleotides :: String -> String
countNucleotides dna = unwords . (map show) . (map length) . group $ sort dna

transcribeRna :: String -> String
transcribeRna dna = map (\c -> if c == 'T' then 'U' else c) dna

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'

reverseComplement :: String -> String
reverseComplement = (map complement) . reverse
