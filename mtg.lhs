Tim Reilly ----- Final Project ------ Winter 2012+2014 ------- tfr@pdx.edu

mtg.lhs

This module covers a good amount of the basic groundwork needed to start 
working with decklists in Haskell.  It covers reading and showing cards,
determining castability based on a group of lands, and some basic functions
for graphing casting costs and determining potential plays.

Currently, the only card types are Lands (and only basic lands) and Spells 
(which groups Creatures/Instants/Sorceries/etc).  Card text is not accounted 
for, only card name and casting cost.

---------------------------------------------------------------------------
-------------        ---- Trying the library ----         -----------------
---------------------------------------------------------------------------

To generate a card, call readCard like this:

  readCard "Voiceless Spirit; 2W"
  
readCard is smart enough to tell the difference between a Spell, which has a
casting cost, and a Land, which doesn't.  With Cards, you can use many other
things in the library.

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

showCardList will nicely format a list of cards so their mana costs line up:

Slayer of the Wicked  3W
Slayer of the Wicked  3W
Angelic Overseer     3WW
Dearly Departed      4WW
Mentor of the Meek    2W
Fiend Hunter         1WW
Fiend Hunter         1WW
Champion of the Parish W

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

possiblePlays uses a list of cards and a list of mana symbols and shows all
combinations of possible plays for a single turn.  Using it with showPlays,
you can see a breakdown of every castable combination of cards.

*MtG> showPlays ( possiblePlays [Spell "Loyal Cathar" [White,White], 
Spell "Selfless Cathar" [White], Spell "Voiceless Spirit" 
[Colorless 2, White]] [White, White, White])

Play 1
------

Play 2
------
Loyal Cathar WW

Play 3
------
Selfless Cathar W

Play 4
------
Loyal Cathar   WW
Selfless Cathar W

Play 5
------
Voiceless Spirit 2W

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

showCostPairs and showCostPercentages will give you a breakdown of costs such
as:

1W 30%
1WW 4%
2W 12%
3W 15%

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

There are also a few demo functions after the module definition.  They contain
a few more quick examples for basic output.


----------------------------------------------------------------------------
-------------    ----------Module Definition-----------      ---------------
----------------------------------------------------------------------------

> module MtG (

Type Definitions:

A *Card* is either a Spell, which have casting costs, or a Lands, which have a
symbol and are used to create mana.

*Hand* and *Deck* are aliases of lists of cards, though the names would be more
useful if the library was expanded further.

*ColorSymbol* is either one of the five colors or "Colorless x", where x is
an integer of colorless mana.  This is done to best reflect how Magic costs
are actually written and used.  *ManaCost* is a list of ColorSymbols. Every
Spell has a ManaCost, even if it is empty.

>     Card(Spell,Land),     -- Spell and Land are constructors
>     Hand,                 -- [Card]
>     Deck,                 -- [Card]
>     ColorSymbol(Colorless, White, Blue, Black, Red, Green),
>     ManaCost,             -- [ColorSymbol]
    
Reading and Converting:
-----------------------

*readCost* takes a next mana costs, like "5WU", and converts it to a ManaCost,
or list of ColorSymbols.  The colorless portion is the trickiest.

*readCard* analyzes a card and then defers to either *readLand* or *readSpell*.

>     readCost,             -- String -> ManaCost
>     readCard,             -- String -> Card
>     readLand,             -- String -> Card
>     readSpell,            -- String -> Card

*readCardFile* takes a file name and maps readCard across the cards it finds.

*cardByName* uses a list of cards as a database and looks up a card by String
name.  *readDecklist* is basically a map of cardByName, except that it does a
bit of special work to expand "Doomed Traveler x4" into four copies of the card.
*readDeckFile* is similar to readDecklist but takes a filename rather than a
list of strings.

>     readCardFile,         -- String -> IO [Card]    
>     cardByName,           -- [Card] -> String -> Card
>     readDecklist,         -- [Card] -> [String] -> Deck
>     readDeckFile,         -- [Card] -> String -> IO (Deck)
    
Showing Cards:
--------------

*showMC* and *showCard* are self-explanitory.

*showCardList* seems simple, but I do a bit of extra work to make sure the
mana costs all line up nicely.  Uses the "tying the knot" example from class
as a start!

*showDeckList* is a special case of the previous function which displays the
number of cards in the deck.

*alignCosts* and *alignCosts'* could probably be private functions but are
really useful for lining up card names with card costs.

>     showMC,               -- [ColorSymbol] -> String
>     showCard,             -- Card -> String
>     showCardList,         -- [Card] -> IO ()
>     showDeckList,         -- [Card] -> IO ()
>     alignCosts,           -- String -> String
>     alignCosts',          -- [String] -> [String]
    
Determining Casting Cost and Castability:
-----------------------------------------

*cmc* finds the "Converted Mana Cost", a term that refers to the cost of a card
if the entire cost was colorless.  For example, "2WUB" would be 5.

*cost* simply extracts the cost of a card.

*cmcCard* is a hybrid of cmc and cost as it takes a card, rather than a 
ManaCost, and find the cmc.  I'm debating rewriting the code to go through cmc
exclusively, as I found it natural to work exclusively with cards.  Having to 
extract the ManaCost got tedious. *cmcCards* is a map shortcut.

*sameCost* is a function that determines if two cards have the same cost, and
not just the same cmc.

>     cmc,                  -- ManaCost -> Int
>     cost,                 -- Card -> ManaCost
>     cmcCard,              -- Card -> Int
>     cmcCards,             -- [Card] -> [Int]
>     sameCost,             -- Card -> Card -> Bool

*totalMana* combines all of the costs of a list of cards.  This required a bit
of extra work to handle both Lands and Spells in the same way.  I could probably
rewrite it to use cmcCard.

*possibleMana* finds all of the combinations possible with a given mana pool.
It was rendered unnecessary by later updates, but it seems like it might still
be useful.

*isCastable* combines cmc and meetsColorReq to determine if a particular
mana cost is castable with a given mana pool.  I thought I might have to use
things like possibleMana to achieve this, but it turns out that if you have 
enough total mana and your needed colors are a subset of your available colors,
you know you can cast the spell. *isCastableCard* is the same, but with cards.

*meetsColorReq* uses *stripColorless* and *isColorSubset* to determine if a mana
cost is a subset of a particular mana pool.

*isGroupCastable* leverages *mergeCosts* to check a whole group of costs to 
determine if they are castable.  

As I mentioned with cmcCard, it seems clear that converting everything over to
being Card based instead of being ManaCost based would require some serious 
rewriting but would make the library significantly easier to use.
    
>     totalMana,            -- [Card] -> [ColorSymbol]
>     possibleMana,         -- [ColorSymbol] -> [[ColorSymbol]]
>     isCastable,           -- ManaCost -> [ColorSymbol] -> Bool
>     isCastableCard,       -- Card -> [ColorSymbol] -> Bool
>     meetsColorReq,        -- ManaCost -> [ColorSymbol] -> Bool
>     stripColorless,       -- ManaCost -> ManaCost
>     isColorSubset,        -- [ColorSymbol] -> [ColorSymbol] -> Bool
>     isGroupCastable,      -- [[ColorSymbol]] -> [ColorSymbol] -> Bool
>     mergeCosts,           -- [[ColorSymbol]] -> [ColorSymbol]
>     mergeCosts',          -- [ColorSymbol] -> [ColorSymbol]

Determining Plays:
------------------

*allPlays* finds all possible play combinations given a list of cards. It 
ignores the costs of the cards.

*possiblePlays* finds all the plays that are castable with a certain amount
of total mana. It uses isPlayCastable to filter the list.

*isPlayCastable* basically just uses isGroupCastable.  It could probably
disappear with some reworking.

*totalCMC* finds the total cmc of a list of Cards.  It could probably be removed
with some reworking in the casting cost area.

*highestPlay* finds a play that the most possible mana.  I'd like to also
implement a similar function which finds all of the most expensive, rather than 
just one.

*showPlays* is a nicely formatted way for showing possible plays.

>     allPlays,             -- [Card] -> [[Card]]
>     possiblePlays,        -- [Card] -> [ColorSymbol] -> [[Card]]
>     isPlayCastable,       -- [ColorSymbol] -> [Card] -> Bool
>     totalCMC,             -- [Card] -> Int
>     highestPlay,          -- [Card] -> [ColorSymbol] -> [Card]
>     showPlays,            -- [[Card]] -> IO ()

Determining Cost Breakdown:

*costPairs* groups a list of cards by cost and shows the count and has a
show function to go with it. *costPercentages* is similar.  Both versions 
are somewhat buggy in that the grouping function does not perfectly sort
costs.

>     costPairs,            -- [Card] -> [(ManaCost, Int)]
>     costPercentages,      -- [Card] -> [(ManaCost, Double)]
>     showCostPairs,        -- [Card] -> IO ()
>     showCostPercentages,  -- [Card] -> IO ()

Shuffling and Random Draws:

*shuffle* works on any list to randomly rearrange it.  *draw* and *draw'* 
take cards off the top and return a pair containing the drawn cards and the
remaining deck.

>     shuffle,               -- [a] -> IO [a]
>     draw,                  -- [Card] -> (Card, [Card])
>     draw',                 -- [Card] -> Int -> ([Card],[Card])

Short Demos:

>     playsDemo,            -- Shows example output from showPlays
>     readAndShowCards,     -- Reads from cards.mtg and outputs them.
>     readAndShowDecklist   -- Reads decklist and matches it against cards.mtg
                        
>  ) where

-------------------------------------------------------------------------
-----------       ------- Function Definitions -------       ------------
-------------------------------------------------------------------------


> import Data.Char (digitToInt)
> import Data.List
> import Text.Regex (splitRegex, mkRegex, matchRegex)
> import Text.Regex.Posix ((=~))
> import Data.Function (on)
> import System.Random


BASIC DEFINITIONS
-----------------

> data Card = Spell String ManaCost 
>           | Land String ColorSymbol
>           deriving (Show, Eq)

> type Hand = [Card]

> type Deck = [Card]

> type ManaCost = [ColorSymbol]

> instance Show ColorSymbol where
>   show (Colorless i) = show i
>   show White = "W"
>   show Blue = "U"
>   show Black = "B"
>   show Red = "R"
>   show Green = "G"

> data ColorSymbol = Colorless Int | White | Blue | Black | Red | Green 
>                  deriving (Eq, Ord)

READING AND CONVERTING
----------------------

> readCost          :: String -> ManaCost
> readCost ""       = []
> readCost ('W':ss) = White : readCost ss
> readCost ('U':ss) = Blue : readCost ss
> readCost ('B':ss) = Black : readCost ss
> readCost ('R':ss) = Red : readCost ss
> readCost ('G':ss) = Green : readCost ss
> readCost  ( d:ss) = Colorless (digitToInt d) : readCost ss

> readCard   :: String -> Card
> readCard s  = if length (splitS) == 1 
>                 then readLand s
>                 else readSpell s
>               where splitS = splitRegex (mkRegex "; *") s

> readLand           :: String -> Card
> readLand "Plains"   = Land "Plains" White
> readLand "Island"   = Land "Island" Blue
> readLand "Swamp"    = Land "Swamp" Black
> readLand "Mountain" = Land "Mountain" Red
> readLand "Forest"   = Land "Forest" Green

> readSpell    :: String -> Card
> readSpell s  = Spell (head splitS) (readCost (last splitS))
>                 where splitS = splitRegex (mkRegex "; *") s

> readCardFile :: String -> IO [Card] 
> readCardFile f = do s <- readFile f
>                     return (map readCard (lines s))

> cardByName          :: [Card] -> String -> Card
> cardByName (c:db) n  = if (nameMatches c n)
>                           then c
>                           else cardByName db n
>                        where nameMatches (Spell cn mc) s = cn == s
>                              nameMatches (Land cn mc) s = cn == s

> readDecklist :: [Card] -> [String] -> Deck
> readDecklist db cs = map (cardByName db) cs'
>                      where cs' = concat $ map expand cs

> readDeckFile :: [Card] -> String -> IO (Deck)
> readDeckFile db f = do cs <- readFile f
>                        return (readDecklist db (lines cs))

> expand :: String -> [String]
> expand s = if (s =~ "x[[:digit:]]+" :: Int) == 0
>               then [s]
>               else replicate quantity (head splitS)
>            where splitS = splitRegex (mkRegex " +x[[:digit:]]+") s
>                  quantity = read (last (splitRegex (mkRegex ".*(x)") s)) :: Int



SHOWING CARDS
-------------
      
> showMC :: [ColorSymbol] -> String
> showMC [] = "0"
> showMC xs  = concat (map show xs)

> showCard            :: Card -> String
> showCard (Spell n mc)  = n ++ "   " ++ showMC mc
> showCard (Land n c)    = n

> showCardList :: [Card] -> IO ()
> showCardList cl = mapM_ putStrLn (compileCardList cl)

> showDeckList :: [Card] -> IO ()
> showDeckList cl = do let cl' = compileCardList cl
>                      putStrLn $ "Deck: (" ++ show (length cl') ++ " cards)"
>                      mapM_ putStrLn cl'

> compileCardList :: [Card] -> [String]
> compileCardList cl = alignCosts' [ showCard c | c <- cl]

> patchLine :: Int -> String -> (Int, String)
> patchLine n cs = (length cs, cn ++ (replicate (1 + n - length cs) ' ') ++ mc)
>                     where ws = words cs
>                           (cn, mc) | (length ws) == 1 = (cs,"")
>                                    | otherwise = (unwords $ init $ ws, last ws)

> alignCosts :: String -> String
> alignCosts s  = unlines (map snd ps)
>   where w = foldr max 0 (map fst ps)
>         ps = map (patchLine w) (lines s)

> alignCosts' :: [String] -> [String]
> alignCosts' s  = map snd ps
>    where w = foldr max 0 (map fst ps)
>          ps = map (patchLine w) s





DETERMINING CASTING COST AND CASTABILITY
----------------------------------------

> cmc                  :: ManaCost -> Int
> cmc []               = 0
> cmc (Colorless c:ss) = c + cmc ss
> cmc (s:ss)           = 1 + cmc ss

> cost :: Card -> ManaCost
> cost (Spell _ c) = c
> cost (Land _ _)  = []

> cmcCard :: Card -> Int
> cmcCard c = cmc (cost c)

> cmcCards   :: [Card] -> [Int]
> cmcCards cs = map cmcCard cs

> sameCost :: Card -> Card -> Bool
> sameCost c1 c2 = cost c1 == cost c2


> totalMana                   :: [Card] -> [ColorSymbol]
> totalMana []                = []
> totalMana ((Land n mc):ls)  = mc : totalMana ls
> totalMana ((Spell n cs):ls) = totalMana ls


> possibleMana :: [ColorSymbol] -> [[ColorSymbol]]
> possibleMana ss = nub [ Colorless (length ss - length x):x 
>                       | x <- subsequences ss]

> isCastable :: ManaCost -> [ColorSymbol] -> Bool
> isCastable mc tm = (cmc mc) <= (cmc tm) && meetsColorReq mc tm

> isCastableCard     :: Card -> [ColorSymbol] -> Bool
> isCastableCard c tm = (cmcCard c) <= (cmc tm) && meetsColorReq (cost c) tm

> meetsColorReq :: ManaCost -> [ColorSymbol] -> Bool
> meetsColorReq mc tm = isColorSubset (stripColorless mc) (stripColorless tm)

> stripColorless :: ManaCost -> ManaCost
> stripColorless (Colorless i:ss) = ss
> stripColorless ss = ss

> isColorSubset :: [ColorSymbol] -> [ColorSymbol] -> Bool
> isColorSubset [] [] = True
> isColorSubset [] ss = True
> isColorSubset ss [] = False
> isColorSubset (s:ss) ss' = elem s ss' && isColorSubset ss (delete s ss')


> isGroupCastable       :: [[ColorSymbol]] -> [ColorSymbol] -> Bool
> isGroupCastable h tm     = isCastable (mergeCosts h) tm

> mergeCosts :: [[ColorSymbol]] -> [ColorSymbol]
> mergeCosts = mergeCosts' . sort . concat

> mergeCosts' :: [ColorSymbol] -> [ColorSymbol]
> mergeCosts' [] = []
> mergeCosts' (Colorless i:Colorless j:h) = mergeCosts' (Colorless (i+j):h)
> mergeCosts' h = h





DETERMINING PLAYS
-----------------

> allPlays  :: [Card] -> [[Card]]
> allPlays h = subsequences h

> possiblePlays     :: [Card] -> [ColorSymbol] -> [[Card]]
> possiblePlays h tm = filter (isPlayCastable tm) (allPlays h)

> isPlayCastable     :: [ColorSymbol] -> [Card] -> Bool
> isPlayCastable tm h = isGroupCastable [ c | Spell _ c <- h] tm

> totalCMC :: [Card] -> Int
> totalCMC cs = sum (map (cmcCard) cs)

efficientPlays      :: [Card] -> [ColorSymbol] -> [[Card]]
efficientPlays h tm  = maximumBy (compare `on` totalCMC) (possiblePlays h tm)

^--Whoops this only returns one play, instead of all the highest plays


> highestPlay      :: [Card] -> [ColorSymbol] -> [Card]
> highestPlay h tm  = maximumBy (compare `on` totalCMC) (possiblePlays h tm)

> showPlays    :: [[Card]] -> IO ()
> showPlays ps  = do let ls = map showPlay (enumPlays ps)
>                    putStrLn (unlines ls)
                  
> showPlay     :: (Int, [Card]) -> String
> showPlay (n, p)  = "Play " ++ (show n) ++ "\n------\n" ++ 
>                        (unlines (compileCardList p))

> enumPlays    :: [[Card]] -> [(Int, [Card])]
> enumPlays ps  = zip [1..(length ps)] ps



DETERMINING COST BREAKDOWN
--------------------------

> costPairs :: [Card] -> [(ManaCost, Int)]
> costPairs cl = [ (cost (head c), length c) | c <- groupCosts cl]

> cmcPairs :: [Card] -> [(Int, Int)]
> cmcPairs cl = [ (head c, length c) | c <- groupBy (==) $ sort $ cmcCards cl]

> costPercentages :: [Card] -> [(ManaCost, Double)]
> costPercentages cl = [ (mc, (fromIntegral n)/(fromIntegral (length cl))) 
>                         | (mc, n) <- costPairs cl]

> groupCosts :: [Card] -> [[Card]]
> groupCosts cs = groupBy (sameCost) (sortBy (compare `on` cmcCard) cs)

> showCostPairs :: [Card] -> IO ()
> showCostPairs cl = do let cps = costPairs cl
>                       let ls = alignCosts' [ showMC mc ++ " " ++ show q 
>                                 | (mc, q) <- cps] 
>                       mapM_ putStrLn ls

> showCmcPairs :: [Card] -> IO ()
> showCmcPairs cl = do let cps = cmcPairs cl
>                      let ls = alignCosts' [ show c ++ " " ++ show q 
>                                 | (c, q) <- cps] 
>                      mapM_ putStrLn ls

> showCostPercentages :: [Card] -> IO ()
> showCostPercentages cl = do let cps = costPercentages cl
>                             let ls = alignCosts' [ showMC mc ++ " " ++ 
>                                         show (toPercent q) ++ "%" | (mc, q) <- cps] 
>                             mapM_ putStrLn ls
>                     where toPercent n = round (100 * n)


SHUFFLING AND RANDOM DRAWS
--------------------------

> randPair   :: a-> IO (Int,a)
> randPair x  = do r <- (randomIO::IO Int)
>                  return (r,x)
                
> shuffle    :: [a] -> IO [a]
> shuffle xs  = do rps <- mapM randPair xs
>                  return $ map snd
>                         $ sortBy (\x y -> compare (fst x) (fst y))
>                         $ rps

> draw    :: [Card] -> (Card, [Card])
> draw []  = error "No cards in deck"
> draw cs  = (head cs, tail cs)


> draw'      :: [Card] -> Int -> ([Card],[Card])
> draw' cs n  = if n <= length cs
>                  then (take n cs, drop n cs)
>                  else error "Not enough cards in deck"


DECK COMPOSITION AND PREDICTION
-------------------------------

> remaining       :: [Card] -> [Card] -> [Card]
> remaining kcs dl = dl \\ kcs


MISC TEST CODE
--------------

> playsDemo = showPlays $ possiblePlays [Spell "Loyal Cathar" [White,White], 
>               Spell "Selfless Cathar" [White], Spell "Voiceless Spirit" 
>               [Colorless 2, White], Spell "Moment of Heroism" [Colorless 1,
>               White], Spell "Thraben Sentry" [Colorless 3, White]] 
>               [White, White, Blue]

> costDemo = showCostPercentages [Spell "Loyal Cathar" [White,White], 
>               Spell "Selfless Cathar" [White], Spell "Voiceless Spirit" 
>               [Colorless 2, White], Spell "Moment of Heroism" [Colorless 1,
>               White], Spell "Thraben Sentry" [Colorless 3, White]] 

TODO: Cost breakdown demo

> readAndShowCards :: IO ()
> readAndShowCards  = do cards <- readCardFile "cards.mtg"
>                        showCardList cards

> readAndShowDecklist :: IO ()
> readAndShowDecklist = do db <- readCardFile "cards.mtg"
>                          cs <- readDeckFile db "weenie.deck"
>                          showDeckList cs
                    

> alignTest :: IO ()
> alignTest = readFile "cards.mtg" >>= (putStr . alignCosts)

The following is useful for out-of-IO testing of deck stuff

> tDB = [Spell "Abbey Griffin" [Colorless 3,White],Spell "Angel of Flight Alabaster" [Colorless 4,White],
>        Spell "Angelic Overseer" [Colorless 3,White,White],Spell "Avacynian Priest" [Colorless 1,White],
>        Spell "Champion of the Parish" [White],Spell "Chapel Geist" [Colorless 1,White,White],
>        Spell "Cloistered Youth" [Colorless 1,White],Spell "Dearly Departed" [Colorless 4,White,White],
>        Spell "Doomed Traveler" [White],Spell "Elder Cathar" [Colorless 2,White],
>        Spell "Elite Inquisitor" [White,White],Spell "Fiend Hunter" [Colorless 1,White,White],
>        Spell "Gallows Warden" [Colorless 4,White],Spell "Geist-Honored Monk" [Colorless 3,White,White],
>        Spell "Loyal Cathar" [White,White],Spell "Mausoleum Guard" [Colorless 3,White],
>        Spell "Mentor of the Meek" [Colorless 2,White],Spell "Sanctuary Cat" [White],
>        Spell "Selfless Cathar" [White],Spell "Silverchase Fox" [Colorless 1,White],
>        Spell "Slayer of the Wicked" [Colorless 3,White],Spell "Spectral Rider" [White,White],
>        Spell "Thraben Purebloods" [Colorless 4,White],Spell "Thraben Sentry" [Colorless 3,White],
>        Spell "Unruly Mob" [Colorless 1,White],Spell "Village Bell-Ringer" [Colorless 2,White],
>        Spell "Voiceless Spirit" [Colorless 2,White],Land "Plains" White]

> tHand = map readCard ["Loyal Cathar; WW", "Selfless Cathar; W", "Sanctuary Cat; W", 
>                       "Unruly Mob; 1W", "Chapel Geist; 1WW", "Plains"]

> tDeck = readDecklist tDB ["Doomed Traveler x4","Elite Inquisitor x2",
>          "Loyal Cathar x4","Selfless Cathar x2","Sanctuary Cat x2",
>          "Chapel Geist x4","Unruly Mob x4","Thraben Sentry x2","Angelic Overseer x1",
>          "Angel of Flight Alabaster x1","Mausoleum Guard x3","Voiceless Spirit x3",
>          "Silverchase Fox x3","Plains x25"]

> minideck =  ["Loyal Cathar","Loyal Cathar","Loyal Cathar","Selfless Cathar",
>                 "Unruly Mob","Unruly Mob","Chapel Geist","Sanctuary Cat",
>                 "Sanctuary Cat","Plains","Plains","Plains","Plains"]