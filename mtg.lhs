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
-------------        ---- Changes Since 2012 ----         -----------------
---------------------------------------------------------------------------

I elected to take FunLangs a second time and use the opportunity to work on
my project some more.  Here is an overview of what is new:

 - Many small bugfixes/rule fixes. Things like the real CMC of Lands,
   ordering of costs so that they group well, etc.
 - More convenience functions for extracting information from Cards, hands,
   ManaCosts, etc.
 - More ways to work with plays, especially efficient plays. Also started
   work on ways of taking repeated turns.
 - Drawing from a random deck is much more convenient.
 - Some preliminary ways of assessing odds of particular outcomes were
   added. As a Magic player, I actually started to glean useful insights
   from this information. All it took me was hours and hours of messing
   around with code!

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

*ColorSymbol* is one of the five colors, "Colorless x", where x is an integer of
colorless mana or an "X" cost.  This is done to best reflect how Magic costs
are actually written and used.  *ManaCost* is a list of ColorSymbols. Every
Spell has a ManaCost, even if it is empty.

>     Card(Spell,Land),     -- Spell and Land are constructors
>     Hand,                 -- [Card]
>     Deck,                 -- [Card]
>     ColorSymbol(Ecks, Colorless, White, Blue, Black, Red, Green),
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
if the entire cost was colorless.  For example, "2WUB" would be 5. X costs are
treated as zero, per MTG's rules, though it isn't smart enough to spend higher
than 0 values when trying to maximize mana.

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
ignores the costs of the cards but it will obey the one land per turn rule.

*possiblePlays* finds all the plays that are castable with a certain amount
of total mana (plus land drop). It uses isPlayCastable to filter the list.

*isPlayCastable* basically just uses isGroupCastable.  It could probably
disappear with some reworking.

*totalCMC* finds the total cmc of a list of Cards.  It could probably be removed
with some reworking in the casting cost area.

*efficientPlay* finds the plays using the most possible mana.  Given that extra
mana allows for a wider range of plays, this will also tend to play a land

*makePlay* subtracts cards from a play from your hand, then returns a tuple
containing the separated "in play" cards from "in hand" cards.

*takeTurn* is some preliminary work on inputting and processing the results
of a computer-driven turn.

*showPlays* is a nicely formatted way for showing possible plays.

>     allPlays,             -- [Card] -> [[Card]]
>     possiblePlays,        -- [Card] -> [ColorSymbol] -> [[Card]]
>     isPlayCastable,       -- [ColorSymbol] -> [Card] -> Bool
>     totalCMC,             -- [Card] -> Int
>     efficientPlays,       -- [Card] -> [ColorSymbol] -> [[Card]]
>     makePlay,             -- [Card] -> [Card] -> ([Card],[Card])
>     takeTurn,             -- ([Card],[Card],[Card])
>     showPlays,            -- [[Card]] -> IO ()

Determining Cost Breakdown:

*costPairs* groups a list of cards by cost and shows the count and has a
show function to go with it. *cmcPairs* and *costPercentages* are similar
but for their obvious differences.

>     costPairs,            -- [Card] -> [(ManaCost, Int)]
>     cmcPairs,             -- [Card] -> [(Int, Int)]
>     costPercentages,      -- [Card] -> [(ManaCost, Double)]
>     showCostPairs,        -- [Card] -> IO ()
>     showCostPercentages,  -- [Card] -> IO ()

Shuffling and Random Draws:

*shuffle* works on any list to randomly rearrange it.  *draw* and *drawN* 
take cards off the top and return a pair containing the drawn cards and the
remaining deck. *draw7FromDeck* goes the extra mile and makes a fresh hand
and deck to mess around with.

>     shuffle,               -- [a] -> IO [a]
>     draw,                  -- [Card] -> (Card, [Card])
>     drawN,                 -- [Card] -> Int -> ([Card],[Card])
>     draw7FromDeck,         -- String -> IO ([Card],[Card])

Deck Composition and Prediction:

I think *chanceOf* has a lot of potential, and I've included three special
case functions to give some ideas of how it could be use.

>     chanceOf,              -- (Card -> Bool) -> [Card] -> Double
>     chanceOfCmc,           -- Int -> [Card] -> Double
>     chanceOfCard,          -- Card -> [Card] -> Double
>     chanceOfCards,         -- [Card] -> [Card] -> Double

Short Demos:

>     costDemo,              -- Shows a breakdown of the costs in the test DB
>     playsDemo,             -- Shows example output from showPlays
>     readAndShowCards,      -- Reads from cards.mtg and outputs them.
>     readAndShowDecklist,   -- Reads decklist and matches it against cards.mtg
>     drawSevenDemo,          -- Reads the decklist, shuffles, and draws 7
>     drawSevenNextIsPlainsDemo, -- Shows the odds that you'll get a Plains
>     draw7ShowPlays         -- Shows the available plays on a fresh draw
                        
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
>           deriving (Show, Eq, Ord)

> type Hand = [Card]

> type Deck = [Card]

> type ManaCost = [ColorSymbol]

> instance Show ColorSymbol where
>   show Ecks  = "X"
>   show (Colorless i) = show i
>   show White = "W"
>   show Blue  = "U"
>   show Black = "B"
>   show Red   = "R"
>   show Green = "G"

> data ColorSymbol = Ecks | Colorless Int | White | Blue | Black | Red | Green 
>                  deriving (Eq, Ord)

READING AND CONVERTING
----------------------

> readCost          :: String -> ManaCost
> readCost ""       = []
> readCost ('W':ss) = White : readCost ss
> readCost ('U':ss) = Blue  : readCost ss
> readCost ('B':ss) = Black : readCost ss
> readCost ('R':ss) = Red   : readCost ss
> readCost ('G':ss) = Green : readCost ss
> readCost ('X':ss) = Ecks  : readCost ss
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
> cmc (Ecks:ss)        = cmc ss
> cmc (Colorless c:ss) = c + cmc ss
> cmc (s:ss)           = 1 + cmc ss

> cost :: Card -> ManaCost
> cost (Spell _ c) = c
> cost (Land _ _)  = []

> cName :: Card -> String
> cName (Spell n _) = n
> cName (Land n _)  = n

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
> stripColorless (Ecks:ss) = ss
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
> allPlays h = nubBy areSamePlay $ filter oneLandOrLess (subsequences h)

> oneLandOrLess :: [Card] -> Bool
> oneLandOrLess cs = olol cs 0
>            where olol [] seen = seen <= 1
>                  olol  (Land _ _:cs) seen = olol cs (seen+1)
>                  olol (Spell _ _:cs) seen = olol cs seen

> possiblePlays     :: [Card] -> [ColorSymbol] -> [[Card]]
> possiblePlays h tm = filter (isPlayCastable tm) (allPlays h)

> areSamePlay             :: [Card] -> [Card] -> Bool
> areSamePlay play1 play2  = asp (sort play1) (sort play2)
>               where asp (c1:cs1) (c2:cs2) = ((cName c1) == (cName c2)) && (asp cs1 cs2)
>                     asp [] [] = True
>                     asp p1 [] = False
>                     asp [] p2 = False

> isPlayCastable     :: [ColorSymbol] -> [Card] -> Bool
> isPlayCastable tm h = isGroupCastable [ c | Spell _ c <- h] (tm++[s | Land _ s <- h])

> totalCMC :: [Card] -> Int
> totalCMC cs = sum (map (cmcCard) cs)

> efficientPlays      :: [Card] -> [ColorSymbol] -> [[Card]]
> efficientPlays h tm  = filter ((highPs==) . totalCMC) pps
>                   where pps = possiblePlays h tm
>                         highPs = maximum $ map totalCMC pps

> makePlay :: [Card] -> [Card] -> ([Card],[Card])
> makePlay h p = (p,(h \\ p))

> showPlays    :: [[Card]] -> IO ()
> showPlays ps  = do let ls = map showPlay (enumPlays ps)
>                    putStrLn (unlines ls)
                  
> showPlay     :: (Int, [Card]) -> String
> showPlay (n, p)  = "Play " ++ (show n) ++ "\n------\n" ++ 
>                        (unlines (compileCardList p))

> enumPlays    :: [[Card]] -> [(Int, [Card])]
> enumPlays ps  = zip [1..(length ps)] ps


> takeTurn        :: ([Card],[Card],[Card]) -> ([Card],[Card],[Card])
> takeTurn (d,b,h) = ((snd afterDraw), newBattle, (snd afterPlay))
>             where afterDraw = draw d
>                   startHand = (fst (afterDraw)):h
>                   mana = totalMana b
>                   topPlay = head $ efficientPlays startHand mana
>                   afterPlay = makePlay startHand topPlay
>                   newBattle = b++(fst afterPlay)


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
> groupCosts cs = groupBy (sameCost) [ thatCmc | x <- [0..10], thatCmc <- costFilt x cs]
>             where costComp = (compare `on` cost)
>                   costFilt x cs = sortBy (costComp) (filter ((x==) . cmcCard) cs) 

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


> drawN      :: [Card] -> Int -> ([Card],[Card])
> drawN cs n  = if n <= length cs
>                  then (take n cs, drop n cs)
>                  else error "Not enough cards in deck"

> draw7FromDeck   :: String -> IO ([Card],[Card])
> draw7FromDeck dn = do db <- readCardFile "cards.mtg"
>                       deck <- readDeckFile db dn
>                       sDeck <- shuffle deck
>                       return (drawN sDeck 7)


DECK COMPOSITION AND PREDICTION
-------------------------------

> remaining       :: [Card] -> [Card] -> [Card]
> remaining kcs dl = dl \\ kcs

> chanceOf        :: (Card -> Bool) -> [Card] -> Double
> chanceOf cond cs = (fromIntegral (length (filter cond cs))) / (fromIntegral (length cs))

> chanceOfCmc  :: Int -> [Card] -> Double
> chanceOfCmc i = chanceOf ((i==) . cmcCard)

> chanceOfCard  :: Card -> [Card] -> Double
> chanceOfCard c = chanceOf (c==)

> chanceOfCards :: [Card] -> [Card] -> Double
> chanceOfCards cs = chanceOf (`elem` cs)


MISC TEST CODE
--------------

> playsDemo = showPlays $ possiblePlays [Spell "Loyal Cathar" [White,White], 
>               Spell "Selfless Cathar" [White], Spell "Voiceless Spirit" 
>               [Colorless 2, White], Spell "Moment of Heroism" [Colorless 1,
>               White], Spell "Thraben Sentry" [Colorless 3, White]] 
>               [White, White, Blue]

> percentDemo = showCostPercentages [Spell "Loyal Cathar" [White,White], 
>                 Spell "Selfless Cathar" [White], Spell "Voiceless Spirit" 
>                 [Colorless 2, White], Spell "Moment of Heroism" [Colorless 1,
>                 White], Spell "Thraben Sentry" [Colorless 3, White]] 

> costDemo = showCostPairs tDB

> readAndShowCards :: IO ()
> readAndShowCards  = do cards <- readCardFile "cards.mtg"
>                        showCardList cards

> readAndShowDecklist :: IO ()
> readAndShowDecklist = do db <- readCardFile "cards.mtg"
>                          cs <- readDeckFile db "weenie.deck"
>                          showDeckList cs

> drawSevenDemo :: IO ()
> drawSevenDemo = do (hand, deck2) <- draw7FromDeck "weenie.deck"
>                    showCardList hand

> drawSevenNextIsPlainsDemo :: IO()
> drawSevenNextIsPlainsDemo  = do (hand, deck2) <- draw7FromDeck "weenie.deck"
>                                 putStrLn "Hand:\n-----"
>                                 showCardList hand
>                                 let chance = chanceOfCard (Land "Plains" White) deck2
>                                 putStrLn "\nChance of Plains being next:"
>                                 print chance

> draw7ShowPlays :: IO ()
> draw7ShowPlays = do (hand, deck) <- draw7FromDeck "weenie.deck"
>                     putStrLn "Hand:\n-----"
>                     showCardList hand
>                     putStrLn "\nBest Plays:\n----"
>                     showPlays $ efficientPlays hand []

> alignTest :: IO ()
> alignTest = readFile "cards.mtg" >>= (putStr . alignCosts)

The following is useful for out-of-IO testing of deck stuff

> tCosts = [[Colorless 3,White],[Colorless 2,White,White],
>           [Colorless 1,White,White,White],[Colorless 3,White],
>           [Colorless 3,White],[Colorless 4],[Colorless 3,White]]

> tCosts2 = [[Colorless 3,White],[Colorless 2,White,White],
>            [White,White,White],[Colorless 6,White],
>            [Colorless 3,White],[Colorless 2],[White]]

> tDB = [Spell "Abbey Griffin" [Colorless 3,White],Spell "Angel of Flight Alabaster" [Colorless 4,White],
>        Spell "Angelic Overseer" [Colorless 3,White,White],Spell "Avacynian Priest" [Colorless 1,White],
>        Spell "Champion of the Parish" [White],Spell "Chapel Geist" [Colorless 1,White,White],
>        Spell "Cloistered Youth" [Colorless 1,White],Spell "Dearly Departed" [Colorless 4,White,White],
>        Spell "Doomed Traveler" [White],Spell "Elder Cathar" [Colorless 2,White],
>        Spell "Elite Inquisitor" [White,White],Spell "Fiend Hunter" [Colorless 1,White,White],
>        Spell "Gallows Warden" [Colorless 4,White],Spell "Geist-Honored Monk" [Colorless 3,White,White],
>        Spell "Loyal Cathar" [White,White],Spell "Mausoleum Guard" [Colorless 3,White],
>        Spell "Mentor of the Meek" [Colorless 2,White],Spell "Mikaeus, the Lunarch" [Ecks,White],
>        Spell "Sanctuary Cat" [White],
>        Spell "Selfless Cathar" [White],Spell "Silverchase Fox" [Colorless 1,White],
>        Spell "Slayer of the Wicked" [Colorless 3,White],Spell "Spectral Rider" [White,White],
>        Spell "Thraben Purebloods" [Colorless 4,White],Spell "Thraben Sentry" [Colorless 3,White],
>        Spell "Unruly Mob" [Colorless 1,White],Spell "Village Bell-Ringer" [Colorless 2,White],
>        Spell "Voiceless Spirit" [Colorless 2,White],Land "Plains" White]

> tHand = map readCard ["Loyal Cathar; WW", "Selfless Cathar; W", "Sanctuary Cat; W", 
>                       "Unruly Mob; 1W", "Chapel Geist; 1WW", "Plains", "Island"]

> tDeck = readDecklist tDB ["Doomed Traveler x4","Elite Inquisitor x2",
>          "Loyal Cathar x4","Selfless Cathar x2","Sanctuary Cat x2",
>          "Chapel Geist x4","Unruly Mob x4","Thraben Sentry x2","Angelic Overseer x1",
>          "Mikaeus, the Lunarch x1","Mausoleum Guard x3","Voiceless Spirit x3",
>          "Silverchase Fox x3","Plains x25"]

> minideck =  ["Loyal Cathar","Loyal Cathar","Loyal Cathar","Selfless Cathar",
>                 "Unruly Mob","Unruly Mob","Chapel Geist","Sanctuary Cat",
>                 "Sanctuary Cat","Plains","Plains","Plains","Plains"]