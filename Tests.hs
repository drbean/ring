module Tests where

import Control.Monad
import Data.Maybe
import Data.Char
import Data.List

import Data.DRS

import PGF
import Ring
import Representation
import Evaluation
import Model

-- handler gr core tests = putStr $ unlines $ map (\(x,y) -> x++show y) $ zip (map (++"\t") tests ) ( map (\string -> map (\x -> core ( x) ) (parse gr (mkCId "DicksonEng") (startCat gr) string)) tests )

-- import System.Environment.FindBin

gr :: IO PGF
gr = readPGF "./Ring.pgf"

langs :: IO [Language]
langs = liftM languages gr

lang :: IO Language
lang = liftM head langs

morpho :: IO Morpho
morpho = liftM2 buildMorpho gr lang

liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c
liftOp f a b = a >>= \a' -> return (f a' b)

miss :: [String] -> IO [String]
miss ws =
	liftOp morphoMissing morpho ws

cat2funs :: String -> IO ()
cat2funs cat = do
	gr' <- gr
	let fs = functionsByCat gr' (mkCId cat)
	let ws = filter (isLower . head . showCId) fs
	let is = map (reverse . dropWhile (\x ->  (==) x '_' || isUpper x) . reverse .showCId ) ws
	putStrLn (unwords is)

catByPOS :: String -> IO ()
catByPOS  pos = do
	gr' <- gr
	let allCats = categories gr'
	let cats = filter (isPrefixOf pos . showCId) allCats
	putStrLn (unwords (map showCId cats))

trans = id

run f tests = do
  gr	<- readPGF "./Ring.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map f ps
  let zs = zip (map (++"\t") tests) (map (map (showExpr []) ) ts)
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

ans tests = do
  gr	<- readPGF "./Ring.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map ( (linear gr) <=< transform ) ) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show $ unwords (map displayResult y))) zs) )

displayResult = fromMaybe "Nothing"

reps tests = do
  gr	<- readPGF "./Ring.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map (\x -> (((unmaybe . rep) x) (term2ref drsRefs var_e) ))) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

lf tests = do
	gr	<- readPGF "./Ring.pgf"
	let ss = map (chomp . lc_first) tests
	let ps = map ( parses gr ) ss
	let ts = map (map (\p -> drsToLF (((unmaybe . rep) p) (DRSRef "r1"))) ) ps
	let zs = zip (map (++"\t") tests) ts
	putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

fol tests = do
	gr	<- readPGF "./Ring.pgf"
	let ss = map (chomp . lc_first) tests
	let ps = map ( parses gr ) ss
	let ts = map (map (\p -> drsToFOL ( (unmaybe . rep) p (term2ref drsRefs var_e) ) ) ) ps
	let zs = zip (map (++"\t") tests) ts
	putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

dic_test = [

	"Renee gives a wedding ring worth $1,850 to the Salvation Army to help poor children."
	, "Gerta gives the Salvation Army $21,000 for Renee's ring, but she doesn't want the ring. "
	, "Gerta wants the Salvation Army to return to Renee the ring, which belongs to Renee's husband."
	, "Gerta wants the ring, which belongs to Renee's husband, returned to Renee."
	, "Renee, who is a widow, doesn't want Gerta, whose husband is also dead, to return the ring to her."

  ]

-- vim: set ts=2 sts=2 sw=2 noet:
