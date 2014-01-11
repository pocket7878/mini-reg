{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reg where

import  Text.Show.Functions

data RegTerm a = Is (a -> Bool)
data RegExp  a = Or (Either (RegTerm a) (RegExp a)) (Either (RegTerm a) (RegExp a)) | And (Either (RegTerm a) (RegExp a)) (Either (RegTerm a) (RegExp a)) | Star (Either (RegTerm a) (RegExp a)) | Lone (Either (RegTerm a) (RegExp a)) | Some (Either (RegTerm a) (RegExp a))
type Reg a = (Either (RegTerm a) (RegExp a))

star :: Reg a -> Reg a
star a = (Right (Star a))

lone :: Reg a -> Reg a
lone a = (Right (Lone a))

some :: Reg a -> Reg a
some a = (Right (Some a))

and :: Reg a -> Reg a -> Reg a
and a b = (Right (And a b))
(&&) :: Reg a -> Reg a -> Reg a
(&&) = Reg.and

or :: Reg a -> Reg a -> Reg a
or a b = (Right (Or a b))
(||) :: Reg a -> Reg a -> Reg a
(||) = Reg.or

get :: (Eq a,Show a) => a -> Reg a
get a = (Left (Is (\x -> x == a)))

gets :: (Eq a, Show a) => [a] -> Reg a
gets (x:y:[]) = (Right (And (get x) (get y)))
gets (x:ys) = (Right (And (get x) (gets ys)))

is :: (a -> Bool) -> Reg a
is f = (Left (Is f))

instance Show a => Show (RegTerm a) where
    show (Is a) = "#<Is>"

instance Show a => Show (RegExp a) where
    show (Or a b) = (show a) ++ "+" ++ (show b)
    show (And a b) = (show a) ++ (show b)
    show (Star a) = (show a) ++ "*"
    show (Lone a) = (show a) ++ "?"
    show (Some a) = (show a) ++ "+"

instance Show a => Show (Reg a) where
    show (Left a) = show a
    show (Right a) = "(" ++ show a ++ ")"
