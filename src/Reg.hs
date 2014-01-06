{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reg where

data RegTerm a = Take a 
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

get :: a -> Reg a
get a = (Left (Take a))

gets :: [a] -> Reg a
gets (x:y:[]) = (Right (And (Left (Take x)) (Left (Take y))))
gets (x:ys) = (Right (And (Left (Take x)) (gets ys)))

instance Show a => Show (RegTerm a) where
    show (Take a) = show a

instance Show a => Show (RegExp a) where
    show (Or a b) = (show a) ++ "+" ++ (show b)
    show (And a b) = (show a) ++ (show b)
    show (Star a) = (show a) ++ "*"
    show (Lone a) = (show a) ++ "?"
    show (Some a) = (show a) ++ "+"

instance Show a => Show (Reg a) where
    show (Left a) = show a
    show (Right a) = "(" ++ show a ++ ")"
