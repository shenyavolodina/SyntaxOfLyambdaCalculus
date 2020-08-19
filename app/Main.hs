module Main where

import LambdaCalculus


main :: IO ()
main = do
    print ("Testing functions:  isFreeTest(Is 'z' a free variable for lx.ly.xyz?);")
    print ("isFreshTest (Is 'z' a fresh variable for lx.ly.xyz?)")
    print ("isSubtermTest (Is ly.xyz a subterm for lx.ly.xyz?)")
    print ("isBoundTest (Is 'x' a bound variable for lx.ly.xyz?)")
    print("Term for tests lx.ly.xyz {Abs \"x\" (Abs \"y\" (App (App (Var \"x\") (Var \"y\")) (Var \"z\")))} ")-- λ x . λ y . xyz

isFreeTest = do
    let term1 = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z")))
   
    print ("Is 'z' a free variable for lx.ly.xyz? : ", (isFree term1 "z"))

isFreshTest = do
    let term1 = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z")))
       
    print ("Is 'z' a fresh variable for lx.ly.xyz? : ", (isFresh term1 "z"))
    
isSubtermTest = do
    let term1 = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z")))
           
    print ("Is ly.xyz a subterm for lx.ly.xyz? : ", (isSubterm term1 (Abs "y" (App (App (Var "x") (Var "y")) (Var "z")))))
            
isBoundTest = do
    let term1 = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z")))
               
    print ("Is 'x' a bound variable for lx.ly.xyz? : ", (isBound term1 "x"))