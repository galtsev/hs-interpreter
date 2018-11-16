{-# Language OverloadedStrings, QuasiQuotes #-}
import Test.Hspec
import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity(..))
import Data.ByteString as BS
import Data.String.Here.Uninterpolated (here)
import Data.Aeson (eitherDecodeStrict)

import Decoder
import Interpreter
import Interp.Result
import Interp.State
import StdLib

type Res = Result ([Position], Error)

runM:: Term -> [(Name, Value)] -> Res Value
runM term env = snd $ runState (runResultT (interp term (M.fromList env))) []

shouldEval:: Term -> [(Name, Value)] -> Value -> Expectation
shouldEval term env expected = runM term env `shouldBe` (Ok expected)

shouldFailWith:: Term -> [(Name, Value)] -> Error -> Expectation
shouldFailWith term env expected = case runM term env of
    Ok _ -> expectationFailure "Expected Left"
    Err (_, err) -> err `shouldBe` expected

shouldFailWithLabel:: Term -> [(Name, Value)] -> Error -> [Position] -> Expectation
shouldFailWithLabel term env err tb = runM term env `shouldBe` (Err (tb, err))

shouldDecode:: BS.ByteString -> [(Name, Value)] -> Value -> Expectation
shouldDecode src vars expected = do
    case eitherDecodeStrict src of
        Left msg -> expectationFailure msg
        Right term -> shouldEval term vars expected

pass:: Expectation
pass = pure ()

main :: IO ()
main = hspec $ do
    describe "interp" $ do
        it "const eval to content" $
            shouldEval (Const 12) [] (Num 12)
        it "var eval to refered value" $
            shouldEval (Var "x") [("x", Num 32)] $ Num 32
        it "missing var eval to Wrong" $
            shouldFailWith (Var "x") [("y", Num 77)] $ UnboundVar "x"
        it "add eval to sum" $
            let
                term = Add (Const 4) (Var "z")
            in
                shouldEval term [("z", Num 69)] $ Num 73
        it "add fail if arg eval to not a number" $
            let
                term = Add (Const 8) (Lam "x" (Var "x"))
            in
                shouldFailWith term [] ExpectedNumber
        it "add fail if first argument fail" $
            let
                term = Add (Var "x") (Const 12)
            in
                shouldFailWith term [] $ UnboundVar "x"
        it "add fail if second argument fail" $
            let
                term = Add (Const 43) (Var "y")
            in
                shouldFailWith term [] $ UnboundVar "y"
        it "applying lambda" $
            let
                lam = Lam "x" $ Add (Const 3) (Var "x")
                term = App lam (Const 6)
            in
                shouldEval term [] $ Num 9
        it "applying non-lambda should fail" $
            let
                term = App (Const 2) (Const 3)
            in
                shouldFailWith term [] ExpectedFunction
        it "lambda may get value from env" $
            let
                -- \x -> x + e
                lam = Lam "x" $ Add (Var "e") (Var "x")
                term = App lam (Const 7)
            in
                shouldEval term [("e", Num 9)] $ Num 16
        it "nested lambdas" $
            let
                -- \x y -> x + y + y
                lam = Lam "x" $ Lam "y" $ Add (Var "x") (Add (Var "y") (Var "y"))
                -- x = 2, y = t
                term = App (App lam (Const 2)) (Var "t")
            in
                shouldEval term [("t", Num 64)] (Num 130)

    describe "decoders" $ do
        it "simple const" $
            shouldDecode "123" [] $ Num 123
        it "simple var" $
            shouldDecode [here| "x" |] [("x", Num 93)] $ Num 93
        it "add application" $
            let 
                src  = [here| ["add", 12, "x"] |]
            in
                shouldDecode src [("x", Num 34)] $ Num 46
        it "lambda" $
            let
                src = [here| 
                    [ 
                        ["lambda", "x",
                            ["add", 3, "x"] ],
                        17
                    ] 
                |]
            in
                shouldDecode src [] $ Num 20

    describe "stdlib" $ do
        it "neg should negate number" $
            shouldEval (App (Var "neg") (Const 23)) stdlib $ Num (-23)
        it "mul should multiply" $
            let
                term = App (App (Var "mul") (Const 5)) (Var "t")
            in
                shouldEval term (("t", Num 7):stdlib) $ Num 35

    describe "positioning" $ do
        it "simple err" $
            shouldFailWith (FailWith (Custom "simple")) [] (Custom "simple")
        it "simple labelled" $
            let
                err = Custom "lbl"
                lbl = Label "here"
                term = Lbl lbl (FailWith err)
            in
                shouldFailWithLabel term [] err [lbl]
        it "nested labels" $
            let
                err = Custom "lb2"
                outer = Label "outer"
                inner = Label "inner"
                term = Lbl outer (Lbl inner (FailWith err))
            in
                shouldFailWithLabel term [] err [inner, outer]
        it "add with non-num arg fail at outer level" $
            let
                outer = Label "add"
                inner = Label "badInt"
                term = Lbl outer (Add (Const 12) (Lbl inner (ConstBool True)))
            in
                shouldFailWithLabel term [] ExpectedNumber [outer]
        it "add with failing arg fail at arg level" $
            let
                err = Custom "badArgErr"
                outer = Label "add"
                inner = Label "badArg"
                term = Lbl outer (Add (Const 12) (Lbl inner (FailWith err)))
            in
                shouldFailWithLabel term [] err [inner, outer]


