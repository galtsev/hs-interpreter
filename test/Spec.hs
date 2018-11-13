import Test.Hspec
import qualified Data.Map.Strict as M
import Interpreter


emptyEnv:: Env
emptyEnv = M.empty

shouldEval:: Term -> [(Name, Value)] -> Value -> Expectation
shouldEval term env expected = 
    interp term (M.fromList env) `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "interp" $ do
        it "const eval to content" $
            interp (Const 12) emptyEnv `shouldBe` Num 12
        it "var eval to refered value" $
            interp (Var "x") (M.fromList [("x", Num 32)]) `shouldBe` Num 32
        it "missing var eval to Wrong" $
            interp (Var "x") (M.fromList [("y", Num 77)]) `shouldBe` Wrong
        it "add eval to sum" $
            let
                term = Add (Const 4) (Var "z")
                env = M.fromList [("z", Num 69)]
            in
                interp term env `shouldBe` Num 73
        it "applying lambda" $
            let
                lam = Lam "x" $ Add (Const 3) (Var "x")
                term = App lam (Const 6)
            in
                interp term emptyEnv `shouldBe` Num 9
        it "lambda may get value from env" $
            let
                -- \x -> x + e
                lam = Lam "x" $ Add (Var "e") (Var "x")
                term = App lam (Const 7)
            in
                interp term (M.fromList [("e", Num 9)]) `shouldBe` Num 16
        it "nested lambdas" $
            let
                -- \x y -> x + y + y
                lam = Lam "x" $ Lam "y" $ Add (Var "x") (Add (Var "y") (Var "y"))
                -- x = 2, y = t
                term = App (App lam (Const 2)) (Var "t")
            in
                shouldEval term [("t", Num 64)] (Num 130)
