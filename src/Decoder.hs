{-# Language OverloadedStrings #-}
module Decoder where

import Data.Text (unpack)
import Data.Aeson
import Data.Vector (toList)
import Data.Scientific (toBoundedInteger)
import qualified Data.Maybe as MB

import Interpreter

instance FromJSON Term where
    parseJSON (Number v) = pure $ Const $ MB.fromMaybe (0::Int) . toBoundedInteger $ v
    parseJSON (String name) = pure $ Var $ unpack name
    parseJSON (Array ar) = case toList ar of
        [(String "add"), arg1, arg2] -> Add <$> parseJSON arg1 <*> parseJSON arg2
        [(String "lambda"), (String name), body] -> Lam (unpack name) <$> parseJSON body
        [fn, arg] -> App <$> parseJSON fn <*> parseJSON arg
    parseJSON other = fail $ "unexpected value:" ++ show other
