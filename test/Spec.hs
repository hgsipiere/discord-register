import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec

import Lib

main = hspec.parallel $ describe "parsing" $ do
  it "parse K as name or underscore" (parse nameOrUnderscore' "" "K" `shouldParse` "K")
  it "attempt to parse 2 as name or underscore" (parse nameOrUnderscore' "" `shouldFailOn` "2")
  it "parse _ as name or underscore" (parse nameOrUnderscore' "" "_" `shouldParse` "_")
  it "attempt to parse Kelly as full name" (parse fullName "" `shouldFailOn` "Kelly")
  it "parse Kelly Bloggs as full name" (parse fullName "" "Kelly Bloggs" `shouldParse` "Kelly Bloggs")
  it "parse Kelly _ as full name" (parse fullName "" "Kelly _" `shouldParse` "Kelly _")
