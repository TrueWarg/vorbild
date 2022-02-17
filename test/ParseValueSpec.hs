{-# LANGUAGE OverloadedStrings #-}

module ParseValueSpec where

import           Test.Hspec

import           Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict    as Map
import           Vorbild

spec :: Spec
spec = do
  describe "value parsing tests" $ do
    sample1
    sample2
    sample3
    sample4
    sample5
    errSample1
    errSample2
    errSample3
    errSample4

sample1 = do
  let raws =
        Map.fromList 
        [ ("module_name", "gauss-dsb")
        , ("file_name", "core")
        , ("package_name", "com.{{^module_name}}.{{^file_name}}")
        , ("file_path", "{{^package_name#replace '.' '/'}}")
        ]
  it "sample1" $
    shouldBe
      (parseValues placeHolderConfig1 raws)
      (Right
         (Map.fromList $
          [ (TemplateValueId "module_name", [Single "gauss-dsb"])
          , (TemplateValueId "file_name", [Single "core"])
          , ( TemplateValueId "package_name"
            , [ Single "com."
              , Compound [] (fromList [Single "gauss-dsb"])
              , Single "."
              , Compound [] (fromList [Single "core"])
              ])
          , ( TemplateValueId "file_path"
            , [ Compound
                  [Replace "." "/"]
                  (fromList
                     [ Single "com."
                     , Compound [] (fromList [Single "gauss-dsb"])
                     , Single "."
                     , Compound [] (fromList [Single "core"])
                     ])
              ])
          ]))

sample2 = do
  let raws =
        Map.fromList 
        [ ("module_name", "linear")
        , ("file_name", "MATRIX")
        , ( "package_name"
          , "com.{{^module_name}}.{{^file_name#toLower#replace '.' '/'}}")
        ]
  it "sample2" $
    shouldBe
      (parseValues placeHolderConfig1 raws)
      (Right
         (Map.fromList $
          [ (TemplateValueId "module_name", [Single "linear"])
          , (TemplateValueId "file_name", [Single "MATRIX"])
          , ( TemplateValueId "package_name"
            , [ Single "com."
              , Compound [] (fromList [Single "linear"])
              , Single "."
              , Compound [ToLower, Replace "." "/"] (fromList [Single "MATRIX"])
              ])
          ]))

sample3 = do
  let raws =
        Map.fromList 
        [ ("dir", "dir_name")
        , ("subfir", "sub_dir_name")
        , ("full_path", "||~~dir||/||~~subfir^replace '_' '*'||")
        ]
  it "sample3" $
    shouldBe
      (parseValues placeHolderConfig2 raws)
      (Right
         (Map.fromList $
          [ (TemplateValueId "dir", [Single "dir_name"])
          , (TemplateValueId "subfir", [Single "sub_dir_name"])
          , ( TemplateValueId "full_path"
            , [ Compound [] (fromList [Single "dir_name"])
              , Single "/"
              , Compound [Replace "_" "*"] (fromList [Single "sub_dir_name"])
              ])
          ]))

sample4 = do
  let raws =
        Map.fromList 
        [("module", "MODULE"), ("flow", "||~~module^toLower||")]
  it "sample4" $
    shouldBe
      (parseValues placeHolderConfig2 raws)
      (Right
         (Map.fromList $
          [ (TemplateValueId "module", [Single "MODULE"])
          , ( TemplateValueId "flow"
            , [Compound [ToLower] (fromList [Single "MODULE"])])
          ]))

sample5 = do
  let raws =
        Map.fromList 
        [ ("screen", "ScreenName")
        , ("package", "*Open*&Prefix&screen^Mod^toLower*Close*")
        ]
  it "sample5" $
    shouldBe
      (parseValues placeHolderConfig3 raws)
      (Right
         (Map.fromList $
          [ (TemplateValueId "screen", [Single "ScreenName"])
          , ( TemplateValueId "package"
            , [Compound [ToLower] (fromList [Single "ScreenName"])])
          ]))

errSample1 = do
  let raws =
        Map.fromList 
        [ ("screen", "ScreenName")
        , ("dir", "{{^module}}")
        , ("module", "{{^some_unknown_value}}")
        ]
  it "errSample1" $
    shouldBe
      (parseValues placeHolderConfig1 raws)
      (Left $ UnkonwnName "some_unknown_value")

errSample2 = do
  let raws = Map.fromList [("screen", "ScreenName"), ("dir", "{{^dir}}"), ("module", "{{^screen}}")]
  it "errSample2" $
    shouldBe
      (parseValues placeHolderConfig1 raws)
      (Left $ CycleDeclaration "dir")

errSample3 = do
  let raws = Map.fromList [("screen", "ScreenName"), ("dir", "{{^kek}}"), ("module", "{{^screen}}")]
  it "errSample3" $
    shouldBe
      (parseValues placeHolderConfig1 raws)
      (Left $ UnkonwnName "kek")

errSample4 = do
  let raws = Map.fromList [("screen", "{{^module}}"), ("dir", "{{^screen}}"), ("module", "{{^dir}}")]
  it "errSample4" $
    shouldBe
      (parseValues placeHolderConfig1 raws)
      (Left $ CycleDeclaration "screen")

placeHolderConfig1 =
  PlaceholderConfig
    { openTag = "{{"
    , closeTag = "}}"
    , valuePrefix = "^"
    , modifierSeparator = "#"
    }

placeHolderConfig2 =
  PlaceholderConfig
    { openTag = "||"
    , closeTag = "||"
    , valuePrefix = "~~"
    , modifierSeparator = "^"
    }

placeHolderConfig3 =
  PlaceholderConfig
    { openTag = "*Open*"
    , closeTag = "*Close*"
    , valuePrefix = "&Prefix&"
    , modifierSeparator = "^Mod^"
    }

instance Show ValueParsingError where
  show (UnkonwnName valueName) = "Unknow value with name: " <> valueName
  show (CycleDeclaration valueName) = "Self declaration value with name: " <> valueName
