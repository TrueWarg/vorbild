{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module GenerationSpec where

import           Test.Hspec
import           Text.RawString.QQ

import           Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict    as Map
import           Vorbild

spec :: Spec
spec = do
  describe "generation tests" $ do
    sampleDir1
    sampleDir2
    sampleDir3
    sampleFile1
    sampleDir4
    sampleErr1
    sampleErr2

sampleDir1 = do
  let values =
        Map.fromList $
        [ (TemplateValueId "module_name", [Single "gauss-dsb"])
        , ( TemplateValueId "package_name"
          , [ Single "com."
            , Compound [] (fromList [Single "gauss-dsb"])
            , Single "."
            , Compound [] (fromList [Single "core"])
            ])
        ]
      dirs = [Dir "module.{{^package_name}}"]
  it "sampleDir1" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values dirs)
      (Right [Dir "module.com.gauss-dsb.core"])

sampleDir2 = do
  let values =
        Map.fromList $
        [ (TemplateValueId "module_name", [Single "gauss-dsb"])
        , ( TemplateValueId "package_name"
          , [ Single "com."
            , Compound [ToCamel] (fromList [Single "gauss-dsb"])
            , Single "."
            , Compound [ToTitle] (fromList [Single "core"])
            ])
        ]
      dirs = [Dir "module.{{^package_name#}}"]
  it "sampleDir2" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values dirs)
      (Right [Dir "module.com.gaussDsb.Core"])

sampleDir3 = do
  let values =
        Map.fromList $
        [ (TemplateValueId "module_name", [Single "gauss-dsbB"])
        , ( TemplateValueId "package_name"
          , [ Single "com."
            , Compound [ToSnake] (fromList [Single "gauss-dsbB"])
            , Single "."
            , Compound [ToKebab] (fromList [Single "mainCore"])
            ])
        ]
      dirs = [Dir "module.{{^package_name#}}"]
  it "sampleDir3" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values dirs)
      (Right [Dir "module.com.gauss_dsb_b.main-core"])

sampleFile1 = do
  let values =
        Map.fromList $
        [ (TemplateValueId "package_name", [Single "paper.list"])
        , (TemplateValueId "screen_name", [Single "Papers"])
        , (TemplateValueId "todo_message", [Single "NOT IMPLEMENTED"])
        , ( TemplateValueId "full_path"
          , [ Compound
                [Replace "." "/"]
                (fromList [Single "paper.list", Single "/"])
            , Compound [ToLower] (fromList [Single "Papers"])
            ])
        ]
      template =
        [r|
        package org.warg.ui.{{^package_name}}

        import BaseController

        internal class {{^screen_name}}Controller : BaseController {
          val initConfig = TODO("{{^todo_message#toLower}}")

          override fun onCreate(view: View) {
                   super.onCreate(view)
          }
        }
       |]
      expected =
        [r|
        package org.warg.ui.paper.list

        import BaseController

        internal class PapersController : BaseController {
          val initConfig = TODO("not implemented")

          override fun onCreate(view: View) {
                   super.onCreate(view)
          }
        }
       |]
      files = [FileAndContent "{{^full_path}}" template]
  it "sampleFile1" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values files)
      (Right [FileAndContent "paper/list/papers" expected])

sampleDir4 = do
  let values =
        Map.fromList $
        [ (TemplateValueId "module_name", [Single "gauss-dsb"])
        , ( TemplateValueId "package_name"
          , [ Single "com."
            , Compound [] (fromList [Single "Gauss-dsb"])
            , Single "."
            , Compound [] (fromList [Single "core"])
            ])
        ]
      dirs = [Dir "module.||~~package_name^toLower^replace - .||"]
  it "sampleDir4" $
    shouldBe
      (generateFromTemplates placeHolderConfig2 values dirs)
      (Right [Dir "module.com.gauss.dsb.core"])

sampleErr1 = do
  let values =
        Map.fromList $ [(TemplateValueId "module_name", [Single "gauss-dsb"])]
      dirs = [Dir "module.||~~some_unknown_value||"]
  it "sampleDir4" $
    shouldBe
      (generateFromTemplates placeHolderConfig2 values dirs)
      (Left $
       InTmpValueParsingError
         "some_unknown_value"
         "module.||~~some_unknown_value||")

sampleErr2 = do
  let values = Map.fromList $ [(TemplateValueId "lol", [Single "gauss-dsb"])]
      dirs = [Dir "{{^kek}}"]
  it "sampleDir4" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values dirs)
      (Left $ InTmpValueParsingError "kek" "{{^kek}}")

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
