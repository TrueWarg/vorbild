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
    sample_dir_1
    sample_file_1
    sample_dir_2
    sample_err_1
    sample_err_2

sample_dir_1 = do
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
  it "sample_dir_1" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values dirs)
      (Right [Dir "module.com.gauss-dsb.core"])

sample_file_1 = do
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
  it "sample_file_1" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values files)
      (Right [FileAndContent "paper/list/papers" expected])

sample_dir_2 = do
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
  it "sample_dir_2" $
    shouldBe
      (generateFromTemplates placeHolderConfig2 values dirs)
      (Right [Dir "module.com.gauss.dsb.core"])

sample_err_1 = do
  let values =
        Map.fromList $ [(TemplateValueId "module_name", [Single "gauss-dsb"])]
      dirs = [Dir "module.||~~some_unknown_value||"]
  it "sample_dir_2" $
    shouldBe
      (generateFromTemplates placeHolderConfig2 values dirs)
      (Left $
       InTmpValueParsingError
         "some_unknown_value"
         "module.||~~some_unknown_value||")

sample_err_2 = do
  let values =
        Map.fromList $ [(TemplateValueId "lol", [Single "gauss-dsb"])]
      dirs = [Dir "{{^kek}}"]
  it "sample_dir_2" $
    shouldBe
      (generateFromTemplates placeHolderConfig1 values dirs)
      (Left $
       InTmpValueParsingError
         "kek"
         "{{^kek}}")

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
