{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ModifyFilesSpec where

import           Test.Hspec
import           Text.RawString.QQ

import           Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import           Vorbild

spec :: Spec
spec = do
  describe "file modification tests" $ do
    sampe1
    sampe2
    sampe3
    sampe4
    sampe5
    errSample1
    errSample2

sampe1 = do
  let config = ValuesAndConfig values placeHolderConfig
      content =
        [r|
      include ':app'
      include ':setting'
      |]
      descriptors = [Descriptor Nothing Nothing [Append "{{^packageName}}"]]
      expected =
        [r|
      include ':app'
      include ':setting'
      lil.word.package|]
  it "sampe1" $
    shouldBe (execModifications config content descriptors) (Right $ expected)

sampe2 = do
  let config = ValuesAndConfig values placeHolderConfig
      content =
        [r|
      include ':app'
      include ':org'
      include ':setting'
      |]
      edges = Edges "include ':app'" "include ':setting'"
      descriptors =
        [Descriptor Nothing (Just edges) [Prepend "\n      {{^packageName}}"]]
      expected =
        [r|
      include ':app'
      lil.word.package
      include ':org'
      include ':setting'
      |]
  it "sampe2" $
    shouldBe (execModifications config content descriptors) (Right $ expected)

sampe3 = do
  let config = ValuesAndConfig values placeHolderConfig
      content =
        [r|
include ':app'
  innerStart
  innerEnd

  tag
    dInnerStart
    dInnerEnd
  tag
include ':setting'
|]
      descriptors =
        [ Descriptor
            Nothing
            (Just $ Edges "  innerStart" "  innerEnd")
            [Append "  ++\n"]
        , Descriptor
            Nothing
            (Just $ Edges "    dInnerStart" "    dInnerEnd")
            [Append "    ++++\n"]
        , Descriptor
            Nothing
            (Just $ Edges "  tag" "  tag")
            [Append "  --d@{{^packageName#replace . @}}\n"]
        , Descriptor
            Nothing
            (Just $ Edges "include ':app'" "include ':setting'")
            [Prepend "\nROOT_{{^packageName}}"]
        ]
      expected =
        [r|
include ':app'
ROOT_lil.word.package
  innerStart
  ++
  innerEnd

  tag
    dInnerStart
    ++++
    dInnerEnd
  --d@lil@word@package
  tag
include ':setting'
|]
  it "sampe3" $
    shouldBe (execModifications config content descriptors) (Right $ expected)

sampe4 = do
  let config = ValuesAndConfig values placeHolderConfig
      content =
        [r|
dependelcies {
  mit3
  mit1
  aeg
  wang
  sang
}

ske {
}
|]
      descriptors =
        [ Descriptor
            Nothing
            (Just $ Edges "dependelcies {\n" "\n}")
            [Append "\n  {{^path#replace '/' '@'}}\n", SortLines]
        , Descriptor Nothing (Just $ Edges "ske {" "}") [AppendOnce " ["]
        , Descriptor Nothing (Just $ Edges "ske {" "}") [AppendOnce "]\n"]
        , Descriptor
            Nothing
            (Just $ Edges "ske {" "\n}")
            [PrependOnce "\n %%%%%"]
        , Descriptor Nothing (Just $ Edges " [" "]") [Append "*"]
        ]
      expected =
        [r|
dependelcies {
  aeg
  lil@word@package
  lil@word@package
  mit1
  mit3
  sang
  wang
}

ske {
 %%%%%
 [**]
}
|]
      firstResult = execModifications config content descriptors
      secondResult =
        firstResult >>= (\txt -> execModifications config txt descriptors)
  it "sampe4" $ shouldBe secondResult (Right $ expected)

sampe5 = do
  let config = ValuesAndConfig values placeHolderConfig
      content = "lil.word.package\naaa\nlil*word*package"
      descriptors =
        [ Descriptor
            Nothing
            (Just $ Edges "{{^packageName}}" "{{^path#replace / *}}")
            [Append "bbb\n", Append "ddd\n", Append "ccc\n", SortLinesDesc]
        ]
      expected = "lil.word.packageddd\nccc\nbbb\naaa\nlil*word*package"
  it "sampe5" $
    shouldBe (execModifications config content descriptors) (Right $ expected)

errSample1 = do
  let config = ValuesAndConfig values placeHolderConfig
      content =
        [r|
lil.word.package
lil*word*package
|]
      label = Just $ T.unpack "label"
      descriptors =
        [ Descriptor
            label
            (Just $ Edges "{{^package_name}}" "{{^path#replace / *}}")
            [Append "ccc\n"]
        ]
      expected = Left $ SegmentParsingError label $ T.unpack "package_name"
  it "errSample1" $
    shouldBe (execModifications config content descriptors) expected

errSample2 = do
  let config = ValuesAndConfig values placeHolderConfig
      content = "tag tag"
      label = Just $ T.unpack "label"
      descriptors =
        [ Descriptor
            label
            (Just $ Edges "tag" "tag")
            [Append "{{^package_name}}"]
        ]
      expected = Left $ SegmentParsingError label $ T.unpack "package_name"
  it "errSample2" $
    shouldBe (execModifications config content descriptors) expected

values =
  Map.fromList $
  [ (TemplateValueId "featureName", [Single "word"])
  , ( TemplateValueId "packageName"
    , [Single "lil.", Compound [] (fromList [Single "word"]), Single ".package"])
  , ( TemplateValueId "path"
    , [ Compound
          [Replace "." "/"]
          (fromList
             [ Single "lil."
             , Compound [] (fromList [Single "word"])
             , Single ".package"
             ])
      ])
  ]

placeHolderConfig =
  PlaceholderConfig
    { openTag = "{{"
    , closeTag = "}}"
    , valuePrefix = "^"
    , modifierSeparator = "#"
    }
