{-# LANGUAGE BlockArguments #-}

module FoldToTreeSpec (spec) where

import Data.Tree (Tree (Node))
import Test.Hspec (Spec, describe, it, shouldBe)

import FoldToTree (Chapter, Item (Item, level, text), withBuilder, withFoldl)

spec :: Spec
spec = do
  describe "withoBuilder" do
    it "returns chapters" do
      withBuilder input `shouldBe` output
  describe "withFoldl" do
    it "returns chapters" do
      withFoldl input `shouldBe` output

input :: [Item]
input =
  [ Item {level = 1, text = "1章 JavaScriptの第一歩"},
    Item {level = 1, text = "2章 データ型と変数"},
    Item {level = 2, text = "2.1 変数の識別"},
    Item {level = 2, text = "2.2 スコープ"},
    Item {level = 1, text = "第3章 演算子と文"},
    Item {level = 2, text = "3.1 文の形式"},
    Item {level = 2, text = "3.2 単純文"},
    Item {level = 3, text = "3.2.1 代入文"},
    Item {level = 3, text = "3.2.2 演算文"}
  ]

output :: [Chapter]
output =
  [ pure "1章 JavaScriptの第一歩",
    Node
      "2章 データ型と変数"
      [ pure "2.1 変数の識別",
        pure "2.2 スコープ"
      ],
    Node
      "第3章 演算子と文"
      [ pure "3.1 文の形式",
        Node
          "3.2 単純文"
          [ pure "3.2.1 代入文",
            pure "3.2.2 演算文"
          ]
      ]
  ]
