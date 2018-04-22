{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S

import qualified Data.Map as Map

import Data.Aeson
import qualified Data.Aeson.Types as Aeson

import SMC as SMC
import SMC.Render as SMC

instance (Generic e, ToJSON e) => ToJSON (Hyperedge e)
instance (Generic e, FromJSON e) => FromJSON (Hyperedge e)

instance (Generic v, ToJSON v, Generic e, ToJSON e) => ToJSON (Hypergraph v e)
instance (Generic v, FromJSON v, Generic e, FromJSON e) => FromJSON (Hypergraph v e)

instance (Generic e, ToJSON e) => ToJSON (Expr e) where
  toJSON = genericToJSON Aeson.defaultOptions {
    Aeson.sumEncoding = Aeson.TwoElemArray }

instance (Generic e, FromJSON e) => FromJSON (Expr e) where
  parseJSON = genericParseJSON Aeson.defaultOptions {
    Aeson.sumEncoding = Aeson.TwoElemArray }

default (MisoString)

{-["Seq",-}
  {-[["Seq",[["Generator","EB1"],["Par",[["Generator","EB3"],["Id",[]]]]]],["Generator","EB2"]]]-}

data Gen = Gen String Int Int
  deriving(Eq, Read, Show, Generic)

instance ToJSON   Gen
instance FromJSON Gen

genLabel :: Gen -> String
genLabel (Gen x _ _) = x

genType :: Gen -> (Int, Int)
genType (Gen _ i o) = (i, o)

genToSvg :: Gen -> View action
genToSvg g =
  case toGraph (return . genType) (Generator g) of
    Just gen -> SMC.toView genLabel genType gen
    Nothing  -> "toGraph error"

viewShow :: Show a => a -> View action
viewShow = text . S.ms . show

data Model = Model
  { generatorsJSON :: MisoString
  , generators     :: [Gen]
  , patternJSON    :: MisoString
  , graphJSON      :: MisoString
  } deriving(Eq, Read, Show, Generic)

emptyModel :: Model
emptyModel = Model gs [] p g
  where
    gs = ""
    p  = ""
    g  = ""

data Msg
  = NoOp
  | EditGeneratorsJSON MisoString
  | EditPatternJSON MisoString
  | EditGraphJSON MisoString

instance ToJSON Model

main :: IO ()
main = startApp App { initialAction = NoOp, ..}
  where
    model = emptyModel
    update = updateModel
    view = viewModel
    events = defaultEvents
    mountPoint = Nothing
    subs = []

updateModel :: Msg -> Model -> Effect Msg Model
updateModel (EditGeneratorsJSON s) m = do
  case eitherDecode (S.fromMisoString s) of
    Left err -> pure $ m
    Right x  -> pure $ m { generators = x }
updateModel (EditPatternJSON s) m = do
  pure $ m { patternJSON = s }
updateModel _ m = pure m

viewModel :: Model -> View Msg
viewModel Model{..} = bsRows
  [ viewHeader
  , [rowSpacer]
  , viewSignature generators
  , [rowSpacer]
  , patternGraphEditor
  ]

rowSpacer :: View action
rowSpacer = div_ [class_ "col-12", style_ s] []
  where s = Map.singleton "min-height" "3rem"

----------- Bootstrap utils --------------

bsRows :: [[View action]] -> View action
bsRows rows = div_ [class_ "container"] $ fmap f rows
  where
    f x = div_ [class_ "row"] x

col2 :: ([View action], [View action]) -> [View action]
col2 (ls, rs) =
  [ div_ [ class_ "col-6" ] ls
  , div_ [ class_ "col-6" ] rs
  ]

-- Textarea edit box
editBox :: MisoString -> (MisoString -> action) -> View action
editBox ph f = textarea_ [onInput f, placeholder_ ph, style_ s] []
  where s = Map.fromList [("width", "100%"), ("font-family", "mono")]

------------ Title -------------

viewHeader :: [View action]
viewHeader = [div_ [class_ "col-12"]
      [h1_ [style_ s] ["SMC Matching Demo"]]
    ]
  where s = Map.singleton "text-align" "center"

------------ Generator Setup views -----------------

exampleGenerators :: MisoString
exampleGenerators =
  "[[\"E1\", 1, 2], \n [\"E2\", 2, 1], \n [\"E3\", 1, 1], \n [\"E4\", 1, 1]]"

viewSignature :: [Gen] -> [View Msg]
viewSignature gs = [left, right] where
  left = div_ [ class_ "col-6" ]
    [ h4_ [] ["Generators"]
    , div_ []
      [ p_ [] ["Enter some example generators in JSON format. For example:"]
      , pre_ [] [text exampleGenerators]
      , p_ [] ["creates two generators labeled E1 and E2 with types (1,2) and (2,1)."]
      ]
    , editBox "enter JSON-encoded list of generators" EditGeneratorsJSON
    , p_ [] ["You should see the parsed generators in the table to the right"]
    ]

  right = div_ [ class_ "col-6" ] [viewGeneratorsTable gs]

-- | Display the current system's generators in a table.
viewGeneratorsTable :: [Gen] -> View action
viewGeneratorsTable gs = table_ [class_ "table"] [h, b]
  where
    h = thead_ [] [tr_ [] $ fmap th ["label", "inputs", "outputs", "diagram"]]
    th s = th_ [scope_ "col"] [text s]

    b = tbody_ [] $ fmap tr gs
    tr g@(Gen s i o) = tr_ [scope_ "row"] $
      fmap (td_ [] . pure) [text (S.ms s), viewShow i, viewShow o, genToSvg g]


---------------- Pattern and Context graph -------------------

exampleGraphExpression :: MisoString
exampleGraphExpression = "Seq\n  (Seq (Generator \"E1\")\n       (Par (Generator \"E3\") Id)\n  )\n  (Generator \"E2\")\n"

examplePatternExpression :: MisoString
examplePatternExpression = "Seq\n  (Seq (Par Id (Generator \"E1\"))\n       (Par Twist Id)\n  )\n  (Par Id (Generator \"E2\"))\n"

patternGraphEditor :: [View Msg]
patternGraphEditor = [left, center, right] where
  col4 = div_ [ class_ "col-4" ]

  left = col4
    [ h4_ [] ["Pattern + Context"]
    , div_ []
      [ p_ [] [ "Enter two expression ASTs: a pattern and a context in which to match it."]
      , p_ [] [ "Try this for the graph:"]
      , pre_ [] [text exampleGraphExpression]
      , p_ [] [ "And this for the pattern:"]
      , pre_ [] [text examplePatternExpression]
      ]
    ]

  center = col4
    [ h4_ [] ["graph expression AST"]
    , editBox "graph in which to search" EditGraphJSON
    ]

  right  = col4
    [ h4_ [] ["pattern expression AST"]
    , editBox "pattern to find match for" EditPatternJSON
    ]


page :: [([View action], [View action])] -> View action
page rows = div_ [class_ "container"] $ fmap f rows
  where
    f (l, r) =
      div_ [class_ "row"]
        [ div_ [ class_ "col-6" ] l
        , div_ [ class_ "col-6" ] r
        , hr_ [] []
        ]
