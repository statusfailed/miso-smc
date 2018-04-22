{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Main where

import GHC.Generics

import Control.Monad
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

import Data.Aeson
import qualified Data.Aeson.Types as Aeson

import SMC as SMC
import SMC.Render as SMC

import Data.Map (Map(..))
import qualified Data.Map as Map

import Text.Read

default (MisoString)

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
    Just gen -> SMC.toView Map.empty genLabel genType gen
    Nothing  -> "toGraph error"

viewShow :: Show a => a -> View action
viewShow = text . S.ms . show

data Model = Model
  { generatorsJSON :: MisoString
  , generators     :: Map String (Int, Int)
  , patternParse   :: Either String (Hypergraph Int String)
  , graphParse     :: Either String (Hypergraph Int String)
  , matchConvex    :: Bool -- accept convex matches only?
  } deriving(Eq, Read, Show, Generic)

emptyModel :: Model
emptyModel = Model "" Map.empty (Left "no data") (Left "no data") False

data Msg
  = NoOp
  | EditGeneratorsJSON MisoString
  | EditPatternAST MisoString
  | EditGraphAST MisoString
  | MatchConvex Bool

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

updateModel (EditPatternAST s) m@Model{..} = do
  pure $ m { patternParse = parseGraph generators (S.fromMisoString s) }

updateModel (EditGraphAST s) m@Model{..} = do
  pure $ m { graphParse = parseGraph generators (S.fromMisoString s) }

updateModel (MatchConvex x) m = pure $ m { matchConvex = x }

updateModel _ m = pure m



parseGraph
  :: Map String (Int, Int) -> String
  -> Either String (Hypergraph Int String)
parseGraph gs s = readEither s >>= f
  where
    f = maybe (Left msg) Right . toGraph (flip Map.lookup gs)
    msg = "couldn't convert to graph. Check generator names + types match?"


viewModel :: Model -> View Msg
viewModel m@Model{..} = bsRows
  [ viewHeader
  , [rowSpacer]
  , viewSignature generators
  , [rowSpacer]
  , patternGraphEditor m
  , [rowSpacer]
  , viewMatching m
  , [rowSpacer]
  , [div_ [class_ "col-12"]
      [ h4_ [] ["debugging"]
      , pre_ [] [text . S.ms . show $ m]
      ]
    ]
  ]

rowSpacer :: View action
rowSpacer = div_ [class_ "col-12", style_ s] []
  where s = Map.singleton "min-height" "3rem"

----------- Bootstrap utils --------------

col4 = div_ [ class_ "col-4" ]
col6 = div_ [ class_ "col-6" ]
col12 = div_ [ class_ "col-6" ]

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
viewHeader = [ col12 [h1_ [style_ s] ["SMC Matching Demo"]] ]
  where s = Map.singleton "text-align" "center"

------------ Generator Setup views -----------------

exampleGenerators :: MisoString
exampleGenerators =
  "{\n  \"E1\": [1, 2],\n  \"E2\": [2, 1],\n  \"E3\": [1, 1],\n  \"E4\": [1, 1]\n}\n"

viewSignature :: Map String (Int, Int) -> [View Msg]
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
viewGeneratorsTable :: Map String (Int, Int) -> View action
viewGeneratorsTable gs = table_ [class_ "table"] [h, b]
  where
    h = thead_ [] [tr_ [] $ fmap th ["label", "inputs", "outputs", "diagram"]]
    th s = th_ [scope_ "col"] [text s]

    b = tbody_ [] . fmap (tr . mkGen) $ Map.toList gs

    tr g@(Gen s i o) = tr_ [scope_ "row"] $
      fmap (td_ [] . pure) [text (S.ms s), viewShow i, viewShow o, genToSvg g]

    mkGen (s,(i,o)) = Gen s i o


---------------- Pattern and Context graph -------------------

exampleGraphExpression :: MisoString
exampleGraphExpression = "Seq\n  (Seq (Generator \"E1\")\n       (Par (Generator \"E3\") Id)\n  )\n  (Generator \"E2\")\n"

examplePatternExpression :: MisoString
examplePatternExpression = "Seq\n  (Seq (Par Id (Generator \"E1\"))\n       (Par Twist Id)\n  )\n  (Par Id (Generator \"E2\"))\n"

patternGraphEditor :: Model -> [View Msg]
patternGraphEditor m = [left, center, right] where
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
    , editBox "graph in which to search" EditGraphAST
    , displayGraph (graphParse m) (generators m)
    ]

  right  = col4
    [ h4_ [] ["pattern expression AST"]
    , editBox "pattern to find match for" EditPatternAST
    , displayGraph (patternParse m) (generators m)
    ]

-- TODO: lose Map.! - fix hypergraph-rewriting to error if gen doesn't exist.
displayGraph
  :: Either String (Hypergraph Int String)
  -> Map String (Int, Int)
  -> View action
displayGraph hg gs =
  case hg of
    Left  e -> div_ [] [text $ S.ms e]
    Right v -> SMC.toView Map.empty id (gs Map.!) v


-------------- Matching --------------

checkbox :: Bool -> (Bool -> action) -> View action
checkbox x f =
  input_
    [ class_ "form-check-input"
    , type_ "checkbox"
    , checked_ x
    , onClick (f $ not x)
    ] []

viewMatching :: Model -> [View Msg]
viewMatching m@Model{..} = [left, right]
  where
    left  = col6
      [ h4_ [] ["Matching Options"]
      , p_  []
        [ checkbox matchConvex MatchConvex
        , "convex matches only"
        ]
      ]

    right = col6 $ case liftM2 (,) graphParse patternParse of
      Right (g, p) ->
        let
          filterMatch m = case matchConvex of
            True  -> convexVE g $ matchingToVE m
            False -> True
        in case filter filterMatch (match g p) of
              (x:_) -> [ highlightView generators x g ]
              _ -> ["no matches found"]

      Left  err    -> [text . S.ms $ err]

highlightView :: Map String (Int, Int) -> Matching -> Hypergraph v String -> View action
highlightView gs m =  SMC.toView stroke id (gs Map.!)
  where stroke = mkStroke m

mkStroke :: Matching -> Map VE String
mkStroke (Matching nodes edges) = Map.fromList (vs ++ es)
  where
    vs = fmap ((,"red") . V . snd) (Bimap.toList nodes)
    es = fmap ((,"red") . E . snd) (Bimap.toList edges)


-- Badly placed functions

matchingToVE :: Matching -> [VE]
matchingToVE (Matching nodes edges) =
  fmap V (Bimap.elems nodes) ++ fmap E (Bimap.elems edges)
