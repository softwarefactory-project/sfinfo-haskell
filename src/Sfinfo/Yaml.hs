-- | A format preserving yaml transformer
module Sfinfo.Yaml
  ( transformYaml,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.YAML.Event
import Data.YAML.Token (Encoding (UTF8))
import qualified Data.Yaml
import Debug.Trace (trace)

headBodyTail :: [a] -> Int -> ([a], [a], [a])
headBodyTail l c = (begin, body, end)
  where
    (begin, rest) = splitAt c l
    (body, end) = splitAt (length rest - c) rest

--transform :: BSL.ByteString -> BSL.ByteString
transformYaml :: (ToJSON a, FromJSON a) => (Maybe Text -> a -> a) -> BSL.ByteString -> BSL.ByteString
transformYaml callBack inputDocument =
  writeEvents UTF8 $
    go (inputEvents inputDocument) Nothing Nothing []
  where
    objToEvent obj = objBody
      where
        doc = inputEvents $ BSL.fromStrict $ Data.Yaml.encode obj
        (_, objBody, _) = headBodyTail doc 2
    inputEvents bs = map eEvent $ rights $ parseEvents bs
    -- encodeSubMap yMap = toStrict $ writeEventsText $ [StreamStart, DocumentStart NoDirEndMarker] <> yMap <> [DocumentEnd False, StreamEnd]
    encodeSubMapBS yMap = BSL.toStrict $ writeEvents UTF8 $ [StreamStart, DocumentStart NoDirEndMarker] <> yMap <> [DocumentEnd False, StreamEnd]
    processMap :: [(Maybe Text, Event)] -> [Event]
    processMap tupleMap =
      --trace ("\n==DEBUG== " <> show keyName <> "\n" <> T.unpack innerMapEncoded <> "\n==END==\n")
      outputMap
      where
        -- innerMapEncoded = encodeSubMap yMap
        outputMap = case Data.Yaml.decodeEither' (encodeSubMapBS yMap) of
          Right obj -> objToEvent $ callBack keyName obj
          Left _ -> begin <> go body Nothing Nothing [] <> end
        keyName = fst $ head tupleMap
        yMap = map snd tupleMap
        (begin, body, end) = headBodyTail yMap 1
    go ::
      -- | The input event list
      [Event] ->
      -- | Opening map counter
      Maybe Int ->
      -- | The last key name
      Maybe Text ->
      -- | The accumulated (lastKey, elements) of the current map
      [(Maybe Text, Event)] ->
      -- | The transformed list of event
      [Event]
    go [] Nothing _ _ = []
    -- Push comment after mapping end
    go (c@(Comment _) : me@MappingEnd : xs) count lastKey acc = go (me : c : xs) count lastKey acc
    -- A complete map happen, process it
    go xs (Just 0) _ acc = processMap (reverse acc) <> go xs Nothing Nothing []
    -- A new map begin
    go (e@MappingStart{} : es) count lastKey acc = go es (Just $ 1 + fromMaybe 0 count) lastKey ((lastKey, e) : acc)
    -- A previous map ended
    go (e@MappingEnd : es) count lastKey acc = go es (Just $ (-1) + fromMaybe 0 count) lastKey ((Nothing, e) : acc)
    -- The rest of the elements
    go (x : xs) count lastKey acc = case count of
      -- We are in a map, accumulate elements
      Just _ -> go xs count newLastKey ((lastKey, x) : acc)
      -- Otherwise we continue
      Nothing -> x : go xs count newLastKey acc
      where
        newLastKey = case x of
          Scalar _ _ Plain value -> Just value
          _ -> Nothing
    go _ _ _ _ = error "Unknown state"
