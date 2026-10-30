module Node.Z.H2h.Challonge
  ( getEventData
  ) where

import Node.Z.Prelude

import Node.Z.Gql as Gql
import Node.Z.H2h.Builder as B
import Node.Z.Puppeteer as P
import Z.Bk.Elimination.Round as Round
import Z.CLM.Stats.Season (tz'chicago)
import Z.Gql.Error as GqlE
import Z.Gql.Warning as GqlW
import Z.H2h.Error as H2hE
import Z.H2h.Module
  ( p'baseSets
  , p'depth
  , p'entrants
  , p'gfEIds
  , p'hasReset
  , p'isDropRound
  , p'nonGfEIds
  , p'prev
  , p'roundInd
  , p'winnerId
  )
import Z.H2h.Module as H2h
import Z.H2h.Warning as H2hW

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ x'withReturn \xReturn -> do
  { client, networkControl, slug } <- r'ask
  let { cachePath } = client
  let eCacheOnlyEmpty = H2hE.Gql GqlE.CacheOnlyEmpty
  cached <- getCached slug cachePath networkControl
  whenJust cached xReturn
  when (networkControl == Gql.CacheOnly) $ e'fail eCacheOnlyEmpty
  res <- getEventDataImpl
  writeToCache slug cachePath res
  pure res
  where
  fullPath slug path = path /./ ("CHALLONGE-" <> slug <> ".json")
  writeToCache _ Nothing _ = pure unit
  writeToCache slug (Just path) res =
    we'tellMappedHush (H2hW.Gql <<< GqlW.CacheWrite) $ x'encodeTextFileP
      (fullPath slug path)
      res
  getCached _ Nothing _ = pure Nothing
  getCached _ _ Gql.ForceFetch = pure Nothing
  getCached slug (Just path) _ = we'tellMappedMHush mapMDecodeErr $ Just
    <$> x'decodeTextFile @H2h.Event (fullPath slug path)
  mapMDecodeErr e@(DecodeError _) = [ H2hW.Gql $ GqlW.CacheDecode e ]
  mapMDecodeErr _ = []

getEventDataImpl :: forall x. H2h.Event <@@ B.BuildX x
getEventDataImpl = do
  P.xUseBrowser H2hE.PuppeteerBrowserResource browserOpts $ \browser -> do
    x'info { op: "newPage" }
    page <- pDo "newPage" "" $ P.xNewPage browser
    x'info { op: "xSetViewport" }
    pDo "xSetViewport" "1920x1080" $ P.xSetViewport page 1920 1080
    { slug } <- r'ask
    let url = "https://challonge.com/" <> slug
    x'info { op: "xGoto", url }
    pDo "xGoto" url $ P.xGoto page url do
      s'setPure'b @"waitUntil" P.DOMContentLoaded
    pWaitFor page ".redesigned-meta-list .item .text"
    pWaitFor page ".title #title"
    pWaitFor page ".bracket-svg .match .match--player"
    s'eval initialState
      $ x'eval_ @"baseSets" @(X'HashMap Int BaseSet)
      $ x'eval_ @"entrants" @(X'HashMap SorN H2h.Entrant)
      $ readPageData page
  where
  initialState =
    { isDE: false
    , nameOrE: Left $ H2hE.MissingData "event.name"
    , dateOrE: Left $ H2hE.MissingData "event.date"
    }

  readPageData page = do
    { slug } <- r'ask
    itemEls <- pEls page ".redesigned-meta-list .item"
    forM_ itemEls $ \el -> do
      itemLabel <- pEl el ".item-label" >>= pInnerText
      itemText <- pEl el ".text" >>= pInnerText
      when (itemLabel == "Start Time" || itemLabel == "Start") do
        date <- H2hE.ParseTime !$! e'runParser itemText parseDate
        s'setPure'b @"dateOrE" date
      when (itemLabel == "Game") do
        s'setPure'b @"nameOrE" itemText
      when (itemLabel == "Format") do
        s'set'b @"isDE" $ itemText == "Double Elimination"
    name <- s'view'b @"nameOrE" >>= e'ok
    date <- s'view'b @"dateOrE" >>= e'ok
    isDE <- s'view'b @"isDE"
    tournamentName <- pEl page ".title #title" >>= pInnerText
    bracketEls <- pEls page ".bracket-svg"
    forM_ bracketEls $ \bracketEl -> do
      matchEls <- pEls bracketEl ".match"
      forM_ matchEls \matchEl -> x'eval_ @"winnerId" @(X'Ref (Maybe SorN)) do
        setId <- pReadDataAttr matchEl "match" >>=
          \matchAttr -> H2hE.ParseMatchId !$! e'runParser matchAttr parseInt
        playerEls <- pEls matchEl ".match--player"
        slots <- forM playerEls $ \playerEl -> do
          entrantId <- pReadIdDataAttr playerEl "participant"
          playerName <- pEl playerEl "title" >>= pInnerHtml
          scoreEl <- pEl playerEl ".match--player-score"
          scoreClass <- pGetAttribute scoreEl "class"
          scoreS <- pInnerHtml scoreEl
          score <- H2hE.ParseScore !$! do
            e'runParser scoreS parseInt <#> H2h.mkScoreCount
          forM_ (str'split (Pattern " ") scoreClass) $ \cn -> do
            when (cn == "-winner") $ p'winnerId @<=> entrantId
          let
            participant =
              { prefix: Nothing
              , gamerTag: playerName
              , playerOrder: 1
              , player:
                  { id: sOrN $ "Challonge-" <> playerName <> "-playerId"
                  , gamerTag: playerName
                  , prefix: Nothing
                  , pronouns: Nothing
                  , name: Nothing
                  , socials: hm'empty
                  , images: hm'empty
                  }
              }
          x'insert @"entrants" entrantId
            { id: entrantId
            , standing: { placement: 0, isFinal: false }
            , participants: [ participant ]
            , participant
            }
          pure { entrantId: Just entrantId, score }
        let emptySlot = { entrantId: Nothing, score: H2h.NoScore }
        let slotA = jOr emptySlot (nth slots 0)
        let slotB = jOr emptySlot (nth slots 1)
        winnerId <- (@.) p'winnerId
        let
          winner =
            if isNothing winnerId then Nothing
            else if winnerId == slotA.entrantId then Just pos
            else Just neg
        let baseSet = { winner, id: setId, slots: slotA ~ slotB }
        x'insert @"baseSets" setId baseSet
    baseSetList <- (@.) p'baseSets
      <#> arr'reverse
      <<< arr'sortWith (g_ @"id")
      <<< hm'vals
    isComplete <- x'withReturn \xReturn -> do
      forM_ baseSetList $ \baseSet -> do
        when (isNothing baseSet.winner) (xReturn false)
      pure true
    let
      runSetsLoop =
        x'eval_ @"prev" @(X'Ref (Maybe { base :: BaseSet, round :: Round.T }))
          <<< x'eval @"depth" @(X'Ref Int) 0
          <<< x'eval @"roundInd" @(X'Ref Int) 0
          <<< x'eval @"isDropRound" @(X'Ref Boolean) true
          <<< x'eval @"hasReset" @(X'Ref Boolean) false
          <<< x'eval_ @"gfEIds" @(X'HashSet SorN)
          <<< x'eval_ @"nonGfEIds" @(X'HashSet SorN)
    roundSets <- runSetsLoop do
      roundSets' <- forM baseSetList $ \baseSet -> do
        prev <- (@.) p'prev
        isDropRound <- (@.) p'isDropRound
        let prevSet = prev <#> \p -> p.base
        let prevRound = prev <#> \p -> p.round
        let wasGrands = jOrF $ prevRound <#> Round.isGrands
        let wasLosers = jOrF $ prevRound <#> Round.isLosers
        let sameSlots = (slotsKey baseSet) == mSlotsKey prevSet
        let isGrands = isNothing prev && isDE || (wasGrands && sameSlots)
        when (isGrands && wasGrands) $ p'hasReset @= true
        forM_ (arr'fromFoldable baseSet.slots) $ \slot -> do
          whenJust slot.entrantId $ \entrantId -> do
            (if isGrands then x'cons @"gfEIds" else x'cons @"nonGfEIds")
              entrantId
        gfEIds <- (@.) p'gfEIds
        nonGfEIds <- (@.) p'nonGfEIds
        seenAllGFEntrants <- x'withReturn \xReturn -> do
          forM_ (hs'vals gfEIds) $ \id -> do
            when (not (hs'has id nonGfEIds)) (xReturn false)
          pure true
        let nowLosers = (not isGrands) && (wasGrands || wasLosers)
        let isLosers = nowLosers && not seenAllGFEntrants
        when ((not isLosers && wasLosers) || (not isGrands && wasGrands)) do
          p'depth @= 0
        setDepth <- (@.) p'depth
        let round = elimRound isDE setDepth isGrands isLosers isDropRound
        let slotA ~ slotB = baseSet.slots
        whenJust slotA.entrantId \eA -> whenJust slotB.entrantId \eB -> do
          whenJust baseSet.winner \w -> when isComplete do
            let
              setFinStanding = \id placement -> x'modify @"entrants" id $
                rec'set @"standing" { placement, isFinal: true }
            let wId ~ lId = if w == pos then eA ~ eB else eB ~ eA
            if isGrands && not wasGrands then do
              setFinStanding wId 1
              setFinStanding lId 2
            else if isLosers then do
              let p2Inc = inc $ p2 setDepth
              setFinStanding lId $ if isDropRound then p2Inc else 3 * p2Inc / 2
            else if isDE && setDepth == 0 then do
              setFinStanding wId 1
              setFinStanding lId 2
            else if isDE then do
              setFinStanding lId $ inc $ p2 setDepth
            else pure unit
        p'roundInd @# inc
        depth <- (@.) p'depth
        roundInd <- (@.) p'roundInd
        when (p2 depth <= roundInd) do
          p'roundInd @= 0
          if (isDropRound && isLosers) then do p'isDropRound @= false
          else do
            p'isDropRound @= true
            p'depth @# inc
        p'prev @<=> { base: baseSet, round }
        p'prev @<=> { base: baseSet, round }
        pure $
          { round
          , set:
              { isDQ: false
              , isBye: false
              , roundText: ""
              , overrideScoreText: Nothing
              , doesCount: isComplete
              , id: sOrN $ slug <> show baseSet.id
              , winner: baseSet.winner
              , slots: baseSet.slots
              , eventOrder: -1 * (Round.roundInd round)
              , games: []
              }
          }
      hasReset <- (@.) p'hasReset
      if (not hasReset) then pure roundSets'
      else pure $ set (ix 0 # o_ @"round") (Round.Grands true) roundSets'
    let lSets = arr'filter (Round.isLosers <<< g_ @"round") roundSets
    let wSets = arr'filter (Round.isWinners <<< g_ @"round") roundSets
    let maxWR = jOr0 $ maximum $ wSets <#> Round.roundTypeInd <<< g_ @"round"
    let maxLR = jOr0 $ maximum $ lSets <#> Round.roundTypeInd <<< g_ @"round"
    entrants <- (@.) p'entrants
    let profileImageUrl = "https://i.imgur.com/7MsdKge.jpeg"
    let
      mkSet = \rs -> rs.set `(~.) @"roundText"` roundLabel rs.round maxWR maxLR
    let mkSetT = \rs -> Tuple rs.set.id $ mkSet rs
    let sets = hm'fromFoldable $ roundSets <#> mkSetT
    pure
      { id: sOrN $ "Challonge-" <> slug <> "-eventId"
      , name
      , slug
      , state: if isComplete then "COMPLETED" else "ACTIVE"
      , site: H2h.Challonge
      , phaseGroups:
          [ { id: sOrN 1
            , phase: { id: sOrN 1, name: "Bracket", phaseOrder: 1 }
            , displayIdentifier: "1"
            , sets
            }
          ]
      , entrants
      , tournament:
          { id: sOrN $ "Challonge-" <> slug <> "-tournamentId"
          , name: tournamentName
          , images: hm'fromFoldable [ H2h.Profile /\ profileImageUrl ]
          , date: date
          }
      }
  browserOpts = do
    let uaOpt = "--user-agent=" <> userAgent
    s'set'b @"args" [ uaOpt, "--no-sandbox", "--disable-setuid-sandbox" ]
  mSlotsKey (Just { slots: (eA ~ eB) }) =
    str'joinWith "|" $ arr'sort
      [ mEntrantIdKey eA.entrantId, mEntrantIdKey eB.entrantId ]
  mSlotsKey _ = "__"
  slotsKey s = mSlotsKey $ Just s
  mEntrantIdKey (Just id) = show id
  mEntrantIdKey _ = "_"
  -- elimRound isDE depth isGrands isLosers isDropRound
  elimRound _ _ true _ _ = Round.Grands false
  elimRound isDE depth _ false _ = Round.Winners isDE depth
  elimRound _ depth _ _ isDropRound = Round.Losers isDropRound depth
  roundLabel (Round.Grands true) _ _ = "Finals (reset)"
  roundLabel (Round.Grands _) _ _ = "Finals"
  roundLabel s@(Round.Losers _ _) _ maxL = (<>) "Losers Round " $ show
    $ maxL
    - Round.roundTypeInd s
    + 1
  roundLabel (Round.Winners true 0) _ _ = "Semifinals"
  roundLabel (Round.Winners false 0) _ _ = "Finals"
  roundLabel (Round.Winners false 1) _ _ = "Semifinals"
  roundLabel s@(Round.Winners _ _) maxW _ = (<>) "Winners Round " $ show
    $ maxW
    - Round.roundTypeInd s
    + 1

  pDo
    :: forall xx a
     . String
    -> String
    -> E' JsError + EA' H2hE.T xx @@> a
    -> EA' H2hE.T xx @@> a
  pDo s1 s2 m = H2hE.Puppeteer s1 s2 !$! m

  pDoPorE
    :: forall xx pOrE a
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> E' JsError + EA' H2hE.T xx @@> a
    -> EA' H2hE.T xx @@> a
  pDoPorE pOrE s m = H2hE.Puppeteer (P.context pOrE) s !$! m

  pEls
    :: forall xx pOrE
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> EA' H2hE.T xx @@> Array P.Element
  pEls pOrE sel = pDoPorE pOrE sel $ P.xEls pOrE sel

  pEl
    :: forall xx pOrE
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> EA' H2hE.T xx @@> P.Element
  pEl pOrE sel = pDoPorE pOrE sel $ P.xEl pOrE sel

  pInnerText pOrE = pDoPorE pOrE "innerText" $ P.xInnerText pOrE
  pInnerHtml pOrE = pDoPorE pOrE "innerHtml" $ P.xInnerHtml pOrE
  pGetAttribute pOrE attr = pDoPorE pOrE ("getAttribute('" <> attr <> "')") $
    P.xGetAttribute pOrE attr
  pReadDataAttr e n = pGetAttribute e ("data-" <> n <> "-id")
  pReadIdDataAttr e n = pReadDataAttr e n <#> sOrN
  pWaitFor page sel = pDo "waitFor" sel do
    x'info { op: "waitFor", sel }
    P.xWaitForSelector page sel $ s'setPure'b @"timeout" 120000

userAgent :: String
userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36" <+>
  "(KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

parseMonth :: forall m. ParserT String m Int
parseMonth = parseTry (parseStringAs "January" 1)
  <|> parseTry (parseStringAs "February" 2)
  <|> parseTry (parseStringAs "March" 3)
  <|> parseTry (parseStringAs "April" 4)
  <|> parseTry (parseStringAs "May" 5)
  <|> parseTry (parseStringAs "June" 6)
  <|> parseTry (parseStringAs "July" 7)
  <|> parseTry (parseStringAs "August" 8)
  <|> parseTry (parseStringAs "September" 9)
  <|> parseTry (parseStringAs "October" 10)
  <|> parseTry (parseStringAs "November" 11)
  <|> parseTry (parseStringAs "December" 12)
  <|> parseFail "Expected %Month%"

parseAMorPM :: forall m. ParserT String m Boolean
parseAMorPM = parseTry (parseStringAs "AM" false)
  <|> parseTry (parseStringAs "PM" true)
  <|> parseFail "Expected AM|PM"

parseDate :: forall m. ParserT String m DateTime
parseDate = do
  month <- parseMonth
  parseString_ " "
  day <- parseInt
  parseString_ ","
  parseString_ " "
  year <- parseInt
  parseString_ " at "
  hour <- parseInt
  let baseDate = dateTime'mkDate year month day tz'chicago
  parseString_ ":"
  minute <- parseInt
  let timeSpan = TS'hours hour :+: TS'minutes minute
  parseString_ " "
  isPM <- parseAMorPM
  let pmTimeSpan = TS'hours $ toNumber $ if isPM then 12 else 0
  pure $ baseDate :~: timeSpan :+: pmTimeSpan

type BaseSet =
  { id :: Int
  , winner :: Maybe PairKey
  , slots :: Pair H2h.Slot
  }
