module Z.Node.H2h.Builder.Startgg.Queries
  ( IdStub
  , ImagesStub
  , PageInfo
  , PageNode
  , PageNodes
  , TourneyDataRes
  , TourneyDataVars
  , phaseGroupData
  , tourneyData
  , tourneyDataSmall
  ) where

import Z.Node.Gql as Gql
import Z.Node.H2h.Builder.Startgg.Queries.PhaseGroupData as PGDQ
import Z.Node.H2h.Builder.Startgg.Queries.TourneyData as TDQ
import Z.Node.H2h.Builder.Startgg.Queries.TourneyDataSmall as TDSQ
import Z.Z as Z

tourneyData :: Gql.Operation TourneyDataVars TourneyDataRes
tourneyData = Gql.defOperation TDQ.q Z.Proxy Z.Proxy

tourneyDataSmall :: Gql.Operation TourneyDataVars TourneyDataRes
tourneyDataSmall = Gql.defOperation TDSQ.q Z.Proxy Z.Proxy

phaseGroupData :: Gql.Operation PhaseGroupDataVars PhaseGroupDataRes
phaseGroupData = Gql.defOperation PGDQ.q Z.Proxy Z.Proxy

type PhaseGroupDataVars = { page :: Int, phaseGroupId :: Int }

type TourneyDataVars = { pageE :: Int, pageS :: Int, slug :: String }

type TourneyDataRes =
  { event ::
      { id :: Int
      , name :: String
      , slug :: String
      , state :: String
      , tournament ::
          { id :: Int
          , name :: String
          , endAt :: Int
          , images :: ImagesStub
          }
      , standings ::
          PageInfo
            (placement :: Int, isFinal :: Boolean, entrant :: IdStub)
      , entrants ::
          PageInfo
            ( initialSeedNum :: Int
            , participants ::
                Array
                  { gamerTag :: String
                  , prefix :: Z.Maybe String
                  , player ::
                      { id :: Int
                      , gamerTag :: String
                      , prefix :: Z.Maybe String
                      , user ::
                          Z.Maybe
                            { genderPronoun :: Z.Maybe String
                            , name :: Z.Maybe String
                            , images :: ImagesStub
                            , authorizations ::
                                Z.Maybe
                                  ( Array
                                      { externalUsername :: String
                                      , type :: String
                                      }
                                  )
                            }
                      }
                  }
            )
      , phaseGroups ::
          Array
            { id :: Int
            , displayIdentifier :: String
            , phase :: { id :: Int, name :: String, phaseOrder :: Int }
            }
      }
  }

type PhaseGroupDataRes =
  { id :: Int
  , bracketType :: String
  , sets ::
      PageInfo
        ( fullRoundText :: String
        , round :: Int
        , wPlacement :: Int
        , lPlacement :: Int
        , identifier :: String
        , displayScore :: Z.Maybe String
        , winnerId :: Z.Maybe Int
        , slots :: Array { entrant :: IdStub }
        , state :: Int
        , games ::
            Z.Maybe
              ( Array
                  { winnerId :: Z.Maybe Int
                  , orderNum :: Int
                  , entrant1Score :: Int
                  , entrant2Score :: Int
                  , selections ::
                      Array
                        { id :: Int
                        , entrant :: IdStub
                        , orderNum :: Int
                        , selectionValue :: String
                        , character :: { id :: Int, name :: String }
                        }
                  }
              )
        )
  }

type ImagesStub = Array { url :: String, type :: String }

type PageNode rest = { id :: Int | rest }

type PageNodes rest = Array (PageNode rest)

type PageInfo rest =
  { pageInfo :: { total :: Int }, nodes :: PageNodes rest }

type IdStub = { id :: Int }
