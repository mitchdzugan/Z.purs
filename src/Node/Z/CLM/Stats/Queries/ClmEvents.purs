module Node.Z.CLM.Stats.Queries.ClmEvents
  ( q
  ) where

q :: String
q =
  """query TournamentQuery($after: Timestamp!, $before: Timestamp!, $page: Int!) {
  tournaments(
    query: {
      page: $page
      perPage: 50
      filter: {
        afterDate: $after
        beforeDate: $before
        videogameIds: [1]
        addrState: "IL"
        location: { distanceFrom: "41.881832, -87.623177", distance: "50mi" }
      }
    }
  ) {
    pageInfo {
      total
      page
    }
    nodes {
      id
      events(filter: { videogameId: [1] }) {
        slug
      }
    }
  }
}
"""