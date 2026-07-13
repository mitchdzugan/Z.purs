import { gql, GraphQLClient } from "graphql-request";

export const jsRequestGql = (apiUrl) => (authToken) => (query) => (vars) => {
  const grClient = new GraphQLClient(
    apiUrl,
    !authToken
      ? {}
      : {
          headers: { authorization: `Bearer ${authToken}` },
        },
  );
  const q = gql(query.split("\n"));
  return grClient.request({
    document: q,
    ...(vars ? { variables: vars } : {}),
  });
};
