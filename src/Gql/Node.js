import { gql, GraphQLClient } from "graphql-request";

export const js_requestGql = (apiUrl) => (authToken) => (query) => (vars) => {
  const headers = { authorization: `Bearer ${authToken}` };
  const grClient = new GraphQLClient(apiUrl, !authToken ? {} : { headers });
  const document = gql(query.split("\n"));
  return grClient.request({ document, ...(vars ? { variables: vars } : {}) });
};
