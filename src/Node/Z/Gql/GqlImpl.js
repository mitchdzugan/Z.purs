import { gql, GraphQLClient } from "graphql-request";

export const js_requestGql =
  (mkResponseError) =>
  (Right) =>
  (apiUrl) =>
  (authToken) =>
  (query) =>
  (vars) =>
  () => {
    const headers = { authorization: `Bearer ${authToken}` };
    const grClient = new GraphQLClient(apiUrl, !authToken ? {} : { headers });
    const document = gql(query.split("\n"));
    return grClient
      .request({ document, ...(vars ? { variables: vars } : {}) })
      .then((res) => Right(res))
      .catch((e) => {
        if (e && e.response && e.request) {
          return mkResponseError(e.request.query)(e.request.variables)(
            e.response.data,
          )(e.response.errors)(e.response.extensions)(e.response.status)(
            e.response.headers,
          );
        }
        throw e;
      });
  };
