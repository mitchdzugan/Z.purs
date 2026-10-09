import { WholeHistoryRating } from "whr";

export const js_whr_new = (opts) => () => new WholeHistoryRating(opts);
export const js_whr_createGame =
  (pWin) => (pLoss) => (timestep) => (handicap) => (whr) => () =>
    whr.createGame(pWin, pLoss, "B", timestep, handicap);
export const js_whr_iterate = (count) => (whr) => () => whr.iterate(count);
export const js_whr_ratingsForPlayer = (p) => (whr) => () =>
  whr.ratingsForPlayer(p);
