module Node.Z.CLM.Stats.App.HtmlDoc
  ( HtmlDocOpts
  , mkHtmlDoc
  ) where

import Z.Prelude

import Z.CLM.Stats.Types (ClmTimeline)

type HtmlDocOpts =
  { jsPath :: String
  , cssPath :: String
  , title :: String
  , body :: String
  }

mkHtmlDoc :: ClmTimeline -> Edit HtmlDocOpts -> String
mkHtmlDoc timeline edits = str'joinWith ""
  [ """
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>"""
  , opts.title
  , """</title>
        <link href="""
  , encode opts.cssPath
  , """ rel="stylesheet">
        <script>
            document.head[":tl"] = ("""
  , encode timeline
  , """);
        </script>
        <script defer src="""
  , encode opts.jsPath
  , """</script>
        <script>
            /*to prevent Firefox FOUC, this must be here*/
            let FF_FOUC_FIX;
        </script>
        <style>
@keyframes jump-in {
  0% {
    transform:scale(0.75);
    opacity:0.6;
  }
  80% {
    transform:scale(1.02);
    opacity:0.92;
  }
  to {
    transform:scale(1);
    opacity:1.0;
  }
}

@keyframes jump-out {
  0% {
    transform:scale(1);
    opacity:1.0;
  }
  20% {
    transform:scale(1.1);
    opacity:1.0;
  }
  to {
    transform:scale(0.01);
    opacity:0.0;
  }
}

@keyframes flip-down {
  0% {
    opacity:0.6;
    transform: rotateX(-40deg);
    transform-origin: top;
  }
  100% {
    opacity:1.0;
    transform: rotateX(0);
    transform-origin: top;
  }
}

        </style>
    </head>
    <body class="min-h-screen bg-info dark:bg-info-content">"""
  , opts.body
  , """</body>
</html>"""
  ]
  where
  defaultOpts = { jsPath: "", cssPath: "", body: "", title: "clm stats dev" }
  opts = edit defaultOpts edits