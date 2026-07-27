module Node.Z.Puppeteer
  ( module NodePuppeteer
  ) where

import Node.Z.Puppeteer.PuppeteerImpl (class IsPageOrElement, Browser, Element(..), Element_, Page, PageOrElement, WaitUntil(..), asPageOrElement, context, el, els, getAttribute, goto, goto', innerHtml, innerText, newPage, setViewport, useBrowser, useBrowser', waitForSelector, waitForSelector') as NodePuppeteer

