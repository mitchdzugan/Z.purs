module Node.Z.Puppeteer
  ( module NodePuppeteer
  ) where

import Node.Z.Puppeteer.PuppeteerImpl (class IsPageOrElement, Browser, Element(..), Element_, Page, PageOrElement, WaitUntil(..), asPageOrElement, context, xEl, xEls, xGetAttribute, xGoto, xGoto', xInnerHtml, xInnerText, xNewPage, xSetViewport, xUseBrowser, xUseBrowser', xWaitForSelector, xWaitForSelector') as NodePuppeteer

