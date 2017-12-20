
module Handler.Help where

import Import
import Handler.Caching
import EmbeddedDocs

getHelpR :: Handler Html
getHelpR =
  cacheHtml $
    defaultLayout [whamlet|^{helpLayout Nothing helpIndex}|]

getHelpAuthorsR :: Handler Html
getHelpAuthorsR =
  cacheHtml $
    defaultLayout [whamlet|^{helpLayout (Just "Package authors") helpAuthors}|]

getHelpUsersR :: Handler Html
getHelpUsersR =
  cacheHtml $
    defaultLayout [whamlet|^{helpLayout (Just "Pursuit users") helpUsers}|]

-- TODO: Generate TOC in sidebar automatically

helpLayout :: Maybe Text -> Html -> Html
helpLayout forWhom inner =
  [shamlet|
    <div .page-title .clearfix>
      $maybe whom <- forWhom
        <div .page-title__label>Help for
        <h1 .page-title__title>#{whom}
      $nothing
        <h1 .page-title__title>Help!
    <div .col.col--main>
      ^{inner}
    <div .col.col--aside>
      <dl .grouped-list>
        <dt .grouped-list__title>
          Contents
        <dd .grouped-list__item>
          <a href="/help">Help index
        <dd .grouped-list__item>
          <a href="/help/users">Pursuit users
          <ul>
            <li>
              <a href="/help/users#searching">
                Searching
        <dd .grouped-list__item>
          <a href="/help/authors">Package authors
          <ul>
            <li>
              <a href="/help/authors#submitting-packages">
                How to submit packages
            <li>
              <a href="/help/authors#submit-automated">
                Submitting packages from a script
            <li>
              <a href="/help/authors#package-badges">
                Package badges
    |]
