{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Views where

import Prelude hiding (head, id, div, span)
import qualified Prelude as P
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Internal (Attributable, textValue)
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid ((<>))
import Control.Monad
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.Hamlet

import Types

(!.) :: Attributable h => h -> AttributeValue -> h
tag !. val = tag ! A.class_ val
(!#) :: Attributable h => h -> AttributeValue -> h
tag !# val = tag ! A.id val
(!+) :: Attributable h => h -> AttributeValue -> h
tag !+ val = tag ! A.type_ val
(!=) :: Attributable h => h -> AttributeValue -> h
tag != val = tag ! customAttribute "data-bind" val

stylesheets :: [Text]
stylesheets = map ("/static/" <>) [
  "css/bootstrap.min.css",
  "css/jquery-ui-1.9.0.custom.min.css",
--  "font/fontface.css",
  "css/homeworkpp.css"]
              
javascripts :: [Text]
javascripts = map ("/static/js/" <>) [
  "json2.js",
  "jquery-1.8.1.min.js",
  "jquery-ui-1.9.0.custom.min.js",
  "bootstrap.min.js",
  "knockout-2.1.0.js"]

staticIncludes :: Html
staticIncludes = [shamlet|
  $forall ss <- stylesheets
    <link rel="stylesheet" href="#{ss}">
  $forall js <- javascripts
    <script src="#{js}">
  |]

baseTemplate :: Text -> Maybe Html -> Html -> Html
baseTemplate titleText maybeHeadHtml contentHtml = [shamlet|
  $doctype 5
  <head>
    <title>#{titleText}
    #{staticIncludes}
    $maybe headHtml <- maybeHeadHtml
      #{headHtml}
    $nothing
  <body>
    #{contentHtml}
  |]

pageTemplate :: Text -> Maybe Html -> Text -> Html -> Html
pageTemplate titleText maybeHeadHtml username contentHtml =
  baseTemplate titleText maybeHeadHtml [shamlet|
    <div class="container">
      <div class="row header">
        <div class="span9">
          <a href="/">
            <div class="headerLogo smallCorners dropShadow pull-left">
          <h1 class="logoText">the Stanford Set Ladder
        $if not (username == "")
          <div class="span3 alignRight userBox">
            <p>Logged in as: 
              <strong> #{username}
            <a class="btn btn-small btn-inverse" href="/logout">Log out
      <div class="row">
        <div class="span12">
          #{contentHtml}
    |]

index :: Text -> Html
index username = pageTemplate "Set Ladder: Home" Nothing username $ [shamlet|
  <h1 class="page-header">SFBA ARML Homework++
  |]

preEscapedText :: Text -> Html
preEscapedText = preEscapedToHtml

toHtmlNoneGiven :: Text -> Html
toHtmlNoneGiven t =
  if t == ""
  then span !. "muted" $ "(none)"
  else toHtml t

closeButton :: Html
closeButton =
  button !. "close" !+ "button" ! customAttribute "data-dismiss" "alert" $
  preEscapedText "&times;"
  
modalCloseButton :: Html
modalCloseButton =
  button !. "close" !+ "button" ! customAttribute "data-dismiss" "modal" $
  preEscapedText "&times;"

login :: Html
login = loginMessage ""

loginMessage :: Text -> Html
loginMessage m = baseTemplate "Login to SFBA ARML Homework++" Nothing $ [shamlet|
  <div class="container">
    <div class="row">
      <div class="span12">
        <div class="jumboLogo largeCorners dropShadow">
        <h1 id="JumboHeader" class="alignCenter">Login to SFBA ARML Homework++
    <div class="row">
      <div class="span12" id="LoginControls">
        $if (m == "loginfailed")
          <div class="alert alert-error">
            #{closeButton}
            <strong>Login failed!
            Incorrect username or password.
        $if (m == "registersuccess")
          <div class="alert alert-success">
            #{closeButton}
            <strong>Registration successful!
            You should now be able to log in.
        <form action="/auth" method="POST">
          <div class="control-group">
            <div class="controls">
              <input type="text" id="InputUsername" name="username"
               placeholder="Username" autofocus="autofocus">
          <div class="control-group">
            <div class="controls">
              <input type="password" id="InputPassword" name="password"
               placeholder="Password">
          <div class="control-group">
            <div class="controls">
              <button type="submit" class="btn">Sign in
  |]

notFound :: Html
notFound = baseTemplate "404" Nothing $ [shamlet|
  <div class="container">
    <div class="row">
      <div class="span12">
        <h1 class="alignCenter" style="line-height:1;margin-top:100px;font-size:100px">
          OH NOES :(
        <h1 class="alignCenter">404: page not found
  |]