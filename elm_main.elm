module ElmMain where

import Signal
import Signal exposing ((<~), Signal)

import Time

import Text
import Graphics.Element exposing (Element, show)

port output : Signal String
port output = Signal.map toString (Time.every Time.second)

-- Nasty hack to make the compiler happy
main : Element
main = show "main"

