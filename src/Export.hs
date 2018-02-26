{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Text   as T
import           Elm
import           Schema
import           Servant.Elm
import qualified Server
import           Text.Casing as C
import           Types

instance ElmType User
instance ElmType DateTime
instance ElmType IP
instance ElmType DateTimeAndIP

spec :: Spec
spec = Spec ["Generated"]
            (defElmImports
             : toElmTypeSourceWith    options (Proxy :: Proxy User)
             : toElmEncoderSourceWith options (Proxy :: Proxy User)
             : toElmDecoderSourceWith options (Proxy :: Proxy User)
             : toElmTypeSource    (Proxy :: Proxy DateTime)
             : toElmDecoderSource (Proxy :: Proxy DateTime)
             : toElmTypeSource    (Proxy :: Proxy IP)
             : toElmDecoderSource (Proxy :: Proxy IP)
             : toElmTypeSource    (Proxy :: Proxy DateTimeAndIP)
             : toElmDecoderSource (Proxy :: Proxy DateTimeAndIP)
             : generateElmForAPI  (Proxy :: Proxy Server.API)
            )
          where options = defaultOptions { fieldLabelModifier = T.pack . C.camel . T.unpack . T.drop (T.length "user") }

main :: IO ()
main = specsToDir [spec] "src/frontend"

