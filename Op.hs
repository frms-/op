module Lambdabot.Plugin.Op (opPlugin) where

import Control.Monad (unless)
import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Plugin

import qualified Lambdabot.Message as Msg

type Op = ModuleT () LB

opPlugin :: Module ()
opPlugin = newModule
    { moduleCmds = return
        [(command "op")
            { privileged = True
            , help = say "op"
            , process = opMe
            }
        ]
    }

opMe :: String -> Cmd Op ()
opMe _ = do
  nick_ <- getSender
  chans <- getChannels
  unless (null chans) $
    lb (send (op nick_ (head chans)))


op :: Nick -> Nick -> IrcMessage
op nick_ chan = mkMsg (nTag nick_) "MODE" [nName chan, "+o", nName nick_]

mkMsg :: String -> String -> [String] -> IrcMessage
mkMsg svr cmd params  = IrcMessage
  { ircMsgServer  = svr
  , ircMsgPrefix  = ""
  , ircMsgCommand = cmd
  , ircMsgParams  = params
  , ircMsgLBName  = "outputmessage"
  }

getChannels :: Monad m => Cmd m [Nick]
getChannels  = withMsg (return . Msg.channels)
