-- this file provides utilities such as type aliases

module Utils where

import Calamity
import Calamity.Commands.Context (CalamityCommandContext)
import Calamity.Commands.Types (CommandContext)
import Calamity.Types.Model.Channel.Message (Message)
import CalamityCommands.Context (ConstructContext)
import CalamityCommands.ParsePrefix (ParsePrefix)
import Data.Typeable (Typeable)
import Polysemy (Members)

type EssentialEnv r c =
  ( BotC r,
    Typeable c,
    Tellable c,
    CommandContext c,
    CalamityCommandContext c,
    Members '[ParsePrefix Message, ConstructContext Message c IO ()] r
  )
