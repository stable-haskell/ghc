module Settings.Flavours.Release where

import Settings.Flavours.Performance
import Flavour

releaseFlavour :: Flavour
releaseFlavour =
  -- 1. These interface files will be distributed and the source files never recompiled.
  disableSelfRecompInfo
  -- 2. Include documentation in the interface for tools such as haddock and HLS to use
  $ enableHaddock
  -- 3. Include unit id hashes
  $ enableHashUnitIds
  -- 4. Include hie files (#16901)
  -- $ enableHieFiles
  $ performanceFlavour { name = "release" }
