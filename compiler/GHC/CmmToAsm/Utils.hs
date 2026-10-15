module GHC.CmmToAsm.Utils
   ( topInfoTable
   , entryBlocks
   )
where

import GHC.Prelude

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm hiding (topInfoTable)

-- | Returns the info table associated with the CmmDecl's entry point,
-- if any.
topInfoTable :: GenCmmDecl a RawCmmProcInfo (ListGraph b) -> Maybe RawCmmStatics
topInfoTable (CmmProc infos _ _ (ListGraph (b:_)))
  = mapLookup (blockId b) (raw_info_tbls infos)
topInfoTable _
  = Nothing

-- | Return the list of BlockIds in a CmmDecl that are entry points
-- for this proc (i.e. they may be jumped to from outside this proc).
entryBlocks :: GenCmmDecl a RawCmmProcInfo (ListGraph b) -> [BlockId]
entryBlocks (CmmProc info _ _ (ListGraph code)) = entries
  where
        infos = mapKeys (raw_info_tbls info)
        entries = case code of
                    [] -> infos
                    BasicBlock entry _ : _ -- first block is the entry point
                       | entry `elem` infos -> infos
                       | otherwise          -> entry : infos
entryBlocks _ = []
