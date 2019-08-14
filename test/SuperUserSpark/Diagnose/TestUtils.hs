module SuperUserSpark.Diagnose.TestUtils where

import TestImport

import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Diagnose.Types


-- TODO, the code should be able to handle 'genValid', but currenty it can't.
absPathIn :: Path Abs Dir -> Gen AbsP
absPathIn sandbox =
    scale (+ 5) $ do
        fp <- genListOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
        case parseRelFile fp of
            Nothing -> absPathIn sandbox
            Just p -> pure $ AbsP $ sandbox </> p

absFileIn :: Path Abs Dir -> Gen (Path Abs File, AbsP)
absFileIn sandbox = do
    p <- absPathIn sandbox
    pure (unAbsP p, p)

absDirIn :: Path Abs Dir -> Gen (Path Abs Dir, AbsP)
absDirIn sandbox = do
    p <- absPathIn sandbox
    let u = toPath p
    case parseAbsDir u of
        Nothing -> absDirIn sandbox
        Just ad -> pure (ad, p)
