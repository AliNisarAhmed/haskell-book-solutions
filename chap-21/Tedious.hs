module Tedious where

data Query = Query deriving (Eq, Show)
data SomeObj = SomeObj deriving (Eq, Show)
data IoOnlyObj = IoOnlyObj deriving (Eq, Show)
data Err = Err deriving (Eq, Show)

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)

-- (( traverse makeIoOnlyObj . mapM decodeFn) =<< ) . fetchFn
-------------------
-- pipelineFn query = do
--   a <- fetchFn query  -- IO [String] -- hence a = [String]
-- (map decodeFn a) => [Either Err SomeObj]
-- after sequence => Either Err [SomeObj]
--   case sequence (map decodeFn a) of
--     (Left err) -> return $ Left err
--     (Right res) -> do  -- res :: [SomeObj]
--       a <- makeIoOnlyObj res  -- IO [(SomeObj, IoOnlyObj)]
--       return $ Right a
