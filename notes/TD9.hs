module TD9 where

newtype Id a = Id { runId :: a }
  deriving (Show, Eq)

instance Functor Id where
  fmap g (Id x) = Id (g x)

instance Applicative Id where
  pure = Id
  ig <*> ix = Id $ runId ig (runId ix)  

instance Monad Id where
  ix >>= f = f (runId ix)

-- >>> (+1) <$> (Id 3)
-- Id {runId = 4}
-- >>> (+) <$> (Id 1) <*> (Id 2)
-- Id {runId = 3}
-- >>> Id 3 >>= (\x -> Id (3 + x) >>= (\y -> Id (x + y)))
-- Id {runId = 9}

newtype IdT m a = IdT {runIdT :: m (Id a)}

instance (Show (m a), Functor m) => Show (IdT m a) where
    show = show . fmap runId . runIdT

-- x :: IdT m a
-- runIdT x :: m (Id a)
-- runId :: Id a -> a
-- fmap/m :: (b -> c) -> m b -> m c
-- b = Id a
-- c = a 
-- fmap runId (runIdT x) :: m a
-- show/(m a) :: m a -> String 

instance (Eq (m a), Functor m) => Eq (IdT m a) where
    (==) v1 v2 = fmap runId (runIdT v1) == fmap runId (runIdT v2)

-- v1 :: IdT m a
-- runIdT v1 :: m (Id a)
-- fmap runId (runIdt v1) :: m a
-- v2 :: IdT m a
-- runIdT v2 :: m (Id a)
-- fmap runId (runIdt v2) :: m a

fmapIdT :: (Functor m) => (a -> b) -> IdT m a -> IdT m b
-- fmapIdT f ix = IdT (fmap (fmap f) (runIdT ix))
fmapIdT f = IdT . fmap (fmap f) . runIdT
-- f :: a -> b
-- ix :: IdT m a
-- runIdT ix :: m (Id a)
-- fmap/m :: (c -> d) -> m c -> m d 
-- fmap/Id :: (e -> f) -> Id e -> Id f
-- fmap/Id f :: Id a -> Id b
-- c = Id a, d = Id b
-- fmap/m (fmap/Id f) :: m (Id a) -> m (Id b)
-- fmap/m (fmap/Id f) (runIdt ix) :: m (Id b)
-- IdT (fmap/m (fmap/Id f) (runIdt ix)) :: IdT m b

instance (Functor m) => Functor (IdT m) where
     fmap = fmapIdT

pureIdT :: (Applicative m) => a -> IdT m a
pureIdT = IdT . pure . pure
-- x :: a
-- pure/Id x :: Id a
-- pure/m (pure/Id x) :: m (Id a)

applyIdT :: (Applicative m) => IdT m (a -> b) -> IdT m a -> IdT m b
applyIdT ig ix = IdT $ (<*>) ((<$>) (<*>) (runIdT ig)) (runIdT ix)
-- if :: IdT m (a -> b)
-- ix :: IdT m a
-- runIdT if :: m Id (a -> b)
-- runIdT ix :: m Id a
-- <*>/m :: m (c -> d) -> m c -> m d
-- <*>/Id :: Id (e -> f) -> Id e -> Id f
-- <$>/m :: (g -> h) -> m g -> m h
-- g = Id (a -> b)
-- h = (Id a -> Id b)
-- <$>/m <*>/Id (runIdT if) :: m (Id a -> Id b)
-- <*>/m (<$>/m <*>/Id (runIdT if)) :: m (Id a) -> m (Id b)
-- (<*>/m (<$>/m <*>/Id (runIdT if))) (runIdT ix) :: m (Id b)
-- IdT (<*>/m (<$>/m <*>/Id (runIdT if))) (runIdT ix) :: IdT m b    

instance (Applicative m) => Applicative (IdT m) where
    pure = pureIdT
    (<*>) = applyIdT

bindIdT :: (Monad m) =>  IdT m a -> (a -> IdT m b) -> IdT m b
bindIdT ix ig = IdT $ runIdT ix >>= runIdT . ig . runId
-- ig :: a -> IdT m b
-- ix :: IdT m a
-- runIdT ix :: m (Id a)
-- >>=/m :: m c -> (c -> m d) -> m d 
-- but => IdT m b
-- runIdT . ig :: a -> m (Id b)
-- runIdT . ig . runId :: (Id a) -> m (Id b)
-- c = Id a
-- d = Id b
-- runIdT ix >>= runIdT . ig . runId  :: m (Id b)
-- IdT $ runIdT ix >>= runIdT . ig . runId :: IdT m b

instance (Monad m) => Monad (IdT m) where
    (>>=) = bindIdT

type Id2 a = IdT Id a

id2_vers_id :: Id2 a -> Id a
id2_vers_id = runId . runIdT
-- iv :: Id2 a
-- iv :: IdT Id a
-- runIdT iv :: Id (Id a)
-- runId (runIdT iv) :: Id a

id_vers_id2 :: Id a -> Id2 a
id_vers_id2 = IdT . Id 
-- iv :: Id a
-- Id iv :: Id Id a
-- IdT (Id iv) :: IdT Id a
-- IdT (Id iv) :: Id2 a

prop_iso_id :: (Eq a) => Id a -> Bool
prop_iso_id x = id2_vers_id (id_vers_id2 x) == x

prop_iso_id2 :: (Eq a) => Id2 a -> Bool 
prop_iso_id2 x = id_vers_id2 (id2_vers_id x) == x

type Deep a = IdT (IdT (IdT IO)) a
-- Deep a £=£ IO (Id (Id (Id a)))

progIO :: IO Integer
progIO = do -- do de la monade IO
  putStrLn "Hello"
  str <- getLine
  return $ read str

hello :: Deep ()
hello = (IdT . IdT . IdT)
        (do -- do de la monade IO
            fmap Id (fmap Id (fmap Id (putStrLn "Hello"))))

quarantedeux :: Deep Integer 
quarantedeux = (IdT . IdT . IdT)
               (do -- do de la monade IO
                    return (return (return (return 42))))

runDeep :: Deep a -> IO (Id (Id (Id a)))
runDeep =  runIdT . runIdT . runIdT

deep :: IO a -> Deep a
deep = IdT . IdT . IdT . fmap (Id . Id . Id)

progIODeep :: Deep Integer
progIODeep = do -- do de la monade Deep (qui est une stack de monade)
                deep (putStrLn "Hello") -- :: Deep ()
                str <- deep getLine
                return $ read str -- return de la monade Deep

    -- dire bonjour dans la monade IO Id Id Id
    -- recupere une ligne dans la monade IO Id Id Id
    -- return  dans la monade IO Id Id Id

-- deep (putStrLn "Hello") >>= (\str -> deep getLine >>= (\_ -> return $ read str))

class MonadTrans t where
    lift :: Monad m => m a -> t m a

liftIdT :: (Monad m) => m a -> IdT m a
liftIdT = IdT . fmap Id 
-- mx :: m a
-- IdT (fmap Id but) :: IdT m a
-- fmap Id but :: m (Id a)
-- but :: m a

instance MonadTrans IdT where
    lift = liftIdT

progIOLift :: Deep Integer
progIOLift = do -- do de la monade Deep (qui est une stack de monade)
                lift $ lift $ lift $ putStrLn "Hello" -- :: Deep ()
                str <- lift $ lift $ lift getLine
                return $ read str

-- IOT Maybe Integer -----> Maybe (IO Integer) 
-- IO doit toujours etre a l'exterieur

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id 

instance (MonadIO m) => MonadIO (IdT m) where
    liftIO xio = liftIdT $ liftIO xio
    -- xio :: IO a
    -- but :: IdT m a
    -- liftIO/m :: IO a -> m a
    -- liftIO/m xio :: m a
    -- liftIdT :: m a -> IdT m a

progIOLiftIO :: Deep Integer
progIOLiftIO = do
                liftIO $ putStrLn  "Hello"
                str <- liftIO getLine 
                return $ read str
