{-# Language
 RankNTypes
,ScopedTypeVariables
,KindSignatures
#-}
module System where

import Clock
import ListT
import LinearM
import StackM

import Control.Monad.Identity

data 
   System m service serviceData client clientData 
 = System 
 {initialService     :: service
 ,initialClient      :: client 
 ,initialServiceData :: serviceData
 ,serviceFunction    :: (service -> clientData  -> m (serviceData, service))
 ,clientFunction     :: (client  -> serviceData -> m (clientData , client ))
 }

runSystem :: forall m s sd c cd. Monad m => Clock m -> System m s sd c cd -> (ListT m cd) -- ListT m (s,sd,c,cd)
runSystem c s = clientStream  
 where
  serviceStream ::  (ListT m sd)
  serviceStream = setM $ return $ Just (initialServiceData s, fst  (serviceScanner (initialService s) clientStream))
  clientStream  :: (ListT m cd)
  clientStream  = fst  (clientScanner (initialClient s) $ reStream c serviceStream)
  serviceScanner :: s -> ListT m cd -> (ListT m sd, m s)
  serviceScanner = scannerM (serviceFunction s)
  clientScanner  :: c -> ListT m sd -> (ListT m cd, m c)
  clientScanner  = scannerM (clientFunction s)

egSystem :: System IO Int Int Int Int
egSystem = System 0 0 1 (\s cd -> return (s+cd,s+1)) (\c sd -> return (c+sd,c+2))

go = display $ runSystem clock egSystem

