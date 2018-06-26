{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Text.Printf
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.String
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent
import Control.Exception(AsyncException,fromException,handle,throwIO,finally)


import Data.Aeson (encode,decode,FromJSON,ToJSON,parseJSON,toJSON,(.:),(.=),(.:?),withObject,object)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set

import System.Clock
import System.Random
import Network.HTTP.Types
import qualified Network.Wai
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS

type Position=(Double,Double)

data Msg = Command Text | Person Text Text (Maybe TimeSpec) (Maybe Int) Position (Maybe Position)
            | Path Position Position

instance Show Msg where
  show (Command command) =show command
  show (Person label user birth life pos lastPos) = "<"++show label++"> " ++show user ++ ": " ++
                                              show birth ++ ", "++
                                              show life ++ ", "++
                                              show pos ++", "++
                                              show lastPos
  show (Path start end)="path: "++ show start++", "++ show end

instance ToJSON Msg where
  toJSON (Command command)=object ["kind".=("command"::Text), "command".=command]
  toJSON (Person label user birth life pos lastPos)=object ["kind".=("person"::Text),
                                                      "label".=label,
                                                      "user".=user, "birth".=birth,
                                                      "life".=life, "pos".=pos,
                                                      "lastPos".=lastPos]
  toJSON (Path start end)=object ["kind".=("path"::Text), "start".=start, "end".=end]

instance FromJSON Msg where
  parseJSON=withObject "command or position" $ \o -> do
    kind<-o .: "kind"
    case kind of
      "command"->Command <$> o .: "command"
      "person"->Person <$> o.: "label" <*> o .: "user" <*> o .:? "birth" <*> o .:? "life" <*>
                           o.: "pos" <*> o .:? "lastPos"
      "path"->Path <$> o .: "start" <*> o .: "end"
      _         ->fail ("unknown kind: " ++ kind)

instance ToJSON TimeSpec where
  toJSON (TimeSpec s n)=object ["s" .= s, "ns".=n]

instance FromJSON TimeSpec where
  parseJSON=withObject "time" $ \o-> TimeSpec <$> o.:"s" <*> o.:"ns"


receiveMsg::WS.Connection->IO (Maybe Msg)
receiveMsg conn=do
  msgText <- WS.receiveData conn :: IO ByteString
  report $ fromString $show msgText
  return $ decode msgText

sendMsg::WS.Connection->Msg->IO ()
sendMsg conn msg=do
  report $ fromString $show msg
  WS.sendTextData conn $ encode msg


data Client = Client {getUser::Text
                     ,getConn::WS.Connection
                     ,getBirthDay::TimeSpec
                     ,getLife::Int          --seconds
                     ,getPos::Position
                     ,getLastPos::Position
                     ,isOnline::Bool        
                     }

instance Eq Client where
  client1  == client2 = (getUser client1) == (getUser client2)
instance Ord Client where
  client1 `compare` client2  = (getUser client1) `compare` (getUser client2)


clientToMsg::Text->Client->Msg
clientToMsg label (Client user _ birth life pos lastPos _)=Person label user (Just birth) (Just life) pos (Just lastPos)

pathToMsg::Segment->Msg
pathToMsg (Segment start end)=Path start end



type Clients=[Client]

removeClient :: Client -> Clients->Clients
removeClient client=filter (/=client)
--removeClient  =Set.delete

addClient:: Client -> Clients -> Clients
addClient=(:)
--addClient =Set.insert

userExists::Text->Clients->Bool
userExists user clients=any (\client->getUser client==user) clients
--userExists user clients= not . null $ Set.filter (\client->getUser client==user) clients

updateConn::Text->WS.Connection->Clients->Clients
updateConn user conn =map (\client->
                             if (getUser client==user) then
                                 client {getConn=conn,isOnline=True}
                             else client
                          )


changePos::Text->Position->Clients->Clients
changePos user pos=map (\client->
                             if (getUser client==user) then
                               client {getPos=pos}
                             else client
                          )

synLastPos::Text->Clients->Clients
synLastPos user=map (\client->
                             if (getUser client==user) then
                               client {getLastPos=(getPos client)}
                             else client
                          )
                      
makeOffline::Text->Clients->Clients
makeOffline user =map (\client->
                             if (getUser client==user) then
                               client {isOnline=False}
                             else client
                          )                      
-- updateConn user conn clients=
--   Set.insert (Client user conn birth life) clients
--   where
--     client=findClient user clients
--     birth=getBirthDay client
--     life=getLife client

findClient::Text->Clients->Client
findClient user clients=fromJust $ find (\client->getUser client==user) clients
-- findClient user clients=Set.elemAt n clients
--   where
--     n=Set.findIndex user $ Set.mapMonotonic getUser clients

broadcast :: Msg -> Clients -> IO ()
broadcast message clients = do
    report $ fromString $ show message
    forM_ (filter isOnline clients) $ \client -> sendMsg (getConn client) message



data Segment=Segment {
  start::Position
  ,end::Position
  }
 
type Paths=[Segment]

addPath::Segment->Paths->Paths
addPath=(:)

type State=(Clients,Paths)

initState::State
initState=([],[])

getClients::State->Clients
getClients=fst

getPaths::State->Paths
getPaths=snd

updateClients::(Clients->Clients)->State->State
updateClients t (c1,m1) =(t c1,m1)

updatePaths::(Paths->Paths)->State->State
updatePaths t (c1,m1)=(c1,t m1)

type ServerState = MVar State

readState::ServerState->IO State
readState=readMVar

modifyState_::ServerState->(State->State)->IO ()
modifyState_ serverState t =modifyMVar_ serverState (return . t)




--seconds
timePassed::TimeSpec->TimeSpec->Int
timePassed (TimeSpec s1 n1) (TimeSpec s2 n2)= fromIntegral (s2-s1)

report::Text->IO ()
report _=return ()
--report=T.putStrLn






--   type synonym WS.ServerApp=WS.PendingConnection -> IO ()
wsApp :: ServerState -> WS.ServerApp
wsApp serState req = do
  sock <- WS.acceptRequest req
  WS.forkPingThread sock 30
  user <- WS.receiveData sock
  clients<-liftM getClients $ readState serState
--  WS.sendTextData sock $ encode (map getUser clients)
  if (userExists user clients) then do
    modifyState_ serState $ updateClients $ updateConn user sock
    newClients<-liftM getClients $ readState serState
    let newClient=findClient user newClients
    userHandler serState newClient
  else do
    birth<-getTime Monotonic
    let life=600
    pos<-randomPos
    let newClient=(Client user sock birth life pos pos True)
    modifyState_ serState $ updateClients $ addClient newClient
    userHandler serState newClient

width::Double
width=300

height::Double
height=300

randomPos::IO Position
randomPos=do
  x<-randomRIO (-width/2,width/2)
  y<-randomRIO (-height/2,height/2)
  return (x,y)


isAlive::TimeSpec->Client->Bool
isAlive now client = timePassed (getBirthDay client) now < (getLife client)

dis::Double
dis=150


isVisable::Client->Client->Bool
isVisable c1 c2= ( (not (c2==c1)) &&
                   ( ((abs(x)<dis) && (abs(y)<dis))||
                     ((abs(xx)<dis)&&(abs(yy)<dis)) )
                 )
                where
                  (x1,y1)=getPos c1
                  (x2,y2)=getPos c2
                  (x3,y3)=getLastPos c2
                  x= x1-x2
                  y= y1-y2
                  xx=x1-x3
                  yy=y1-y3

isPathVisable::Segment->Client->Bool
isPathVisable s c=( ((abs(x)<dis)&&(abs(y)<dis)) || ((abs(xx)<dis)&&(abs(yy)<dis)) )
                   where
                     (x1,y1)=start s
                     (x3,y3)=end s
                     (x2,y2)=getPos c                     
                     x= x1-x2
                     y= y1-y2
                     xx=x3-x2
                     yy=y3-y2


userHandler::ServerState -> Client -> IO ()
userHandler serState client=
  flip finally (disconnect client) $ do
    sendMsg (getConn client) (clientToMsg "" client)
    liftM getClients (readState serState) >>=
      return . (filter (\c->isVisable c client)) >>=
      broadcast (clientToMsg "" client)
    liftM getClients (readState serState) >>=
      return . (filter (isVisable client)) >>=
      mapM_ (\c->sendMsg (getConn client) (clientToMsg "" c))
--path
    liftM getPaths (readState serState) >>=
      mapM_ (\s->sendMsg (getConn client) (Path (start s) (end s)))
    userLoop serState client
      where
        disconnect client= do
          let user=getUser client
          modifyState_ serState $ updateClients $ makeOffline user
          report (user `mappend` " disconnected")
        userLoop serState client = do
          msg<-receiveMsg $ getConn client
--          putStrLn $ show msg
          now<-getTime Monotonic          
          if (isAlive now client) then do
--            liftM getClients (readState serState) >>= broadcast (T.concat [getUser client, " : ", msg])
            case msg of
              Just (Person _ _ _ _ pos _)->do
                let newClient=client {getPos=pos}
                modifyState_ serState $ updateClients $ changePos (getUser client) pos
                liftM getClients (readState serState) >>=
                  return . (filter (\c->isVisable c newClient)) >>=
                  broadcast (clientToMsg ""  newClient)

            
                liftM getClients (readState serState) >>=
                  return . (filter (\c->(isVisable c client)
                                     &&(not (isVisable c newClient))
                                   )) >>=
                  broadcast (clientToMsg "delete" client)

            

                liftM getClients (readState serState) >>=
                  return . (filter (\c->(isVisable newClient c)
                                     &&(not (isVisable client c))
                                   )) >>=
                  mapM_ (\c->sendMsg (getConn client) (clientToMsg "" c))

                liftM getClients (readState serState) >>=
                  return . (filter (\c->(isVisable client c)
                                     &&(not (isVisable newClient c))
                                   )) >>=
                  mapM_ (\c->sendMsg (getConn client) (clientToMsg "delete" c))

            
                liftM getPaths (readState serState) >>=
                  return . (filter (\p->(isPathVisable p newClient)
                                   &&(not (isPathVisable p client))
                                   )) >>=
                  mapM_ (\c->sendMsg (getConn client) (pathToMsg c))
   
                  
--                WS.sendTextData (getConn client) $ encode position
                userLoop serState newClient
              Just (Command "Stop")->do
                let start=getLastPos client
                    end=getPos client
                modifyState_ serState $ updatePaths $ addPath (Segment start end)
                liftM getClients (readState serState) >>=
                  return . (filter (isPathVisable (Segment start end))) >>=
                  broadcast (Path start end)
                  
                let newClient=client {getLastPos=(getPos client)}
                modifyState_ serState $ updateClients $ synLastPos (getUser client)
                sendMsg (getConn client) (clientToMsg "" newClient)
                liftM getClients (readState serState) >>=
                  return . (filter (\c->isVisable c newClient)) >>=
                  broadcast (clientToMsg "" newClient)
                -- liftM getClients (readState serState) >>=
                --   return . (filter (isVisable client)) >>=
                --   mapM_ (\c->sendMsg (getConn client) (clientToMsg "" c))
                userLoop serState newClient
              _->userLoop serState client
          else do
            case msg of
              Just (Command "New")->do
--                WS.sendTextData (getConn client) $ encode
                modifyState_ serState $ updateClients $ removeClient client
                liftM getClients (readState serState) >>=
                  return . (filter (\c->(isVisable c client))
                                   ) >>=
                  broadcast (clientToMsg "delete" client)

  --            WS.sendTextData (getConn client) $ encode (getUser client `mappend` "died")
                report $ getUser client `mappend` " died"
              _-> userLoop serState client

staticContent :: Network.Wai.Application
staticContent _ respond=respond $ Network.Wai.responseLBS status400 [] "Not a WebSocket request"

app::ServerState->Network.Wai.Application
app serState = WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp serState) staticContent

config :: Int -> Warp.Settings
config port =Warp.setPort port Warp.defaultSettings

forkSendingThread::Int->ServerState->IO ()
forkSendingThread n serState
  | n <= 0    = return ()
  | otherwise = do
      _ <- forkIO (ignore `handle` go)
      return ()
        where
          go :: IO ()
          go = do
            clients<-liftM getClients $ readState serState
            now<-getTime Monotonic
            -- if(isAlive now client) then do
            --   threadDelay (n * 1000 * 1000)
--              WS.sendTextData (getConn client) ("test"::Text)
            let diedClients=filter (\client->not (isAlive now client)) clients
            mapM_ (\client->WS.sendTextData (getConn client) ("died"::Text)) diedClients
            threadDelay (n * 1000 * 1000)            
            go
            -- else
            --   WS.sendTextData (getConn client) ("died"::Text)
          ignore e = case fromException e of
            Just async -> throwIO (async :: AsyncException)
            Nothing    -> return ()


port :: Int
port = 5000

main :: IO ()
main = do
  serState <- newMVar initState
  printf "Starting server on port %d\n" port
  let dt=10
  forkSendingThread dt serState  
--  Warp.run port $ app state
  Warp.runSettings (config port) (app serState)
