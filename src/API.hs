
-------------------------------------------------------------------------------
-- JS to Haskell rules:
-- - if a JS class can be inherited -> define a Haskell typeclass
-- - if a JS class can be instanciated -> define a Haskell newtype
-------------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module API 
  ( Object3DC(..)

  , TextureLoader(..)
  , newTextureLoader
  , load

  , Texture(..)

  , MaterialC(..)
  , MeshLambertMaterial(..)
  , newMeshLambertMaterial
  , getMatOpt
  , setMat

  , LightC(..)
  , PointLight(..)
  , newPointLight

  , CameraC
  , PerspectiveCamera(..)
  , newPerspectiveCamera

  , Euler(..)
  , getYRot
  , setYRot
  , modifyYRot

  , Vector3(..)
  , newVector3
  , vector3ToXYZ
  , setXYZ
  , getZ
  , setZ

  , Mesh(..)
  , newMesh

  , Scene(..)
  , newScene
  , isScene

  , BufferGeometryC(..)
  , BufferGeometry(..)

  , SphereGeometry(..)
  , newSphereGeometry

  , BoxGeometry(..)
  , newBoxGeometry

  , WebGLRenderer(..)
  , newWebGLRenderer
  , render
  , domElement
  , setSize
  , setAnimationLoop

  , winInnerWidth
  , winInnerHeight
  , appendInBody

  , valToNumber
  ) where

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J hiding (getProp, setProp)

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

new' :: MakeArgs a => (JSVal -> b) -> JSString -> a -> JSM b
new' f name args = do
  v <- jsg ("THREE" :: JSString) ! name
  f <$> J.new v args

mkGetOpt :: (MakeObject a, FromJSVal b) => JSString -> a -> JSM (Maybe b)
mkGetOpt name v = fromJSVal =<< v ! name

mkGet :: (MakeObject a, FromJSVal b) => JSString -> a -> JSM b
mkGet name v = fromJSValUnchecked =<< v ! name

mkSet :: (MakeObject a, ToJSVal b) => JSString -> b -> a -> JSM ()
mkSet name x v = v ^. jss name x

mkModify :: (MakeObject a, ToJSVal b, FromJSVal b) => JSString -> (b -> JSM b) -> a -> JSM b
mkModify name f v = do
  x <- fromJSValUnchecked =<< v ! name
  y <- f x
  v ^. jss name y
  pure y

mkModifyOpt :: (MakeObject a, ToJSVal b, FromJSVal b) => JSString -> (b -> JSM b) -> a -> JSM (Maybe b)
mkModifyOpt name f v = do
  mx <- fromJSVal =<< v ! name
  case mx of
    Nothing -> pure Nothing
    Just x -> do
      y <- f x
      v ^. jss name y
      pure $ Just y

-------------------------------------------------------------------------------
-- Object3D
-------------------------------------------------------------------------------

class Object3DC a where
  -- properties
  getPosition :: a -> JSM Vector3
  setPosition :: Vector3 -> a -> JSM ()
  getRotation :: a -> JSM Euler
  -- methods
  add :: (Object3DC b, MakeArgs b) => a -> b -> JSM ()

instance Object3DC JSVal where
  getPosition = mkGet "position"
  setPosition = mkSet "position"
  getRotation = mkGet "rotation"
  add v x = void $ v # ("add" :: JSString) $ x

-------------------------------------------------------------------------------
-- Scene
-------------------------------------------------------------------------------

newtype Scene = Scene { unScene :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving Object3DC via JSVal

newScene :: JSM Scene
newScene = new' Scene "Scene" ()

-- read-only properties
isScene :: Scene -> JSM Bool
isScene = mkGet "isScene"

-------------------------------------------------------------------------------
-- Light
-------------------------------------------------------------------------------

class Object3DC a => LightC a where
-- read-only properties
  isLight :: a -> JSM Bool
  -- properties
  getIntensity :: a -> JSM Double
  setIntensity :: Double -> a -> JSM ()
  modifyIntensity :: (Double -> JSM Double) -> a -> JSM Double

instance LightC JSVal where
  isLight = mkGet "isLight"
  getIntensity = mkGet "intensity"
  setIntensity = mkSet "intensity"
  modifyIntensity = mkModify "intensity"

-------------------------------------------------------------------------------
-- PointLight
-------------------------------------------------------------------------------

newtype PointLight = PointLight { unPointLight :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (LightC)
  deriving Object3DC via JSVal

newPointLight :: JSM PointLight
newPointLight = new' PointLight "PointLight" ()

-------------------------------------------------------------------------------
-- Material
-------------------------------------------------------------------------------

class MaterialC a where
  isMaterial:: a -> JSM Bool

instance MaterialC JSVal where
  isMaterial = mkGet "isMaterial"

-------------------------------------------------------------------------------
-- MeshLambertMaterial
-------------------------------------------------------------------------------

newtype MeshLambertMaterial = MeshLambertMaterial { unMeshLambertMaterial :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype MaterialC

newMeshLambertMaterial :: JSM MeshLambertMaterial
newMeshLambertMaterial = new' MeshLambertMaterial "MeshLambertMaterial" ()

-- optional properties
getMatOpt :: MeshLambertMaterial -> JSM (Maybe Texture)
getMatOpt = mkGetOpt "map"

setMat :: Texture -> MeshLambertMaterial -> JSM ()
setMat = mkSet "map"

-------------------------------------------------------------------------------
-- Texture
-------------------------------------------------------------------------------

newtype Texture = Texture { unTexture :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

instance FromJSVal Texture where
  fromJSVal = pure .Just . Texture

-------------------------------------------------------------------------------
-- TextureLoader
-------------------------------------------------------------------------------

newtype TextureLoader = TextureLoader { unTextureLoader :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

newTextureLoader :: JSM TextureLoader
newTextureLoader = new' TextureLoader "TextureLoader" ()

-- methods
load :: JSString -> TextureLoader -> JSM Texture
load url (TextureLoader v) = Texture <$> (v # ("load" :: JSString) $ [url])

-------------------------------------------------------------------------------
-- BufferGeometry
-------------------------------------------------------------------------------

class BufferGeometryC a where
  -- read-only properties
  isBufferGeometry :: a -> JSM Bool

instance BufferGeometryC JSVal where
  isBufferGeometry = mkGet "isBufferGeometry"

newtype BufferGeometry = BufferGeometry { unBufferGeometry :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (BufferGeometryC)

-------------------------------------------------------------------------------
-- SphereGeometry
-------------------------------------------------------------------------------

newtype SphereGeometry = SphereGeometry { unSphereGeometry :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (BufferGeometryC)

newSphereGeometry :: JSM SphereGeometry
newSphereGeometry = new' SphereGeometry "SphereGeometry" ()

-------------------------------------------------------------------------------
-- BoxGeometry
-------------------------------------------------------------------------------

newtype BoxGeometry = BoxGeometry { unBoxGeometry :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (BufferGeometryC)

newBoxGeometry :: JSM BoxGeometry
newBoxGeometry = new' BoxGeometry "BoxGeometry" ()

-------------------------------------------------------------------------------
-- Mesh
-------------------------------------------------------------------------------

newtype Mesh = Mesh { unMesh :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (Object3DC)

newMesh :: (ToJSVal a, ToJSVal b, BufferGeometryC a, MaterialC b) => a -> b -> JSM Mesh
newMesh geometry' material' = new' Mesh "Mesh" (geometry', material')

-------------------------------------------------------------------------------
-- Camera
-------------------------------------------------------------------------------

class Object3DC a => CameraC a where
  -- read-only properties
  isCamera :: a -> JSM Bool

instance CameraC JSVal where
  isCamera = mkGet "isCamera"

-------------------------------------------------------------------------------
-- PerspectiveCamera
-------------------------------------------------------------------------------

newtype PerspectiveCamera = PerspectiveCamera { unPerspectiveCamera :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype CameraC
  deriving Object3DC via JSVal

newPerspectiveCamera :: Double -> Double -> Double -> Double -> JSM PerspectiveCamera
newPerspectiveCamera fov' aspect' near' far' = 
  new' PerspectiveCamera "PerspectiveCamera" (fov', aspect', near', far')

-------------------------------------------------------------------------------
-- WebGLRenderer
-------------------------------------------------------------------------------

newtype WebGLRenderer = WebGLRenderer { unWebGLRenderer :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

newWebGLRenderer :: JSM WebGLRenderer
newWebGLRenderer = new' WebGLRenderer "WebGLRenderer" ()

setSize :: WebGLRenderer -> Int -> Int -> Bool -> JSM ()
setSize (WebGLRenderer v) width height updateStyle = void $ v # ("setSize" :: JSString) $ (width, height, updateStyle)

setAnimationLoop :: WebGLRenderer -> JSCallAsFunction -> JSM ()
setAnimationLoop (WebGLRenderer v) f = void $  v # "setAnimationLoop" $ f

render :: (ToJSVal a, Object3DC a, ToJSVal b, CameraC b) => WebGLRenderer -> a -> b -> JSM ()
render (WebGLRenderer v) object camera = void $ v # ("render" :: JSString) $ (object, camera)

-- the WebGLRenderer constructor creates a canvas element which can be added in the DOM
domElement :: WebGLRenderer -> JSM JSVal
domElement (WebGLRenderer v) = v ! "domElement"

-------------------------------------------------------------------------------
-- Euler
-------------------------------------------------------------------------------

newtype Euler = Euler { unEuler :: JSVal }
  deriving (MakeObject, ToJSVal, MakeArgs)

instance FromJSVal Euler where
  fromJSVal = pure .Just . Euler

-- properties
getYRot :: Euler -> JSM Double
getYRot = mkGet "y"

setYRot :: Double -> Euler -> JSM ()
setYRot = mkSet "y"

modifyYRot :: (Double -> JSM Double) -> Euler -> JSM Double
modifyYRot = mkModify "y"

-------------------------------------------------------------------------------
-- Vector3
-------------------------------------------------------------------------------

newtype Vector3 = Vector3 { unVector3 :: JSVal }
  deriving (MakeObject, ToJSVal, MakeArgs)

instance FromJSVal Vector3 where
  fromJSVal = pure .Just . Vector3

newVector3 :: Double -> Double -> Double -> JSM Vector3
newVector3 x y z = new' Vector3 "Vector3" (x, y, z)

-- properties
getZ :: Vector3 -> JSM Double
getZ = mkGet "z"

setZ :: Double -> Vector3 -> JSM ()
setZ = mkSet "z"

-- methods
setXYZ :: Double -> Double -> Double -> Vector3 -> JSM ()
setXYZ x y z (Vector3 v) = void $ v ^. js3 "set" x y z

vector3ToXYZ :: Vector3 -> JSM (Double, Double, Double)
vector3ToXYZ (Vector3 v) = do
  x <- fromJSValUnchecked =<< v ! "x"
  y <- fromJSValUnchecked =<< v ! "y"
  z <- fromJSValUnchecked =<< v ! "z"
  pure (x, y, z)

