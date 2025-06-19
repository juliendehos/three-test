
-------------------------------------------------------------------------------
-- JS to Haskell rules:
-- - if a JS class can be inherited -> define a Haskell typeclass
-- - if a JS class can be instanciated -> define a Haskell newtype
--
-- properties:
-- - Prop / mkProp for properties
-- - OptProp / mkOptProp for optional properties
-- - Ro / mkRo for read-only properties
-------------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J hiding (getProp, setProp)

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

{-

module API 
  ( Object3DC(..)

  , TextureLoader(..)
  , newTextureLoader
  , load

  , Texture(..)

  , MaterialC(..)
  , MeshLambertMaterial(..)
  , newMeshLambertMaterial
  , mapOptProp

  , LightC(..)
  , PointLight(..)
  , newPointLight

  , CameraC
  , PerspectiveCamera(..)
  , newPerspectiveCamera

  , Euler(..)
  , yRotProp

  , Vector3(..)
  , newVector3
  , vector3ToXYZ
  , setXYZ
  , xProp
  , yProp
  , zProp

  , Mesh(..)
  , newMesh

  , Scene(..)
  , newScene
  , isSceneRo

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
  , getProp
  , setProp
  , modifyProp
  , modifyOptProp
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

-- define/construct a property
type Prop a b = a -> Maybe b -> JSM b

mkProp :: (MakeObject a, FromJSVal b, ToJSVal b) => JSString -> Prop a b
mkProp name v mx1 = do
  x0 <- fromJSValUnchecked =<< v ! name
  case mx1 of
    Nothing -> pure x0
    Just x1 -> v ^. jss name x1 >> pure x1

-- define/construct an optional property
type OptProp a b = a -> Maybe b -> JSM (Maybe b)

mkOptProp :: (MakeObject a, FromJSVal b, ToJSVal b) => JSString -> OptProp a b
mkOptProp name v mx1 = do
  mx0 <- fromJSVal =<< v ! name
  case mx1 of
    Nothing -> pure mx0
    Just x1 -> v ^. jss name x1 >> pure Nothing

-- get/set/modify a property or an optional property
getProp :: (a -> Maybe b -> JSM c) -> a -> JSM c
getProp prop v = prop v Nothing

setProp :: (a -> Maybe b -> JSM c) -> b -> a -> JSM ()
setProp prop x v = void $ prop v (Just x)

modifyProp :: Prop a b -> (b -> JSM b) -> a -> JSM ()
modifyProp prop f v = do
  x <- getProp prop v
  y <- f x
  setProp prop y v

modifyOptProp :: OptProp a b -> (b -> JSM b) -> a -> JSM ()
modifyOptProp prop f v = do
  mx <- getProp prop v
  forM_ mx $ \x -> do
    y <- f x
    setProp prop y v

-- read-only property (can be read directly)
type Ro a b = a -> JSM b

mkRo :: (MakeObject a, FromJSVal b) => JSString -> Ro a b
mkRo name v = fromJSValUnchecked =<< v ! name

-------------------------------------------------------------------------------
-- Object3D
-------------------------------------------------------------------------------

class Object3DC a where
  -- properties
  positionProp :: Prop a Vector3
  rotationProp :: Prop a Euler
  -- methods
  add :: (Object3DC b, MakeArgs b) => a -> b -> JSM ()

instance Object3DC JSVal where
  positionProp = mkProp "position"
  rotationProp = mkProp "rotation"
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
isSceneRo :: Ro Scene Bool
isSceneRo = mkRo "isScene"

-------------------------------------------------------------------------------
-- Light
-------------------------------------------------------------------------------

class Object3DC a => LightC a where
-- read-only properties
  isLightRo :: a -> JSM Bool
  -- properties
  intensityProp :: Prop a Double

instance LightC JSVal where
  isLightRo = mkRo "isLight"
  intensityProp = mkProp "intensity"

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
  isMaterialRo :: Ro a Bool

instance MaterialC JSVal where
  isMaterialRo = mkRo "isMaterial"

-------------------------------------------------------------------------------
-- MeshLambertMaterial
-------------------------------------------------------------------------------

newtype MeshLambertMaterial = MeshLambertMaterial { unMeshLambertMaterial :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype MaterialC

newMeshLambertMaterial :: JSM MeshLambertMaterial
newMeshLambertMaterial = new' MeshLambertMaterial "MeshLambertMaterial" ()

-- optional properties
mapOptProp :: OptProp MeshLambertMaterial Texture
mapOptProp = mkOptProp "map"

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
  isBufferGeometryRo :: Ro a Bool

instance BufferGeometryC JSVal where
  isBufferGeometryRo = mkRo "isBufferGeometry"

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
  isCameraRo :: Ro a Bool

instance CameraC JSVal where
  isCameraRo = mkRo "isCamera"

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
yRotProp :: Prop Euler Double
yRotProp = mkProp "y"

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
xProp, yProp, zProp :: Prop Vector3 Double
xProp = mkProp "x"
yProp = mkProp "y"
zProp = mkProp "z"

-- methods
setXYZ :: Double -> Double -> Double -> Vector3 -> JSM ()
setXYZ x y z (Vector3 v) = void $ v ^. js3 "set" x y z

vector3ToXYZ :: Vector3 -> JSM (Double, Double, Double)
vector3ToXYZ (Vector3 v) = do
  x <- fromJSValUnchecked =<< v ! "x"
  y <- fromJSValUnchecked =<< v ! "y"
  z <- fromJSValUnchecked =<< v ! "z"
  pure (x, y, z)

-}

