
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Miso (consoleLog, run)
import Miso.String (ms)

import API

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- newScene 

  light1 <- newPointLight
  light1 & setIntensity 300
  light1 & getPosition >>= setXYZ 8 8 8
  add scene1 light1

  material1 <- newMeshLambertMaterial
  geometry1 <- newSphereGeometry
  mesh1 <- newMesh geometry1 material1
  mesh1 & getPosition >>= setXYZ (-1) 0 0
  add scene1 mesh1

  texture2 <- newTextureLoader >>= load "miso.png"
  material2 <- newMeshLambertMaterial
  material2 & setMat texture2
  geometry2 <- newBoxGeometry
  mesh2 <- newMesh geometry2 material2
  mesh2 & getPosition >>= setXYZ 1 0 0
  add scene1 mesh2

  camera1 <- newPerspectiveCamera 70 (winWidth / winHeight) 0.1 100
  camera1 & getPosition >>= setZ 6 

  renderer1 <- newWebGLRenderer
  setSize renderer1 winWidthI winHeightI True

  setAnimationLoop renderer1 $ \_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & getRotation >>= setYRot (time/1000)
    render renderer1 scene1 camera1

  domElement renderer1 >>= appendInBody 


  -- tests
  light1 & isLight >>= consoleLog . ms . show
  light1 & modifyIntensity (pure . (*2)) >>= consoleLog . ms . show
  light1 & getIntensity >>= valToNumber >>= consoleLog . ms . show
  light1 & getPosition >>= vector3ToXYZ >>= consoleLog . ms . show
  camera1 & getPosition >>= vector3ToXYZ >>= consoleLog . ms . show
  light1 & getPosition >>= getZ >>= valToNumber >>= consoleLog . ms . show

  -- check compile errors
  -- scene1 & getIntensity >>= valToNumber >>= consoleLog . ms . show
  -- scene1 & setIntensity 200
  -- scene1 & setZ 200

