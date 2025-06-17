
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
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
  light1 & set_ intensity 200
  light1 & get_ position >>= setXYZ 8 8 8
  add scene1 light1

  geometry1 <- newSphereGeometry
  material1 <- newMeshLambertMaterial
  mesh1 <- newMesh geometry1 material1
  add scene1 mesh1

  camera1 <- newPerspectiveCamera 70 (winWidth / winHeight) 0.1 100
  camera1 & get_ position >>= set_ z_ 6 

  renderer1 <- newWebGLRenderer
  setSize renderer1 winWidthI winHeightI True
  domElement renderer1 >>= appendInBody 
  render renderer1 scene1 camera1

  -- tests
  light1 & get_ intensity >>= valToNumber >>= consoleLog . ms . show
  light1 & get_ position >>= vector3ToXYZ >>= consoleLog . ms . show
  camera1 & get_ position >>= vector3ToXYZ >>= consoleLog . ms . show
  light1 & get_ position >>= get_ z_ >>= valToNumber >>= consoleLog . ms . show
  light1 & modify_ intensity (pure . (*2)) >>= valToNumber >>= consoleLog . ms . show
  light1 & get_ intensity >>= valToNumber >>= consoleLog . ms . show

  -- check compile errors
  -- scene1 & get_ intensity >>= valToNumber >>= consoleLog . ms . show
  -- scene1 & set_ intensity 200
  -- scene1 & set_ z_ 200

  pure ()

