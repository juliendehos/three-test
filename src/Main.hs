
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Language.Javascript.JSaddle (valToNumber)
import Miso (consoleLog, run)
import Miso.String (ms)

import API

import THREE
import THREE.BoxGeometry
import THREE.Euler
import THREE.Light
import THREE.Mesh
import THREE.MeshLambertMaterial
import THREE.Object3D
import THREE.PerspectiveCamera
import THREE.PointLight
import THREE.Scene
import THREE.SphereGeometry
import THREE.TextureLoader
import THREE.Vector3
import THREE.WebGLRenderer

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- THREE.Scene.new 

  light1 <- THREE.PointLight.new
  light1 & setProp intensityProp 300
  light1 & getProp positionProp >>= setXYZ 8 8 8
  add scene1 light1

  material1 <- THREE.MeshLambertMaterial.new
  geometry1 <- THREE.SphereGeometry.new
  mesh1 <- THREE.Mesh.new geometry1 material1
  mesh1 & getProp positionProp >>= setXYZ (-1) 0 0
  add scene1 mesh1

  texture2 <- THREE.TextureLoader.new >>= load "miso.png"
  material2 <- THREE.MeshLambertMaterial.new
  material2 & setProp mapOptProp texture2
  geometry2 <- THREE.BoxGeometry.new
  mesh2 <- THREE.Mesh.new geometry2 material2
  mesh2 & getProp positionProp >>= setXYZ 1 0 0
  add scene1 mesh2

  camera1 <- THREE.PerspectiveCamera.new 70 (winWidth / winHeight) 0.1 100
  camera1 & getProp positionProp >>= setProp THREE.Vector3.zProp 6 

  renderer1 <- THREE.WebGLRenderer.new
  setSize renderer1 winWidthI winHeightI True

  setAnimationLoop renderer1 $ \_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & getProp rotationProp >>= setProp THREE.Euler.yProp (time/1000)
    render renderer1 scene1 camera1

  domElementRo renderer1 >>= appendInBody 

  -- tests
  light1 & isLightRo >>= consoleLog . ms . show
  light1 & modifyProp intensityProp (pure . (*2)) 
  light1 & getProp intensityProp >>= valToNumber >>= consoleLog . ms . show
  light1 & getProp positionProp >>= vector3ToXYZ >>= consoleLog . ms . show
  camera1 & getProp positionProp >>= vector3ToXYZ >>= consoleLog . ms . show
  light1 & getProp positionProp >>= getProp THREE.Vector3.zProp >>= valToNumber >>= consoleLog . ms . show

  -- check compile errors
  -- scene1 & getProp intensityProp >>= valToNumber >>= consoleLog . ms . show
  -- scene1 & setProp intensityProp 200
  -- scene1 & setProp THREE.Vector3.zProp 200

