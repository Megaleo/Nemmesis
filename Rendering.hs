module Rendering where

import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.Rendering.OpenGL as GLL
import qualified Graphics.Rendering.GLU.Raw as GLU
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import Data.StateVar
import Data.IORef
import System.Exit
import Data.Maybe

-- All the data carried each loop
data State = State { ftglFont :: FTGL.Font
                   , glfwWindow :: GLFW.Window
                   }

type IOState = IORef State

-- Callback to the keyboard
handleKeyboard :: GLFW.KeyCallback
handleKeyboard _ _ _ _ _ = return ()

-- When the close button is pressed
shutdown :: IOState -> GLFW.WindowCloseCallback
shutdown ioState win = do
  state <- get ioState
  GLFW.destroyWindow win
  GLFW.terminate
  FTGL.destroyFont $ ftglFont state
  _ <- exitSuccess
  return ()

--Called when the window is resized
handleResize :: GLFW.WindowSizeCallback
handleResize _ width height = do
  GL.glViewport 0 0 (fromIntegral width) (fromIntegral height) -- Screen limit
  GL.glMatrixMode GL.gl_PROJECTION
  GL.glLoadIdentity
  GL.glOrtho 0 (toEnum width) 0 (toEnum height) (-1) 1
  GL.glMatrixMode GL.gl_MODELVIEW
  GL.glLoadIdentity
  GL.glFlush

-- Initializate OpenGL
openGLInit :: IO ()
openGLInit = do
  GL.glEnable GL.gl_DEPTH_TEST -- Enables depth
  GL.glEnable GL.gl_SMOOTH
  GL.glShadeModel GL.gl_SMOOTH -- Enables smooth color shading
  GL.glClearColor 0.0 0.0 0.0 1.0 -- Clear color to black
  GL.glDepthFunc GL.gl_LEQUAL
  GL.glHint GL.gl_PERSPECTIVE_CORRECTION_HINT GL.gl_NICEST

-- Initializate GLFW
glfwInit :: IO GLFW.Window
glfwInit = do
  GLFW.init
  GLFW.defaultWindowHints
  -- Open Window
  maybeWindow <- GLFW.createWindow 800 -- width
                                   800 -- height
                                   "Nemmesis" -- Window name
                                   Nothing -- Monitor (used in fullscreen)
                                   Nothing -- Window for shared resources
  case maybeWindow of
    Nothing -> do
        putStrLn "Error on opening GLFW window"
        GLFW.terminate
        exitFailure
    Just window -> do
    GLFW.makeContextCurrent (Just window) -- Tell GLFW to use this window
    return window

callbacksInit :: IOState -> IO ()
callbacksInit ioState = do
    state <- get ioState
    let window = glfwWindow state
    GLFW.setKeyCallback window (Just handleKeyboard) -- Keyboard callback
    GLFW.setWindowSizeCallback window  (Just handleResize) -- Window size callback
    GLFW.setWindowCloseCallback window (Just $ shutdown ioState) -- Window close callbac

-- Create and initializate the font
fontInit :: IO FTGL.Font
fontInit = do
    font <- FTGL.createTextureFont "/usr/share/fonts/denemo/Denemo.ttf"
    FTGL.setFontFaceSize font 24 72
    return font

-- Initializate all
initAll :: IO IOState
initAll = do
    window <- glfwInit
    openGLInit
    (w,h) <- GLFW.getFramebufferSize window
    handleResize window w h
    font <- fontInit
    ioState <- newIORef $ State font window
    callbacksInit ioState
    return ioState

-- Draw the scence to the screen
drawScene :: State -> IO ()
drawScene (State font window) = do
    GL.glMatrixMode GL.gl_MODELVIEW
    GL.glLoadIdentity
    GL.glFlush

-- Rendering loop
mainLoop :: IOState -> IO ()
mainLoop ioState = do
  state <- get ioState
  GLFW.pollEvents -- Check for GLFW events
  GL.glClear GL.gl_COLOR_BUFFER_BIT -- Clear the Depth buffer
  GL.glClear GL.gl_DEPTH_BUFFER_BIT -- Clear the Color buffer
  drawScene state -- Draw the Scene (OpenGL)
  GLFW.swapBuffers $ glfwWindow state -- Updates the frame buffer to the window (GLFW )
  mainLoop ioState -- Do the loop again
