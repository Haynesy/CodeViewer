{-# LANGUAGE PackageImports #-}

module Main where

    import Control.Concurrent (threadDelay)
    import Control.Monad (when)
    import Graphics.Rendering.OpenGL hiding (Front)
    import "GLFW-b" Graphics.UI.GLFW as GLFW
    import System.Exit (exitWith, ExitCode(ExitSuccess))
    
    main :: IO ()
    main = do
        putStrLn "Starting...\n"

        let width   = 640
            height  = 480
            
        withWindow width height "CodeViewer (v 0.1.0)" (\window -> do
            initOpenGL width height
            loop window
            exitWith ExitSuccess)

        putStrLn "\nDone!"


    withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
    withWindow width height title renderFunc = do
        GLFW.setErrorCallback $ Just simpleErrorCallback 
        renderContext <- GLFW.init
        when renderContext $ do
            window <- GLFW.createWindow width height title Nothing Nothing
            initOpenGL width height
            case window of
                (Just win) -> do
                    GLFW.makeContextCurrent window
                    renderFunc win
                    GLFW.setErrorCallback $ Just simpleErrorCallback
                    GLFW.destroyWindow win
                Nothing -> return ()
            GLFW.terminate
        where
            simpleErrorCallback e s =
                putStrLn $ unwords [show e, show s]

    initOpenGL :: Int -> Int -> IO ()
    initOpenGL width height = do
        clearColor $= Color4 1 1 1 1
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

    loop :: GLFW.Window -> IO ()
    loop window = do
        GLFW.pollEvents
        renderFrame window
        threadDelay 20000
        key <- keyIsPressed window GLFW.Key'Escape
        if key
            then return ()
            else loop window

    keyIsPressed :: GLFW.Window -> GLFW.Key -> IO Bool
    keyIsPressed window key = isPress `fmap` GLFW.getKey window key

    isPress :: GLFW.KeyState -> Bool
    isPress GLFW.KeyState'Pressed   = True
    isPress GLFW.KeyState'Repeating = True
    isPress _                       = False

    renderFrame :: GLFW.Window -> IO ()
    renderFrame window = do
        clear [ColorBuffer]
        flush
        GLFW.swapBuffers window