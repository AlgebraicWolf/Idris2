module Main

-- Make sure programs with cost centres work as expected when
-- the profiling is disabled.
main : IO ()
main = %costCentre "a cost centre" (putStrLn "Hello, World!")
