PWM
===

Haskell module to generate PWM signal

    -- pulse width and period need to be provided in microseconds
    example :: IO ()
    example = do
        let stream = Stream 20000
        chan <- executeStream stream print
        writeStream chan 10000