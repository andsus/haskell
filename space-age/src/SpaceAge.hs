module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthSeconds :: Num n => n
earthSeconds = 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / secondsFor planet

factorMultiplierOf :: Planet -> Float
factorMultiplierOf Mercury =  0.2408467 
factorMultiplierOf Venus   =  0.61519726
factorMultiplierOf Earth   =  1.0       
factorMultiplierOf Mars    =  1.8808158 
factorMultiplierOf Jupiter =  11.862615 
factorMultiplierOf Saturn  =  29.447498 
factorMultiplierOf Uranus  =  84.016846 
factorMultiplierOf Neptune =  164.79132

secondsFor :: Planet -> Float
secondsFor planet = factorMultiplierOf planet * earthSeconds