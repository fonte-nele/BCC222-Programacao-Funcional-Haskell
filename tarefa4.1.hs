constante :: Double
constante = 6.67 * 10^^(-11)

forca_atracao :: Double->Double->Double->Double
forca_atracao m1 m2 d = constante *(m1*m2/d^2)
