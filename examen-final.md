Examen final
================

# IMPORTAR BASE DE DATOS ECXEL

``` r
library(readxl)
datos<-read_excel("C:/Users/EDIXON MARTINEZ/Documents/BASE DE DATOS R- ESTUDIO FABIAN.xlsx")
```

# Estandarización de variables

``` r
datost<- datos # crear una nueva base de datos o data frame
datost<- scale (datost, center =  TRUE, scale =  TRUE)
datost<- as.data.frame(datost)
```

# Normalidad Multivariante

H0= Normalidad multivariante H1= No Normalidad Multivariante Confianza=
95% Alfa= 5% = 0.05 P value &gt; Alfa: No se rechaza la H0 (Normalidad)
P value &lt; Alfa: Se rechaza la H0 (No Normalidas)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test        Statistic              p value Result
    ## 1 Mardia Skewness 147.511296428187 3.58730539084276e-10     NO
    ## 2 Mardia Kurtosis 2.52323116387352   0.0116281933617952     NO
    ## 3             MVN             <NA>                 <NA>     NO
    ## 
    ## $univariateNormality
    ##           Test           Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk       TIPOS           0.7536  <0.001      NO    
    ## 2 Shapiro-Wilk  VELOCIDAD(KM/H)      0.7039  <0.001      NO    
    ## 3 Shapiro-Wilk    #DE PERSONAS       0.7996   1e-04      NO    
    ## 4 Shapiro-Wilk ALTURA DE VUELO(M)    0.7212  <0.001      NO    
    ## 5 Shapiro-Wilk       MODELO          0.9296  0.0479      NO    
    ## 6 Shapiro-Wilk      PESO(Kg)         0.9177  0.0233      NO    
    ## 
    ## $Descriptives
    ##                     n          Mean Std.Dev      Median        Min      Max
    ## TIPOS              30 -1.397500e-16       1 -0.14290744 -0.8574446 2.715241
    ## VELOCIDAD(KM/H)    30 -4.434274e-18       1  0.02788183 -1.6450281 4.321684
    ## #DE PERSONAS       30  4.127196e-18       1 -0.65604591 -1.0191004 1.668777
    ## ALTURA DE VUELO(M) 30  1.675020e-16       1  0.23305152 -3.2286715 1.192119
    ## MODELO             30 -7.140499e-15       1  0.16483902 -1.7975303 1.499250
    ## PESO(Kg)           30 -1.875670e-17       1 -0.29557085 -1.2950445 2.096332
    ##                          25th       75th       Skew   Kurtosis
    ## TIPOS              -0.8574446 -0.1429074  1.4796958  1.4399550
    ## VELOCIDAD(KM/H)    -0.3554934  0.2927592  2.3784673  9.2305501
    ## #DE PERSONAS       -0.8152804  1.0955330  0.5164472 -1.5569865
    ## ALTURA DE VUELO(M) -0.1925702  0.3749254 -2.1059604  4.4711580
    ## MODELO             -0.6789798  0.8516683 -0.3601338 -1.2149077
    ## PESO(Kg)           -0.6799838  0.7081741  0.5658171 -0.9663702

como el P value es &lt; Alfa se rechaza la H0, por lo tanto no hay
Normalidad.

# Matriz de Correlaciones

H0= correlacion = 0 (no hay correlacion) H1= correlacion diferente de 0
(si hay correlacion)

cuando no se rechaza H0, no se aplica AFE. se rechaza H0, si para
aplicar AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                    TIPOS VELOCIDAD(KM/H) #DE PERSONAS ALTURA DE VUELO(M) MODELO
    ## TIPOS               1.00           -0.10        -0.24              -0.08  -0.28
    ## VELOCIDAD(KM/H)    -0.10            1.00        -0.02               0.47   0.05
    ## #DE PERSONAS       -0.24           -0.02         1.00               0.46   0.59
    ## ALTURA DE VUELO(M) -0.08            0.47         0.46               1.00   0.46
    ## MODELO             -0.28            0.05         0.59               0.46   1.00
    ## PESO(Kg)           -0.20            0.05         0.55               0.36   0.57
    ##                    PESO(Kg)
    ## TIPOS                 -0.20
    ## VELOCIDAD(KM/H)        0.05
    ## #DE PERSONAS           0.55
    ## ALTURA DE VUELO(M)     0.36
    ## MODELO                 0.57
    ## PESO(Kg)               1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                    TIPOS VELOCIDAD(KM/H) #DE PERSONAS ALTURA DE VUELO(M) MODELO
    ## TIPOS               0.00            1.00         1.00               1.00   1.00
    ## VELOCIDAD(KM/H)     0.58            0.00         1.00               0.11   1.00
    ## #DE PERSONAS        0.20            0.92         0.00               0.11   0.01
    ## ALTURA DE VUELO(M)  0.68            0.01         0.01               0.00   0.11
    ## MODELO              0.14            0.79         0.00               0.01   0.00
    ## PESO(Kg)            0.30            0.80         0.00               0.05   0.00
    ##                    PESO(Kg)
    ## TIPOS                  1.00
    ## VELOCIDAD(KM/H)        1.00
    ## #DE PERSONAS           0.02
    ## ALTURA DE VUELO(M)     0.46
    ## MODELO                 0.01
    ## PESO(Kg)               0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) 
correlaciones$r
```

    ##                          TIPOS VELOCIDAD(KM/H) #DE PERSONAS ALTURA DE VUELO(M)
    ## TIPOS               1.00000000     -0.10442193  -0.23885729        -0.07936549
    ## VELOCIDAD(KM/H)    -0.10442193      1.00000000  -0.01980434         0.46550724
    ## #DE PERSONAS       -0.23885729     -0.01980434   1.00000000         0.46409638
    ## ALTURA DE VUELO(M) -0.07936549      0.46550724   0.46409638         1.00000000
    ## MODELO             -0.27927674      0.05071464   0.59295596         0.46431398
    ## PESO(Kg)           -0.19671529      0.04798933   0.54968088         0.35952928
    ##                         MODELO    PESO(Kg)
    ## TIPOS              -0.27927674 -0.19671529
    ## VELOCIDAD(KM/H)     0.05071464  0.04798933
    ## #DE PERSONAS        0.59295596  0.54968088
    ## ALTURA DE VUELO(M)  0.46431398  0.35952928
    ## MODELO              1.00000000  0.57201429
    ## PESO(Kg)            0.57201429  1.00000000

``` r
r <- as.matrix(correlaciones$r)
```

Alfa= 0,05 P value &gt; Alfa: no se rechaza H0 P value &lt; Alfa: se
rechaza H0

# Indicadores de Aplicabilidad del AFE

## Contrasre de Esfericidad de Bartlett

H0: las correlaciones teoricas entre cada par de variables es nulo. H1:
las correlaciones teoricas entre cada par de variables no es nulo.

P value &gt; Alfa: no se aplica al AFE (no se rechaza H0) P value &lt;
Alfa: no se aplica al AFE (se rechaza H0)

``` r
dim(datost)
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n= 30)
```

    ## $chisq
    ## [1] 45.27074
    ## 
    ## $p.value
    ## [1] 6.939147e-05
    ## 
    ## $df
    ## [1] 15

Como el P value es menor que Alfa, se rechaza la H0, por lo tanto, las
correlaciones teoricas entre cada par de variables es nulo, es decir si
es aplicable en el analisis factorialexploratorio (AFE)

## Medidad de Adecuacion muestral de KAISER, MEYER, Y OKLIN (KMO)

Estudian variable por variable si son o no aceptadas en el modelo para
hacer AFE. Se mantiene una variable en el modelo, si el KMO es igual o
mayor a 0,7. Se elimina una variable del modelo, si el KMO es menor es
menor que 0,7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.68
    ## MSA for each item = 
    ##              TIPOS    VELOCIDAD(KM/H)       #DE PERSONAS ALTURA DE VUELO(M) 
    ##               0.62               0.36               0.73               0.60 
    ##             MODELO           PESO(Kg) 
    ##               0.76               0.80

# Determinacion del Numero de Factores a Extraer

``` r
fa.parallel(r, fm= "pa", n.obs= 30, ylabel="Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](examen-final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Con el metodo de ejes principales se tendria que extraeer 1.

## Metodo de Componentes Principales

``` r
fa.parallel(r, fm= "pc", n.obs= 30, ylabel= "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

![](examen-final_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

## Metodo de Maxima Verosimilitud

``` r
fa.parallel(r, fm= "ml", n.obs= 30, ylabel= "Eigenvalues")
```

![](examen-final_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Con este metodo nos da que se recomienda extraer un factor.

# Metodo de extraccion de Factores

## Metodo de analisis de los componentes principales

``` r
acp<- principal(r, nfactors = 1, rotate = "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                      PC1   h2   u2 com
    ## TIPOS              -0.40 0.16 0.84   1
    ## VELOCIDAD(KM/H)     0.26 0.07 0.93   1
    ## #DE PERSONAS        0.80 0.65 0.35   1
    ## ALTURA DE VUELO(M)  0.72 0.51 0.49   1
    ## MODELO              0.83 0.69 0.31   1
    ## PESO(Kg)            0.76 0.58 0.42   1
    ## 
    ##                 PC1
    ## SS loadings    2.66
    ## Proportion Var 0.44
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.15 
    ## 
    ## Fit based upon off diagonal values = 0.84

PC1= Cargas Factoriales de cada variable h2= comunalidad

## Metodo de los ejes Principales o componentes principales iteradas (CPI)

``` r
cpi<- fa(r, nfactors= 1, fm= "pa", rotate = "none", n.obs = 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                      PA1    h2   u2 com
    ## TIPOS              -0.29 0.085 0.91   1
    ## VELOCIDAD(KM/H)     0.17 0.030 0.97   1
    ## #DE PERSONAS        0.76 0.571 0.43   1
    ## ALTURA DE VUELO(M)  0.60 0.357 0.64   1
    ## MODELO              0.80 0.636 0.36   1
    ## PESO(Kg)            0.69 0.471 0.53   1
    ## 
    ##                 PA1
    ## SS loadings    2.15
    ## Proportion Var 0.36
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.73 with Chi Square of  45.27
    ## The degrees of freedom for the model are 9  and the objective function was  0.4 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.11 
    ## The df corrected root mean square of the residuals is  0.14 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  11.12  with prob <  0.27 
    ## The total number of observations was  30  with Likelihood Chi Square =  10.21  with prob <  0.33 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.93
    ## RMSEA index =  0.058  and the 90 % confidence intervals are  0 0.226
    ## BIC =  -20.4
    ## Fit based upon off diagonal values = 0.91
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.91
    ## Multiple R square of scores with factors          0.82
    ## Minimum correlation of possible factor scores     0.65

## Metodo de Maxima Verosimilitud

``` r
mve<- fa(r, nfactors= 1, fm= "ml", rotate = "none", n.obs = 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                      ML1    h2   u2 com
    ## TIPOS              -0.30 0.092 0.91   1
    ## VELOCIDAD(KM/H)     0.12 0.015 0.98   1
    ## #DE PERSONAS        0.76 0.579 0.42   1
    ## ALTURA DE VUELO(M)  0.58 0.335 0.66   1
    ## MODELO              0.80 0.636 0.36   1
    ## PESO(Kg)            0.70 0.494 0.51   1
    ## 
    ##                 ML1
    ## SS loadings    2.15
    ## Proportion Var 0.36
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.73 with Chi Square of  45.27
    ## The degrees of freedom for the model are 9  and the objective function was  0.4 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.11 
    ## The df corrected root mean square of the residuals is  0.15 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  11.44  with prob <  0.25 
    ## The total number of observations was  30  with Likelihood Chi Square =  10.13  with prob <  0.34 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.935
    ## RMSEA index =  0.055  and the 90 % confidence intervals are  0 0.225
    ## BIC =  -20.48
    ## Fit based upon off diagonal values = 0.9
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   0.91
    ## Multiple R square of scores with factors          0.83
    ## Minimum correlation of possible factor scores     0.65

# Representacion Grafica de los Factores Extraidos

# Representacion Grafica DE los Facrores Extraidos

## ACP, CPI ,MVE

SOLO SE PUEDE GRAFICAR CUANDO HAY DOS FACTORES A EXTRAER.

# OBTENCION DE LOS PUNTOS FACTORIALES

## ACP

``` r
library(paran)
```

    ## Loading required package: MASS

``` r
paran(r, iterations= 100,graph= F)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 100 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.989521    3.721574      1.732052
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate = "none", scores = T)
acp1$scores
```

    ##               PC1
    ##  [1,] -2.09023723
    ##  [2,] -2.14387470
    ##  [3,] -1.34844950
    ##  [4,] -1.11820323
    ##  [5,] -0.44166902
    ##  [6,]  1.25537061
    ##  [7,]  0.39830780
    ##  [8,]  1.25411338
    ##  [9,]  1.00403128
    ## [10,]  0.60002491
    ## [11,]  0.94666862
    ## [12,]  1.07504467
    ## [13,]  1.57787397
    ## [14,]  1.46574258
    ## [15,]  0.49822601
    ## [16,]  1.44067867
    ## [17,] -0.05446418
    ## [18,] -0.84723124
    ## [19,] -0.30253820
    ## [20,] -0.22101017
    ## [21,] -0.87360503
    ## [22,] -0.49215335
    ## [23,] -0.50589380
    ## [24,] -0.02905987
    ## [25,] -0.64263147
    ## [26,] -0.15184577
    ## [27,] -0.40990580
    ## [28,] -0.35998736
    ## [29,]  0.47189930
    ## [30,]  0.04477812

## METODO DE LAS COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm= "pa", rotate = "none", n.obs = 30,scores = "regression")
cpi1$scores
```

    ##               PA1
    ##  [1,] -1.72825368
    ##  [2,] -1.67626739
    ##  [3,] -1.22133921
    ##  [4,] -1.00738331
    ##  [5,] -0.21166500
    ##  [6,]  1.03761838
    ##  [7,]  0.50623967
    ##  [8,]  1.26505389
    ##  [9,]  0.95213287
    ## [10,]  0.62301190
    ## [11,]  0.94588944
    ## [12,]  1.07455825
    ## [13,]  1.41249828
    ## [14,]  1.37000816
    ## [15,]  0.34433199
    ## [16,]  1.28926867
    ## [17,] -0.18064266
    ## [18,] -0.98155154
    ## [19,] -0.30112140
    ## [20,] -0.15569136
    ## [21,] -0.84515280
    ## [22,] -0.16531580
    ## [23,] -0.55375848
    ## [24,] -0.13670555
    ## [25,] -0.59405604
    ## [26,] -0.24664597
    ## [27,] -0.64770601
    ## [28,] -0.60333318
    ## [29,]  0.47676390
    ## [30,] -0.04078602

``` r
puntfact_cpi<- cpi1$scores
puntfact_cpi<- as.data.frame(puntfact_cpi)
```

``` r
mvel<- fa(datost[,2:7], nfactors = 1, fm= "ml", rotate = "none", n.obs = 30,scores = "regression")
mvel$scores
```

    ##               ML1
    ##  [1,] -1.65908280
    ##  [2,] -1.63475638
    ##  [3,] -1.14025102
    ##  [4,] -1.01530384
    ##  [5,] -0.22572858
    ##  [6,]  1.05993473
    ##  [7,]  0.45675464
    ##  [8,]  1.25802229
    ##  [9,]  0.96044665
    ## [10,]  0.58450187
    ## [11,]  0.94516734
    ## [12,]  1.09718397
    ## [13,]  1.43614543
    ## [14,]  1.39879012
    ## [15,]  0.42497523
    ## [16,]  1.33654960
    ## [17,] -0.15973408
    ## [18,] -0.92241523
    ## [19,] -0.32075137
    ## [20,] -0.19754131
    ## [21,] -0.83881714
    ## [22,] -0.25090413
    ## [23,] -0.56091086
    ## [24,] -0.12019475
    ## [25,] -0.56598974
    ## [26,] -0.25623853
    ## [27,] -0.67718269
    ## [28,] -0.82307489
    ## [29,]  0.46896179
    ## [30,] -0.05855631

``` r
puntfact_mve<- mvel$scores
puntfact_mve<- as.data.frame(puntfact_mve)
factor.scores(r, mve,method = "Trurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                            ML1
    ## TIPOS              -0.05833038
    ## VELOCIDAD(KM/H)     0.02199243
    ## #DE PERSONAS        0.31565772
    ## ALTURA DE VUELO(M)  0.15229892
    ## MODELO              0.38307845
    ## PESO(Kg)            0.24315378
    ## 
    ## $r.scores
    ##     ML1
    ## ML1   1
    ## 
    ## $R2
    ## [1] 0.8251137
