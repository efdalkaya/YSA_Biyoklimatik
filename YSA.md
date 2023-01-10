Yapay Sinir Aglari ile Biyoklimatik Konfor Alanlarını Modelleme
================

### Kullanılan Kütüphaneler

Yapay sinir ağları ile model üretmek için birçok farklı kütüphane
bulunmaktadır.

İlk olarak verimizi yüklüyoruz.

``` r
library(readxl)

veri <- read_xlsx("Veri_Yillik_2018.xlsx")

# Veriye ait çeşitli bilgileri görüntüleyelim.

# Sutün isimlerini öğrenmek için;
colnames(veri)
```

    ## [1] "x"      "y"      "sicak"  "nem"    "ruzHiz" "solRad" "fes"    "etiket"

``` r
# Verinin yapısını görmek istersek;
str(veri)
```

    ## tibble [1,091 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ x     : num [1:1091] 504810 505367 502505 510679 505959 ...
    ##  $ y     : num [1:1091] 4498274 4497779 4497575 4500075 4501780 ...
    ##  $ sicak : num [1:1091] 15.3 15.3 15.7 15.1 15.3 15.1 15.7 15.8 15.8 15.5 ...
    ##  $ nem   : num [1:1091] 79.6 79.6 79.5 79.7 80 79.7 79.5 79.5 79.4 80 ...
    ##  $ ruzHiz: num [1:1091] 2.1 2.1 2.2 2.1 2.1 2.1 2.2 2.2 2.2 2.1 ...
    ##  $ solRad: num [1:1091] 40.5 50.5 43.2 65.6 56.9 63.8 45.7 46 48.8 55.5 ...
    ##  $ fes   : num [1:1091] 10.3 10.6 10.7 10.8 10.8 10.8 10.8 10.9 11 11 ...
    ##  $ etiket: num [1:1091] 3 3 3 3 3 3 3 3 3 3 ...

``` r
# verinin boyutunu almak için;
dim(veri)
```

    ## [1] 1091    8

``` r
# Verinin özetini almak için;
summary(veri)
```

    ##        x                y               sicak            nem       
    ##  Min.   :444184   Min.   :4488569   Min.   :15.00   Min.   :72.90  
    ##  1st Qu.:465135   1st Qu.:4505116   1st Qu.:19.20   1st Qu.:78.30  
    ##  Median :484351   Median :4519128   Median :20.60   Median :78.90  
    ##  Mean   :482739   Mean   :4518441   Mean   :20.39   Mean   :78.98  
    ##  3rd Qu.:498689   3rd Qu.:4528633   3rd Qu.:21.60   3rd Qu.:79.80  
    ##  Max.   :529596   Max.   :4563288   Max.   :24.50   Max.   :83.70  
    ##      ruzHiz          solRad           fes            etiket     
    ##  Min.   :1.400   Min.   :24.80   Min.   :10.30   Min.   :3.000  
    ##  1st Qu.:2.100   1st Qu.:62.10   1st Qu.:15.10   1st Qu.:4.000  
    ##  Median :2.300   Median :65.00   Median :16.70   Median :4.000  
    ##  Mean   :2.389   Mean   :64.22   Mean   :16.39   Mean   :4.114  
    ##  3rd Qu.:2.700   3rd Qu.:67.80   3rd Qu.:17.75   3rd Qu.:4.000  
    ##  Max.   :3.300   Max.   :82.60   Max.   :20.60   Max.   :5.000

### Veri Önişlem

``` r
# Eksik veri var mi kontrol edilmeli
eksik_veri <- apply(veri, 2, function(x) sum(is.na(x)))
print(eksik_veri)
```

    ##      x      y  sicak    nem ruzHiz solRad    fes etiket 
    ##      0      0      0      0      0      0      0      0

``` r
# Verilerin ayni aralikta olmasi icin normalizasyon işlemi yapılır.
normalizasyon <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
veri <- veri %>%
  mutate(
    x=x,
    y=y, 
    ruzHiz = normalizasyon(ruzHiz), 
    solRad = normalizasyon(solRad),
    sicak = normalizasyon(sicak), 
    fes = fes,
    etiket = as.factor(etiket))
head(veri)
```

    ## # A tibble: 6 × 8
    ##         x        y  sicak   nem ruzHiz solRad   fes etiket
    ##     <dbl>    <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <fct> 
    ## 1 504810. 4498274. 0.0316  79.6  0.368  0.272  10.3 3     
    ## 2 505367. 4497779. 0.0316  79.6  0.368  0.445  10.6 3     
    ## 3 502505. 4497575. 0.0737  79.5  0.421  0.318  10.7 3     
    ## 4 510679. 4500075. 0.0105  79.7  0.368  0.706  10.8 3     
    ## 5 505959. 4501780. 0.0316  80    0.368  0.555  10.8 3     
    ## 6 510595. 4499534. 0.0105  79.7  0.368  0.675  10.8 3

``` r
# Veriler arasindaki korelasyona bakılır.
# Korelasyon incelenir.
cor2018 <- dplyr::select(veri,
                          "nem",
                          "ruzHiz",
                          "solRad",
                          "sicak",
                          "fes")
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(ggcorrplot)
```

    ## Zorunlu paket yükleniyor: ggplot2

``` r
cor.2018 = cor(cor2018)
ggcorrplot(cor.2018,
           type = "upper",
           legend.title = "Pearson Korelasyon Katsayisi",
           show.legend = TRUE,
           hc.order = T,
           colors = c("orange","white","darkgreen"),
           title = "Veriler Arasindaki Korelasyon",
           outline.color = "darkgray",
           ggtheme = ggplot2::theme_bw(),
           lab = F,
           show.diag = FALSE)
```

![](YSA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Model Oluşturma

``` r
# YSA icin veri olusturma
ysa2018 <- dplyr::select(veri,
                         "nem",
                         "ruzHiz",
                         "solRad",
                         "sicak",
                         "etiket")

# Veriyi eğitim ve test diye ayırma
library(rsample)

# Acik bir sekilde orneklemleri kontrol etmek istersek tabakali ayirma yapabiliriz.
# %70 egitim, %30 test olarak veriyi ayırıyoruz.
split_strat <- initial_split(ysa2018, prop=0.7, strata = "etiket")
train <- training(split_strat)
traincsv <- as.data.frame(train)
write.csv(traincsv,"train_70.csv")
test <- testing(split_strat)
testcsv <- as.data.frame(test)
write.csv(test,"test_70.csv")
table(train$etiket) %>% prop.table()
```

    ## 
    ##         3         4         5 
    ## 0.1061599 0.6736566 0.2201835

``` r
table(test$etiket) %>% prop.table()
```

    ## 
    ##         3         4         5 
    ## 0.1067073 0.6737805 0.2195122

``` r
library(neuralnet)
```

    ## 
    ## Attaching package: 'neuralnet'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     compute

``` r
library(NeuralNetTools)
library(NeuralSens)
library(caret)
```

    ## Zorunlu paket yükleniyor: lattice

``` r
# Model geçerlilik için k-fold yöntemi parametrelerini oluşturma
# k değeri 10 olarak girilmiştir.
tc_k_fold <- trainControl(method = "cv", number = 10)

# k-fold yontemi ile model kurma
fit_m1_kfold <- train(etiket ~ .,
                      data = train,
                      method= "nnet",
                      trControl= tc_k_fold,
                      maxit = 50,
                      metric = "Accuracy",
                      trace = FALSE)
fit_m1_kfold
```

    ## Neural Network 
    ## 
    ## 763 samples
    ##   4 predictor
    ##   3 classes: '3', '4', '5' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 686, 686, 687, 686, 688, 687, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   size  decay  Accuracy   Kappa    
    ##   1     0e+00  0.6736733  0.0000000
    ##   1     1e-04  0.6736733  0.0000000
    ##   1     1e-01  0.7851656  0.3587173
    ##   3     0e+00  0.7351053  0.1945599
    ##   3     1e-04  0.6736733  0.0000000
    ##   3     1e-01  0.9214083  0.7995198
    ##   5     0e+00  0.7260485  0.1702703
    ##   5     1e-04  0.7061408  0.1000000
    ##   5     1e-01  0.9526459  0.8949464
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were size = 5 and decay = 0.1.

``` r
fit_m1_kfold$results
```

    ##   size decay  Accuracy     Kappa  AccuracySD    KappaSD
    ## 1    1 0e+00 0.6736733 0.0000000 0.005918049 0.00000000
    ## 2    1 1e-04 0.6736733 0.0000000 0.005918049 0.00000000
    ## 3    1 1e-01 0.7851656 0.3587173 0.147653742 0.46687787
    ## 4    3 0e+00 0.7351053 0.1945599 0.132798531 0.41016828
    ## 5    3 1e-04 0.6736733 0.0000000 0.005918049 0.00000000
    ## 6    3 1e-01 0.9214083 0.7995198 0.108768752 0.31610053
    ## 7    5 0e+00 0.7260485 0.1702703 0.114483510 0.36573692
    ## 8    5 1e-04 0.7061408 0.1000000 0.103419431 0.31622777
    ## 9    5 1e-01 0.9526459 0.8949464 0.039959545 0.09621336

``` r
fit_m1_kfold$resample
```

    ##     Accuracy     Kappa Resample
    ## 1  0.8815789 0.7116358   Fold09
    ## 2  0.9200000 0.8335799   Fold05
    ## 3  0.8947368 0.7546408   Fold08
    ## 4  0.9740260 0.9462103   Fold02
    ## 5  0.9740260 0.9465649   Fold04
    ## 6  0.9610390 0.9156934   Fold07
    ## 7  0.9868421 0.9727403   Fold10
    ## 8  1.0000000 1.0000000   Fold01
    ## 9  0.9736842 0.9477304   Fold03
    ## 10 0.9605263 0.9206681   Fold06

``` r
plotnet(fit_m1_kfold)
```

![](YSA_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
summary(fit_m1_kfold)
```

    ## a 4-5-3 network with 43 weights
    ## options were - softmax modelling  decay=0.1
    ##  b->h1 i1->h1 i2->h1 i3->h1 i4->h1 
    ##   4.57   0.01   1.58   0.49  -9.95 
    ##  b->h2 i1->h2 i2->h2 i3->h2 i4->h2 
    ##  -0.01  -5.32  -0.10  -0.09  -0.27 
    ##  b->h3 i1->h3 i2->h3 i3->h3 i4->h3 
    ##   1.60  -0.10   2.53   2.67   8.24 
    ##  b->h4 i1->h4 i2->h4 i3->h4 i4->h4 
    ##  -0.03   0.00  -0.35  -0.11   0.10 
    ##  b->h5 i1->h5 i2->h5 i3->h5 i4->h5 
    ##   0.05   0.02  -0.12   0.08  -0.03 
    ##  b->o1 h1->o1 h2->o1 h3->o1 h4->o1 h5->o1 
    ##  -3.50  11.69  -0.17  -4.09  -2.24  -3.51 
    ##  b->o2 h1->o2 h2->o2 h3->o2 h4->o2 h5->o2 
    ##   1.38   0.99  -0.06   1.26  -0.88   1.32 
    ##  b->o3 h1->o3 h2->o3 h3->o3 h4->o3 h5->o3 
    ##   1.85 -12.82   0.20   2.65   3.14   2.03

``` r
# Egitim setindeki ozelliklerin önem değerleri
varImp(fit_m1_kfold)
```

    ## nnet variable importance
    ## 
    ##   variables are sorted by maximum importance across the classes
    ##        Overall      3      4      5
    ## sicak   100.00 100.00 100.00 100.00
    ## ruzHiz   63.37  63.37  63.37  63.37
    ## nem      24.34  24.34  24.34  24.34
    ## solRad    0.00   0.00   0.00   0.00

### Modeli Test Verisi Üzerinde Tahmin Ettirme

``` r
# Tahminler
pred_m1 <- predict(fit_m1_kfold, test)
pred_m1
```

    ##   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 3 4 3 3 4 4 4 3 4 4 4 3 4 4 4 4 4 4 4
    ##  [38] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ##  [75] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [112] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [149] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [186] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [223] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [260] 5 4 5 5 4 5 5 5 5 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## [297] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
    ## Levels: 3 4 5

``` r
# Hata matrisi
cm_m1 <- confusionMatrix(pred_m1, test$etiket)
print(cm_m1)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   3   4   5
    ##          3  22   0   0
    ##          4  13 221   7
    ##          5   0   0  65
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.939           
    ##                  95% CI : (0.9074, 0.9624)
    ##     No Information Rate : 0.6738          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8658          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 3 Class: 4 Class: 5
    ## Sensitivity           0.62857   1.0000   0.9028
    ## Specificity           1.00000   0.8131   1.0000
    ## Pos Pred Value        1.00000   0.9170   1.0000
    ## Neg Pred Value        0.95752   1.0000   0.9734
    ## Prevalence            0.10671   0.6738   0.2195
    ## Detection Rate        0.06707   0.6738   0.1982
    ## Detection Prevalence  0.06707   0.7348   0.1982
    ## Balanced Accuracy     0.81429   0.9065   0.9514
