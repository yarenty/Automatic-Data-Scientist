Intro
-----

This is basic demo where we copy functionality of FMA over MLaaS REST API interface.
Demo will show interaction of REST API with background ML services.


Since there is only week for preparation:
 - all interaction will be through POSTMAN
 - interaction will be prepared ahead
 - no security 
 - minimal validation 
 - only FMA functionality (no prediction servicing)
 - no GPU access => no H2O GPU models [or priv. laptop]
 - no GPU access => no H2O GPU model selection [or priv. laptop]
 - no GPU access => no Tensorflow [or priv. laptop]



# STEPS


## List of datasets


```
GET http:\\localhost:8080\v1\datasets
```


## Choose dataset

```
GET http:\\localhost:8080\v1\datasets\name
```


*TODO: check dataset description!*


## ADS - empty list
```
GET  http:\\localhost:8080\v1\ads
```

*List of ADS with statuses - nothing working*


## ADS - create new one
``` 
POST http:\\localhost:8080\v1\ads
```

JSON body:
```
{  
   "name":"kpi_AD",
   "dataset":{  
      "train":"ClusterKPI48",
      "test":"",
      "valid":""
   },
   "afe":{  
      "levels":1,
      "allTransformations":false,
      "transformations":[  
         {  
            "algorithm":"DayOfWeek",
            "params":null
         },
         {  
            "algorithm":"HourOfDay",
            "params":null
         },
         {  
            "algorithm":"MinuteOfDay",
            "params":null
         }
      ]
   },
   "algorithms":{  
      "allAlgorithms":false,
      "objective":"RMSE",
      "algorithms":[  
         {  
            "name":"GBM",
            "hyperParameters":[  
               "ntrees=40",
               "min_depth=3",
               "max_depth=3",
               "distribution=gaussian"
            ]
         }
      ]
   }
}
```


## See response


## Check back server logs


## Ask about operation output


## Model create
```
POST http:\\localhost:8080\v1\models
```

JSON body:
```
{
}
```

## Model version create
```
POST http:\\localhost:8080\v1\models\1\version
```

JSON body
```
{
}
```


## Model get description
```
GET http:\\localhost:8080\v1\models\1
```

*Check output - there is model with default version informaiton*




DATA
----
