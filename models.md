# Models

## Adding model support to LecoSpec

To add a model with a class MyModel, define two functions:
```R
    get_required_veg_indices.MyModel <- function(ml_model, ...){}
```
Which should return the vegetation indices used in the model, and

```R
    apply_model.MyModel <- function(df, model, ...){

        return()
    }
```

Which should return the model predictions as a data frame