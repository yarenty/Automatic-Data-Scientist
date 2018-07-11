package com.yarenty.ml.api.types


case class Metadata(
                     name: String,
                     field: String,
                     value: String
                   )

case class Operation(
                      name: String,
                      metadata: List[Metadata],
                      done: Boolean,
                      error: String,
                      response: Metadata
                    )


