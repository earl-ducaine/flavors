


(defflavor stream () () (:required-methods :direction)
   (:documentation :base-flavor
    "all streams are built on this. This flavor is mostly for typep,
     but also provides default methods for messages which all streams,
     input or output, are required to handle."))
