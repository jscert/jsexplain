# ES5-2016 Changes

## ES5 Notes
### Object Internal Properties
#### Common to All Objects
* `[[Prototype]]`
* `[[Class]]`
* `[[Extensible]]`

* `[[Get]]`
* `[[GetOwnProperty]]`
* `[[GetProperty]]`
* `[[Put]]`
* `[[CanPut]]`
* `[[HasProperty]]`
* `[[Delete]]`
* `[[DefaultValue]]`
* `[[DefineOwnProperty]]`

#### Function Objects
* `[[Construct]]`         (Function/Constructors)
* `[[Call]]`              (Function)
* `[[HasInstance]]`       (Function)
* `[[Scope]]`             (Function)
* `[[FormalParameters]]`  (Function)
* `[[Code]]`              (Function)
* `[[TargetFunction]]`    (Function)
* `[[BoundThis]]`         (Bound Functions)
* `[[BoundArguments]]`    (Bound Functions)

#### Others
* `[[PrimitiveValue]]`    (boxes)
* `[[Match]]`             (RegExp)
* `[[ParameterMap]]`      (arguments)

### List Internal Specification Type
In ES5 lists are only used in cases where they may be treated as either a value or a reference type, assuming
specification variables are treated as per other value types.

## New Specification Items
### Specification Syntax
* `%Intrinsic%` objects are introduced, these appear to correspond somewhat to our existing prealloc labelling.

* Introduction of `?` and `!` operators to handle completion records in psuedocode.
  * `? x` is equivalent to: `ReturnIfAbrupt(x)`.
  * `! x` asserts that x will not be abrupt, and returns the value from the completion record of x:
```
Let cmp be x.
Assert: cmp is never an abrupt completion.
If cmp is a Completion Record, return cmp.`[[Value]]`.
```

### Completion Records Precisely Specified
New specification text has appeared for how to deal with operations on completion types:
* Constructors
  * NormalCompletion() (and implicit normal completions)
  * Completion() (assert that parameter is a completion record)
  * Throw (now precisely defined?)
  * ReturnIfAbrupt() (monadic unpacking of abrupt completions)
  * UpdateEmpty(completionRecord, value) (updates a non-throwing completion record with a value, if empty)

Completion types may also be implicitly coerced to or from a JS value where the
other is expected, and the completion type is normal. In the event of an abrupt
completion, nothing is specified.

### Object Internal Properties
* Object internal slots now can be created dynamically. In the specification at
  present, all calls to ObjectCreate have constant values for the
  internalSlotsList parameter. This means we can use our existing object
  structure of a record with all internal slots predefined, with those optional
  slots having an option type. All undeclared slots will have value None, all
  declared but uninitialised slots get Some undefined. This potentially means
  introducing undefined values for our multiple-dispatch behaviour. It also
  requires a type for the internal slots, so they may be referred to
  dynamically.
  * ObjectCreate(proto, internalSlotsList) now permits dynamic internal slot
    creation
  * OrdinaryCreateFromConstructor(constructor, intrinsicDefaultProto,
    internalSlotsList)

#### Essential Internal Methods
* `[[GetPrototypeOf]]`    (new accessor to `[[Prototype]]` internal property)
* `[[SetPrototypeOf]]`    (new accessor to `[[Prototype]]` internal property)
* `[[IsExtensible]]`      (new accessor to `[[Extensible]]` internal property)
* `[[PreventExtensions]]` (new accessor to `[[Extensible]]` internal property)
* `[[Set]]`               (renamed from `[[Put]]`??)
* `[[OwnPropertyKeys]]`
* Basic forms of Essential Internal Methods extracted to specification functions for reuse:
  * OrdinaryGetPrototypeOf(O)
  * OrdinarySetPrototypeOf(O,V)
  * OrdinaryIsExtensible(O)
  * OrdinaryPreventExtensions(O)
  * OrdinaryOwnPropertyKeys(O)
  * OrdinaryGetOwnProperty(O,P)
  * OrdinaryDefineOwnProperty(O,P,Desc)
    * IsCompatiblePropertyDescriptor(Extensible, Desc, Current)
    * ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current)
  * OrdinaryHasProperty(O,P)

##### Removed
* `[[GetProperty]]`
* `[[DefaultValue]]`
* `[[CanPut]]`
* `[[Enumerate]]` -- was present in ES2015, removed ES2016, see [28 Jan 2016
  TC39 Meeting Minutes](https://github.com/tc39/tc39-notes/blob/master/es7/2016-01/jan-28.md#5xix-proxy-enumerate---revisit-decision-to-exhaust-iterator)

#### Function Objects
##### Essential
* `[[Call]]`
* `[[Construct]]`

### Proxy Specifics
#### New Function Internal Slots
* `[[RevocableProxy]]`

#### Proxy Object Slots
* `[[ProxyTarget]]`
* `[[ProxyHandler]]`
* "Do not have a `[[Prototype]]` slot that requires initialization"

All the following are the standard object internal method slots:
* `[[GetPrototypeOf]]`
* `[[SetPrototypeOf]]`
* `[[IsExtensible]]`
* `[[PreventExtensions]]`
* `[[GetOwnProperty]]`
* `[[DefineOwnProperty]]`
* `[[HasProperty]]`
* `[[Get]]`
* `[[Set]]`
* `[[Delete]]`
* `[[Enumerate]]`
* `[[OwnPropertyKeys]]`
* `[[Call]]`
* `[[Construct]]`

#### Invariant Violations
It seems like this piece of work would be ideally suited to formal Coq proofs.

#### Referenced Functions
* `Type(handler)`
* `GetMethod(handler, "getPrototypeOf")`
* `Call(trap, handler, <<target>>)`
* `IsExtensible(target)`
* `SameValue(x, y)`
* `ToBoolean(x)`
* `ToPropertyDescriptor(x)`
* `CompletePropertyDescriptor(x)`
* `IsCompatiblePropertyDescriptor(x, y, z)`
* `FromPropertyDescriptor(x)`
* `IsPropertyKey(P)`
* `IsDataDescriptor(D)`
* `IsAccessorDescriptor(D)`
* `CreateListFromArrayLike(A, <<String, Symbol>>)`
* `CreateArrayFromList(L)`

#### ReferencedIntrinsics
* %ObjectPrototype%
* %Proxy%


### Misc
* Reference Specification Functions
  * GetThisValue(V)
  * InitializeReferencedBinding(V, W)

* Descriptor Specification Functions
  * CompletePropertyDescriptor(Desc)

* FunctionAllocate() given a name (ex-13.2)

* Built-in functions get their own special `[[Call]]` and `[[Construct]]` internals

* JSON.stringify gives writeable local variable access to subroutines

### List Internal Specification Type
Lists are now treated implicitly as reference-like values, for example in
[8.1.1.4.7 Global Environment Records DeleteBinding(N)](https://tc39.github.io/ecma262/#sec-global-environment-records-deletebinding-n)
step 7b, `varNames` is assigned the List value from `envNames.[[VarNames]]`. Subsequently `varNames` has an element
removed. The algorithm does not explicitly propagate this value back to the `envNames` record.


