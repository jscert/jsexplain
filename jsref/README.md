# JSRef'
  This directory contains a reference interpreter for ECMAScript, designed to be
  written in a syntax that is structurally close to [the
  standard](https://tc39.github.io/ecma262/).

## Monadic Binders
  The ECMAScript standard uses the `!` and `?` prefix operators as shorthands
for the abstract operations of `ReturnIfAbrupt` and ...
  ~_ret~ forms of binders are provided to be wrapped in the Return monad for
  use inside of a ~let%ret~,
  UPPERCASE forms of binders are the ~!~/"never abrupt" spec-text equivalent
  binders to the standard (~ReturnIfAbrupt~/~?~) binders.

## Naming Conventions
  Function names are lower, snake\_cased versions of those defined in the
  standard.

### Object Internal Methods
  Object internal methods which are defined with the `[[FunctionName]]` syntax
  in the standard are prefixed with `object_internal_`.

  As object internal methods use dynamic dispatch, the `object_internal_` form
  of the method will be a dispatch function. With further functions defined for
  each applicable exotic object's implementation.

  The name of the exotic object will be prefixed to the function name. For
  example, `ordinary_object_internal_foo` and `proxy_object_internal_foo` could
  be defined for the `[[Foo]]` internal method for Ordinary and Proxy objects.
  `object_internal_foo` would be the function to dispatch a `O.[[Foo]]` call to
  the appropriate function depending on the implementations defined on the `O`
  object.
<<<<<<< HEAD
||||||| merged common ancestors

### Type Constructor Names
  At present, many type constructors are prefixed with the string ``. This
  is for legacy reasons only, and is to be removed in the future. (See issue
  #19)


=======

### Type Constructor Names
  At present, many type constructors are prefixed with the string ``. This
  is for legacy reasons only, and is to be removed in the future. (See issue
  #19)


>>>>>>> Coq prefix
