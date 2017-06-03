# passwords

This is a project originally built by and for the Austin Haskell Meetup. 

The main goals of this project are to highlight the differences between `Monad` and `Applicative` and also to introduce the useful `validation` library with its `AccValidation` datatype. 

`AccValidation` is interesting because its `Applicative` instance allows for error accumulation. Because `Applicative` functions do not short-circuit on an adverse input the way `Monad` does, the `AccValidation` type can use this parallel execution to accumulate (using `Semigroup`'s `mappend`) errors. In this small program, we want to tell the user if the username was invalid or the password or both. 

Because `AccValidation` comes from a library, we need to build this as a project and list the `validation` library in our `.cabal` file. 

## Prerequisites

To get the most out of this project, it's helpful if:

 - you are comfortable with Haskell syntax;  
 - you have a solid understanding of types and typeclasses;
 - you've been introduced to the `Applicative` and `Monad` typeclasses;

Things this project aims to demonstrate:  

 - a key difference between `Monad` and `Applicative`;  
 - initializing a Stack project.

## Overview

We aim to construct a record of a user that contains the username and password, only if those inputs meet *our very stringent* criteria. First, we strip any leading whitespace (and punctuation, in the case of the username) off the inputs, then check them to make sure they do not exceed our maximum lengths. Assuming the inputs pass those checks, we "pack" them into their newtype wrappers, `Username` and `Password`. Please note that `mkName` and `mkNm` are the same thing: one written with nested `case` expressions and the second with `(>>=)`. 

Now that we have valid usernames and passwords in their respective datatypes, we need to construct a `User` record that is a product of a `Username` and a `Password`. We use `validUser` and `validPwd` to give each possible error a unique error message. By using the applicative operator and the `AccValidation` type in `mkUser`, we can make a list of those error messages. The `display` function allows us to display those errors in a nice format. Finally, `main` calls for user input and brings all of it together to return a `User` record or one or more error messages. 

## Using this project


## Reading

