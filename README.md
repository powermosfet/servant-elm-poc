_servant-elm-poc_
=================

A boilerplate repo for running servant + elm on heroku

#Heroku setup:

    $ heroku buildpacks:set https://github.com/srid/heroku-buildpack-elm
    $ heroku buildpacks:add --index 2 https://github.com/mfine/heroku-buildpack-stack
