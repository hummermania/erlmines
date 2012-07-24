#!/bin/bash

#erl -noshell -pa ./ebin -eval 'application:start(erlmines)'
erl -smp auto -pa ./ebin -eval 'application:start(erlmines)'
