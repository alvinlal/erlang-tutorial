#!/bin/bash

erl -make && cd ebin/ && erl -eval "application:start(simple_whatsapp)"