#!/bin/sh

#**** BEGIN LICENSE BLOCK *****
#
# Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
#
# The contents of this file are subject to the Apache 2 license:
# http://www.apache.org/licenses/LICENSE-2.0.html
#
# ***** END LICENSE BLOCK *****/

export NODE_PATH=/usr/local/lib/node_modules/

mocha comptest.js
