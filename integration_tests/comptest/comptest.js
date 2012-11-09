/***** BEGIN LICENSE BLOCK *****
 *
 * Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
 *
 * The contents of this file are subject to the Apache 2 license:
 * http://www.apache.org/licenses/LICENSE-2.0.html
 *
 * ***** END LICENSE BLOCK *****/


var mocha = require ('mocha')
var comptest = require('__cy/cy/test/comptest/0.0.1')

describe('Cy compiler output', function(){
  
  describe('generated module', function(){
    it('should have __cy_source_md5', function(){
      comptest.__cy_source_md5.should.b.a('string');
    })
  })
})
