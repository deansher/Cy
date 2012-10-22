var mocha = require ('mocha')
var comptest = require('__staq/staq/test/comptest/0.0.1')

describe('Staque compiler output', function(){
  
  describe('generated module', function(){
    it('should have __staq_source_md5', function(){
      comptest.__staq_source_md5.should.b.a('string');
    })
  })
})
