var mocha = require ('mocha')
var comptest = require('__cy/cy/test/comptest/0.0.1')

describe('Cy compiler output', function(){
  
  describe('generated module', function(){
    it('should have __cy_source_md5', function(){
      comptest.__cy_source_md5.should.b.a('string');
    })
  })
})
