module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],
    
    clean: ["tmp", "output"],
  
    pscMake: {
      lib: {
        src: ["<%=libFiles%>"]
      }
    },

    dotPsci: ["<%=libFiles%>"]
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
 
  grunt.registerTask("make", ["pscMake:lib", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make"]);
};
