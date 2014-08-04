module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],
    
    clean: ["tmp", "output"],
  
    psc: {
      options: {
        modules: ["Main"]
      },
      lib: {
        src: ["<%=libFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=libFiles%>"]
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
 
  grunt.registerTask("make", ["psc:lib", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make"]);
};
