module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],

    clean: ["tmp", "output"],

    connect: {
      server: {
        options: {
          keepalive: true,
          port: 8000
        }
      }
    },

    psc: {
      options: {
        main: "Main",
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
  grunt.loadNpmTasks('grunt-contrib-connect');
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("make", ["psc:lib", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make"]);
};
