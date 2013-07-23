module.exports = function(grunt) {
    // Project configuration.
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        browserify2: {
            compile: {
                entry: "./public/js/main.js",
                compile: "./public/min/pastebeest.js"
            }
        },
        uglify: {
            options: {
                banner: '/*! <%= pkg.name %> <%= grunt.template.today("yyyy-mm-dd") %> */\n'
            },
            client: {
                src: 'public/min/pastebeest.js',
                dest: 'public/min/pastebeest.min.js'
            },
            server: {
                src: 'pastebeest.js',
                dest: 'server.js'
            }
        },
        jshint: {
            all: [
                "pastebeest.js",
                "settings.js",
                "public/js/**/*.js"
            ]
        }
    });

    // Load the plugin that provides the "uglify" task.
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-browserify2');
    grunt.loadNpmTasks('grunt-contrib-jshint');

    // Default task(s).
    grunt.registerTask('default', ['jshint', 'browserify2', 'uglify']);
};