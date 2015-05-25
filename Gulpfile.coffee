gulp = require 'gulp'
sass = require 'gulp-sass'
connect = require 'gulp-connect'
jade = require 'gulp-jade'
concat = require 'gulp-concat'
uglify = require 'gulp-uglify'
minifyCSS = require 'gulp-minify-css'
es = require "event-stream"
del = require "del"
order = require "gulp-order"
revall = require "gulp-rev-all"
autoprefixer = require 'gulp-autoprefixer'
shell = require 'gulp-shell'


dest = "out"
cdn = "cdn"
base = "src"

files =
  jade : 'src/index.jade'
  assets : ['src/assets/*', 'src/vendor/**']
  scripts : ['vendor/d3.js']
  styles : [
    'src/vendor/normalize.css'
    'src/vendor/fonts.css'

    'src/styles/main.sass'
  ]

processStyles = (files) ->
  sassFile = /\.sass/
  files.sass = []
  files.css = []
  for style, idx in files.styles
    if sassFile.test style
      files.sass.push style
      files.styles[idx] = style.replace sassFile, '.css'
    else files.css.push style

  # remove base from files.scripts
  baseReg =  new RegExp "^#{base}\\/"
  files.styles = files.styles.map (f) -> f.replace baseReg, ''


processStyles files

gulp.task 'sass', () ->
  gulp.src files.sass, base : base
    .pipe sass indentedSyntax: true
    .pipe autoprefixer()
    .pipe gulp.dest dest

gulp.task 'assets', () ->
  gulp.src files.assets, base : base
    .pipe gulp.dest dest

gulp.task 'jade', () ->

  gulp.src files.jade, base : base
    .pipe jade
      locals:
        prod: false
        scripts: files.scripts
        styles: files.styles
    .pipe gulp.dest dest


gulp.task 'watch', ->
  gulp.watch files.coffee, ['coffee']
  gulp.watch './src/styles/**/*.sass', ['sass']
  gulp.watch files.assets, ['assets']
  gulp.watch files.jade, ['jade']

gulp.task 'connect', ->
  connect.server
    root: dest
    port: 9000
    livereload: false

gulp.task 'connect:cdn', ->
  connect.server
    root: 'cdn'
    port: 9000
    livereload: false



gulp.task 'combine:styles',  ->
  files.styles = ['styles/all.min.css']
  sass = gulp.src files.sass, base: base
    .pipe sass indentedSyntax: true
    .pipe autoprefixer()
  css = gulp.src files.css, base: base
  es.merge sass, css
    .pipe concat files.styles[0]
    .pipe minifyCSS()
    .pipe gulp.dest dest

gulp.task 'min-jade', ['combine:styles' ], ->
  gulp.src files.jade, base : base
    .pipe jade
      locals:
        prod: true
        scripts: files.scripts
        styles: files.styles
    .pipe gulp.dest dest

gulp.task 'copy', ['min-jade'], ->
  gulp
    .src files.assets, base : base
    .pipe gulp.dest dest

gulp.task 'cdn', ['copy'], ->
    gulp.src "#{dest}/**"
      .pipe revall ignore: ['.html', '.jpg', '.png']
      .pipe gulp.dest cdn


gulp.task 'default', [
  'watch'
  'jade'
  'sass'
  'assets'
  'connect'
]

gulp.task 'dist', [
  'combine:styles'
  'min-jade'
  'copy'
  'cdn'
]
