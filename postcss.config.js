class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-Za-z0-9-_:\/]+/g) || [];
  }
}

module.exports = {
  plugins: [
    require('tailwindcss')('tailwind.js'),
    require('autoprefixer'),
    require('postcss-preset-env')({
      features: {
        'nesting-rules': true
      }
    }),
    process.env.NODE_ENV == 'production' ? require('@fullhuman/postcss-purgecss')({ content: ['**/*.{elm,pug}'], extractors: [{ extractor: TailwindExtractor, extensions: ['elm', 'pug'] }] }) : null
  ].filter(plugin => plugin)
}
