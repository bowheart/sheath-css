/*
	Sheath-css
	Another library by Joshua Claunch -- https://github.com/bowheart
*/
if (typeof sheath !== 'function') throw new Error('Sheath-css Error: Sheath must be defined on the global scope before Sheath-css can be loaded.')

// Use sheath.registerMod() in place of an IIFE to house Sheath-css's code.
sheath.registerMod('css', function(link) {
	/*
		SheathCss -- A private utility for keeping track of/manipulating css modules.
	*/
	var SheathCss = {
		linkedModules: {},
		requestedModules: {},
		
		addModule: function(name) {
			this.linkedModules[name] = true
		},
		
		compileSheet: function() {
			var moduleNames = Object.keys(this.linkedModules)
			var sheet = ''
			
			for (var i = 0; i < moduleNames.length; i++) {
				var moduleVal = this.linkedModules[moduleNames[i]]
				if (typeof moduleVal === 'string') sheet += moduleVal
			}
			return sheet
		},
		
		error: function(msg) {
			throw new SyntaxError('Sheath-css Errror: ' + msg)
		},
		
		// Handler for 'css!' dependencies.
		handle: function(name, resolve, previous) {
			if (this.linkedModules[name] || this.requestedModules[name]) return // we've already requested this css module or it's already defined; nothing more to do
			
			// Defer attempting to load this css module until the current execution thread ends (in case it's defined between now and then).
			setTimeout(function() {
				if (this.linkedModules[name]) return
				this.requestedModules[name] = true
				
				var filename = sheath.config.asyncResolver(name, 'css!')
				if (!filename) return // no file found for this module
				
				sheath.load(filename)
			}.bind(this))
		},
		
		injectSheet: function() {
			var content = this.compileSheet()
			var blob = new Blob([content], {type: 'text/css'})
			var url = URL.createObjectURL(blob)
			var link = document.createElement('link')
			link.rel = 'stylesheet'
			link.href = url
			document.head.appendChild(link)
		}
	}
	
	
	/*
		ColorHelper -- A private utility for housing complex color-manipulation logic.
	*/
	var ColorHelper = {
		calcHue: function(r, g, b, delta, max) {
			return (function() {
				if (delta === 0) return 0
				if (max === r) return 60 * (((g - b) / delta) % 6)
				if (max === g) return 60 * (((b - r) / delta) + 2)
				return 60 * (((r - g) / delta) + 4)
			}() + 360) % 360 // ensure sure the number is positive
		},
		calcSaturation: function(delta, l) {
			if (delta === 0) return 0
			return delta / (1 - Math.abs(2 * l - 1))
		},
		calcLightness: function(max, min) {
			return (max + min) / 2
		},
		
		calcRed: function(c, x, h) {
			if (h < 60 || h >= 300) return c
			return h < 120 || h >= 240 ? x : 0
		},
		calcGreen: function(c, x, h) {
			if (h < 60 || h >= 180 && h < 240) return x
			return h < 180 ? c : 0
		},
		calcBlue: function(c, x, h) {
			if (h >= 300 || h >= 120 && h < 180) return x
			return h >= 180 ? c : 0
		},
		
		/*
			colorToHsla() -- Turn any valid css color string into an object with h, s, l, and a properties.
			See this method's error message for the list of valid formats.
		*/
		colorToHsla: function(str) {
			str = str.trim()
			if (str.slice(0, 3) === 'hsl') return this.parseHslStr(str)
			if (str[0] === '#') return this.parseHexStr(str)
			if (str.slice(0, 3) === 'rgb') return this.parseRgbStr(str)
			SheathCss.error('Invalid color string: "' + str + '". Valid formats: "#a", "#ac", "#fda", "#ef3a4b", "rgb(0, 0, 0)", "rgba(0, 0, 0, 0.3)", "hsl(0, 1%, 1%)", and "hsla(0, 1%, 1%, 0.3)".')
		},
		
		decToHex: function(dec, fixed) {
			var asHex = dec.toString(16)
			return fixed ? (Array(fixed + 1).join(0).split('') + asHex).slice(-fixed) : asHex
		},
		
		hexToDec: function(hex, fixed) {
			return parseInt(hex, 16)
		},
		
		hslaToRgba: function(h, s, l, a) {
			var c = (1 - Math.abs(2 * l - 1)) * s
			var x = c * (1 - Math.abs((h / 60) % 2 - 1))
			var m = l - c / 2
			var r = Math.round((this.calcRed(c, x, h) + m) * 255)
			var g = Math.round((this.calcGreen(c, x, h) + m) * 255)
			var b = Math.round((this.calcBlue(c, x, h) + m) * 255)
			return {r: r, g: g, b: b, a: a}
		},
		
		normalizePercent: function(percent) {
			if (typeof percent === 'number') return percent
			if (typeof percent !== 'string') SheathCss.error('Percentage must be a string or number. Received "' + typeof percent + '".')
			var asNumber = Number(percent.slice(-1) === '%' ? percent.slice(0, -1) : percent)
			if (isNaN(asNumber)) SheathCss.error('Invalid percentage, "' + percent + '"')
			return asNumber
		},
		
		/*
			parseHexStr() -- Parses a hexadecimal color string. String must start with '#' and contain 1, 2, 3, or 6 hexadecimal digits.
		*/
		parseHexStr: function(str) {
			str = str.slice(1)
			if (/[^0-9a-fA-F]/.test(str) || !~[1, 2, 3, 6].indexOf(str.length)) {
				SheathCss.error('Invalid hexadecimal color: "' + str + '". Valid formats: "#a", "#ac", "#fda", and "#ef3a4b".')
			}
			
			// Normalize to a 6-digit hex string
			if (str.length === 1) str = str + str + str + str + str + str
			if (str.length === 2) str = str + str + str
			if (str.length === 3) str = str[0] + str[0] + str[1] + str[1] + str[2] + str[2]
			
			var r = this.hexToDec(str.slice(0, 2))
			var g = this.hexToDec(str.slice(2, 4))
			var b = this.hexToDec(str.slice(4))
			
			return this.rgbaToHsla(r, g, b, 1)
		},
		
		parseHslStr: function(str) {
			var split = str.split('(')
			var func = split.shift().trim()
			if (!~['hsl', 'hsla'].indexOf(func)) SheathCss.error('Invalid color function, "' + func + '".')
			
			var params = split.shift().split(',').map(function(number) {
				return number.replace(/\s/g, '')
			})
			if (func === 'hsl' && params.length !== 3) SheathCss.error('The hsl() color function takes exactly 3 parameters. ' + params.length + ' given. Source: "' + str + '".')
			if (func === 'hsla' && params.length !== 4) SheathCss.error('The hsla() color function takes exactly 4 parameters. ' + params.length + ' given. Source: "' + str + '".')
			
			var h = Number(params.shift())
			if (isNaN(h)) SheathCss.error('Hue must be a number. Source: "' + str + '".')
			
			var sStr = params.shift()
			var s = Number(sStr.slice(0, -1))
			if (s !== 0 && sStr.slice(-1) !== '%' || isNaN(s)) SheathCss.error('Saturation must be a percentage value. Source: "' + str + '".')
			
			var lStr = params.shift()
			var l = Number(lStr.slice(0, -1))
			if (l !== 0 && lStr.slice(-1) !== '%' || isNaN(l)) SheathCss.error('Lightness must be a percentage value. Source: "' + str + '".')
			
			var aStr = params.shift()
			var a = Number(aStr)
			if (aStr && isNaN(a) || a < 0 || a > 1) SheathCss.error('Alpha must be a number between 0 and 1')
			
			return {h: h, s: s, l: l, a: a}
		},
		
		parseRgbStr: function(str) {
			var split = str.split('(')
			var func = split.shift().trim()
			if (!~['rgb', 'rgba'].indexOf(func)) SheathCss.error('Invalid color function, "' + func + '".')
			
			var params = split.shift().split(',').map(function(number) {
				return number.replace(/\s/g, '')
			})
			if (func === 'rgb' && params.length !== 3) SheathCss.error('The rgb() color function takes exactly 3 parameters. ' + params.length + ' given. Source: "' + str + '".')
			if (func === 'rgba' && params.length !== 4) SheathCss.error('The rgba() color function takes exactly 4 parameters. ' + params.length + ' given. Source: "' + str + '".')
			
			var r = Number(params.shift())
			if (isNaN(r)) SheathCss.error('Red value must be a number. Source: "' + str + '".')
			
			var g = Number(params.shift())
			if (isNaN(g)) SheathCss.error('Green value must be a number. Source: "' + str + '".')
			
			var b = Number(params.shift())
			if (isNaN(b)) SheathCss.error('Blue value must be a number. Source: "' + str + '".')
			
			var aStr = params.shift()
			var a = Number(aStr)
			if (aStr && isNaN(a) || a < 0 || a > 1) SheathCss.error('Alpha must be a number between 0 and 1')
			
			return this.rgbaToHsla(r, g, b, a)
		},
		
		rgbaToHsla: function(r, g, b, a) {
			r = r / 255
			g = g / 255
			b = b / 255
			var max = Math.max(r, g, b)
			var min = Math.min(r, g, b)
			var delta = max - min
			
			var l = this.calcLightness(max, min)
			var s = this.calcSaturation(delta, l)
			var h = this.calcHue(r, g, b, delta, max)
			
			return {h: h, s: s, l: l, a: a}
		},
		
		rgbToHex: function(r, g, b) {
			var hex = this.decToHex(r, 2) + this.decToHex(g, 2) + this.decToHex(b, 2)
			
			// Reduce to a 3-digit hexadecimal number, if possible (e.g. #ff00dd -> #f0d).
			if (hex[0] === hex[1] && hex[2] === hex[3] && hex[4] === hex[5]) hex = hex[0] + hex[2] + hex[4]
			return '#' + hex
		}
	}
	
	
	/*
		Context -- A class for tracking nested selectors in css snippets.
	*/
	var Context = function(parser, parentContext, selectors) {
		this.parser = parser
		this.parentContext = parentContext
		this.content = []
		this.buffer = ''
		this.childrenContexts = []
		this.childContext = null
		this.selectors = selectors || []
	}
	Context.prototype = {
		add: function(str) {
			if (str === ';') return this.flushBuffer(str)
			this.buffer += str
		},
		
		childContextStart: function() {
			if (this.parser.peek() !== '{') return false
			
			this.parser.next() // discard the '{'
			return true
		},
		
		childContextEnd: function() {
			this.childrenContexts.push(this.childContext)
			this.childContext = null
			return this
		},
		
		combinedSelectors: function() {
			if (this.cachedSelectors) return this.cachedSelectors
			
			var selectors = this.selectors
			if (!selectors.length) return this.cachedSelectors = selectors
			
			var parentSelectors = this.parentContext.combinedSelectors()
			if (!parentSelectors.length) return this.cachedSelectors = selectors
			
			return this.cachedSelectors = parentSelectors.map(function(parentSelector) {
				return selectors.map(function(selector) {
					return parentSelector + (selector[0] === '&' ? selector.slice(1) : ' ' + selector)
				})
			}).reduce(function(mem, next) {
				return mem.concat(next)
			}, [])
		},
		
		contextEnd: function() {
			if (this.parser.peek() !== '}') return false
			
			this.flushBuffer()
			this.parser.next() // discard the '}'
			return true
		},
		
		flushBuffer: function(str) {
			if (str) this.buffer += str
			this.buffer = this.buffer.trim()
			if (!this.buffer) return // nothing to do
			
			this.content.push(this.buffer)
			this.buffer = ''
		},
		
		newChildContext: function(content) {
			this.flushBuffer()
			if (!content && typeof this.content[this.content.length - 1] !== 'string') SheathCss.error('Unexpected token, "{"')
			
			return this.childContext = contextFactory(this.parser, this, content || this.content.pop())
		},
		
		toString: function() {
			this.flushBuffer() // put anything still in the buffer on there
			
			var content = this.content.join('')
			if (!content) return this.childrenContexts.join('')
			
			var str = this.combinedSelectors().join(',') + '{' + content + '}'
			return str + this.childrenContexts.join('')
		}
	}
	
	
	var MediaQueryContext = function(parser, parentContext) {
		Context.call(this, parser, parentContext)
		this.declaration = []
		this.inDeclaration = true
		this.declarationBuffer = ''
		this.inExpression = false
		this.expressionBuffer = ''
		this.expressionIgnoreCount = 0
	}
	MediaQueryContext.prototype = Object.create(Context.prototype, {
		add: { value: function(str) {
			if (!this.inDeclaration) return Context.prototype.add.call(this, str)
			if (str === '{') {
				this.parser.next() // discard the '{'
				return this.inDeclaration = false
			}
			return this.addToDeclaration(str)
		} },
		
		addToDeclaration: { value: function(str) {
			if (str === '(') {
				if (this.inExpression) this.expressionIgnoreCount++
				else return this.expressionStart(str)
			} else if (str === ')') {
				if (!this.inExpression) SheathCss.error('Unexpected token "' + str + '" in media query declaration.')
				if (this.ignoreCount) this.ignoreCount--
				else return this.expressionEnd(str)
			}
			if (this.inExpression) this.expressionBuffer += str
			else this.declarationBuffer += str
		} },
		
		childContextStart: { value: function() {
			if (this.inDeclaration) return false
			return Context.prototype.childContextStart.call(this)
		} },
		
		expressionStart: { value: function(str) {
			this.flushDeclarationBuffer()
			this.expressionBuffer += str // stick the opening '(' on there
			this.inExpression = true
		} },
		
		expressionEnd: { value: function(str) {
			this.expressionBuffer += str // stick the closing ')' on there
			this.flushExpressionBuffer()
			this.inExpression = false
		} },
		
		flushDeclarationBuffer: { value: function() {
			this.declarationBuffer = this.declarationBuffer.trim()
			if (!this.declarationBuffer) return // nothing to do
			
			this.declaration.push(this.declarationBuffer)
			this.declarationBuffer = ''
		} },
		
		flushExpressionBuffer: { value: function() {
			this.expressionBuffer = this.expressionBuffer.trim()
			if (!this.expressionBuffer) return // nothing to do
			
			this.declaration.push(this.expressionBuffer)
			this.expressionBuffer = ''
		} },
		
		toString: { value: function() {
			var content = Context.prototype.toString.call(this)
			if (!content) return '' // empty media query; ignore
			
			return '@media ' + this.declaration.join(' ') + '{' + content + '}'
		} },
		
		cachedSelectors: { get: function() {
			return this.parentContext.combinedSelectors()
		} }
	})
	
	
	var RootContext = function() {
		Context.apply(this, arguments)
	}
	RootContext.prototype = Object.create(Context.prototype, {
		contextEnd: { value: function() {
			if (this.parser.peek() !== '}') return false
			
			SheathCss.error('Mismatched brackets. You\'re missing an opening "{"')
		} },
		
		toString: { value: function() {
			this.flushBuffer()
			var str = this.content.join('')
			str += this.childrenContexts.join('')
			return str
		} }
	})
	
	
	function contextFactory(parser, parentContext, selectors) {
		if (!selectors) return new RootContext(parser)
		if (selectors === '@media') return new MediaQueryContext(parser, parentContext)
		
		selectors = selectors.split(',').map(function(selector) {
			return selector.trim()
		})
		return new Context(parser, parentContext, selectors)
	}
	
	
	/*
		CssParser -- Parse a nested css snippet
	*/
	var CssParser = function(input) {
		this.input = input
		this.context = contextFactory(this)
	}
	CssParser.prototype = {
		quoteRegex: /['"`]/,
		whitespaceRegex: /\s/,
		
		parse: function() {
			var next
			while (next = this.peek()) {
				if (this.quoteRegex.test(next)) this.STRING()
				else if (next === '/') this.COMMENT()
				else if (this.whitespaceRegex.test(next)) this.WHITESPACE()
				else if (next === '@') this.AT_RULE()
				else if (this.context.childContextStart()) this.newContext()
				else if (this.context.contextEnd()) this.endContext()
				else this.context.add(this.next())
			}
			if (!(this.context instanceof RootContext)) SheathCss.error('Mismatched brackets. You\'re missing a closing "}".')
			return this.context.toString()
		},
		
		next: function(length) {
			var next = length ? this.input.slice(0, length) : this.input[0]
			this.input = this.input.slice(length || 1)
			return next
		},
		
		peek: function(length) {
			return length ? this.input.slice(0, length) : this.input[0]
		},
		
		newContext: function(content) {
			this.context = this.context.newChildContext(content)
		},
		
		endContext: function() {
			this.context = this.context.parentContext.childContextEnd()
		},
		
		lineComment: function() {
			var content = ''
			var next
			while ((next = this.peek()) && next !== '\n') {
				this.next() // discard
				content += next
			}
			this.context.add(content, 'COMMENT')
		},
		blockComment: function() {
			// Get the '/*'
			var content = this.next() + this.next()
			var next
			
			while ((next = this.peek(2)) && next !== '*/') {
				content += this.next()
			}
			// Get the '*/'
			content += this.next() + this.next()
			
			this.context.add(content, COMMENT)
		},
		
		AT_RULE: function() {
			if (this.peek(6) !== '@media') return this.context.add(this.next())
			
			this.newContext(this.next(6))
		},
		
		COMMENT: function() {
			var next = this.peek(2)
			if (next === '//') return this.lineComment()
			if (next === '/*') return this.blockComment()
			this.context.add(this.next())
		},
		
		STRING: function() {
			var type = this.next() // double or single quote or backtick (multi-line)
			var next
			var str = type
			
			while ((next = this.peek()) && next !== type) {
				switch (next) {
					case '\\':
						this.next() // strip the escape character
						str += this.next() // stick whatever comes next on there
						continue
					case '\n':
						if (type !== '`') SheathCss.error('Unexpected multi-line string')
					default:
						str += this.next()
				}
			}
			if (next !== type) SheathCss.error('Unexpected end of file')
			str += this.next() // remove the closing quote
			this.context.add(str, 'STRING')
		},
		
		WHITESPACE: function() {
			this.context.add(this.next(), 'WHITESPACE')
		}
	}
	
	
	/*
		sheath.css() -- A utility for creating modular css with dependency injection.
		Signature is exactly the same as sheath()
	*/
	var css = function(name, deps, factory) {
		SheathCss.addModule(name)
		
		link('css!' + name, deps, function() {
			var result = factory.apply(this, arguments)
			SheathCss.linkedModules[name] = result || this.exports
			return null // null will always get injected into dependents of css! modules
		})
	}
	
	
	/*
		sheath.css.evaluate() -- Turn nested, sass-style css selectors into legit, browser-friendly css.
	*/
	css.evaluate = function(css) {
		if (typeof css !== 'string') {
			throw new TypeError('Sheath-css Error: sheath.css.evaluate() expects a string. Received "' + typeof css + '".')
		}
		var parser = new CssParser(css)
		var result = parser.parse()
		return result
	}
	
	
	/*
		sheath.css.inject() -- Create a blob url and insert a link tag into the dom with that url as its href.
	*/
	css.inject = function() {
		SheathCss.injectSheet()
	}
	
	
	/*
		sheath.jsExpr() -- Run a JavaScript expression in the middle of a css snippet
		Returned value depends on the type of its argument:
			Array -- join with no delimiter
			Object -- output the key-val pairs as css prop-val pairs -- {margin: 0} -> 'margin: 0;'
			Everything else -- output directly
	*/
	css.jsExpr = function(expr) {
		if (Array.isArray(expr)) return expr.join('')
		
		// Non-objects and objects with custom toString() properties get stringified and returned directly.
		if (expr === null || typeof expr === 'undefined') return ''
		if (typeof expr.toString !== 'function') throw new Error('Sheath.js Error - sheath.css.jsExpr() - Expression value must have a toString() method')
		
		var asString = expr.toString()
		if (asString !== Object.prototype.toString.call(expr)) return asString
		
		// It's an object. Compile all the key-val pairs.
		return Object.keys(expr).map(function(key) {
			return key + ': ' + expr[key] + ';'
		}).join('')
	}
	
	
	/*
		sheath.css.sheet() -- Creates and returns the entire compiled stylesheet as a string.
	*/
	css.sheet = function() {
		return SheathCss.compileSheet()
	}
	
	
	/*
		sheath.css.utils -- Provide basic color manipulation utilities for the rest of the app.
	*/
	css.utils = sheath(function() {
		var Color = function(str) {
			if (!(str instanceof Color) && typeof str !== 'string') {
				throw new TypeError('SheathCss.utils error: Colors must be a string or another instance of SheathCss.utils.Color. Received "' + typeof str + '".')
			}
			var hsla = str instanceof Color ? str.hsla() : ColorHelper.colorToHsla(str)
			
			this.h = hsla.h
			this.s = hsla.s
			this.l = hsla.l
			this.a = hsla.a
		}
		Color.prototype = {
			clone: function() { return new Color(this) },
			
			hsla: function() {
				// Normalize values. Ensure that h is in the 0-359 range.
				this.h = (this.h % 360 + 360) % 360
				
				// Ensure that s, l, and a are in the 0-1 range.
				this.s = Math.max(0, Math.min(1, this.s))
				this.l = Math.max(0, Math.min(1, this.l))
				this.a = Math.max(0, Math.min(1, this.a))
				return {h: this.h, s: this.s, l: this.l, a: this.a}
			},
			
			darken: function(percent) {
				var newColor = this.clone()
				return newColor._darken(percent)
			},
			_darken: function(percent) {
				percent = ColorHelper.normalizePercent(percent) / 100
				this.l -= percent
				return this
			},
			
			lighten: function(percent) {
				var newColor = this.clone()
				return newColor._lighten(percent)
			},
			_lighten: function(percent) {
				percent = ColorHelper.normalizePercent(percent) / 100
				this.l += percent
				return this
			},
			
			toHex: function() {
				var hsla = this.hsla()
				var rgba = ColorHelper.hslaToRgba(hsla.h, hsla.s, hsla.l, hsla.a)
				return ColorHelper.rgbToHex(rgba.r, rgba.g, rgba.b)
			},
			
			toRgba: function() {
				var hsla = this.hsla()
				var rgba = ColorHelper.hslaToRgba(hsla.h, hsla.s, hsla.l, hsla.a)
				return 'rgba(' + rgba.r + ',' + rgba.g + ',' + rgba.b + ',' + rgba.a + ')'
			},
			
			toString: function() {
				return this.alpha === 1 ? this.toHex() : this.toRgba()
			},
			
			get hue() { return this.h },
			get saturation() { return this.s },
			get lightness() { return this.l },
			get alpha() { return this.a }
		}
		
		/*
			SheathCss.utils.darken() -- Ensures that c is a Color and darkens it by percentage.
		*/
		var darken = function(c, percentage) {
			if (c instanceof Color) return c.darken(percentage)
			c = color(c)
			return c._darken(percentage) // use the mutating form, since we just created it
		}
		
		/*
			SheathCss.utils.lighten() -- Ensures that c is a Color and lightens it by percentage.
		*/
		var lighten = function(c, percentage) {
			if (c instanceof Color) return c.lighten(percentage)
			c = color(c)
			return c._lighten(percentage) // use the mutating form, since we just created it
		}
		
		/*
			SheathCss.utils.color() -- A factory for creating instances of the Color class.
		*/
		var color = function(c) {
			return new Color(c)
		}
		
		return {
			Color: Color,
			darken: darken,
			lighten: lighten,
			color: color
		}
	})
	
	
	// Expose some api:
	if (typeof module === 'object' && typeof module.exports === 'object') {
		module.exports = css
	} else if (typeof window === 'object') {
		window.SheathCss = css
	}
	
	
	/*
		CssMod -- The actual mod we're registering with Sheath.
	*/
	var CssMod = {
		api: css,
		handle: SheathCss.handle.bind(SheathCss)
	}
	
	return CssMod
})
