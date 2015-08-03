all :
	tsc --noImplicitAny --outDir _build --project src

_build/main.min.js : _build/main.js
	ccjs _build/main.js --compilation_level=ADVANCED_OPTIMIZATIONS > _build/main.min.js
