NotSoFast: NotSoFast-exe app.js


start: NotSoFast
	stack exec NotSoFast-exe


install: NotSoFast
	stack install


clean:
	rm -rf .stack-work/
	rm -f ./dist/app.js
	rm -rf ./client/.pulp-cache
	rm -rf ./client/bower_components
	rm -rf ./client/output


app.js:
	cd ./client && \
	bower install && \
	pulp build --optimise --to ../dist/app.js


NotSoFast-exe: setup
	stack build


setup:
	stack setup
