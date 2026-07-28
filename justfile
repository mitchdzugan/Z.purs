[parallel]
xdom: xdom-build xdom-serve

xdom-build:
    vite build test/Web/HelloXDOM/ -w

xdom-serve:
    python3 -m http.server -d ./test/Web/HelloXDOM/dist/