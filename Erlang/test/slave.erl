-module(slave).
-compile(export_all).

loopslave(N) ->master ! {fromslave, N}.
