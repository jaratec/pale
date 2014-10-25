pale
----

ObjectPal code parser

ObjectPal is the (scripting? form?) language of the Paradox database.
I have developed this small parser in order to fix some problem in some legacy code that was poorly maintained. It does not follow any spec, I had to adapt to what was needed for the project at my workplace. So, be warned that the parser does deviate from normal ObjectPal code in one important way: it does handle attributes as if they were methods, that is, syntactically considers them to have brackets (like method calls). Also, it does not recognize most commands in the ObjectPal standard library.
My knowledge of ObjectPal is sketchy at best, so mistakes are entirely possible. I'm publishing the code, maybe it will help someone some day.

