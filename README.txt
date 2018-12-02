===============================================================================
FACILSERVIL : `Easy Server`                           Dead-simple, dead-useful.
===============================================================================

Sometimes, it's just annoying and time-draining to deal with all of the
intricacies of :usockets-- if, to that, you say something like...

        "I just want to write a simple TCP server, dammit!
         There's got to be an easier way!"

... then Facilservil is for you!

Facilservil ("easy server") abstracts away the entire server bits, allowing you
to focus on flexibility.


----------------------------------------
FEATURES
----------------------------------------

 * Multi-user
 * Shutdown protection
 * Input-handling
 * Simple logging system
 * Flexibility
 * UTF-8


----------------------------------------
DEMONSTRATION
----------------------------------------

Load up Facilservil, then run this in your repl:
    
       (facilservil:ex-server 8888)

Now, connect your computer on port 8888!
If you're on LiGNUx or BSD, you can use "telnet localhost 8888"

It'll show you a pretty example server. :)

In reality, 'ex-server is just a small function for demonstration-- the
example server really looks like this:

        (facilservil:server "0.0.0.0" port
	                    'facilservil:connect-ex 'facilservil:disconnect-ex
			    'facilservil:input-handle-ex)

It runs #'connect-ex when you connect, #'disconnect-ex when you disconnect,
and #'input-handle-ex after you finish a command, to handle your input.

If you wanna see this example's code (is there a better way to learn?), look
in "/src/ex.lisp".

For a different kind of example (less interesting, since it takes no user
input), look at QOTDD (https://git.eunichx.us/qotdd).

For a more comprehensive guide to Facilservil, look to USAGE, coming right up.


----------------------------------------
USAGE
----------------------------------------

To use Facilservil, just use the `facilservil:server` function somewhere:

        (facilservil:server host port
                            connecting disconnecting input-handler
                            &key (command-byte 10) (halting 'halt-ex))

"host" and "port" are, obviously, the host-IP and port, respectively.
"connecting"	is the function that will be executed when a user connects.
"disconnecting"	is the function that will run when someone disconnects.
"input-handler"	interprets the input of a user, when they complete a command.
"command-byte"	is the byte which determines if they've completed a command.
            	it's 10 by default, which is newline.
"halting"    	is the function executed when the server shuts down.


Basically, you write the "connecting", "disconnecting", and "input-handler"
functions (maybe "halting"), and you've got a handy-dandy server.

These functions you write must accept the following arguments:
        connecting	(socket client-id)
        disconnecting	(socket client-id)
        input-handler	(socket client-id input-string)
	halting    	()

Let's say that you want to write a connecting function which sends a friendly
"welcome" message. You'd do that like this, just about:

        (defun my-connecting-function (socket client-id)
	  (client-write socket
	    (format nil "Hey, welcome to this server, ~A! <3" client-id)
	    'T))

This will write to the connecting user's socket (which is passed to your
function), "Hey, welcome to this server," followed by their client-id.

Every user, upon connection, is given a client ID number which is correlated
with their connection socket. This ID number is random, and can be up to
999999 in value.

To send a message to a user, get their socket, and run
          (facilservil:client-write socket string &optional newline)

"newline", which defaults to nil, determines whether or not your message is
followed with a newline.

I'll give you one more quick example-- then you'll probably get usage.

Let's write a quick input handler.
           (defun my-input-handler (socket client-id input-string)
	     (if (equal input-string "Hello")
		(facilservil:client-write socket "And hi to you!" 't)
		(facilservil:client-write socket "You won't even say hi?" 'T)))

It should be pretty self-explanatory.
You just scale up from them-- write a nice command parser or something, and
you're on your way! :)

Now, one more thing: to run the server with your new functions, just do this:

            (facilservil:server host port
	    'my-connecting-function 'facilservil:disconnect-ex
	    'my-input-handler)


TIP: When it comes to user data, you might want to use a hash-table or two which
     correlate client-ids and user data. ^_^


----------------------------------------
NON-FEATURES (TODO)
----------------------------------------

 * Ctrl-C etc will crash servers
 * Some UTF characters don't go over well
 * Internationalization should be supported--
   right now, logs etc are always in English.
   Not cool.
 * You should be able to log to a file.


----------------------------------------
BORING STUFF
----------------------------------------
License is in COPYING (GNU GPLv3)
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/facilservil
