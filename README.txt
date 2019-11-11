===============================================================================
FACILSERVIL : `Easy Server`
===============================================================================

Sometimes, it's just annoying and time-draining to deal with all of the
intricacies of :usockets— facilservil abstracts away all of those bits.


----------------------------------------
FEATURES
----------------------------------------

 * Multi-threaded
 * Input-handling
 * Logging system
 * UTF-8


----------------------------------------
DEMONSTRATION
----------------------------------------

Load up Facilservil, then run this in your repl:
    
       (facilservil:ex-server "localhost" 8888)

Now, connect your computer on port 8888.
This example server is a chat server, so it might be useful to connect a couple
of times, for demonstration's sake.

If you're on LiGNUx or BSD, you can use "telnet localhost 8888"


In reality, 'ex-server is just a small function for demonstration-- the
example server really looks like this:

	(facilservil:server host port
	                    #'ex-connect #'ex-disconnect #'ex-input #'ex-loop)

It runs #'ex-connect when you connect, #'ex-disconnect when you disconnect,
and #'ex-input-ex after you finish a command, and #'ex-loop after handling
everyone's input (or after timeout of waiting for input).


For a different kind of example (less interesting, since it takes no user
input), look at QOTDD (https://git.eunichx.us/qotdd).

For a more comprehensive guide to Facilservil, look to USAGE, coming right up.



----------------------------------------
USAGE
----------------------------------------

To use Facilservil, just use the `facilservil:server` function somewhere.

"host" and "port" are, obviously, the host-IP and port, respectively.
"on-connect"	is the function that will be executed when a user connects.
"on-disconnect"	is the function that will run when someone disconnects.
"on-input"	is the function that will run when someone sends a command
"on-loop"	is the function running when all input is complete/timeouts


Basically, you write the "connecting", "disconnecting", and "input-handler"
functions (maybe "halting"), and you've got a handy-dandy server.

These functions you write must accept the following arguments:
        on-connect	(connection connection-list)
        on-disconnect	(connection connection-list)
        on-input	(connection input-string connection-list)
	on-loop    	(connection-list)


You can use #'send to send strings to a given connection.

Each connection has a built-in hashtable (for storing user-IDs, usernames,
whatever you need). You can store/set a variable with #'bury, and retrieve with
#'dig.



----------------------------------------
BORING STUFF
----------------------------------------
Based on Trout's server.
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/facilservil.git
