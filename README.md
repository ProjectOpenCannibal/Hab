<pre>Hab (Haskell Bot)
======================

A simple IRC bot written in haskell (clever name huh?).

This is a current work in progress, documentation may not be current.

To run Hab in your own IRC channel first make sure you have ghc and the Haskell Platform installed on your PC.
http://www.haskell.org/platform/

After that is complete clone this repo and reconfigure the connection variables found in Socket.hs

The following variables need to be modified before running Hab

	server (optional, if using freenode leave as is)
	port (dependent on server settings, default is 6667)
	chan (a default channel to join)
	nick (change the Nick to something other than Hab; this is registered on Freenode)
	realname (this should reference the details of your bot)
	deftopic (a default topic for your main channel, give bot operator privs to use)
	source (change if you want to point to your source)

The Admin names in Eval.hs will also need to be updated if the bot is to respond to anyone.

run Hab using the following:

	ghci Main.hs
	runbot

For a listing of general commands just talk to Hab either in channel or private message and include '!commands' in your message

For a listing of admin commands (assuming you are an admin) private message Hab '~commands'

In addition to the above commands there are also 3 non-listed commands at the top of eval (if you edited eval you'll already know this)
